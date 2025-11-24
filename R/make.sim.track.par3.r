make.sim.track.par3 <- function(par_array, simorder, sp, bath = NULL,
                                sstmat = NULL, boxmat, seaslen = 30,
                                sstol = 2, mcoptions = setup.parallel(), ...)
{
  require(SatTagSim)
  if(!requireNamespace("geosphere", quietly = TRUE)) {
    use_geosphere <- FALSE
  } else use_geosphere <- TRUE

  # Helper functions defined once
  deg2rad <- function(deg) deg * pi / 180
  gcd.hf_vec <- function(lon1, lat1, lon2, lat2) {
    R <- 6371
    long1 <- deg2rad(lon1); lat1 <- deg2rad(lat1)
    long2 <- deg2rad(lon2); lat2 <- deg2rad(lat2)
    dl <- long2 - long1; dt <- lat2 - lat1
    a <- sin(dt/2)^2 + cos(lat1) * cos(lat2) * sin(dl/2)^2
    c <- 2 * asin(pmin(1, sqrt(a)))
    R * c
  }

  # Precompute grid vectors/matrices to avoid repeated which.min
  box_lon <- as.vector(boxmat$lon); box_lat <- as.vector(boxmat$lat)
  # create function to map lon/lat to indices on boxmat grid quickly using nearest index via which.min
  box_index <- function(lon, lat) {
    xidx <- which.min((lon - box_lon)^2)
    yidx <- which.min((lat - box_lat)^2)
    c(xidx, yidx)
  }

  # Precompute parbox vector
  parbox <- as.numeric(attributes(par_array)$dimnames[[1]])

  # Precompute bath grid vectors if bath provided
  if(!is.null(bath)) {
    bath_lon <- as.vector(bath$lon); bath_lat <- as.vector(bath$lat)
    get_bath_val <- function(lon, lat) {
      xi <- which.min((lon - bath_lon)^2)
      yi <- which.min((lat - bath_lat)^2)
      bath$data[yi, xi]
    }
  } else {
    get_bath_val <- function(...) NA_real_
  }

  # Precompute sst grid coordinates for quick subset when sstmat present
  if(!is.null(sstmat)) {
    sst_lon <- as.vector(sstmat$lon); sst_lat <- as.vector(sstmat$lat)
    # matrix of coordinates for geosphere distance calls
    sst_grid_coords <- as.matrix(expand.grid(sst_lon, sst_lat))
    names(sst_grid_coords) <- NULL
  }

  # sample multivariate function once
  .get.samp <- function (vec, npoints = 1, ci = 0.95) {
    vec <- as.numeric(vec)
    Sigma <- matrix(vec[1:4], 2, 2) * ci
    mu <- c(vec[5:6])
    if (sum(Sigma) > 0) {
      mvrnorm(npoints, mu, Sigma)
    } else mu
  }

  # function to find nearest sst point >= sstol within expanding box
  find.next.sst_fast <- function(lon, lat, sst_month_data, sst_lon, sst_lat, sst_grid_coords, sstol, expand = 10) {
    # subset by bounding box
    xlim <- c(lon - expand, lon + expand)
    ylim <- c(lat - expand, lat + expand)
    xok <- sst_lon >= xlim[1] & sst_lon <= xlim[2]
    yok <- sst_lat >= ylim[1] & sst_lat <= ylim[2]
    # indices in expanded grid: need to build indices for expand.grid ordering
    # but simpler: filter sst_grid_coords by x/y bounds
    mask <- sst_grid_coords[,1] >= xlim[1] & sst_grid_coords[,1] <= xlim[2] &
            sst_grid_coords[,2] >= ylim[1] & sst_grid_coords[,2] <= ylim[2]
    if(!any(mask)) return(c(lon, lat))
    sub_coords <- sst_grid_coords[mask, , drop = FALSE]
    sub_vals <- sst_month_data[mask]

    good_idx <- which(sub_vals >= sstol)
    if(length(good_idx) == 0) return(c(lon, lat))

    if(use_geosphere) {
      dists <- geosphere::distHaversine(cbind(lon, lat), sub_coords)/1000
    } else {
      dists <- mapply(function(xy) gcd.hf_vec(lon, lat, xy[1], xy[2]), split(sub_coords, row(sub_coords)))
    }

    # choose one of the nearest 10% or nearest if few points
    ord <- order(dists)
    top_n <- max(1, ceiling(length(ord) * 0.1))
    idx <- sample(ord[seq_len(top_n)], 1)
    as.numeric(sub_coords[idx, ])
  }

  # Function to get initial sample avoiding bath
  getsp <- function(sp_coord, var = c(1,0,0,1), bath_obj = bath) {
    vec <- c(var, sp_coord)
    x <- .get.samp(vec, 1)
    if(!is.null(bath_obj)) {
      while(get_bath_val(x[1], x[2]) > 0) {
        vec <- vec + c(.5, 0, 0, .5, 0, 0)
        x <- .get.samp(vec, 1)
      }
    }
    x
  }

  # Parallel loop
  out <- foreach(i = sp, .options.multicore = mcoptions, .packages = c("SatTagSim", "MASS")) %dopar% {
    # local copies for speed
    msp <- if(!is.null(bath)) getsp(i, bath = bath) else as.numeric(i)
    uvmult <- 30 / seaslen

    # preallocate maximum rows: length(simorder)*seaslen, each row lon,lat,month
    maxrows <- length(simorder) * seaslen
    temp_mat <- matrix(NA_real_, nrow = maxrows, ncol = 3)
    rowi <- 0

    for(seas in simorder) {
      # build sst month values once
      if(!is.null(sstmat)) {
        sst_month_data <- as.vector(sstmat$data[,,seas])
      }

      for(j in seq_len(seaslen)) {
        # fast box lookup
        bi <- box_index(msp[1], msp[2])
        xidx <- bi[1]; yidx <- bi[2]
        tbox <- boxmat$box[xidx, yidx]

        if(is.na(tbox)) {
          if(j == 1) {
            msp <- if(!is.null(bath)) getsp(i, bath = bath) else as.numeric(i)
            bi <- box_index(msp[1], msp[2]); xidx <- bi[1]; yidx <- bi[2]
            tbox <- boxmat$box[xidx, yidx]
          } else {
            msp <- temp_mat[rowi, 1:2]
            bi <- box_index(msp[1], msp[2]); xidx <- bi[1]; yidx <- bi[2]
            tbox <- boxmat$box[xidx, yidx]
          }
        }

        pbidx <- which(parbox == tbox)
        # defensive: if no pbidx, skip step
        if(length(pbidx) == 0) {
          next
        }

        u <- par_array[pbidx, seas, 1] * uvmult
        v <- par_array[pbidx, seas, 2] * uvmult
        D <- par_array[pbidx, seas, 3]
        usd <- par_array[pbidx, seas, 4]
        vsd <- par_array[pbidx, seas, 5]
        Dsd <- par_array[pbidx, seas, 6]

        uvec <- c(u, usd); vvec <- c(v, vsd); Dvec <- c(D, Dsd)

        ulim <- c(-50, 50) * uvmult
        vlim <- c(-50, 50) * uvmult
        Dlim <- c(0, 5000)

        t1 <- SatTagSim::simm.kf(2, uvec, vvec, Dvec, msp, ulim, vlim, Dlim)[2, ]
        t1 <- as.numeric(t1)

        # SST checks
        if(!is.null(sstmat)) {
          ii <- 1
          if(get.sst.mask.val <- function(lon, lat, mask, month) {
               # local simple index lookup using sstmat lon/lat vectors
               xi <- which.min((lon - sst_lon)^2)
               yi <- which.min((lat - sst_lat)^2)
               mask$data[xi, yi, month]
             }; get.sst.mask.val(t1[1], t1[2], sstmat, seas) < sstol) {
            # reverse advective attempt
            t1 <- SatTagSim::simm.kf(2, u = c(-uvec[1], uvec[2]), v = c(-vvec[1], vvec[2]),
                                     D = c(Dvec[1], 1000), msp, ulim, vlim, Dlim)[2, ]
            t1 <- as.numeric(t1)
          }
          while(get.sst.mask.val(t1[1], t1[2], sstmat, seas) < sstol) {
            samp <- find.next.sst_fast(t1[1], t1[2], sst_month_data, sst_lon, sst_lat,
                                      sst_grid_coords, sstol, expand = 5 * ii)
            t1 <- as.numeric(samp)
            ii <- ii + 1
            if(ii > 20) break
          }
        }

        # bath check
        if(!is.null(bath)) {
          while(get_bath_val(t1[1], t1[2]) > 0) {
            t1 <- SatTagSim::simm.kf(2, uvec, vvec, Dvec, msp, ulim, vlim, Dlim)[2, ]
            t1 <- as.numeric(t1)
          }
        }

        rowi <- rowi + 1
        temp_mat[rowi, ] <- c(t1[1], t1[2], seas)
        msp <- ifelse(is.na(t1), msp, t1)
      }
    }

    if(rowi == 0) return(data.frame(lon = numeric(0), lat = numeric(0), Month = integer(0)))
    tsim <- as.data.frame(temp_mat[1:rowi, , drop = FALSE])
    names(tsim) <- c("lon", "lat", "Month")
    tsim
  }

  out
}
