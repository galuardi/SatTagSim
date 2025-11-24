make.sim.track.par2_rcpp <- function(par_array, simorder, sp, bath = NULL,
                                     sstmat = NULL, boxmat, seaslen = 30,
                                     sstol = 2, mcoptions = setup.parallel(), ...)
{
  require(SatTagSim)
  require(MASS) # for mvrnorm used by .get.samp

  # Precompute grid vectors/matrices to avoid repeated which.min
  box_lon <- as.vector(boxmat$lon); box_lat <- as.vector(boxmat$lat)

  # Fast nearest index using Rcpp nearest_index (returns 0-based)
  box_index <- function(lon, lat) {
    xi0 <- nearest_index(box_lon, lon)
    yi0 <- nearest_index(box_lat, lat)
    c(xi0 + 1, yi0 + 1) # convert to R 1-based
  }

  parbox <- as.numeric(attributes(par_array)$dimnames[[1]])

  if(!is.null(bath)) {
    bath_lon <- as.vector(bath$lon); bath_lat <- as.vector(bath$lat)
    get_bath_val <- function(lon, lat) {
      xi <- nearest_index(bath_lon, lon) + 1
      yi <- nearest_index(bath_lat, lat) + 1
      bath$data[yi, xi]
    }
  } else {
    get_bath_val <- function(...) NA_real_
  }

  if(!is.null(sstmat)) {
    sst_lon <- as.vector(sstmat$lon)
    sst_lat <- as.vector(sstmat$lat)
    # sstgrid vector ordering must match find_next_sst_cpp expectation (expand.grid(sst_lon, sst_lat))
    # sst_month_vec <- as.vector(sstmat$data[,,month]) will work if sstmat$data is [lon, lat, month] with lon varying fastest
  }

  .get.samp <- function (vec, npoints = 1, ci = 0.95) {
    vec <- as.numeric(vec)
    Sigma <- matrix(vec[1:4], 2, 2) * ci
    mu <- c(vec[5:6])
    if (sum(Sigma) > 0) {
      mvrnorm(npoints, mu, Sigma)
    } else mu
  }

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

  # wrapper for Rcpp find_next_sst_cpp
  find.next.sst_rcpp <- function(lon, lat, sst_month_vec, expand = 10, max_iter = 20) {
    res <- find_next_sst_cpp(as.numeric(lon), as.numeric(lat),
                             sst_lon, sst_lat, as.numeric(sst_month_vec),
                             as.numeric(sstol), as.numeric(expand), as.integer(max_iter))
    as.numeric(res)
  }

  out <- foreach(i = sp, .options.multicore = mcoptions, .packages = c("SatTagSim","MASS")) %dopar% {
    msp <- if(!is.null(bath)) getsp(i, bath = bath) else as.numeric(i)
    uvmult <- 30 / seaslen

    maxrows <- length(simorder) * seaslen
    temp_mat <- matrix(NA_real_, nrow = maxrows, ncol = 3)
    rowi <- 0

    for(seas in simorder) {
      if(!is.null(sstmat)) {
        sst_month_vec <- as.vector(sstmat$data[,,seas])
      }

      for(j in seq_len(seaslen)) {
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
        if(length(pbidx) == 0) next

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

        if(!is.null(sstmat)) {
          ii <- 1
          # local accessor for sst value at a point via nearest index
          get_sst_val_at <- function(lon_, lat_, month_) {
            xi <- nearest_index(sst_lon, lon_) + 1
            yi <- nearest_index(sst_lat, lat_) + 1
            sstmat$data[xi, yi, month_]
          }

          if(get_sst_val_at(t1[1], t1[2], seas) < sstol) {
            t1 <- SatTagSim::simm.kf(2,
                                     u = c(-uvec[1], uvec[2]),
                                     v = c(-vvec[1], vvec[2]),
                                     D = c(Dvec[1], 1000),
                                     msp, ulim, vlim, Dlim)[2, ]
            t1 <- as.numeric(t1)
          }

          while(get_sst_val_at(t1[1], t1[2], seas) < sstol) {
            samp <- find.next.sst_rcpp(t1[1], t1[2], sst_month_vec, expand = 5 * ii, max_iter = 20)
            t1 <- as.numeric(samp)
            ii <- ii + 1
            if(ii > 20) break
          }
        }

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
