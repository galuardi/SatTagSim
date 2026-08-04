#' Simulate tracks in parallel
#'
#' @param par_array
#' @param simorder
#' @param sp
#' @param bath
#' @param sstmat
#' @param boxmat
#' @param seaslen
#' @param sstol
#' @param mcoptions
#' @param ...
#'
#' @returns
#'
#' @export
#' @examples
sim_tracks_par <- function (par_array = par_array, simorder = simorder, sp = spts, 
    bath = bath, sstmat = sstmat, boxmat = boxmat, seaslen = 30, 
    sstol = 2, mcoptions = setup.parallel(), ...) 
# foreach(i = sp, .options.multicore = mcoptions) %do% {
  foreach(i = sp, .options.multicore = mcoptions) %dopar% {
    print(paste("Currently processing i:", paste(i, collapse = ", ")))
    require(SatTagSim)
    .get.samp <- function(vec, npoints, ci = 0.95) {
        vec = as.numeric(vec)
        Sigma <- matrix(vec[1:4], 2, 2) * ci
        mu <- c(vec[5:6])
        if (sum(Sigma) > 0) {
            ndata <- mvrnorm(npoints, mu, Sigma)
            return(ndata)
        }
    }
    .get.bath <- function(lon, lat, BATH) {
        X = as.vector(BATH$lon)
        Y = as.vector(BATH$lat)
        xidx = which.min((lon - X)^2)
        yidx = which.min((lat - Y)^2)
        BATH$data[yidx, xidx]
    }
    getsp <- function(sp, var = c(1, 0, 0, 1), bath = bath) {
        vec = c(var, sp)
        x = .get.samp(vec, 1)
        while (.get.bath(x[1], x[2], BATH = bath) > 0) {
            x = .get.samp(vec, 1)
            vec = c(var + c(0.5, 0, 0, 0.5), sp)
        }
        x
    }
    get.sst.mask.val <- function(lon, lat, mask, month) {
        X = as.vector(mask$lon)
        Y = as.vector(mask$lat)
        xidx = which.min((lon - X)^2)
        yidx = which.min((lat - Y)^2)
        mask$data[xidx, yidx, month]
    }
    deg2rad <- function(deg) return(deg * pi/180)
    gcd.hf <- function(long1, lat1, long2, lat2) {
        R <- 6371
        delta.long <- (long2 - long1)
        delta.lat <- (lat2 - lat1)
        a <- sin(delta.lat/2)^2 + cos(lat1) * cos(lat2) * sin(delta.long/2)^2
        c <- 2 * asin(min(1, sqrt(a)))
        d = R * c
        return(d)
    }
    find.next.sst <- function(lon, lat, sstdf, sstol, expand = 10) {
        names(sstdf) = c("lon", "lat", "sst")
        xlim = c(lon - expand, lon + expand)
        ylim = c(lat - expand, lat + expand)
        xidx = sstdf$lon >= xlim[1] & sstdf$lon <= xlim[2]
        yidx = sstdf$lat >= ylim[1] & sstdf$lat <= ylim[2]
        xyidx = which((xidx + yidx) == 2)
        pt = c(lon, lat)
        sstdf_sub = sstdf[xyidx, ]
        gidx = which(sstdf_sub[, 3] >= sstol)
        if (length(gidx) > 0) {
            dists = apply(sstdf_sub[gidx, 1:2], 1, function(x) gcd.hf(pt[1], 
                pt[2], x[1], x[2]))
            idxmin = sample(which(dists <= quantile(dists, 0.1)), 
                1)
            as.numeric(sstdf_sub[gidx[idxmin], 1:2])
        }
        else {
            c(lon, lat)
        }
    }
    if (!is.null(bath)) {
        msp = getsp(i, bath = bath)
    }
    else {
        msp = as.numeric(i)
    }
    uvmult = 30/seaslen
    temp = NULL
    for (seas in simorder) {
        if (!is.null(sstmat)) {
            sstdf = data.frame(expand.grid(sstmat$lon, sstmat$lat), 
                sst = as.vector(sstmat$data[, , seas]))
        }
        for (j in 1:seaslen) {
            xidx = which.min((msp[1] - boxmat$lon)^2)
            yidx = which.min((msp[2] - boxmat$lat)^2)
            tbox = boxmat$box[xidx, yidx]
if (is.na(tbox)) {
                # If the fish has previous steps, go back exactly one step and only grab Lon/Lat
                if (!is.null(temp) && nrow(temp) > 0) {
                    msp = as.numeric(temp[nrow(temp), 1:2])
                } else {
                    # Only default back to the starting point if it's the absolute first day of January
                    msp = as.numeric(i)
                }
                
                # Recalculate the box using the safe fallback coordinates
                xidx = which.min((msp[1] - boxmat$lon)^2)
                yidx = which.min((msp[2] - boxmat$lat)^2)
                tbox = boxmat$box[xidx, yidx]
                }
          # if (is.na(tbox) & j == 1) {
            #     if (!is.null(bath)) {
            #       msp = getsp(i, bath = bath)
            #     }
            #     else {
            #       msp = as.numeric(i)
            #     }
            #     xidx = which.min((msp[1] - boxmat$lon)^2)
            #     yidx = which.min((msp[2] - boxmat$lat)^2)
            #     tbox = boxmat$box[xidx, yidx]
            # }
            # if (is.na(tbox) & j > 1) {
            #     msp = temp[j - 1, ]
            #     xidx = which.min((msp[1] - boxmat$lon)^2)
            #     yidx = which.min((msp[2] - boxmat$lat)^2)
            #     tbox = boxmat$box[xidx, yidx]
            # }
            parbox = as.numeric(attributes(par_array)$dimnames[[1]])
            pbidx = which(parbox == tbox)
            u = c(par_array[pbidx, seas, 1]) * uvmult
            v = c(par_array[pbidx, seas, 2]) * uvmult
            D = c(par_array[pbidx, seas, 3])
          
          # --- ADD THIS FAILSAFE BLOCK ---
        if (length(pbidx) == 0 || any(is.na(c(u, v, D)))) {
            # We hit a map boundary or a cell with no reference data.
            # Break the loop to stop swimming, but save the track up to this point.
            break
        }
          
            usd = c(par_array[pbidx, seas, 4])
            vsd = c(par_array[pbidx, seas, 5])
            Dsd = c(par_array[pbidx, seas, 6])
            u = c(u, usd)
            v = c(v, vsd)
            D = c(D, Dsd)
            Dorig = D
            uorig = u
            vorig = v
            ulim = c(-50, 50) * uvmult
            vlim = c(-50, 50) * uvmult
            Dlim = c(0, 5000)
# Existing initial move calculation
        t1 = SatTagSim::simm.kf(2, u, v, D, msp, ulim, vlim, Dlim)[2, ]
        
        # --- NEW: THE INVISIBLE WALL REFLECTION ---
        # 1. Check if the proposed t1 lands off the map or in a missing parameter box
        check_xidx = which.min((t1[1] - boxmat$lon)^2)
        check_yidx = which.min((t1[2] - boxmat$lat)^2)
        check_tbox = boxmat$box[check_xidx, check_yidx]
        
        hit_wall <- is.na(check_tbox)
        if (!hit_wall) {
            check_pbidx = which(parbox == check_tbox)
            if (length(check_pbidx) == 0 || is.na(par_array[check_pbidx, seas, 1])) {
                hit_wall <- TRUE
            }
        }
        
        # 2. If it hit the wall, reverse u and v and recalculate the step
        if (hit_wall) {
            # Multiply mean u and v by -1 to bounce the fish backwards.
            # We temporarily boost the diffusion (D[1] = 1000) to help it scatter away from the edge.
            t1 = SatTagSim::simm.kf(2, u = c(-1 * u[1], u[2]), 
                                       v = c(-1 * v[1], v[2]), 
                                       D = c(D[1], 1000), 
                                       msp, ulim, vlim, Dlim)[2, ]
            t1 = as.numeric(t1)
        }
            if (!is.null(sstmat)) {
                ii = 1
                if (get.sst.mask.val(t1[1], t1[2], sstmat, seas) < 
                  sstol) {
                  t1 = SatTagSim::simm.kf(2, u = c(-1 * u[1], 
                    u[2]), v = c(-1 * v[1], v[2]), D = c(D[1], 
                    1000), msp, ulim, vlim, Dlim)[2, ]
                  t1 = as.numeric(t1)
                }
                while ((get.sst.mask.val(t1[1], t1[2], sstmat, 
                  seas)) < sstol) {
                  tsamp = find.next.sst(t1[1], t1[2], sstdf, 
                    sstol, expand = 5 * ii)
                  t1 = as.numeric(tsamp)
                  ii = ii + 1
                }
            }
            if (!is.null(bath)) {
                while (.get.bath(t1[1], t1[2], bath) > 0) {
                  t1 = SatTagSim::simm.kf(2, u, v, D, msp, ulim, vlim, Dlim)[2, ]
                }
            }
            temp = rbind(temp, cbind(t(t1), seas))
            msp = ifelse(is.na(t1), msp, t1)
        }
    }
tsim = as.data.frame(temp)
    
    # Check if the fish actually made any valid steps before renaming
    if (nrow(tsim) > 0) {
        names(tsim) = c("lon", "lat", "Month")
    } else {
        # If the track failed on day 1, return an empty dataframe so parallel doesn't crash
        tsim = data.frame(lon = numeric(0), lat = numeric(0), Month = numeric(0))
    }
    
    tsim
}
