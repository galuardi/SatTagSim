# ' Raster mask to array
#' Function to tranfrom a raster brick of sst to a 3d array and orient properly
#' @param rmask raster mask of monthly suitable temperatures fro Atlantic bluefin tuna
#'
#' @return a 3D array
#' @export
#'
#' @examples
#' data(rmask)
#' sst = rmask2array(rmask)
rmask2array <- function(rmask) {
  if (inherits(rmask, "SpatRaster")) {
    xy <- terra::crds(rmask[[1]])
    arr <- terra::as.array(rmask)
  } else {
    xy <- raster::coordinates(rmask[[1]])
    if (raster::hasValues(rmask)) {
      arr <- raster::as.array(rmask)
    } else {
      dims <- c(raster::nrow(rmask), raster::ncol(rmask), raster::nlayers(rmask))
      arr <- array(NA_real_, dim = dims)
    }
  }
  sstmat <- list(
    lon = sort(unique(xy[, 1])),
    lat = sort(unique(xy[, 2]))
  )
  d <- dim(arr)
  sstdata <- array(NA_real_, dim = c(d[2], d[1], d[3]))
  for (i in seq_len(d[3])) {
    m <- arr[, , i]
    sstdata[, , i] <- t(m[nrow(m):1, , drop = FALSE])
  }
  sstmat$data <- sstdata
  sstmat
}
