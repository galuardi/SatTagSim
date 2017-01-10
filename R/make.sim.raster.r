#' Make sim raster
#'
#' Make a raster of simulation results

#' @param simdat Simulated track returned from \code{\link{make.sim.track.par}}
#' @param xmn longitude minimum
#' @param xmx longitude maximum
#' @param ymn latitude minimum
#' @param ymx latitude maximum
#' @param boxsize size of pixel in nautical miles (minutes)
#'
#' @return raster; cell values are counts of positions per pixel
#' @author Benjamin Galuardi
#' @export
#'
#' @examples
#' see Vignettes
#'
make.sim.raster  <- function(simdat, xmn = -100, xmx = 30, ymn = 0, ymx = 60, boxsize= 60){
  # require(raster)
  r = raster(nrow=(ymx-ymn)*60/boxsize, ncol= (xmx-xmn)*60/boxsize, xmn = xmn, xmx = xmx, ymn = ymn, ymx = ymx)
  simdat$CID = 1
  coordinates(simdat) = ~lon+lat
  sr = rasterize(simdat, r, field = 'CID', fun='count') #[[1]]
  return(sr)
}
