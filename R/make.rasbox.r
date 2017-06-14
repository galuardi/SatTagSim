#' Rasterize a spatial structure
#' returns either a raster or cell matrix based on a spatial box structure
#'
#' @param boxes spatial polygons
#' @param raster Logical. If False, returns a matrix
#' @param rrow number of rows for output
#' @param rcol number of columns for output
#'
#' @return a raster or matrix
#' @export
#'
#' @examples
#' par(mfrow = c(3,1))
#' data(box11)
#' r = make.rasbox(box11)
#' m = make.rasbox(box11, raster = F)
#' plot(box11, border = 2); world(add = T)
#' plot(r); world(add = T)
#' world(add=F); image.plot(m$lon, m$lat, m$box, add=T)
make.rasbox <- function(boxes = box11, rrow = 26*5, rcol = 29*5, raster = T){
  rasbox = raster(boxes, nrow = rrow, ncol = rcol)
  extent(rasbox) = extent(boxes)

  rasbox = rasterize(boxes, rasbox, 'ID')

  # # fix the bay of biscay
  # rasbox[cellFromXY(rasbox, c(-2.5, 42.5))] = 8
  # rasbox[cellFromXY(rasbox, c(-2.5, 45.5))] = 8

  if(raster ==T){
    rasbox
  }else{
    # make a matrix for speed
    boxmat = list()
    boxmat$lon = unique(coordinates(rasbox)[,1])
    boxmat$lat = sort(unique(coordinates(rasbox)[,2]))
    boxmat$box = t(as.matrix(flip(rasbox, 2)))
    boxmat
    }
}
