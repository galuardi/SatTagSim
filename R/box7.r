#' Datasets used in SatTagSim of 7-box schema of the north Atlantic
#'
#' \itemize{
#' \item 7 box schema used in the Kerr et al. operational model
#' \item 8 box schema modified from Kerr et al. operational model
#' \item Longhurst areas for North Atlantic and World
#' \item 10 degree boxes
#' }
#' @aliases box8 lha.WORLD lha.NATL wgridspp
#' @export
#' @usage
#' data(box7)
#' data(box8)
#' data(lha.w)
#' data(lha.natl)
#' data(wgridspp)

#' @format SpatialPointsDataFrame
#' @examples
#' library(fields)
#' data(box7)
#' plot(box7, border = 'salmon', lwd=2)
#' world(add = T)
"box7"
