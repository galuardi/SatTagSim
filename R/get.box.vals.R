#' Get overlay of boxes from a track
#'
#' @param dat a dataframe track with lon, lat and Month columns
#' @param boxes spatial polygons of overlay strata
#'
#' @return
#' @export
#' @seealso  \code{\link{get.first.box}}
#' @examples
#'
get.box.vals <-
function(dat, boxes){
	dat = as.data.frame(dat)
	names(dat)= c('lon','lat','Month')
	sidx = make.seas.idx(dat)
	dat$season = NA
for(i in 1:4) dat$season[sidx[[i]]]=i
	coordinates(dat) = ~lon+lat
	proj4string(dat) <- boxes@proj4string
	idx=over(dat, as(boxes, "SpatialPolygons"))
	data.frame(dat, box=idx)
}
