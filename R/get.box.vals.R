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
get.box.vals <- function(dat, boxes) {
  dat <- as.data.frame(dat)
  if (ncol(dat) >= 3) {
    names(dat)[1:3] <- c("lon", "lat", "Month")
  }
  sidx <- make.seas.idx(dat)
  dat$season <- NA_integer_
  for (i in 1:4) {
    dat$season[sidx[[i]]] <- i
  }

  boxes_sf <- if (inherits(boxes, "sf")) boxes else sf::st_as_sf(boxes)
  boxes_sf <- sf::st_make_valid(boxes_sf)

  crs_box <- sf::st_crs(boxes_sf)
  if (is.na(crs_box)) crs_box <- 4326

  pts_sf <- sf::st_as_sf(dat, coords = c("lon", "lat"), crs = crs_box, remove = FALSE)
  hits <- sf::st_intersects(pts_sf, boxes_sf)
  idx <- vapply(hits, function(h) if (length(h) > 0) as.integer(h[1]) else NA_integer_, integer(1))

  dat$box <- idx
  dat
}
