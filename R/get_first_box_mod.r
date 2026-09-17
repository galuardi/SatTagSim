
#' Get First Box 
#'
#' @param simdat
#' @param syear
#' @param boxes
#' @param seas.len
#'
#' @returns
#'
#' @export
#' @examples
#' Modernized First Box Function (Alias)
#'
#' @inheritParams get.first.box
#' @export
get_first_box_mod <- function(simdat, syear = 2000, boxes = box7, seas.len = 90) {
  get.first.box(simdat = simdat, syear = syear, boxes = boxes, seas.len = seas.len)
}
