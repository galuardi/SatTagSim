#' Make seasonal index of tracks
#'
#' @param btracks
#'
#' @return a list of logical indices by season
#' @export
#' @details btracks must have a 'Month' column
#' @examples
#' see vignette
make.seas.idx <- function(btracks) {
  m <- as.numeric(btracks$Month)
  list(
    winter = m %in% 1:3,
    spring = m %in% 4:6,
    summer = m %in% 7:9,
    fall   = m %in% 10:12
  )
}
