#' Make seasonal index of tracks
#'
#' @param btracks
#'
#' @return a list of logical indices by season
#' @export
#' @details btracks must have a 'Month' column
#' @examples
#' see vignette
make.seas.idx <- function (btracks)
{
  winidx = btracks$Month == 1 | btracks$Month == 2 | btracks$Month ==
    3
  spridx = btracks$Month == 4 | btracks$Month == 5 | btracks$Month ==
    6
  sumidx = btracks$Month == 7 | btracks$Month == 8 | btracks$Month ==
    9
  fallidx = btracks$Month == 10 | btracks$Month == 11 | btracks$Month ==
    12
  seasidx = list(winter = winidx, spring = spridx, summer = sumidx,
                 fall = fallidx)
  seasidx
}
