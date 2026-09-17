#' Get u and v subfunction
#'
#' Get advective parameters from an estimated track or section of track
#' @param track track returned from Kalman filter estimation. Columns must be in the following order: Day, Month, Year, Longitude, Latitude
#'
#' @return u and v advection for the track, or section of track.
#' @seealso \code{\link{merge.par}} \code{\link{get.kfD}} \code{\link{get.allpar}}
#' @author Benjamin Galuardi
#' @export
#'
#' @examples
#' none. Typically used within other functions
get.uv <- function(track = nsfish[1:10, ]) {
  track <- as.data.frame(track)
  # Keep only complete, finite rows for the required 5 columns
  valid <- complete.cases(track[, 1:5]) &
    is.finite(track[, 1]) & is.finite(track[, 2]) & is.finite(track[, 3]) &
    is.finite(track[, 4]) & is.finite(track[, 5])
  track <- track[valid, , drop = FALSE]
  n <- nrow(track)
  if (n < 2) return(c(u = NA_real_, v = NA_real_))

  lon <- track[, 4]
  lat <- track[, 5]

  # Conversion to nautical miles
  a1_n <- -(lon[n] - lon[1]) * 60 * cos(lat[n] * pi / 180)
  a2_diff <- (lat[n] - lat[1]) * 60

  dates <- as.Date(paste(track[, 3], track[, 2], track[, 1], sep = "-"))
  dayAL_n <- as.numeric(dates[n] - dates[1])
  if (is.na(dayAL_n) || dayAL_n == 0) return(c(u = NA_real_, v = NA_real_))

  u <- a1_n / dayAL_n
  v <- a2_diff / dayAL_n
  c(u = unname(u), v = unname(v))
}
