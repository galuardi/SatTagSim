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
get.uv <- function(track = nsfish[1:10,]){

  # CONVERSION FUNCTIONS TO AND FROM NAUTICAL MILES
  z <- function(a1, a2) {
    lon <- -a1/(60 * cos(a2/60 * pi/180)) + lon[1]
    lat <- a2/60
    return(cbind(lon, lat))
  }
  zi <- function(lon, lat) {
    a1 <- -(lon - lon[1]) * 60 * cos(lat * pi/180)
    a2 <- lat * 60
    return(cbind(a1, a2))
  }

  track[track == "NaN"] <- NA
  track[track == "-Inf"] <- NA
  track[track == "Inf"] <- NA
  track <- na.omit(track)
  lon <- track[, 4]
  lat <- track[, 5]
  a1a2 <- zi(lon, lat)
  a1 <- a1a2[, 1]
  a2 <- a1a2[, 2]
  date <- mdy.date(day = track[, 1], month = track[, 2], year = track[,
                                                                      3])
  dayAL <- date - date[1]
  n <- nrow(track)
  u <- (a1[n] - a1[1])/dayAL[n]
  v <- (a2[n] - a2[1])/dayAL[n]
  c(u,v)

}
