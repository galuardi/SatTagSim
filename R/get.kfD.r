#' Get D from KF estimates
#'
#' Function to add diffusion (D) and sd(D) from Kalman filter estimations. D and sd(D) must be added  to the track as columns beforehand. D is taken from kalmn filter estimte or is a fixed value used in the state space estimation process
#'
#' @param tracks Track(s) returned from Kalman filter estimation. Columns must be in the following order: Day, Month, Year, Longitude, Latitude and include D, sd(D), Month and TagID (exactly as spelled and capitalized)
#'
#' @return a data frame of Month, TagID,  D, Dsd, nrec (number of records)
#' @seealso \code{\link{merge.par}} \code{\link{get.uv}} \code{\link{get.allpar}}
#' @author Benjamin Galuardi
#' @export
#'
#' @examples
#' see vignette
get.kfD <- function(tracks = nsfish){
  nrec.df = ddply(as.data.frame(tracks), .(Month, TagID), function(x) c(mean(x$D, na.rm=T), mean(x$Dsd, na.rm=T), nrow(x)))
  names(nrec.df)[3:5] = c('D','Dsd','nrec')
  # nrec.df = ddply(tracks, c('TagID', 'Month'), function(x) nrow(x))
  # names(nrec.df)[3] = 'nrec'

  # REPLACE UD D WITH KF ESTIMATED D
  # pidx = match(nrec.df$TagID, tracks$TagID)  # matches parameters from KF to the allpar object
  # nrec.df$D  = tracks[pidx,'D']
  # nrec.df$Dsd  = tracks[pidx,'Dsd']

  # substitute NA's with mean value for all fish
  meanD = mean(nrec.df$D, na.rm=T)
  nrec.df$D[is.na(nrec.df$D)] = meanD
  meanDsd = mean(nrec.df$Dsd, na.rm=T)
  nrec.df$Dsd[is.na(nrec.df$Dsd)] = meanDsd
  nrec.df$nrec[is.na(nrec.df$nrec)] = 0
  nrec.df
}
