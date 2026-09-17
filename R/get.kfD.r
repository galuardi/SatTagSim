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
get.kfD <- function(tracks = nsfish) {
  df <- as.data.frame(tracks)
  nrec.df <- df |>
    dplyr::group_by(Month, TagID) |>
    dplyr::summarise(
      D = mean(D, na.rm = TRUE),
      Dsd = mean(Dsd, na.rm = TRUE),
      nrec = dplyr::n(),
      .groups = "drop"
    ) |>
    as.data.frame()

  meanD <- mean(nrec.df$D, na.rm = TRUE)
  meanDsd <- mean(nrec.df$Dsd, na.rm = TRUE)
  if (!is.nan(meanD)) nrec.df$D[is.na(nrec.df$D)] <- meanD
  if (!is.nan(meanDsd)) nrec.df$Dsd[is.na(nrec.df$Dsd)] <- meanDsd
  nrec.df$nrec[is.na(nrec.df$nrec)] <- 0
  nrec.df
}
