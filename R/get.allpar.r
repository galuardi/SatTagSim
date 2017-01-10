#' Get u and v parameterss
#'
#' Function to get monthly u and v advection parameters for a group of tracks
#'
#' @param tracks A group of tracks returned from Kalman filter estimation. Columns must be in the following order: Day, Month, Year, Longitude, Latitude
#' @param parallel Logical. if parallel, uses plyr built in parallel capabilities
#'
#' @return a data frame of TagID, Month, u, v and a default value of D (diffusion) for each track for each month. This result is a pre requisite for \code{\link{merge.par}}
#' @seealso \code{\link{merge.par}} \code{\link{get.uv}}
#' @author Benjamin Galuardi
#' @export
#'
#' @examples
#' see Vignettes
get.allpar <- function (tracks = nsfish, parallel = F){
  # require(plyr)
  getuvsub = function(temp){
    xx = temp[, c("Day", "Month", "Year", "Longitude", "Latitude",
                  "MaxTemp")]
    xx$MaxTemp = 24
    tab = table(xx$Month)
    rem.mon = as.numeric(attributes(tab)$dimnames[[1]])[tab <
                                                          5]
    if (length(rem.mon) >= 1) {
      for (j in 1:length(rem.mon)) {
        xx = xx[xx$Month != rem.mon[j], ]
      }
    }
    pars = ddply(xx, "Month", function(x) get.uv(x), .parallel=parallel)
  }
  allpar = NULL
  # pb <- txtProgressBar(style = 3, ...)
  # tl = length(tnames)
  # ii = 1/tl
  # Sys.sleep(.5)
  allpar = ldply(dlply(tracks, 'TagID', function(x) getuvsub(x), .parallel=parallel)	)
  # Sys.sleep(1)
  # close(pb)
  names(allpar)[2:4] = c('Month',"u", "v")
  allpar = allpar[,1:4]
  allpar$D = 500 # default value.. will be substituted later
  allpar$nrec = 0 # default value.. will be substituted later
  allpar
}
