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
#'
get.allpar <- function(tracks = nsfish, parallel = FALSE) {
  tracks <- as.data.frame(tracks)
  tag_list <- split(tracks, tracks$TagID)

  calc_tag <- function(temp) {
    tab <- table(temp$Month)
    keep_months <- as.numeric(names(tab)[tab >= 5])
    temp <- temp[temp$Month %in% keep_months, c("Day", "Month", "Year", "Longitude", "Latitude")]
    if (nrow(temp) == 0) return(NULL)

    mon_list <- split(temp, temp$Month)
    res_list <- lapply(names(mon_list), function(m) {
      uv <- get.uv(mon_list[[m]])
      data.frame(Month = as.numeric(m), u = unname(uv[1]), v = unname(uv[2]))
    })
    do.call(rbind, res_list)
  }

  all_list <- lapply(names(tag_list), function(tid) {
    res <- calc_tag(tag_list[[tid]])
    if (!is.null(res) && nrow(res) > 0) {
      data.frame(TagID = tid, res, stringsAsFactors = FALSE)
    } else {
      NULL
    }
  })

  allpar <- do.call(rbind, all_list)
  if (is.null(allpar) || nrow(allpar) == 0) {
    return(data.frame(TagID = character(0), Month = numeric(0), u = numeric(0), v = numeric(0), D = numeric(0), nrec = numeric(0)))
  }
  allpar$D <- 500
  allpar$nrec <- 0
  rownames(allpar) <- NULL
  allpar
}
