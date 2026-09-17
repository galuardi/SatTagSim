#' Transition probability matrix generator
#'
#' This function is designed to run on a data frame of many simulated tracks. It loops through each one to get transitions and sums the probabilities for the entire set of simulated tracks. This function uses the first area occupied at the beginning of the time period, and transitions to the first occupied area in the next time period.
#'
#' @param datbox data frame returned from \code{\link{get.first.box}}
#' @param nyears dummy variable that sets up a dataframe to be filled. Should be greater than the number of years simulated
#' @param adims array dimensions. For a 7-box area and 4 seasons, c(7, 7, 4)
#' @param perc logical. Return results as a percentage of the row total
#'
#' @return a list of transition probabilities for each time period. The row index represents the previous area and the column represnts the current area for the given time period.
#' @export
#'
#' @examples
#' see vignette

get.trans.prob <- function(datbox, nyears = 100, adims = c(7, 7, 4), perc = TRUE, ...) {
  dname3 <- if (adims[3] == 4) c("Winter", "Spring", "Summer", "Fall") else as.character(seq_len(adims[3]))

  allmat <- array(
    0,
    dim = adims,
    dimnames = list(as.character(seq_len(adims[1])), as.character(seq_len(adims[2])), dname3)
  )

  tab <- table(datbox$pbox, datbox$cbox, datbox$season)
  n_seas <- dim(allmat)[3]

  for (i in seq_len(min(n_seas, dim(tab)[3]))) {
    r_match <- match(rownames(tab[, , i]), dimnames(allmat)[[1]])
    c_match <- match(colnames(tab[, , i]), dimnames(allmat)[[2]])
    valid_r <- !is.na(r_match)
    valid_c <- !is.na(c_match)
    allmat[r_match[valid_r], c_match[valid_c], i] <- tab[valid_r, valid_c, i]
  }

  fillone <- function(trans) {
    zero_rows <- which(rowSums(trans) == 0)
    if (length(zero_rows) > 0) {
      diag(trans)[zero_rows] <- 1
    }
    trans
  }

  tmat <- vector("list", n_seas)
  names(tmat) <- dname3

  for (i in seq_len(n_seas)) {
    mat_i <- allmat[, , i]
    if (perc) {
      r_sums <- rowSums(mat_i)
      mat_i <- mat_i / r_sums
      mat_i[is.nan(mat_i)] <- 0
      mat_i <- fillone(mat_i)
    }
    tmat[[i]] <- mat_i
  }

  tmat
}
