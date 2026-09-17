#' Merge u,v,D parameters
#' Function to merge UV and D parameter data frames together
#'
#' @param uvpar dataframe of u and v advection parameters from \code{\link{get.allpar}}
#' @param Dpar dataframe of u and v advection parameters from \code{\link{get.allpar}}
#' @param track tagID index. If merging a single (or group) track/tagid, this is ithe tagid to merge. Otherwise, all records are merged
#' @param return.mean logical; should a mean set of values per month be returned? If False, mean values for each month, for each fish, are returned.
#'
#' @return data frame of: Month, u, v, D, sd.u, sd.v, sd.D
#' @seealso \code{\link{get.kfD}} \code{\link{get.uv}} \code{\link{get.allpar}}
#' @author Benjamin Galuardi
#' @export
#' @rawNamespace export(merge.par)
#'
#' @examples
#' data("nsfish")
#' uvpar = get.allpar(as.data.frame(nsfish))
#' Dpar = get.kfD(as.data.frame(nsfish))
#' simpar = merge.par(uvpar, Dpar, return.mean=T)
#'
merge.par <- function(uvpar, Dpar, track = NULL, return.mean = FALSE) {
  uvpar <- as.data.frame(uvpar)
  Dpar <- as.data.frame(Dpar)

  # Fast vectorized match by TagID and Month
  tag_col_uv <- intersect(c("TagID", "tagid"), names(uvpar))[1]
  tag_col_d  <- intersect(c("TagID", "tagid"), names(Dpar))[1]

  key_uv <- paste(uvpar[[tag_col_uv]], uvpar$Month, sep = "_")
  key_d  <- paste(Dpar[[tag_col_d]], Dpar$Month, sep = "_")

  m <- match(key_uv, key_d)
  matched <- !is.na(m)
  if (any(matched)) {
    uvpar$D[matched] <- Dpar$D[m[matched]]
    uvpar$nrec[matched] <- Dpar$nrec[m[matched]]
  }

  calc_month_stats <- function(df) {
    df |>
      dplyr::group_by(Month) |>
      dplyr::summarise(
        u    = weighted.mean(u, nrec, na.rm = TRUE) * -1,
        v    = weighted.mean(v, nrec, na.rm = TRUE),
        D    = weighted.mean(D, nrec, na.rm = TRUE),
        sd.u = sqrt(weighted.var(u, nrec, na.rm = TRUE)),
        sd.v = sqrt(weighted.var(v, nrec, na.rm = TRUE)),
        sd.D = sqrt(weighted.var(D, nrec, na.rm = TRUE)),
        .groups = "drop"
      ) |>
      as.data.frame()
  }

  if (!is.null(track)) {
    tpar <- uvpar[uvpar[[tag_col_uv]] == track, , drop = FALSE]
    parmean1 <- calc_month_stats(tpar)
  }

  if (return.mean) {
    parmean2 <- calc_month_stats(uvpar)

    if (!is.null(track)) {
      tpar.merge <- merge(parmean1, parmean2, by = "Month", all = TRUE)
      nidx <- is.na(tpar.merge[, 2])
      tpar.merge[nidx, 2:4] <- tpar.merge[nidx, 8:10]
      tpar.merge[, 5:7] <- tpar.merge[, 11:13]
      tparmean <- data.frame(tpar.merge[, 1:7], global.value = as.logical(nidx))
      names(tparmean) <- c("Month", "u", "v", "D", "sd.u", "sd.v", "sd.D", "global")
      return(tparmean)
    } else {
      return(parmean2)
    }
  } else {
    uvpar$u <- uvpar$u * -1
    return(uvpar)
  }
}
