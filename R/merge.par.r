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
#'
#' @examples
#' data("nsfish")
#' uvpar = get.allpar(as.data.frame(nsfish))
#' Dpar = get.kfD(as.data.frame(nsfish))
#' simpar = merge.par(uvpar, Dpar, return.mean=T)
#'
merge.par <- function (uvpar, Dpar, track = NULL, return.mean = F)
{
  for (i in 1:12) {
    for (j in unique(uvpar$TagID)) {
      idx1 = which(uvpar$Month == i & uvpar$TagID == j)
      idx2 = which(Dpar$Month == i & Dpar$TagID == j)
      if (length(idx1) > 0 & length(idx2) > 0) {
        uvpar$D[idx1] = Dpar$D[idx2]
        uvpar$nrec[idx1] = Dpar$nrec[idx2]
      }
    }
  }
  if (!is.null(track)) {
    tpar = subset(uvpar, tagid == track)
    parmean1 = data.frame(
      Month = as.numeric(levels(as.factor(tpar$Month))),
      u = daply(
        tpar, .variables = c("Month"), .fun = function(x)
          weighted.mean(x$u,
                        x$nrec, na.rm = T)
      ) * -1, v = daply(
        tpar, .variables = c("Month"),
        .fun = function(x)
          weighted.mean(x$v, x$nrec,
                        na.rm = T)
      ), D = daply(
        tpar, .variables = c("Month"),
        .fun = function(x)
          weighted.mean(x$D, x$nrec,
                        na.rm = T)
      ), sd.u = sqrt(daply(
        tpar, .variables = c("Month"),
        .fun = function(x)
          weighted.var(x$u, x$nrec,
                       na.rm = T)
      )), sd.v = sqrt(daply(
        tpar, .variables = c("Month"),
        .fun = function(x)
          weighted.var(x$v, x$nrec,
                       na.rm = T)
      )), sd.D = sqrt(daply(
        tpar, .variables = c("Month"),
        .fun = function(x)
          weighted.var(x$D, x$nrec,
                       na.rm = T)
      ))
    )
  }
  if (return.mean) {
    parmean2 = data.frame(
      Month = as.numeric(levels(as.factor(uvpar$Month))),
      u = daply(
        uvpar, .variables = c("Month"), .fun = function(x)
          weighted.mean(x$u,
                        x$nrec, na.rm = T)
      ) * -1, v = daply(
        uvpar, .variables = c("Month"),
        .fun = function(x)
          weighted.mean(x$v, x$nrec,
                        na.rm = T)
      ), D = daply(
        uvpar, .variables = c("Month"),
        .fun = function(x)
          weighted.mean(x$D, x$nrec,
                        na.rm = T)
      ), sd.u = sqrt(daply(
        uvpar, .variables = c("Month"),
        .fun = function(x)
          weighted.var(x$u, x$nrec,
                       na.rm = T)
      )), sd.v = sqrt(daply(
        uvpar, .variables = c("Month"),
        .fun = function(x)
          weighted.var(x$v, x$nrec,
                       na.rm = T)
      )), sd.D = sqrt(daply(
        uvpar, .variables = c("Month"),
        .fun = function(x)
          weighted.var(x$D, x$nrec,
                       na.rm = T)
      ))
    )
    if (!is.null(track)) {
      tpar.merge = merge(parmean1, parmean2, by = "Month",
                         all = T)
      nidx = is.na(tpar.merge[, 2])
      tpar.merge[nidx, 2:4] = tpar.merge[nidx, 8:10]
      tpar.merge[, 5:7] = tpar.merge[, 11:13]
      tparmean = data.frame(tpar.merge[, 1:7], global.value = as.logical(nidx))
      parnames = c("Month", "u", "v", "D", "sd.u", "sd.v",
                   "sd.D", "global")
      names(tparmean) = parnames
      tparmean
    }
    else {
      parmean2
    }
  }
  else {
    uvpar$u = uvpar$u * -1
    uvpar
  }
}
