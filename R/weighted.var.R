weighted.var <- function(x, w, na.rm = FALSE) {
  if (na.rm) {
    keep <- !is.na(x) & !is.na(w)
    w <- w[keep]
    x <- x[keep]
  }
  sum.w <- sum(w)
  sum.w2 <- sum(w^2)
  denom <- sum.w^2 - sum.w2
  if (is.na(denom) || denom <= 0) return(NA_real_)
  mean.w <- sum(x * w) / sum.w
  (sum.w / denom) * sum(w * (x - mean.w)^2)
}
