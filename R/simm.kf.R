#' Simulate KF
#'
#' Correlated random walk simluator based on advection diffusion process equation
#'
#' @param n number of points to simulate
#' @param u vector of mean and standard deviation of advection in u (longitude) direction, in nautical miles/day
#' @param v vector of mean and standard deviation of advection in v (latitude) direction, in nautical miles/day
#' @param D vector of mean and standard deviation of Diffusion, in nautical miles^2/day
#' @param sp vector of starting point (lon, lat)
#'
#' @return a two column matrix of longitude and latitude
#' @details
#' uses truncated normal distributions. Bounds are:
#' \tabular{lll}{
#' Parameter \tab lower bound \tab upper bound \cr
#' u \tab -50  \tab 50  \cr
#' v \tab -50  \tab 50  \cr
#' D \tab 0  \tab 5000
#' }
#'
#' @author Benjamin Galuardi
#' @references
#' Sibert, J. R., Musyl, M. K. and Brill, R. W. 2003. Horizontal movements of bigeye tuna (Thunnus obesus) near Hawaii determined by Kalman filter analysis of archival tagging data. Fish. Oceanogr. 12(3): 141?151.
#' Calenge, C. (2006) The package adehabitat for the R software: a tool for the analysis of space and habitat use by animals. Ecological Modelling, 197, 516-519
#' @export
#' @seealso \code{\link{make.sim.track.par}}
#'
#' @examples
#' # Three examples of differing advection and Diffusion parameter combinations
#' par(mfrow=c(1,3))
#' plot(simm.kf(n = 1000, u = c(5,1), v = c(5,1), D = c(10,1), sp = c(-70,40)), typ = 'o', pch = 19, col =2, xlab = 'lon', ylab = 'lat')
#' plot(simm.kf(n = 1000, u = c(10,1), v = c(0,1), D = c(10,1), sp = c(-70,40)), typ = 'o', pch = 19, col =3, xlab = 'lon', ylab = 'lat')
#' plot(simm.kf(n = 1000, u = c(0,0), v = c(0,0), D = c(5000,1000), sp = c(-70,40)), typ = 'o', pch = 19, col =4, xlab = 'lon', ylab = 'lat')
#'
simm.kf <- function(n = 100, u = c(0, 1), v = c(0, 1), D = c(100, 50),
                    sp = c(-70, 40), ulim = c(-50, 50), vlim = c(-50, 50),
                    Dlim = c(0, 5000)) {
  # Conversion factors between degrees and nautical miles (mean earth radius 6371 km)
  deg_to_nm <- (pi / 180) * 6371 / 1.852
  nm_to_deg <- 1 / deg_to_nm

  nsp <- sp * deg_to_nm

  Di <- truncnorm::rtruncnorm(n, a = Dlim[1], b = Dlim[2], mean = D[1], sd = D[2])
  ui <- truncnorm::rtruncnorm(n, a = ulim[1], b = ulim[2], mean = u[1], sd = u[2])
  vi <- truncnorm::rtruncnorm(n, a = vlim[1], b = vlim[2], mean = v[1], sd = v[2])

  # Diagonal covariance error: e ~ N(0, 2 * Di[1])
  sd_e <- sqrt(max(0, 2 * Di[1]))
  ei1 <- stats::rnorm(n, mean = 0, sd = sd_e)
  ei2 <- stats::rnorm(n, mean = 0, sd = sd_e)

  est1 <- c(nsp[1], nsp[1] + cumsum(ui + ei1))
  est2 <- c(nsp[2], nsp[2] + cumsum(vi + ei2))

  est <- cbind(est1, est2) * nm_to_deg
  colnames(est) <- c("lon", "lat")
  est
}


