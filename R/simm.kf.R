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
simm.kf <- function(n=100, u=c(0,1), v=c(0,1), D=c(100,50), sp=c(-70,40), ulim = c(-50,50), vlim = c(-50,50), Dlim = c(0, 5000)){
# 	require(MASS)
# 	require(truncnorm)

	deg2rad <- function(deg) return(deg*pi/180)
	rad2deg <- function(rad) return(rad/pi*180)
	myzinv <- function(x) deg2rad(x)*6371/1.852 # raduis of earth in km converted to nm
	myz <- function(x) rad2deg(x*1.852/6371)


	#convert starting point
	# nsp = zinv(sp)
	nsp = myzinv(sp)  # converts to nm using radius of earth

	## changed 1-7-14. u v and D are now in nm to start with.
	# Di = rtruncnorm(1, a=0, b=5000, mean = D[1], sd = D[2])
	Di = rtruncnorm(n, a = Dlim[1], b = Dlim[2], mean = D[1], sd = D[2])
	ui = rtruncnorm(n, a = ulim[1], b = ulim[2], mean = u[1], sd = u[2])
	vi = rtruncnorm(n, a = vlim[1], b = vlim[2], mean = v[1], sd = v[2])

	Q = matrix(c(2*Di[1], 0, 0, 2*Di[1]),2,2)
	# Q = matrix(c(0, 2*Di[1], 2*Di[1],0 ),2,2)

	# Qvar = matrix(c(2*D,0,0,2*D),2,2)

	# covariance of error (e_i)
	ei = mvrnorm(n, mu=c(0,0), Sigma=Q) #

	# est = cbind(cumsum(ui), cumsum(vi)) + cumsum(ei)	## original
	# est = cbind(cumsum(ui)+cumsum(ei[,1]), cumsum(vi)+cumsum(ei[,2]))
	# est = cbind(cumsum(ui)+ei[,1], cumsum(vi)+ei[,2])
	est = cbind(cumsum(ui+ei[,1]), cumsum(vi+ei[,2]))
	est = cbind(nsp[1]+est[,1], nsp[2]+est[,2])

	# lon = (nsp[1]+est[,1])/llon.km(lat)
	# lat  = z(nsp[2]+est[,2])

	# rbind(sp,z.v(est))
	est = rbind(nsp, est)
	# est = as.data.frame(t(apply(est, 1, z)))
	# est = as.data.frame(t(apply(est, 1, myz)))
	est = t(apply(est, 1, myz))
	# names(est) = c('x','y')
	# coordinates(est) = ~x+y
	# est@proj4string = CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')
	est
	# points(rbind(sp,z.v(est)), pch=19, cex=.5, col=4)
}


