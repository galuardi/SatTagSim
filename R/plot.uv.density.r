
#' Diagnostic ggplots of advection
#'
#' @param allpar data frame returned from \code{\link{get.allpar}}
#' @param save should the plot be saved
#' @param fname filename
#' @param ulims plot limits in u (x) direction
#' @param vlims plot limits in v (y) direction
#' @param ... additional arguements to ggsave
#'
#' @return a ggplot
#' @export
#'
#' @examples
#' see vignette
plot.uv.density <- function(allpar, save=F, fname=NULL, ulims = c(-50,50), vlims = c(-50,50), ...){
require(ggplot2)
usd = tapply(allpar$u, allpar$Month, sd, na.rm=T)
umean = tapply(allpar$u, allpar$Month, mean, na.rm=T)
vsd = tapply(allpar$v, allpar$Month, sd, na.rm=T)
vmean = tapply(allpar$v, allpar$Month, mean, na.rm=T)

# par(mfrow=c(3,4))
myd=NULL
allp=NULL
ii = 1
for(i in 1:4) {
	for(j in 1:3){
# x11()
	u <- rtruncnorm(1000, a=-50, b= 50, umean[ii], usd[ii])
	v <-   rtruncnorm(1000, a=-50, b= 50, vmean[ii], vsd[ii])
	myd <- rbind(myd, cbind(u, v, rowid=rep(i,1000), colid = rep(j, 1000),month = rep(ii, 1000)))
	# myd = data.frame(xvar, yvar)
	ii = ii+1
	temp = data.frame(u, v)
	allp[[ii]]= ggplot(temp, aes(x=u, y=v)) + stat_density2d(aes(fill = ..level..), geom="polygon", colour='white')+coord_cartesian(ulims, vlims)+scale_fill_gradient(low="lightblue", high="grey50")+geom_hline(yintercept=0, lty=2,col=2)+geom_vline(xintercept=0,lty=2,col=2)
	}
}

myd = as.data.frame(myd)
myd$labs = factor(myd$month, label=month.name)
 # myd$labs2=factor(myd$month, levels = month.name)

p1 = ggplot(myd, aes(x=u, y=v)) + stat_density2d(aes(fill = ..level..), geom="polygon")+coord_cartesian(ulims, vlims)+scale_fill_gradient(low="lightblue", high="salmon")+geom_hline(yintercept=0, lty=2,col=2)+geom_vline(xintercept=0,lty=2,col=2)+facet_wrap(~labs, nrow=3)


circleFun <- function(center = c(0,0),diameter = 1, npoints = 100){
    r = diameter / 2
    tt <- seq(0,2*pi,length.out = npoints)
    xx <- center[1] + r * cos(tt)
    yy <- center[2] + r * sin(tt)
    return(data.frame(u = xx, v = yy))
}

dat=circleFun(c(0,0), diameter=20, npoints=100)
p2 = p1+geom_path(data=dat, mapping=aes(u,v))
dat=circleFun(c(0,0), diameter=10, npoints=100)
p2 = p2+geom_path(data=dat, mapping=aes(u,v))
dat=circleFun(c(0,0), diameter=5, npoints=100)
p2 = p2+geom_path(data=dat, mapping=aes(u,v))
p2 = p2+theme_bw()


if(save==T){
	# pdf(10,10, file=fname, ...)
	ggsave(p2, file=fname,...)
	# dev.off()
}else{
p2
}
}




