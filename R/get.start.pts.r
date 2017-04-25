#' Get starting points
#' Get starting points for simulations based on distribution of observed locations for a given month.
#'
#' @param dat input data. see \link{\code{nsdata}}
#' @param n number of starting points
#' @param posnames names of the longitude and latitude columns in the data frame
#'
#' @return a list of starting points
#' @export
#' @details The number of starting points is the number of tracks desired for simulation in a given month
#' @examples
#' library(fields)
#' data(nsfish)
#' spts = get.start.pts(nsfish)
#' world(xlim = c(-100, 0), ylim = c(20, 50))
#' lapply(spts, points, pch = 19, cex = .4)
get.start.pts <- function(dat, n = 100, months = 1:12, mask = rmask, posnames = c('coords.x1','coords.x2')){
	spts = lapply(months, function(i){
					  tmp =  as.data.frame(dat[dat$Month==i,])
					  nidx = which(names(tmp)%in%posnames)
					  names(tmp)[nidx] = c('lon','lat')
					  tmpr = make.sim.raster(tmp)
					  tmpr = raster::resample(tmpr, mask)*mask[[i]]
					  tmpdf = as.data.frame(as(tmpr, 'SpatialPointsDataFrame'))
					  tmpdf[,1] = tmpdf[,1]/max(tmpdf[,1])
					  pts = tmpdf[sample(1:nrow(tmpdf), n, prob = tmpdf[,1], replace = T),2:3] # note: the sampl
					  # spts[[i]] = pts
				  }
				)
		names(spts) = months
		spts
}

