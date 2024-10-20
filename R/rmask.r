#' Raster mask of World Ocean Atlas SST
#'
#' Raster version of the world Ocean atlas SST mask for Atlantic bluefin tuna. Temperatures are flagged between 10-28 C, as most suitable according to empirical records from tagged fish.
#'
#' @aliases woasst
#' @format A raster stack with 12 layers, ont for each month
#' @source World Ocean Atlas data:

#'Boyer, T.P., J. I. Antonov, O. K. Baranova, C. Coleman, H. E. Garcia, A. Grodsky, D. R. Johnson, R. A. Locarnini, A. V. Mishonov, T.D. O'Brien, C.R. Paver, J.R. Reagan, D. Seidov, I. V. Smolyar, and M. M. Zweng, 2013: World Ocean Database 2013, NOAA Atlas NESDIS 72, S. Levitus, Ed., A. Mishonov, Technical Ed.; Silver Spring, MD, 209 pp., \url{http://doi.org/10.7289/V5NZ85MT}
#'
#' @examples
#' data(rmask)

#'# SET UP THE WOA SST MASK AS A 3D ARRAY
#'sstmat = list(lon = sort(unique(coordinates(rmask[[1]])[,1]))
#'              ,lat = sort(unique(coordinates(rmask[[1]])[,2]))
#')
#'sstdata = array(NA, dim = dim(rmask)[c(2,1,3)])
#'for(i in 1:(dim(rmask)[3])) sstdata[,,i] = rot90(as.array(rmask)[,,i],3)
#'# sstmat$data = apply(sstdata, 1:2, sum, na.rm = T)

#'sstmat$data = sstdata

#'# If using the running average suitability (Recommended)
#'sstm = sstdata
#'for(i in 2:11){
#'  sstm[,,i] = apply(sstdata[,,c(i-1,i,i+1)], 1:2, sum, na.rm = T)
#'}
#'sstm[,,1] = apply(sstdata[,,c(12, 1, 2)], 1:2, sum, na.rm = T)
#'sstm[,,12] = apply(sstdata[,,c(11, 12, 1)], 1:2, sum, na.rm = T)
#'sstmat$data = sstm
#'rm(sstdata, sstm)
#'
#'image.plot(sstmat$lon, sstmat$lat, sstmat$data[,,1], nlevel = 4, col = terrain.colors(4), main = 'January temperature suitability', xlab = "longitude", ylab = "latitude")

#' @usage
#' library(fields)
#' library(raster)
#' data(rmask)
#' plot(sum(rmask, na.rm=T))
#' title('Number of months with suitable surface temperature')
#'
#' data(woasst)
#' image.plot(woasst$lon, woasst$lat, apply(woasst$sst, 1:2, mean, na.rm=T))
#' title('World Ocean Atlas mean temperature')
"rmask"
