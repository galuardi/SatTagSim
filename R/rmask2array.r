# ' Raster mask to array
#' Function to tranfrom a raster brick of sst to a 3d array and orient properly
#' @param rmask raster mask of monthly suitable temperatures fro Atlantic bluefin tuna
#'
#' @return a 3D array
#' @export
#'
#' @examples
#' data(rmask)
#' sst = rmask2array(rmask)
rmask2array <- function(rmask){
  # require(matlab)
  sstmat = list(lon = sort(unique(coordinates(rmask[[1]])[,1]))
                ,lat = sort(unique(coordinates(rmask[[1]])[,2]))
                # ,data = as.array(rmask)
                )
  sstdata = array(NA, dim = dim(rmask)[c(2,1,3)])
  for(i in 1:(dim(rmask)[3])) sstdata[,,i] = rot90(as.array(rmask)[,,i],3)
  sstmat$data = sstdata
  sstmat
}
