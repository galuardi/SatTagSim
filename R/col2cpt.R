#' Make a GMT color ramp file (.cpt)
#'
#' @param nmin numeric minimum of the ramp
#' @param nmax numeric maximum of the ramp
#' @param clist catenated color list
#' @param outfile outpur filename (.cpt)
#' @param length number of segments between min and max
#'
#' @return a .cpt file
#' @export
#' @author Benjamin Galuardi
#' @details I love GMT. No other reason for this to be in here.
#' @references Wessel, P., Smith, W.H.F., Scharroo, R., Luis, J., and Wobbe, F. 2013. Generic Mapping Tools: Improved Version Released. Eos, Transactions American Geophysical Union 94(45): 409–410. doi:10.1002/2013EO450001.
#' http://gmt.soest.hawaii.edu/doc/latest/index.html
#'
#' @examples
#'
#' col2cpt(nmin = 0, nmax = 40, clist = c("lightcyan", "royalblue", "blue", "lemonchiffon", "orange", "red"),  outfile = 'my_colors.cpt', length=100)
col2cpt <-
function(nmin=0, nmax=1, clist = c("lightcyan", "royalblue", "blue", "lemonchiffon", "orange", "red"), outfile = 'mycol.cpt', length=100){
mycol = colorRampPalette(clist, space = "Lab")
# make a GMT .cpt file..
mynums = seq(nmin,nmax, length=length)
mycoltab = t(col2rgb(mycol(length)))

cptout=cbind(mynums[1:(length-1)], mycoltab[1:(length-1),], mynums[2:(length)], mycoltab[2:(length),])

write.table(cptout, file=outfile, col.names=F, row.names=F)
cat("B 255 255 255 \n
F 255 255 255 \n
N 255 255 255", file = outfile, append=T, sep ="  ")
}
