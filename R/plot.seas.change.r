#' Plot percent change from previous season
#'
#' @param sr list of seasonal raster results from
#' @param box overlay of boxes for transfer areas
#'
#' @return a plot
#' @export
#' @examples
#' # after running make.sim.track.par
#' simdatdf = ldply(simdat)
#' simdatdf$seas[simdatdf$Month%in%c(1,2,3)] = 1
#' simdatdf$seas[simdatdf$Month%in%c(4,5,6)] = 2
#' simdatdf$seas[simdatdf$Month%in%c(7,8,9)] = 3
#' simdatdf$seas[simdatdf$Month%in%c(10,11,12)] = 4
#' sr = dlply(simdatdf, 'seas', function(x) make.sim.raster(x, boxsize=60))
#' plot.seas.change(sr)
plot.seas.change <- function(sr, box = NULL, col = fields::tim.colors(100), par = c(2,2),mar = c(2,2,2,2)){
  a = 1:length(sr)
  if(length(a) == 4){
    seasons = c('Winter','Spring','Summer','Fall')
    b = c(4, 1:3)
  }
  if(length(a) == 12){
    seasons = month.name
    b = c(12, 1:11)
    }


  par(mfrow = par)
  par(mar = mar)

  for(i in a){
    # if (i == 1){
    #   mdiff = max(abs(values(sr[[1]]- sr[[4]])), na.rm=T)
    #   plot((sr[[1]]- sr[[4]])/mdiff, interp=T, col = col, axes=F)
    # }else{
      mdiff = max(abs(values(sr[[i]]-sr[[b[i]]])), na.rm=T)
      plot((sr[[i]]-sr[[b[i]]])/mdiff, interp=T, col = col, axes=F)
    # }
    rpts = xyFromCell(sr[[i]], which(!is.na(values(sr[[i]]))))
    world(add = T, col = hsv(0,0,.8, alpha = .25), fill = T)
    if(!is.null(box)){
      plot(box, add = T, border = 'darkred', lwd = 2)
      text(coordinates(box)[,1], coordinates(box)[,2], box@data$ID, font=2, cex=1.2)
    }
    degAxis(1)
    degAxis(2)
    title(paste0(seasons[i], ' % change'))
    box()
  }

}

