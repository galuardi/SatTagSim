# compare one year tracks vs two year tracks
# 100,000 tracks WABT > 185 cm LPRC and NOAA only

## one year tracks
#
lon = as.vector(sapply(seq(1, length(allsimdat), by = 3), function(x) c(allsimdat[[x]][1:48])))
lat = as.vector(sapply(seq(2, length(allsimdat), by = 3), function(x) c(allsimdat[[x]][1:48])))
Month = as.vector(sapply(seq(3, length(allsimdat), by = 3), function(x) c(allsimdat[[x]][1:48])))

simdatdf_1 = data.frame(lon, lat, Month)
simdatdf_1$seas[simdatdf_1$Month%in%c(1,2,3)] = 1
simdatdf_1$seas[simdatdf_1$Month%in%c(4,5,6)] = 2
simdatdf_1$seas[simdatdf_1$Month%in%c(7,8,9)] = 3
simdatdf_1$seas[simdatdf_1$Month%in%c(10,11,12)] = 4


## two year tracks
lon = as.vector(sapply(seq(1, length(allsimdat), by = 3), function(x) c(allsimdat[[x]])))
lat = as.vector(sapply(seq(2, length(allsimdat), by = 3), function(x) c(allsimdat[[x]])))
Month = as.vector(sapply(seq(3, length(allsimdat), by = 3), function(x) c(allsimdat[[x]])))

simdatdf_2 = data.frame(lon, lat, Month)

simdatdf_2$seas[simdatdf_2$Month%in%c(1,2,3)] = 1
simdatdf_2$seas[simdatdf_2$Month%in%c(4,5,6)] = 2
simdatdf_2$seas[simdatdf_2$Month%in%c(7,8,9)] = 3
simdatdf_2$seas[simdatdf_2$Month%in%c(10,11,12)] = 4

# names(simdatdf)[which(names(simdatdf)%in%c('lon,','lat'))] = c('lon','lat')

sr_1 = dlply(simdatdf_1, 'seas', function(x) make.sim.raster(x, boxsize = 60))
sr_2 = dlply(simdatdf_2, 'seas', function(x) make.sim.raster(x, boxsize = 60))

par(mfrow=c(2,2))
par(mar=c(2,2,2,2))

for(i in 1:4){
  plot(sr_2[[i]]-sr_1[[i]], interp=F, col=mycol(256), axes=F)
  world(add=T, col='grey80', fill=F)
  #   plot(box7, add=T, border='salmon', lwd=2)
  #   text(coordinates(box7)[,1], coordinates(box7)[,2], box7@data$ID, font=2, cex=1.2)
  plot(box11, add=T, border='salmon', lwd=2)
  text(coordinates(box11)[,1], coordinates(box11)[,2], box11@data$ID, font=2, cex=1.2)
  degAxis(1)
  degAxis(2)
  title(seasons[i])
  box()
}

# for(i in 1:4){
#   plot(sr[[i]]/max(values(sr[[i]]), na.rm=T), zlim = c(0.01,1), interp=F, col=mycol(256), axes=F)
#   rpts = xyFromCell(sr[[i]], which(!is.na(values(sr[[i]]))))
#   world(add=T, col='grey80', fill=F)
#   #   plot(box7, add=T, border='salmon', lwd=2)
#   #   text(coordinates(box7)[,1], coordinates(box7)[,2], box7@data$ID, font=2, cex=1.2)
#   plot(box11, add=T, border='salmon', lwd=2)
#   text(coordinates(box11)[,1], coordinates(box11)[,2], box11@data$ID, font=2, cex=1.2)
#   degAxis(1)
#   degAxis(2)
#   title(seasons[i])
#   box()
# }

