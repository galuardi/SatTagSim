#======================================================================================#
# Vary the starting point according to the amount of points in a raster cell each month
#======================================================================================#

setwd('C:/Users/benjamin.galuardi/Google Drive/PHD/R-package/')

library(SatTagSim)

# load data
data(nsfish)
# or use full set...
load("C:/Users/benjamin.galuardi/Google Drive/PHD/tagd/tagdata_wD.Rdata")
data(bath)
data(box7)
data(rmask)

# SET UP THE WOA SST AS A 3D ARRAY
# Function does not work!!!
# sstmat = rmask2array(rmask)
sstmat = list(lon = sort(unique(coordinates(rmask[[1]])[,1]))
                ,lat = sort(unique(coordinates(rmask[[1]])[,2]))
                # ,data = as.array(rmask)
                )
  sstdata = array(NA, dim = dim(rmask)[c(2,1,3)])
  for(i in 1:(dim(rmask)[3])) sstdata[,,i] = rot90(as.array(rmask)[,,i],3)
  # sstmat$data = apply(sstdata, 1:2, sum, na.rm = T)

sstmat$data = sstdata
## If using the running average suitability
{
  sstm = sstdata
  for(i in 2:11){
   sstm[,,i] = apply(sstdata[,,c(i-1,i,i+1)], 1:2, sum, na.rm = T)
  }
  sstm[,,1] = apply(sstdata[,,c(12, 1, 2)], 1:2, sum, na.rm = T)
  sstm[,,12] = apply(sstdata[,,c(11, 12, 1)], 1:2, sum, na.rm = T)

  sstmat$data = sstm
  rm(sstdata, sstm)
}
# setup an sst dataframe for minimizing distance to non-NA area
# sstdfl = lapply(1:12, function(i){
#       xyz = cbind(expand.grid(sstmat$lon, sstmat$lat), as.vector(t(sstmat$data[,,i])))
#       names(xyz) = c('x','y','z')
#       # gidx = which(!is.na(xyz$z))
#       xyz
#     }
# )

#---------------------------------------------------------------#
# Select a criteria for subsetting data
#---------------------------------------------------------------#
# datlg = as.data.frame(nsfish)
# Or use full set of fish...
datlg = as.data.frame(dat[dat$Length>185&dat$DAL>=180,])

# datlg = as.data.frame(dat[dat$Length>185&dat$DAL>=180,])
uvpar = get.allpar(datlg)
Dpar = get.kfD(datlg)
allpar = merge.par(uvpar, Dpar, return.mean=F)

#---------------------------------------------------------------#
## GGPLOT
#---------------------------------------------------------------#
library(ggplot2)

pdat = ddply(datlg, .(TagID), .fun=function(x) c(x$Length[1], x$D[1], x$Dsd[1]))
names(pdat)[2:4] = c('CFL','D','Dsd')

ggplot(pdat, aes(x=CFL, y=D)) +
  geom_errorbar(aes(ymin=D-Dsd, ymax=D+Dsd), colour="black", width=.1) +
  geom_abline(aes(intercept=mean(D, na.rm=T), slope=0, colour='red', width = 2))+
  geom_abline(aes(intercept=mean(D+Dsd, na.rm=T), slope=0, colour='blue', width = 2))+
  geom_abline(aes(intercept=mean(D-Dsd, na.rm=T), slope=0, colour='blue', width = 2))+
  geom_point( size=3, shape=21, fill="white") + # 21 is filled circle
  # theme(axis.text.x  = element_blank())+
  ggtitle(paste0("D distributions for ",nrow(pdat) ," tagged fish") )

  dev.off()
## NOTE: No variation in D for this dataset. Diffusion was fixed when estimating these tracks

# ggsave('abft_gt_185cfl_gt180d_D.png', width = 8, height = 6)

#---------------------------------------------------------------#
# MERGE UV AND D PARAMETER SETS
#---------------------------------------------------------------#
simpar = merge.par(uvpar, Dpar, return.mean=T)

seasons=c('Winter','Spring','Summer','Fall')
seas.cols=c('lightblue', 'springgreen', 'orange', 'bisque3')
mycol = colorRampPalette(c("lightgreen", "skyblue","firebrick2"), space = "Lab")

#=======================================================================#
# GET STARTING POINTS
# WEIGHTS ARE THE PROPORTION OF POSITIONS PER CELL
# USE ONE DEGREE CELLS AS STARTING POINTS
#=======================================================================#
spts = get.start.pts(datlg, 100)

#---------------------------------------------------------------#
# SET MONTH ORDER
#---------------------------------------------------------------#
morder = array(rep(1:12, 12), dim=c(12, 12))

for(i in 1:12){
  if(i==1) morder[i,] = 1:12
  else morder[i,] = c(i:12,1:((1:12)[i]-1))
}

#==========================================================================================#
# RUN 100 FISH STARTING IN EACH MONTH FROM THEIR RESPECTIVE SAMPLED STARTING POINTS
# seaslen = 1  gives one point per month. This should be analagous to how the advection term is being derived... total advectoin per month divided by days at liberty
#==========================================================================================#

#*************** SET UP PARALLEL PROCESSING ****************************************#
mcoptions = setup.parallel()
#***********************************************************************************#

# ONE HUNDRED RELEASES IN EACH MONTH, 30 LOCS/MONTH
stime = Sys.time()
  # cat(Sys.time()-stime)
simdat = list()
for(i in 1:12){
  print(i)
  temp = make.sim.track.par(n = 100, tpar = simpar, morder = rep(morder[i,],2), sp = spts[[i]], bath = bath, sst = sstmat, seaslen = 30) #nrow(spts[[i]])
  simdat = c(simdat, temp)
runtime = Sys.time()-stime
print(runtime)
}

#---------------------------------------------------------------#
# MAKE A DATA FRAME OF THE RESULTS
#---------------------------------------------------------------#
simdatdf = ldply(simdat)
seasidx = make.seas.idx(simdatdf)
simdatdf$seas = 1
simdatdf$seas[seasidx$spring==T] = 2
simdatdf$seas[seasidx$summer==T] = 3
simdatdf$seas[seasidx$fall==T] = 4
rm(seasidx)

names(simdatdf)[which(names(simdatdf)%in%c('lon,','lat'))] = c('lon','lat')
# make rasters of binned locations

#---------------------------------------------------------------#
# Make Rasters of Results
#---------------------------------------------------------------#
sr = dlply(simdatdf, 'seas', function(x) make.sim.raster(x, boxsize=60))

# x11()
par(mfrow=c(2,2))
par(mar=c(2,2,2,2))
for(i in 1:4){
  plot(sr[[i]]/max(values(sr[[i]]), na.rm=T), zlim = c(0.01,1), interp=T, col=mycol(256), axes=F)
  # contour(sr[[i]]/max(values(sr[[i]]), na.rm=T), levels = c(.05, .5), col=c(4,2), lwd=1, interp=T)
  rpts = xyFromCell(sr[[i]], which(!is.na(values(sr[[i]]))))
  # ud = kernelUD(as.data.frame(rpts), id=rep(1,nrow(rpts)), grid = 200)
  # ud[[1]]$UD[ud[[1]]$UD==0] = NA
  # image.plot(ud[[1]]$UD, zlim = quantile(ud[[1]]$UD, c(0.05,1), na.rm=T), xlim = c(-100, 40), ylim = c(0,65), col = mycol(100), axes=F, xlab='', ylab='')
  # contour(ud[[1]]$UD, levels = quantile(ud[[1]]$UD, c(.5), na.rm=T), add=T)
  world(add=T, col='grey80', fill=F)
  plot(box7, add=T, border='salmon', lwd=2)
  text(coordinates(box7)[,1], coordinates(box7)[,2], box7@data$ID, font=2, cex=1.2)
  degAxis(1)
  degAxis(2)
  title(seasons[i])
  box()
}
#
# x11()
# # savePlot('UD_largefish_gt180d_vary_start_720d_12kfish', type='png')
# savePlot('UD_largefish_gt180d_vary_start_720d_12kfish_monthly_step', type='png')
# dev.off()
#
# plot(sum(rmask, na.rm=T))
# lapply(simdat, lines, col=2, type='l')

#---------------------------------------------------------------#
# example plot...
#---------------------------------------------------------------#
# x11()
#  plot(rmask[[1]], col = 'white')
#  for(i in 1:12) plot(rmask[[i]], add=T, col = hsv(.25, .9,1/i, alpha = .15))

# image.plot(sstmat$lon, sstmat$lat, apply(sstmat$data, 1:2, sum, na.rm=T), col = terrain.colors(12))

# one month
par(mfrow=c(3,4))
# simdatdf = ldply(simdat)
for(i in 1:12){
  image.plot(sstmat$lon, sstmat$lat, sstmat$data[,,i], col = hsv(seq(.4,.5,length=2),.9,.9,alpha = .1))
  # lapply(simdat, function(x) points(x[x$Month==i,1:2], col = 'red', pch = 3, cex=.1))
  points(simdatdf[simdatdf$Month==i,1:2], col = hsv(
    0.1,.9,.9,alpha = .1), pch = 3, cex=.1)
  world(add=T)
}

#  points(simdat[[1]][1,1:2], col = 4, pch = 19, cex=1.3)
# savePlot('rmask_simdat_example', type = 'png')
# dev.off()


#----------------------------------------------------------------#
# Profile code
#---------------------------------------------------------------#
# Rprof('sim.parallel.out')
# tmp = make.sim.track.par(n = 10, tpar = simpar, morder = rep(morder[1,],2), sp = spts[[i]], bath = NULL, sst = sstmat, seaslen = 30) #nrow(spts[[i]])
# Rprof(NULL)
#----------------------------------------------------------------#

#====================================================================================================#
# GET SEASONAL MARKOV TRANSITIONS
#====================================================================================================#
datbox.tracks = get.first.box(simdat, 2000, box7, return.tracks=T)
datbox = get.first.box(simdat, 2000, box7, return.tracks=F)
boxtrans = get.trans.prob(datbox, return.all=F, nyears=100, sseas=1, sarea=3, adims = c(7,7,4))

# plot it
x11()
par(mfrow=c(2,2))
par(mar=c(4,4,5,3))
for(i in 1:4){
  image(1:7, 1:7, fliplr(t(boxtrans[[i]])), axes=F, col=rev(terrain.colors(49)), ylab='', xlab='')
  axis(2, labels=paste0('s',rev(1:7)), at=1:7)
  axis(3, labels=paste0('e',(1:7)), at=1:7)
  title(seasons[i], font=2)
  abline(a=8, b=-1, col='grey50', lty=2)
  for(j in 1:7) text(1:7, rep(j,7),  round(fliplr(t(boxtrans[[i]]))[,j],3), font=2)
  box()
}

# savePlot('Transmat_largefish_gt180d_vary_start_720d_12kfish', type='png')
savePlot('Transmat_largefish_gt180d_vary_start_720d_12kfish_monthly_step', type='png')
dev.off()

save('simdat', 'simdatdf', 'simpar','datbox','boxtrans', file = 'simdat_large_gt180d_vary_start_monthly_step.Rdata')
# save('simdat', 'simdatdf', 'simpar','datbox','boxtrans', file = 'simdat_large_gt180d_vary_start.Rdata')
# saves the monthly step run
}

#===========================================================================================#
# SMALLER FISH
#===========================================================================================#
{
datsm = as.data.frame(dat[dat$Length<185&dat$DAL>=180,])
uvpar = get.allpar(datsm)
Dpar = get.kfD(datsm)

## GGPLOT
pdat = ddply(datsm, .(TagID), .fun=function(x) c(x$Length[1], x$D[1], x$Dsd[1]))
names(pdat)[2:4] = c('CFL','D','Dsd')

x11()
ggplot(pdat, aes(x=CFL, y=D)) +
  geom_errorbar(aes(ymin=D-Dsd, ymax=D+Dsd), colour="black", width=.1) +
  geom_abline(aes(intercept=mean(D, na.rm=T), slope=0, colour='red', width = 2))+
  geom_abline(aes(intercept=mean(D+Dsd, na.rm=T), slope=0, colour='blue', width = 2))+
  geom_abline(aes(intercept=mean(D-Dsd, na.rm=T), slope=0, colour='blue', width = 2))+
  geom_point( size=3, shape=21, fill="white") + # 21 is filled circle
  # theme(axis.text.x  = element_blank())+
  ggtitle(paste0("D distributions for ",nrow(pdat) ," tagged fish <185 cm") )

ggsave('abft_lt_185cfl_lt180d_D.png', width = 8, height = 6)
dev.off()

# MERGE UV AND D PARAMETER SETS
simpar = merge.par(uvpar, Dpar, return.mean=T)

seasons=c('Winter','Spring','Summer','Fall')
seas.cols=c('lightblue', 'springgreen', 'orange', 'bisque3')
mycol = colorRampPalette(c("lightgreen", "skyblue","firebrick2"), space = "Lab")

#=======================================================================#
# GET STARTING POINTS
# WEIGHTS ARE THE PROPORTION OF POSITIONS PER CELL
# USE ONE DEGREE CELLS AS STARTING POINTS
#
# THE NUMBER OF STARTING POINTS WILL EQUAL THE NUMBER OF SIMULATIONS!
#=======================================================================#
spts = NULL

par(mfrow=c(3,4))
for(i in 1:12){
  par(mar = c(1,1,1,1))
  tmp =  as.data.frame(datsm[datsm$Month==i,])
  nidx = which(names(tmp)%in%c('coords.x1','coords.x2'))
  names(tmp)[nidx] = c('lon','lat')
  tmpr = make.sim.raster(tmp)
  #plot(tmpr/max(values(tmpr), na.rm=T))
  tmpdf = as.data.frame(as(tmpr, 'SpatialPointsDataFrame'))
  tmpdf[,1] = tmpdf[,1]/max(tmpdf[,1])
  pts = tmpdf[sample(1:nrow(tmpdf), 1000, prob = tmpdf[,1], replace = T),2:3]
  ud = kernelUD(as.data.frame(pts), id=rep(1,1000))
  ud[[1]]$UD[ud[[1]]$UD==0] = NA
  #   image.plot(ud[[1]]$UD, zlim = quantile(ud[[1]]$UD, c(0.05,1), na.rm=T), xlim = c(-100, 40), ylim = c(0,65), col = mycol(100), axes=F, xlab='', ylab='')
  #   world(add=T, col='cornsilk4', fill=T, border='cornsilk4')
  #   plot(box7, add=T, border ='salmon', lwd=2)
  #   degAxis(1)
  #   degAxis(2)
  #   points(jitter(pts$x,.05), jitter(pts$y, .05), pch=3, col=4, cex=.1)
  #   title(month.name[i])
  #   text(coordinates(box7)[,1], coordinates(box7)[,2], box7@data$ID, font=2, cex=1.2)
  spts[[i]] = pts
}

names(spts) = 1:12

## EXAMPLE PLOT
x11()
plot(tmp$lon, tmp$lat, xlab = '', ylab = '')
points(pts, col=2, pch=19)
title('starting point example: December')
contour(ud[[1]]$UD, add=T, col=3, lwd=2)
legend('bottomright', legend = c('locations','resampled starting pts','weights (UD)'), lty = c(0,0,1), col = c(1,2,3), pch = c(1,19,-1), lwd = c(0,0,2))
savePlot('Starting point example', 'png')

rm(pts, tmp, tmpdf, ud)
dev.off()



# alter month order
morder = array(rep(1:12, 12), dim=c(12, 12))
for(i in 1:12){
  if(i==1) morder[i,] = 1:12
  else morder[i,] = c(i:12,1:((1:12)[i]-1))
}
#==========================================================================================#
# RUN 1000 FISH STARTING IN EACH MONTH FROM THEIR RESPECTIVE SAMPLED STARTING POINTS
# seaslen = 1  gives one point per month. This should be analagous to how the advection term is being derived... total advectoin per month divided by days at liberty
#==========================================================================================#
stime = Sys.time()
# simdat = sapply(1:12, function(i) make.sim.list(simpar, rep(morder[i,],2)
#                   , spts = spts[[i]], bath=bath, sst=rmask, seaslen = 30)) #DAILY STEP
simdat = pbsapply(1:12, function(i) make.sim.list(simpar, rep(morder[i,],2)
                                                  , spts = spts[[i]], bath=bath, sst=rmask, seaslen = 1)) # MONTHLY STEP

runtime = Sys.time()-stime
runtime
simdat1 = simdat

stime = Sys.time()
# simdat = pbsapply(2:12, function(i) make.sim.list(simpar, rep(morder[i,],2)
#                       , spts = spts[[i]], bath=bath, sst=rmask, seaslen = 30)) #DAILY STEP
simdat = sapply(1:12, function(i) make.sim.list(simpar, rep(morder[i,],2)
                                                , spts = spts[[i]][1:5,], bath=bath, sst=rmask, seaslen = 1))  #MONTHLY STEP
runtime = Sys.time()-stime
runtime

simdat = c(simdat1, simdat)

simdatdf = ldply(simdat)
seasidx = make.seas.idx(simdatdf)
simdatdf$seas = 1
simdatdf$seas[seasidx$spring==T] = 2
simdatdf$seas[seasidx$summer==T] = 3
simdatdf$seas[seasidx$fall==T] = 4
rm(seasidx)

names(simdatdf)[which(names(simdatdf)%in%c('lon,','lat'))] = c('lon','lat')
# make rasters of binned locations

sr = dlply(simdatdf, 'seas', function(x) make.sim.raster(x, boxsize=60))

x11()
par(mfrow=c(2,2))
par(mar=c(2,2,2,2))
for(i in 1:4){
  plot(sr[[i]]/max(values(sr[[i]]), na.rm=T), zlim = c(0.05,1), interp=T, col=mycol(256), axes=F)
  # contour(sr[[i]]/max(values(sr[[i]]), na.rm=T), levels = c(.05, .5), col=c(4,2), lwd=1, interp=T)
  rpts = xyFromCell(sr[[i]], which(!is.na(values(sr[[i]]))))
  # ud = kernelUD(as.data.frame(rpts), id=rep(1,nrow(rpts)), grid = 200)
  # ud[[1]]$UD[ud[[1]]$UD==0] = NA
  # image.plot(ud[[1]]$UD, zlim = quantile(ud[[1]]$UD, c(0.05,1), na.rm=T), xlim = c(-100, 40), ylim = c(0,65), col = mycol(100), axes=F, xlab='', ylab='')
  # contour(ud[[1]]$UD, levels = quantile(ud[[1]]$UD, c(.5), na.rm=T), add=T)
  world(add=T, col='grey80', fill=T)
  plot(box7, add=T, border='salmon', lwd=2)
  text(coordinates(box7)[,1], coordinates(box7)[,2], box7@data$ID, font=2, cex=1.2)
  degAxis(1)
  degAxis(2)
  title(seasons[i])
  box()
}

# savePlot('UD_smallfish_lt180d_vary_start_720d_12kfish', type='png')
savePlot('UD_smallfish_lt180d_vary_start_720d_12kfish_monthly_step', type='png')
dev.off()

# plot(sum(rmask, na.rm=T))
# lapply(simdat, lines, col=2, type='l')

#====================================================================================================#
# GET SEASONAL MARKOV TRANSITIONS
#====================================================================================================#
datbox.tracks = get.first.box(simdat, 2000, box7, return.tracks=T)
datbox = get.first.box(simdat, 2000, box7, return.tracks=F)
boxtrans = get.trans.prob(datbox, return.all=F, nyears=100, sseas=1, sarea=3, adims = c(7,7,4))

# plot it
x11()
par(mfrow=c(2,2))
par(mar=c(4,4,5,3))
for(i in 1:4){
  image(1:7, 1:7, fliplr(t(boxtrans[[i]])), axes=F, col=rev(terrain.colors(49)), ylab='', xlab='')
  axis(2, labels=paste0('s',rev(1:7)), at=1:7)
  axis(3, labels=paste0('e',(1:7)), at=1:7)
  title(seasons[i], font=2)
  abline(a=8, b=-1, col='grey50', lty=2)
  for(j in 1:7) text(1:7, rep(j,7),  round(fliplr(t(boxtrans[[i]]))[,j],3), font=2)
  box()
}

# savePlot('Transmat_smallfish_lt180d_vary_start_720d_12kfish', type='png')
savePlot('Transmat_smallfish_lt180d_vary_start_720d_12kfish_monthly_step', type='png')


# save('simdat', 'simdatdf', 'simpar','datbox','boxtrans', file = 'simdat_small_lt185_180d_vary_start.Rdata')

# saves the monthly step run
save('simdat', 'simdatdf', 'simpar','datbox','boxtrans', file = 'simdat_small_lt185_180d_vary_start_monthly_step.Rdata')
}



#-------------------------------------------------------------------------------------------------------#
# Some scratch work
#-------------------------------------------------------------------------------------------------------#
{
#smalldat or tracks... smalldat should have the parameters too

smalldat = dat[dat$Length<185,]

seasidx = make.seas.idx(smalldat)
smalldat$seas = 1
smalldat$seas[seasidx$spring==T] = 2
smalldat$seas[seasidx$summer==T] = 3
smalldat$seas[seasidx$fall==T] = 4


# WEIGHTS ARE THE PROPORTION OF POSITIONS PER CELL
# USE ONE DEGREE CELLS AS STARTING POINTS

spts = NULL

par(mfrow=c(3,4))
for(i in 1:12){
  tmp =  as.data.frame(smalldat[smalldat$Month==i,])
  names(tmp)[28:29] = c('lon','lat')
  tmpr = make.sim.raster(tmp)
  plot(tmpr/max(values(tmpr), na.rm=T))
  tmpdf = as.data.frame(as(tmpr, 'SpatialPointsDataFrame'))
  tmpdf[,1] = tmpdf[,1]/max(tmpdf[,1])
  pts = tmpdf[sample(1:nrow(tmpdf), 1000, prob = tmpdf[,1], replace = T),2:3]
  points(jitter(pts$x,.05), jitter(pts$y, .05), pch=19, col=4, cex=.1)
  spts[[i]] = pts
}

names(spts) = 1:12

#----------------------------------------------------------------------------------------------#
# get all u and v and match to the master data set. This will make subsetting easier.
# not working!!!
alluv = get.allpar(as.data.frame(dat))

datdf = as.data.frame(dat)

datdf$u = datdf$v = 0

for(i in unique(datdf$TagID)){
  for(j in unique(datdf$Month)){
    idx1 = datdf[datdf$TagID==i&datdf$Month==j,]
    idx2 = alluv[alluv$TagID==i&alluv$Month==j,]
    if(nrow(idx2>0)){
      datdf$u[idx1] = alluv$u[idx2]
      datdfv[idx1] = alluv$v[idx2]
    }
  }
}
#----------------------------------------------------------------------------------------------#
#try running the make.sim.track in an array

sps = data.frame(lon=-71:-69, lat = 38:40)

t1 = Sys.time()
temp1 = apply(sps, 1, function(x) make.sim.track(simpar, c(9:12, 1:8), sp=c(x), bath= bath, sst=NULL))
t2 = Sys.time()
t2-t1

t1 = Sys.time()
temp2 = apply(sps, 1, function(x) make.sim.track(simpar, c(9:12, 1:8), sp=c(x), bath= bath, sst=rmask))
t2 = Sys.time()
t2-t1


t1 = Sys.time()
temp3 = make.etrack.list(simpar,c(9:12, 1:8), c(sps[1,]), bath=bath, 3, sst = rmask, .parallel=T)
t2 = Sys.time()
t2-t1
}



#=====================================================================================#
####  TRANSITION PLOTS
#=====================================================================================#
# for(i in c(3,4,1,2)){
  # jpeg(height=10, width=10, res=300, units='in', file=paste0('big180-Transitions-',seasons[i],'.jpg'), family='Times')
  transitionPlot(boxtrans[[i]], box_txt=paste0('area-',1:7), tot_spacing = .05,
                 box_width = 1/6,
                 fill_start_box = "darkseagreen3",
                 txt_start_clr = "black",
                 fill_end_box = "lightblue",
                 txt_end_clr = "white",
                 pt=20,
                 min_lwd = 1,
                 max_lwd = 20,
                 lwd_prop_total = TRUE)  #,': ',round(100*mmat[,,i],3),'%'
  # title(seasons[i], cex=2)
#   dev.off()
# }

  library(Gmisc)

  aa =

  transitionPlot(trn_mtrx_3D,
                 txt_start_clr = c("white", "black"),
                 fill_start_box = c("#5C246E", "#BFEFFF"),
                 type_of_arrow = "gradient")
