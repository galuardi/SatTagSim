


stime = Sys.time()

simdat = list()
msims = 1

print(paste0('simulating ', length(spts)*msims, ' tracks for ', nyears, ' years'))

mcoptions =  setup.parallel()

for(mm in 1:12){
  # subsp = spts[[mm]][1:1000,]
  subsp = spts[[mm]][sample(1:nrow(spts[[mm]]), msims, replace = T),]
  subsp$row = 1:nrow(subsp)
  sp = dlply(subsp, 'row', function(x) x[,1:2])
  simorder = morder[mm,]

  print(paste0('simulating ', length(sp), ' tracks starting in ', month.name[mm]))

  # this fixes the start point issue but creates another in that the last few points generate NULL tracks..
  # use setup.parallel each time to reset the processor connection

test = make.sim.track.par(tpar = simpar, morder = rep(simorder, nyears), sp = sp, bath = bath, sstmat = sstmat, seaslen = npmon, mcoptions = mcoptions)

  simdat[[mm]] = test
  runtime = Sys.time()-stime
  print(paste0('elapsed time: ', runtime))
}

print(paste0('elapsed time: ', runtime))

simdat = unlist(simdat, recursive = F)


# MAKE A DATA FRAME OF THE RESULTS
# simdatdf = ldply(lapply(simdat, function(x) ldply(x)))
simdatdf = ldply(simdat)

# simdatdf = ldply(lapply(simdat, function(x) melt(x, c('lon,','lat','Month'))))
# seasidx = make.seas.idx(simdatdf)
simdatdf$seas[simdatdf$Month%in%c(1,2,3)] = 1
simdatdf$seas[simdatdf$Month%in%c(4,5,6)] = 2
simdatdf$seas[simdatdf$Month%in%c(7,8,9)] = 3
simdatdf$seas[simdatdf$Month%in%c(10,11,12)] = 4
# rm(seasidx)

names(simdatdf)[which(names(simdatdf)%in%c('lon,','lat'))] = c('lon','lat')

#---------------------------------------------------------------#
# Make Rasters of Results
sr = dlply(simdatdf, 'seas', function(x) make.sim.raster(x, boxsize=60))
# plot(stack(sr))

# x11()

par(mfrow=c(2,2))
for(i in 1:4){
  plot(sr[[i]], col=mycol(256))
  world(add=T)
  plot(box11, add=T, border='salmon', lwd=2)
  text(coordinates(box11)[,1], coordinates(box11)[,2], box11@data$ID, font=2, cex=1.2)
}



#====================================================================================================#
# GET SEASONAL MARKOV TRANSITIONS
#====================================================================================================#

print('generating transition matrices')

# try new version of function... it works!!
datbox = get.first.box(simdat, 2000, box7)
boxtrans = get.trans.prob(datbox, return.all=F, nyears=100, sseas=1, sarea=3, adims = c(7,7,4))

# plot it
library(ggplot2)
library(reshape2)

names(boxtrans) = seasons

plot.boxtrans(boxtrans)
#
# bb = melt(boxtrans)
# bb$seasons = c(sapply(1:4, function(x) rep(x, nrow(bb)/4)))
# bb$seasons = reorder(bb$L1, bb$seasons)

# x11()
# ggplot(bb, aes(x=Var2, y = Var1, fill = value))+
#   geom_tile()+
#   # geom_label(label = round(bb$value,2), colour = 'white', fontface = 'bold')+
#   geom_text(label = round(bb$value,2), col = 'white', fontface = 'bold', size = 5)+
#   scale_fill_distiller(palette = "BrBG", direction = 1)+
#   facet_wrap(~seasons)+
#   xlab('End (Current)Area')+
#   ylab('Start (Previous) Area')+
#   theme(strip.text = element_text(size=15)
#         ,  axis.title.x = element_text(size = 20)
#         ,  axis.title.y = element_text(size = 20))

## Saving the new GBYP run
save.image('C:/Users/ben/SkyDrive/PHD_SIMS/SCRS_SIMS/abft_SIMS_GBYP_ALL_1000_V2.Rdata')


