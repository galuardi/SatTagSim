get.sst.mask.val <- function (lon, lat, mask, month)
{
  X = as.vector(mask$lon)
  Y = as.vector(mask$lat)
  xidx = which.min((lon - X)^2)
  yidx = which.min((lat - Y)^2)
  mask$data[xidx, yidx, month] # flagged off for additive months
  # mask$data[xidx, yidx]
}

deg2rad <- function(deg) return(deg * pi/180)
rad2deg <- function(rad) return(rad/pi * 180)
myzinv <- function(x) deg2rad(x) * 6371/1.852
myz <- function(x) rad2deg(x * 1.852/6371)

sp = c(-77.375, 33.125)

nsp = myzinv(sp)

i = 1

samp2 = mvrnorm(100, mu = c(u[1], v[1]) , Sigma = matrix(c(u[2],0,0,v[2]) , 2, 2))
samp2[,1] = nsp[1]+samp2[,1]
samp2[,2] = nsp[2]+samp2[,2]
samp2 = myz(samp2)

samp1 = mvrnorm(1000, mu = c(u[1], v[1]) , Sigma = matrix(c(583, 0, 0, 583) , 2, 2))
samp1[,1] = nsp[1]+samp1[,1]
samp1[,2] = nsp[2]+samp1[,2]
samp1 = myz(samp1)

samp.mask1 = sapply(1:length(samp1[, 1]), function(j) get.sst.mask.val(samp1[j,1], samp1[j, 2], sstmat, 1))
samp.mask2 = sapply(1:length(samp2[, 1]), function(j) get.sst.mask.val(samp2[j,1], samp2[j, 2], sstmat, 1))

plot(samp1, col = c('grey90','lightblue','royalblue','purple')[samp.mask1], pch = 19)
points(sp[1], sp[2], pch = 21, cex =2, col=1, bg = 'pink')

points(samp2, pch = 19, col = hsv(0,.9,.9,alpha=.15))

