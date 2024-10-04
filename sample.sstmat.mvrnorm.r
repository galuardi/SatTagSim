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

sp = c(-95.375, 23.125)

nsp = myzinv(sp)

pbidx = 1
seas = 3
uvmult = 1

u = c(par_array[pbidx, seas, 1])*uvmult
v = c(par_array[pbidx, seas, 2])*uvmult
D = c(par_array[pbidx, seas, 3])*uvmult
usd = c(par_array[pbidx, seas, 4])
vsd = c(par_array[pbidx, seas, 5])
Dsd = c(par_array[pbidx, seas, 6])

u = c(u, usd)
v = c(v, vsd)
D = c(D, Dsd)

samp2 = mvrnorm(100, mu = c(u[1], v[1]) , Sigma = matrix(c(u[2],0,0,v[2]) , 2, 2))
samp2[,1] = nsp[1]+samp2[,1]
samp2[,2] = nsp[2]+samp2[,2]
samp2 = myz(samp2)

# use this one!! this emulates simm.kf
samp1 = mvrnorm(1000, mu = c(u[1], v[1]) , Sigma = matrix(c(2*583, 0, 0, 2*583) , 2, 2))
samp1[,1] = nsp[1]+samp1[,1]
samp1[,2] = nsp[2]+samp1[,2]
samp1 = myz(samp1)

samp.mask1 = sapply(1:length(samp1[, 1]), function(j) get.sst.mask.val(samp1[j,1], samp1[j, 2], sstmat, 1))
samp.mask2 = sapply(1:length(samp2[, 1]), function(j) get.sst.mask.val(samp2[j,1], samp2[j, 2], sstmat, 1))

plot(samp1, col = c('grey90','lightblue','royalblue','purple')[samp.mask1], pch = 19, xlim = c(-100, -70), ylim = c(10, 35))
points(sp[1], sp[2], pch = 21, cex =2, col=1, bg = 'pink')

points(samp2, pch = 19, col = hsv(0,.9,.9,alpha=.15))
world(add=T, col = 'grey70', fill=T)
lines(simm.kf(100 ,u, v, D, sp), typ='o', pch = 19, cex = .3)

# explore what sign the u advection should be... look at all areas and

par(mfrow = c(3,4))
  for(i in 1:12){
  plot(box11, add=F, border = 'salmon', xlim = c(-110, -70), ylim = c(10, 35), main = month.abb[i])
  # image(seq(-100, 45, length = dim(rasbox)[2]), seq(-50, 80, length = dim(rasbox)[1]), rot90(as.array(rasbox)[,,1], 3), col = grey.colors(12), add=T)
  grid(nx = 29, ny = 26)
  plot(map , add=T)
  plot(box11, add=T, border = 'salmon')
  degAxis(1)
  degAxis(2)

  pt = coordinates(box11)[1,]

  pt = c(-96, 22)

  for(j in 1:100){
      pbidx = 1  # box value
      seas = i # month/season
      seaslen = 1 # multiplier for daily or monthly sims
      uvmult = 30/seaslen

      u = c(par_array[pbidx, seas, 1])*uvmult
      v = c(par_array[pbidx, seas, 2])*uvmult
      D = c(par_array[pbidx, seas, 3])*uvmult
      usd = c(par_array[pbidx, seas, 4])
      vsd = c(par_array[pbidx, seas, 5])
      Dsd = c(par_array[pbidx, seas, 6])

      u = c(u, usd)
      v = c(v, vsd)
      D = c(D, Dsd)

      ulim = c(-50, 50)*uvmult
      vlim = c(-50, 50)*uvmult
      Dlim = c(0, 5000)*uvmult

     # lines(simm.kf(seaslen+1 ,u, v, D, pt, ulim, vlim, Dlim)[2:seaslen,], typ='o', pch = 19, col = hsv(seq(0,1, length =12)[i], .9, .9), cex = .3)
      lines(simm.kf(seaslen+1 ,u, v, D, pt, ulim, vlim, Dlim)[2:seaslen,], typ='o', pch = 19, col = 1, cex = .3)

  }
  points(pt[1], pt[2], bg = 3, pch = 21, cex = 2)
  }


legend('bottom', ncol = 4, legend = month.abb, lty = 1, pch = 19, pt.cex = 1, col = hsv(seq(0,1, length =12), .9, .9))

