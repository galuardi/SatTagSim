
simdat = list()

boxmat = list()
boxmat$lon = unique(coordinates(rasbox)[,1])
boxmat$lat = sort(unique(coordinates(rasbox)[,2]))
boxmat$box = t(as.matrix(flip(rasbox, 2)))

for(i in 1:50){
  print(i)
  if(!is.null(bath)){
    msp = getsp(as.numeric(sp[[i]]), bath = bath)
  }else{
    msp = as.numeric(i)
  }
  uvmult = 30/seaslen

  # object will be the simulated track
  temp = NULL

  for(seas in simorder){   # winter, spring, summer, fall

    # browser()
    if(!is.null(sstmat)){
      sstdf = data.frame(expand.grid(sstmat$lon, sstmat$lat), sst = as.vector(sstmat$data[,,seas]))
    }


    for(j in 1:seaslen) {

      # msps = SpatialPoints(t(as.matrix(msp)))
      # tbox = raster::extract(rasbox, msps)

      xidx = which.min((msp[1] - boxmat$lon)^2)
      yidx = which.min((msp[2] - boxmat$lat)^2)

      tbox = boxmat$box[xidx, yidx]

      parbox = as.numeric(attributes(par_array)$dimnames[[1]])

      pbidx = which(parbox==tbox)

      u = c(par_array[pbidx, seas, 1])*uvmult
      v = c(par_array[pbidx, seas, 2])*uvmult
      D = c(par_array[pbidx, seas, 3])*uvmult
      usd = c(par_array[pbidx, seas, 4])
      vsd = c(par_array[pbidx, seas, 5])
      Dsd = c(par_array[pbidx, seas, 6])

      u = c(u, usd)
      v = c(v, vsd)
      D = c(D, Dsd)

      Dorig = D
      uorig = u
      vorig = v

      ulim = c(-50, 50)*uvmult
      vlim = c(-50, 50)*uvmult
      Dlim = c(0, 5000)*uvmult

      t1 = SatTagSim::simm.kf(2, u, v, D, msp, ulim, vlim, Dlim)[2,]

      if(!is.null(sstmat)){

      ii=1
      if((get.sst.mask.val(t1[1], t1[2], sstmat, seas)) < sstol){
        t1 = SatTagSim::simm.kf(2, u = c(-1*u[1], u[2]), v = c(-1*v[1], v[2]), D = c(D[1], 1000), msp, ulim, vlim, Dlim)[2,]
        # tsamp = find.next.sst(t1[1], t1[2], sstdf, sstol, expand = 5*ii)
        # t1 = as.numeric(tsamp)
          while((get.sst.mask.val(t1[1], t1[2], sstmat, seas)) < sstol){
              tsamp = find.next.sst(t1[1], t1[2], sstdf, sstol, expand = 5*ii)
              t1 = as.numeric(tsamp)
              ii = ii+1
          }
      }

      if(!is.null(bath)){
        while(.get.bath(t1[1], t1[2], bath)>0){
          t1 = simm.kf(2, u, v, D, msp, ulim, vlim, Dlim)[2,]
        }
      }
      temp = rbind(temp,  cbind(t(t1), seas))
      msp = ifelse(is.na(t1), msp, t1)

    }
 }

  tsim = as.data.frame(temp)
  names(tsim) = c('lon,','lat','Month')
  # lines(tsim[,1:2])
  simdat[[i]] = tsim
  }
}

# plot(rasbox)
plot(box11, add=F, border = 'salmon', xlim = c(-120, -0), ylim = c(0, 50))
image(seq(-100, 45, length = 145), seq(-50, 80, length = 130), rot90(as.array(rasbox)[,,1], 3), col = terrain.colors(12), add=T)
grid(nx = 29, ny = 26)
plot(map , add=T)
plot(box11, add=T, border = 'salmon')
degAxis(1)
degAxis(2)
box()


month.colors = data.frame(Month = as.numeric(month.colors[,1]), color = as.character(month.colors[,2]))
# names(month.colors) = c('Month', 'color')

simdat = lapply(simdat, function(x) merge(x, month.colors, by = 'Month')[,c('lon,','lat','Month', 'color')])

lapply(simdat, function(x) lines(x[,1], x[,2], col = as.character(x$color), typ ='o', pch = 19, cex = .4))


