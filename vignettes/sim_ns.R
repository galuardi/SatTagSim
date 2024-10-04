## ----eval = F, echo = T--------------------------------------------------
#  library(SatTagSim)
#  data(box7)
#  data(rmask)
#  sst = rmask2array(rmask)
#  mcoptions = setup.parallel()

## ---- fig.width = 7, fig.show = 'asis', echo = F, eval = T---------------
library(fields)
library(raster)
library(SatTagSim)
data(box7)
plot(box7, xlim = c(-100, 40), ylim = c(0,65))
world(add=T)
plot(box7, add = T, border = 'salmon', lwd=2)
text(coordinates(box7)[,1], coordinates(box7)[,2], box7@data[,3], cex = 2, col = 'darkred')
degAxis(1)
degAxis(2)
box()
title('7-box')

