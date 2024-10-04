get.start.pts <- function (dat, n = 100, months = 1:12, mask = rmask, posnames = c("coords.x1", 
                                                                  "coords.x2"), xmn = -100, xmx = 30, ymn = 0, ymx = 60, ...) 
{
  spts = lapply(months, function(i) {
    tmp = as.data.frame(dat[dat$Month == i, ])
    nidx = which(names(tmp) %in% posnames)
    names(tmp)[nidx] = c("lon", "lat")
    tmpr = make.sim.raster(tmp, xmn = xmn, xmx = xmx, ymn = ymn, ymx = ymx)
    tmpr[tmpr < 0] = 0
    tmpr = raster::resample(tmpr, mask) * mask[[i]]
    tmpr[tmpr < 0] = 0
    tmpdf = as.data.frame(as(tmpr, "SpatialPointsDataFrame"))
    tmpdf[, 1] = tmpdf[, 1]/max(tmpdf[, 1])
    pts = tmpdf[sample(1:nrow(tmpdf), n, prob = tmpdf[, 1], 
                       replace = T), 2:3]
  })
  names(spts) = months
  spts
}