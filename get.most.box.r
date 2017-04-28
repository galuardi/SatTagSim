get.most.box <- function(simdatdf, syear=2000, boxes = box7, time.int = 'seqmonth', boxname = 'box', sim.len = 720){

  # do this for each tagid...

  n = sim.len

  names(simdat) = 1:length(simdat)
  datbox1 = get.box.vals(simdatdf[,2:4], boxes = boxes)
  datbox1$TagID = as.vector(apply(data.frame(names(simdat)), 1, rep, n)) # 90 days in a season
  years = syear:((syear+n)-1)
  # sdat = datbox1[1:720,]

  get.most.box.month = function(sdat, time.len = 24, days = 30, boxname = 'box'){
    mseq = as.vector(sapply(1:time.len, function(i) rep(i, days)))
    sdat$seqmonth = rep(mseq, length(unique(sdat$TagID))) # not good naming...
    tagbox = as.numeric(attributes((daply(sdat, c('seqmonth', boxname), function(x) nrow(x))))$dimnames[[2]])
    most.box = tagbox[apply(daply(sdat, c('seqmonth', boxname), function(x) nrow(x)),1,which.max)]
    most.box
  }

  # simdatsub = ldply(simdat, function(x) x[seq(1, n, by = seas.len),])[,2:4]
  simdatsub = ddply(datbox1, 'TagID', function(x) data.frame(Month = rep(unique(x$Month), 2), cbox = get.most.box.month(x)))

  #
  # msimdat = melt(simdat, c('lon,','lat','Month'))
  # names(msimdat)[4] = 'TagID'
  # box.vals = get.box.vals(msimdat, boxes = box11)
  # msimdat$season = box.vals$season
  # msimdat$box = box.vals$box
  # mseq = as.vector(sapply(1:24, function(i) rep(i,30)))
  # msimdat$seqmonth = rep(mseq, length(unique(msimdat$TagID)))
  #
  #
  # blist = dlply(msimdat, c('TagID'), function(x) table(x$box, x$seqmonth))
  #
  # bdf = lapply(blist, function(x) as.numeric(row.names(x)[apply(x, 2, which.max)]))
  # row.names(bdf) = bdf[,1]
  # bdf = bdf[,2:25]
  #
  #
  #
  # dlply(msimdat, .(TagID, month), function(x) table(x$box, x$season))
  #
  # head(melt(dlply(msimdat, .(TagID), function(x) table(x$box, x$season))))
  #
  # dlply(msimdat, .(TagID), function(x) )
  #
  #
  # names(simdat) = 1:length(simdat)
  # # datbox1 = get.box.vals(simdat, boxes = boxes)
  #
  #
  # simdatsub = ldply(simdat, function(x) x[seq(1, n, by = 90),])[,2:4] # 90 days in a season
  # # datbox1 = get.box.vals(simdatsub, boxes = boxes)
  #
  # datbox1$TagID = as.vector(apply(data.frame(names(simdat)), 1, rep, n/90)) # 90 days in a season
  # years = syear:((syear+n/360)-1)
  # datbox1$Year = as.vector(apply(data.frame(years), 1, rep, n/180)) # 180 is the total of 2 seasons for each year
  # datbox1$cbox = datbox1$box
  #
  # flbox = ddply(datbox1, c('Year','season', 'TagID'), function(x) c(x$box[1]))
  #
  # names(flbox)[4] = c('cbox')
  #==================================#
  # function that calculates movement in terms of changes from the previous area	i.e. box 4-->6 = 2, box 6-->4 = -2
  #==================================#
  get.btrans=function(x){
    nseas = nrow(x)
    c(0,diff(x$cbox))
  }

  flbox = ddply(datbox1, 'TagID', function(x) data.frame(x, btrans=get.btrans(x)))

  flbox$pbox = flbox$cbox-flbox$btrans

  flbox = flbox[,c('Year','season','TagID','pbox','btrans','cbox')]

  flbox
}

