## Run a simulation of full western data using time/area paramters


# Load Data
setwd('C:/Users/ben/Google Drive/PHD/COLLABORATIONS/SCRS_SIMS/')
load('LPRC_NOAA_GBYP_AZTI_DATA.Rdata')

# Load PRELIM DATA
print('loading fish data')
# load("C:/Users/ben/Google Drive/PHD/tagd/SENSITIVTY_RUNS/prelim_datasets.Rdata")
load("../../tagd/SENSITIVTY_RUNS/prelim_datasets.Rdata")

# subset PLRC for large fish only
bigidx = lprc$Length>=185
lprc = lprc[bigidx,]

# add Dsd to noaa data
noaa$Dsd = 0


tracks = lprc[,c('Day','Month','Year','TagID' , 'coords.x1', 'coords.x2', 'D', 'Dsd')]
names(tracks)[5:6] = c('Longitude','Latitude')

tracks = rbind(tracks@data, noaa[,c('Day','Month','Year','TagID' , 'Longitude','Latitude', 'D', 'Dsd')])
coordinates(tracks) = ~Longitude+Latitude


######

# make raster from box structure for inline query during simulation

rasbox = raster(box11, nrow = 26*5, ncol = 29*5)
extent(rasbox) = extent(box11)
# values(rasbox) = box11@data$ID

rasbox = rasterize(box11, rasbox, 'ID')

# fix the bay of biscay
rasbox[cellFromXY(rasbox, c(-2.5, 42.5))] = 8
rasbox[cellFromXY(rasbox, c(-2.5, 45.5))] = 8

# make a matrix for speed
boxmat = list()
boxmat$lon = unique(coordinates(rasbox)[,1])
boxmat$lat = sort(unique(coordinates(rasbox)[,2]))
boxmat$box = t(as.matrix(flip(rasbox, 2)))


plot(box11, add=F, border = 'salmon')
image(seq(-100, 45, length = dim(rasbox)[2]), seq(-50, 80, length = dim(rasbox)[1]), rot90(as.array(rasbox)[,,1], 3), col = tim.colors(12), add=T)
grid(nx = 29, ny = 26)
plot(map , add=T)
plot(box11, add=T, border = 'salmon')
degAxis(1)
degAxis(2)

## make a 3D array of parameters
## Month and 11 box area

tracks@proj4string = box11@proj4string
tracks$box = over(tracks, box11)$ID

missing = (1:11)%in%unique(tracks$box)

par2 = daply(as.data.frame(tracks), c('box', 'TagID', 'Month'), function(x) get.uv(x[,c('Day','Month','Year','Longitude','Latitude')]))
pnames = dimnames(par2)
pnames$box = as.character(1:11)
par_test = array(NA, dim = c(11, dim(par2)[2:4]), dimnames = pnames)
par_test[missing,,,] = par2
par2 = par_test
rm(par_test)

tagwts = daply(as.data.frame(tracks), c('box', 'Month'), function(x) nrow(x))
pnames = dimnames(tagwts)
pnames$box = as.character(1:11)
tw2 = array(NA, dim = c(11, dim(tagwts)[2]), dimnames = pnames)
tw2[missing,] = tagwts
tagwts = tw2
rm(tw2)


# advection
ubox = apply(par2[,,,1], 3, rowMeans, na.rm=T)*-1
vbox = apply(par2[,,,2], 3, rowMeans, na.rm=T)

### for missing months, use the most recent months value.. not using weighted values here...here

fill.month.par = function(x){
  require(magic)

  if(length(which(!is.na(x))) == 0){
       y=x
  } else{
  sn = min(which(!is.na(x)))
  y = magic::shift(x, sn-1)

    for(i in which(is.na(y))) y[i] = y[which(!is.na(y))[max(which(which(!is.na(y)) < i))]]

  if(sn != 1) y = magic::shift(y, -(sn-1))
  }
  y
  }

apply(ubox, 1, function(x) fill.month.par(x))




