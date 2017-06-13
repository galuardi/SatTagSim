#' Make spatio-temporal array of movement parameters
#' This function generates monthly advection and diffusion mean and standard deviation for each area in the spatial strata used. Missing months default to the previous months value. Missing areas should be filled in by the user.
#' @param tracks spatial points data frame of tracks
#' @param inbox spatial polygons (shapefile) of areas
#' @param rasbox optional raster version of spatial polygons for areas. If included, rrows and rcols may be NULL
#' @param rrows number of rows for raster creation
#' @param rcols number of columns for raster creation
#' @param use_wts T/F for using weights
#' @param missvec vector of missing rows (areas) in final output. This can be obtained by running once with this function left as NULL
#' @param fillvec vector of replacement rows for missvec. This is a subjectie decision by the user and is dependent on the spatial strata used
#'
#' @return
#' @export
#'
#' @details
#' The default size raster created is 130 degrees latitude and 145 degrees longitude, with 5 degree cells. This should be specific to the spatial strata used (e.g. the 11 box model) and should ideally have cells that split the areas along ploygon lines.
#'
#' @examples
make.par.array <- function(tracks, inbox, rasbox = NULL, rrows = 26*5, rcols = 29*5, use_wts = NULL, missvec = NULL, fillvec = NULL){

require(abind)
require(magic)

  # browser()

fill.month.par = function(x){
  require(magic)

  if(length(which(!is.na(x))) == 0){
    y=x
  } else{
    sn = min(which(!is.na(x)))
    sn = -sn+1
    y = magic::shift(x, sn)

    for(i in which(is.na(y))) y[i] = y[which(!is.na(y))[max(which(which(!is.na(y)) < i))]]

    if(sn != -1) y = magic::shift(y, -sn)
  }
  y
}


  if(is.null(rasbox))  {
    rasbox = raster(inbox, nrow = rrows, ncol = rcols)
    extent(rasbox) = extent(inbox)

    rasbox = rasterize(inbox, rasbox, 'ID')

    ## fix the bay of biscay
    # rasbox[cellFromXY(rasbox, c(-2.5, 42.5))] = 8
    # rasbox[cellFromXY(rasbox, c(-2.5, 45.5))] = 8

  }

  # make a matrix for speed
boxmat = list()
boxmat$lon = unique(coordinates(rasbox)[,1])
boxmat$lat = sort(unique(coordinates(rasbox)[,2]))
boxmat$box = t(as.matrix(flip(rasbox, 2)))


## make a 3D array of parameters
## Month and 11 box area

tracks@proj4string = inbox@proj4string
tracks$box = as.numeric(over(tracks, inbox)$ID)
tracks = tracks[!is.na(tracks$box),]

boxvec = sort(inbox@plotOrder)

missing = (boxvec)%in%as.numeric(unique(tracks$box))

par2 = daply(as.data.frame(tracks), c('box', 'TagID', 'Month'), function(x) get.uv(x[,c('Day','Month','Year','Longitude','Latitude')]))
pnames = dimnames(par2)
pnames$box = as.character(boxvec)
par_test = array(NA, dim = c(length(boxvec), dim(par2)[2:4]), dimnames = pnames)
par_test[missing,,,] = par2
par2 = par_test
rm(par_test)

  if(!is.null(use_wts)){
    tagwts = daply(as.data.frame(tracks), c('box', 'Month'), function(x) nrow(x))
    pnames = dimnames(tagwts)
    pnames$box = as.character(boxvec)
    tw2 = array(NA, dim = c(length(boxvec), dim(tagwts)[2]), dimnames = pnames)
    tw2[missing,] = tagwts
    tagwts = tw2
    rm(tw2)
  }


# advection
ubox = apply(par2[,,,1], 3, rowMeans, na.rm=T)*-1
vbox = apply(par2[,,,2], 3, rowMeans, na.rm=T)

### for missing months, use the most recent months value.. not using weighted values here...here

u2 = t(apply(ubox, 1, function(x) fill.month.par(x)))
v2 = t(apply(vbox, 1, function(x) fill.month.par(x)))


# advection variance
ubox_sd = apply(par2[,,,1], c(1,3), sd, na.rm=T)
vbox_sd = apply(par2[,,,2], c(1,3), sd, na.rm=T)

u2_sd = t(apply(ubox_sd, 1, function(x) fill.month.par(x)))
v2_sd = t(apply(vbox_sd, 1, function(x) fill.month.par(x)))



# Diffusion
d_month_box = ddply(as.data.frame(tracks), 'box', get.kfD)

dbox = daply(d_month_box, c('box', 'Month'), function(x) mean(x$D))
dbox_sd = daply(d_month_box, c('box', 'Month'), function(x) mean(x$Dsd))
pnames = dimnames(dbox)
pnames$box = as.character(boxvec)
par_test = array(NA, dim = c(length(boxvec), dim(dbox)[2]), dimnames = pnames)
par_test[missing,] = dbox
dbox = par_test

par_test = array(NA, dim = c(length(boxvec), dim(dbox)[2]), dimnames = pnames)
par_test[missing,] = dbox_sd
dbox_sd = par_test
rm(par_test)

# fill in box/months with no observations
dbox = t(apply(dbox, 1, function(x) fill.month.par(x)))
dbox_sd = t(apply(dbox_sd, 1, function(x) fill.month.par(x)))

if(!is.null(missvec)){

  for(i in 1:length(missvec)){
    u2[missvec[i],] = u2[fillvec[i],]
    v2[missvec[i],] = v2[fillvec[i],]
    u2_sd[missvec[i],] = u2_sd[fillvec[i],]
    v2_sd[missvec[i],] = v2_sd[fillvec[i],]
    dbox[missvec[i],] = dbox[fillvec[i],]
    dbox_sd[missvec[i],] = dbox_sd[fillvec[i],]
  }

  # u2_sd[is.na(u2_sd)] = 0 # if there are not enough obs for an sd, fill in zeros

  # do I need this???
   for(i in 1:ncol(u2_sd)) u2_sd[is.na(u2_sd[,i]), i] =  sd(u2_sd[,i], na.rm=T)
  for(i in 1:ncol(v2_sd)) v2_sd[is.na(v2_sd[,i]), i] =  sd(v2_sd[,i], na.rm=T)
  dbox_sd[is.na(dbox_sd)] = 0 # if there are not enough obs for an sd, fill in a global sd for that month

}

# Build the array: Box x Month x c(u, v, D, sd.u, sd.v, sd.D)
par_array = abind::abind(u2, v2, dbox, u2_sd, v2_sd, dbox_sd, along = 3, new.names = c('u','v','D','u.sd','v.sd','D.sd'))
par_array

}


