# make raster from box structure for inline query during simulation

rasbox = raster(box11, nrow = 26*5, ncol = 29*5)
extent(rasbox) = extent(box11)
# values(rasbox) = box11@data$ID

rasbox = rasterize(box11, rasbox, 'ID')

# fix the bay of biscay
rasbox[cellFromXY(rasbox, c(-2.5, 42.5))] = 8
rasbox[cellFromXY(rasbox, c(-2.5, 45.5))] = 8

plot(box11, add=F, border = 'salmon')
image(seq(-100, 45, length = 29), seq(-50, 80, length = 26), rot90(as.array(rasbox)[,,1], 3), col = tim.colors(12), add=T)
grid(nx = 29, ny = 26)
plot(map , add=T)
plot(box11, add=T, border = 'salmon')
degAxis(1)
degAxis(2)

## make a 3D array of parameters
## Month and 11 box area

nsfish@proj4string = box11@proj4string
nsfish$box = over(nsfish, box11)$ID

missing = (1:11)%in%unique(nsfish$box)

par2 = daply(as.data.frame(nsfish), c('box', 'TagID', 'Month'), function(x) get.uv(x[,c('Day','Month','Year','Longitude','Latitude')]))
pnames = dimnames(par2)
pnames$box = as.character(1:11)
par_test = array(NA, dim = c(11, dim(par2)[2:4]), dimnames = pnames)
par_test[missing,,,] = par2
par2 = par_test
rm(par_test)

tagwts = daply(as.data.frame(nsfish), c('box', 'Month'), function(x) nrow(x))
pnames = dimnames(tagwts)
pnames$box = as.character(1:11)
tw2 = array(NA, dim = c(11, dim(tagwts)[2]), dimnames = pnames)
tw2[missing,] = tagwts
tagwts = tw2
rm(tw2)

# advection
ubox = apply(par2[,,,1], 3, rowMeans, na.rm=T)*-1
vbox = apply(par2[,,,2], 3, rowMeans, na.rm=T)

for(i in 1:ncol(ubox)) ubox[is.na(ubox[,i]),i] = stats::weighted.mean(ubox[,i], w = tagwts[,i], na.rm = T)

for(i in 1:ncol(vbox)) vbox[is.na(vbox[,i]),i] = stats::weighted.mean(vbox[,i], w = tagwts[,i], na.rm = T)

# advection variance
ubox_sd = apply(par2[,,,1], c(1,3), sd, na.rm=T)
vbox_sd = apply(par2[,,,2], c(1,3), sd, na.rm=T)

for(i in 1:ncol(ubox_sd)) ubox_sd[is.na(ubox_sd[,i]),i] = stats::weighted.mean(ubox_sd[,i], w = tagwts[,i], na.rm = T)

for(i in 1:ncol(vbox_sd)) vbox_sd[is.na(vbox_sd[,i]),i] = stats::weighted.mean(vbox_sd[,i], w = tagwts[,i], na.rm = T)

# Diffusion
d_month_box = ddply(as.data.frame(nsfish), 'box', get.kfD)

dbox = daply(d_month_box, c('box', 'Month'), function(x) mean(x$D))
dbox_sd = daply(d_month_box, c('box', 'Month'), function(x) sd(x$D))
pnames = dimnames(dbox)
pnames$box = as.character(1:11)
par_test = array(NA, dim = c(11, dim(dbox)[2]), dimnames = pnames)
par_test[missing,] = dbox
dbox = par_test

par_test = array(NA, dim = c(11, dim(dbox)[2]), dimnames = pnames)
par_test[missing,] = dbox_sd
dbox_sd = par_test

rm(par_test)

for(i in 1:ncol(dbox)) dbox[is.na(dbox[,i]),i] = stats::weighted.mean(dbox[,i], w = tagwts[,i], na.rm = T)

# Diffusion variance
for(i in 1:ncol(dbox_sd)) dbox_sd[is.na(dbox_sd[,i]),i] = stats::weighted.mean(dbox_sd[,i], w = tagwts[,i], na.rm = T)

# Build the array: Box x Month x c(u, v, D, sd.u, sd.v, sd.D)
par_array = abind::abind(ubox, vbox, dbox, ubox_sd, vbox_sd, dbox_sd, along = 3, new.names = c('u','v','D','u.sd','v.sd','D.sd'))

# ADD EXTRACT FUNCTION TO SIMULATION
# ADD PAR_ARRAY TO SIMULATION


