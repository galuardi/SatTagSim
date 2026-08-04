## ----setwd, eval = T---------------------------------------------------------------------------------------------------------------------------------------------
# setwd("/Users/wessleymerten/Desktop/data_analysis/movement_matrix/")


## ----global_options, include = FALSE-----------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo  =  FALSE, warning  =  FALSE, 
											message  =  FALSE, cache  =  FALSE,
											progress  =  TRUE, verbose  =  FALSE
											, comment  =  FALSE
											, fig.width  =  8
											, fig.height  =  6
											# , dev  =  'html'
											)



## ----setup_data, eval  =  T--------------------------------------------------------------------------------------------------------------------------------------
setwd('C:/Users/benjamin.galuardi/Documents/MYPROJECTS/MAHI-TMP/')

library(SatTagSim) # use this is already installed
devtools::load_all('C:/Users/benjamin.galuardi/Documents/GitHub/SatTagSim/') # use this when testing
library(rworldmap)
library(rworldxtra)
library(tidyverse)
library(raster)
library(sf)
data(myramps, package = 'SatTagSim')
data(woasst)
# load('C:/Users/benjamin.galuardi/Documents/GitHub/SatTagSim/data/myramps.rda') # use this approach if the ramps dont load


## ----make rmask, eval = T----------------------------------------------------------------------------------------------------------------------------------------

rmask1 = woasst
rmask1$sst[rmask1$sst > 32] = NA
rmask1$sst[rmask1$sst < 18] = NA
rmask1$sst[!is.na(rmask1$sst)] = 1

my_rmask = apply(rmask1$sst, MARGIN = 3, FUN = function(x){
raster(x = rot90(x, 1)
       , xmn = min(rmask1$lon)
       , xmx = max(rmask1$lon)
       , ymn = min(rmask1$lat)
       , ymx = max(rmask1$lat))
}
) 

 my_rmask =  stack(my_rmask)

  raster::crs(my_rmask) = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

  names(my_rmask) = month.name

my_rmask_raster <- my_rmask


## ----setup_boxes, eval= T----------------------------------------------------------------------------------------------------------------------------------------

my_boxes <- read_sf("dolphinfish_boxes.shp") |> 
    mutate(`ID` = `Id`) |> 
  sf::as_Spatial() 

myworld =  rworldmap::getMap(resolution  =  'high') %>% 
  sf::st_as_sf()

cols = c(month.colors[,2])
names(cols) = month.colors[,1]

seasons  =  c("Winter", "Spring", "Summer", "Fall")

# get data ----
dlfish <- read.csv("dlfish.csv", header=TRUE, sep=",")
dlfish <- sf::st_as_sf(dlfish, coords = c('Longitude', 'Latitude'), crs=4326) 

dlfish = dlfish  %>% 
sf::st_as_sf() %>% 
mutate(seas = case_when(Month %in% c(1,2,3) ~ 'Winter'
,Month%in%c(4,5,6)  ~'Spring'
,Month%in%c(7,8,9)  ~ 'Summer'
,Month%in%c(10,11,12)  ~'Fall'
)) %>% 
mutate(Month = as.factor(Month), seas = as.factor(seas)) #|> sf::as_Spatial()


dlfish %>% 
  st_set_crs(4326) %>% 
  ggplot() +
  # 1. Plot the fish tracks
  geom_sf(aes(col = Month)) +
  scale_color_manual(values = cols, aesthetics = c("colour", "fill"), na.value = NA) +
  # 2. Plot the land map
  geom_sf(data = myworld, fill = 'grey90', col = 'grey50') +
  # 3. Plot the grid boxes
  geom_sf(data = my_boxes |> st_as_sf(), fill = NA) + 
  # 4. Add the labels (Change 'ID' to your actual column name, e.g., 'plotOrder')
  geom_sf_text(data = my_boxes|> st_as_sf(), aes(label = Id), size = 6, color = "#7f0e0e") +
  # 5. Set coordinates and theme
  coord_sf(xlim = c(-100, -15), ylim = c(10, 50), crs = 4326, datum = sf::st_crs(4326)) +
  theme_bw()

## ----plot_tags_raster, fig.width  =  8, fig.height  =  6, caption  =  'dolphinfish.'-----------------------------------------------------------------------------

make.tag.raster  <- function(simdat, xmn  =  -100, xmx  =  30, ymn  =  0, ymx  =  60, boxsize =  60){
  r  =  raster(nrow = (ymx-ymn)*60/boxsize, ncol =  (xmx-xmn)*60/boxsize, xmn  =  xmn, xmx  =  xmx, ymn  =  ymn, ymx  =  ymx)
    simdat$CID  =  1
  sr  =  rasterize(simdat, r, field  =  'CID', fun = 'count') #[[1]]
  return(sr)
}

ras2df = function(r){
  rpts  =  coordinates(r)
  rpts = rpts %>%
    as_tibble() %>%
    mutate(val = as.vector(values(r)))
  rpts
}


rdf  =  dlfish %>% 
group_by(seas) %>%
group_map(~make.tag.raster(.x)) 

names(rdf) = seasons


rdf = lapply(rdf, ras2df)

for(i in 1:4) rdf[[i]]$season = factor(names(rdf)[i])
# for(i in 1:12) rdf[[i]]$Month = factor(names(rdf)[i]) 

rdf = do.call(rbind, rdf) %>% 
dplyr::rename('locations' = 'val')


rdf %>%              
ggplot()+
  geom_tile( mapping = aes(x, y, fill = locations))+
  scale_fill_viridis_c()+
  geom_sf(myworld, mapping = aes(), fill = 'grey90', col = 'grey50')+
  coord_sf(xlim = c(-100, -20), ylim = c(10, 50), crs = NULL, datum = sf::st_crs(4326)) +
  xlab('')+
  ylab('')+
#  facet_wrap(~season)+
  theme_bw()+
      theme(strip.text = element_text(size = 14)
          # ,  axis.title.x = element_text(size = axis.text)
          # ,  axis.title.y = element_text(size = axis.text)
          # , legend.title = element_blank()
          )



## plot my boxes 

my_boxes %>% 
  sf::st_as_sf() %>% 
  ggplot()+
  geom_sf(myworld, mapping = aes(), fill = 'grey90', col = 'grey50')+
  geom_sf(col = 'salmon', lwd = 1, fill = 'transparent')+
  coord_sf(xlim = c(-100, -20), ylim = c(10, 50), crs = NULL, datum = sf::st_crs(4326)) +
  geom_text(data = my_boxes@data, inherit.aes = F, mapping = aes(coordinates(my_boxes)[,1], coordinates(my_boxes)[,2]), cex = 8, label =  my_boxes@data$Zone, col = 'darkred')+
  xlab('')+
  ylab('')+
  theme_bw()


## ----plot_boxes, fig.show  =  'asis', echo  =  F, eval  =  T, fig.width  =  8, fig.height  =  6, fig.cap  =  '11- box stratification from Lauretta et al. (2016) spatial summary'----

lpts  =  coordinates(my_boxes)

my_boxes %>% 
  sf::st_as_sf() %>% 
  ggplot()+
  geom_sf(myworld, mapping = aes(), fill = 'grey90', col = 'grey50')+
  geom_sf(col = 'salmon', lwd = 1, fill = 'transparent')+
  coord_sf(xlim = c(-100, -30), ylim = c(0, 50), crs = NULL, datum = sf::st_crs(4326)) +
  geom_text(data = my_boxes@data, inherit.aes = F, mapping = aes(lpts[,1], lpts[,2]), cex = 8, label =  my_boxes@data$Id, col = 'darkred')+
  xlab('')+
  ylab('')+
  theme_bw()




## ----plot_WOA_1, eval  =  T, fig.show  =  'asis', fig.cap  =  'World Ocean Atlas (2013) mean temperature'--------------------------------------------------------
library(fields)
library(raster)
library(matlab)

# Do it as a raster!!
sstr  =  flip(raster(xmn  =  min(woasst$lon), xmx  =  max(woasst$lon), ymn  =  min(woasst$lat), ymx  =  max(woasst$lat), resolution  =  c(diff(woasst$lon)[1], diff(woasst$lat)[1]), vals  =  t(apply(woasst$sst, 1:2, mean, na.rm  =  T))), 'y')


sstr  %>%  
  ras2df()  %>%  
  ggplot()+
  geom_tile(mapping = aes(x,y, fill = val))+
  scale_fill_gradientn(colors = tim.colors(256), na.value = NA)+
  geom_sf(myworld, mapping = aes(), fill = 'grey90', col = 'grey50')+
   coord_sf(xlim = c(-100, 40), ylim = c(0, 70), crs = NULL, datum = sf::st_crs(4326))+
  theme_bw()+
  ggtitle('World Ocean Atlas mean temperature')+
  xlab('')+
  ylab('')+
  theme(legend.title=element_blank(), axis.title.x = element_text(''), axis.title.y = element_text(''))



## ----plot_ecdf, eval  =  T, fig.show  =  'asis',  fig.cap  =  'Tag Measured Temperatures'------------------------------------------------------------------------

dlfishdf =  dlfish %>% 
  sf::st_as_sf() 

dlfish_vlines = dlfishdf %>% 
  as.data.frame() %>% 
  group_by(Month) %>% 
  dplyr::summarise(meant = mean(MaxTemp, na.rm = T)
                   , sdt =  sd(MaxTemp, na.rm = T)
                   , qlo = quantile(MaxTemp, .025, na.rm = T)
                   , qhi = quantile(MaxTemp, .975, na.rm = T)) %>% 
 mutate(sdt_low = meant - 1.96*sdt, sdt_hi =  meant + 1.96*sdt) 

 dlfishdf %>% 
   mutate(`Month Name` = month.name[Month]) %>% 
  ggplot(aes(MaxTemp)) +
  stat_ecdf(geom = "step")+
  geom_vline(data = dlfish_vlines, aes(xintercept = meant), color = 'red')+
   geom_vline(data = dlfish_vlines, aes(xintercept = qlo), color = 'lightblue', size = .75, linetype = 5)+
   geom_vline(data = dlfish_vlines, aes(xintercept = qhi), color = 'lightblue', size = .75, linetype = 5)+
  facet_wrap(~Month)+
   ylab('')+
   theme_light()

## ----sst_pref_plot, fig.cap  =  'Number of months with suitable surface temperature based on WOA and tag measured temperatures'----------------------------------
sstmat  =  list(lon  =  sort(unique(coordinates(my_rmask[[1]])[,1]))
                ,lat  =  sort(unique(coordinates(my_rmask[[1]])[,2]))
                )
  sstdata  =  array(NA, dim  =  dim(my_rmask)[c(2,1,3)])


  for(i in 1:(dim(my_rmask)[3])) sstdata[,,i]  =  rot90(as.array(my_rmask)[,,i],3)
  
  sstdata[is.na(sstdata)]  =  0
  
sstmat$data  =  sstdata

sstmatr  =  t(flip(raster(apply(sstmat$data, 1:2, sum, na.rm  =  T)), 'x'))
extent(sstmatr)  =  extent(sstr)

woadf = ras2df(sstmatr) %>% 
  dplyr::rename('Months' = 'val') 

woadf$Months[woadf$Months == 0] = NA

ggplot(woadf)+
  geom_sf(myworld, mapping = aes(), col = 'grey20', fill = 'grey90')+
  geom_tile(mapping = aes(x,y, fill = Months))+
  scale_fill_viridis_c(na.value  = 'transparent')+
  coord_sf(xlim = c(-100, 40), ylim = c(0, 70), crs = NULL, datum = sf::st_crs(4326)) +
  xlab('')+
  ylab('')+
  theme_bw()


## ----sst_pref_plot_12months, fig.cap  =  'Number of months with suitable surface temperature. This plot shows a rolling sum of the current and next months binary values. This assists the simulation avoid entrainment in poor areas while accounting for where the fish might go next.'----

sstdata[sstdata==0]  =  NA

sstdata2  =  sstdata

  for(i in 1:12){
    idx  =  c(i, i+1)
    if(i == 1){
        idx  =  c(12, 1, 2)
    }
    if(i == 12){
      idx  =  c(11, 12, 1) 
    }
    else idx  =  c(i-1, i, i+1)
    sstdata2[,,i]  =  apply(sstdata[,,idx], 1:2, sum, na.rm  =  T)+sstdata[,,i]
  }

sstdata2[is.na(sstdata2)]  =  0  
sstmat$data  =  sstdata2



sstdata2 = sstdata2  %>%  apply(3, FUN = function(x) {raster(x)  %>%  t()  %>%  flip(direction = 2)})  %>%  stack()

extent(sstdata2) = c(min(sstmat$lon), max(sstmat$lon), min(sstmat$lat), max(sstmat$lat))

names(sstdata2) = month.name

sstdata_coords <- xyFromCell(sstdata2, seq_len(ncell(sstdata2)))
sstdata2 <- stack(as.data.frame(getValues(sstdata2)))
names(sstdata2) <- c('n_suitable', 'Month')
sstdata2 <- cbind(sstdata_coords, sstdata2)


ggplot(sstdata2) + 
  geom_tile(aes(x, y, fill = n_suitable)) +
  facet_wrap(~ Month) +
  scale_fill_viridis_c() +
  geom_sf(myworld, mapping = aes(), col = 'grey20', fill = 'grey90')+
  coord_sf(xlim = c(-100, 40), ylim = c(0, 70), crs = NULL, datum = sf::st_crs(4326)) +
  xlab('')+
  ylab('')+
  theme_bw()

## ----get_params, echo  =  T, eval  =  T---------------------------------------------------------------------------------------------------------------------

xy = st_coordinates(dlfish)

# spoof a line for january to fool simpar creation (didn't work...)
spoof <- dlfish |> 
 mutate(Longitude = xy[,1]
    , Latitude = xy[,2]
    , Month = as.numeric(as.character(Month))) |>
  slice(1) |> 
  # Set every column to NA first
  mutate(across(everything(), ~ NA)) |> 
  # Fill in your custom values
  mutate(
    Year = 2025
    , Month = 1
    , Day = 1
    , Longitude = 0
    , Latitude = 0
  )


dlfish_sp <- dlfish |> 
 mutate(Longitude = xy[,1]
    , Latitude = xy[,2]
    , Month = as.numeric(as.character(Month))) |>
  bind_rows(spoof) |> 
  sf::as_Spatial()


simpar  =  make.par.array(tracks  =  dlfish_sp
  , inbox  =  my_boxes
  , rasbox  =  NULL
  , rrows  =  26*5
  , rcols  =  29*5
  , use_wts  =  NULL
  # , missvec = NULL
  # ,  fillvec = NULL
   , missvec  =  c(1,3,4,6), fillvec  =  c(2,2,5,5)
)
 

# make plots of the simpar tables ----
# skip if you don't need these outputs 

# iwalk loops through the list of 2D matrices and provides the index (.y)
iwalk(asplit(round(simpar, 3), 3), ~ {
  
  # 1. Create a dynamic filename (e.g., "simpar_slice_1.csv")
  filename <- paste0("simpar_slice_", .y, ".csv")
  
  # 2. Write the 2D matrix (.x) directly to a CSV file
  # Note: row.names = FALSE prevents R from adding an extra column of row numbers
  write.csv(.x, file = filename, row.names = FALSE)
  
})


#------------------------------------------------------------------------#
# Set number of sims, simulation steps per month and number of years

msims  =  50 # simulations to be started in each month
npmon  =  4 # number of simulation steps per month.
nyears  =  1 # number of years to simulate for each track
nreps  =  50 # number of replicates for variance calculation
sstol  =  2   # number of suitable months for each daily simulation step
#------------------------------------------------------------------------#

simvals  =  data.frame(sim_param  =  c('msims','npmon','nyears','sstol'), value  =  c(msims, npmon, nyears, sstol), description  =  c('simulations to be started in each month','number of simulation steps per month','number of years to simulate for each track','number of suitable months for each daily simulation step'))

# knitr::kable(simvals, caption  =  "Control parameters for simulation") %>% kableExtra::kable_paper()

#------------------------------------------------------------------------#
# Set month order of simulations to correspond to release month
morder  =  array(rep(1:12, 12), dim = c(12, 12))

for(i in 1:12){
  if(i==1) morder[i,]  =  1:12
  else morder[i,]  =  c(i:12,1:((1:12)[i]-1))
}
#------------------------------------------------------------------------#

morder  =  data.frame(morder, row.names  =  month.abb)
names(morder)  =  paste0('m',1:12)

# knitr::kable(morder, caption  =  'Month order for multi-month start simulation', row.names  =  T) %>% kableExtra::kable_paper()

## ----get_start_points_1, eval  =  T, echo  =  T------------------------------------------------------------------------------------------------------------------
tracksdf <- dlfish %>% 
  filter(!is.na(box)) %>% 
  # Extract X (Longitude) and Y (Latitude) from the geometry
  mutate(
    Longitude = sf::st_coordinates(.)[, "X"],
    Latitude  = sf::st_coordinates(.)[, "Y"],
    Month = as.numeric(Month)
  ) %>% 
  # Now it is safe to drop the spatial geometry
  sf::st_drop_geometry()


## ----get_start_points, eval  =  T, echo  =  T--------------------------------------------------------------------------------------------------------------------
ds  =  dlfish |> 
  as_Spatial() |> 
  as.data.frame() |> 
  dplyr::rename(Longitude = coords.x1, Latitude = coords.x2)

ds  =  ds[,c('TagID','Day','Month','Year','Longitude','Latitude')]

# no data for January
spts  =  get.start.pts(ds, msims, months  =  2:12, posnames  =  c('Longitude','Latitude'))

# need to spoof starting points 
spts_spoof = do.call(rbind, spts) |> 
  sample_n(50) |> 
  as.data.frame()

spts = c(list(spts_spoof), spts)
names(spts) = 1:12


## ----make_boxmat, eval  =  T-------------------------------------------------------------------------------------------------------------------------------------
rasbox  =  make.rasbox(my_boxes, raster  =  T)

# make a matrix for speed
boxmat  =  list()
boxmat$lon  =  unique(coordinates(rasbox)[,1])
boxmat$lat  =  sort(unique(coordinates(rasbox)[,2]))
boxmat$box  =  t(as.matrix(flip(rasbox, 2)))



## ----simulate, eval  =  F----------------------------------------------------------------------------------------------------------------------------------------
# 
mcoptions  =  setup.parallel()
# 
# #---------------------------------------------------------------#
# # SIMULATIONS
# 
print(paste0('simulating ', length(spts)*msims, ' tracks for ', nyears, ' years'))
stime  =  Sys.time()
simdat  =  list()

ncores  =  detectCores()/2
cl  =  makeCluster(ncores, type = 'SOCK') # if on Linux, use the FORK!
registerDoParallel(cl, cores  =  ncores)

for(i in 1:12){
  subsp  =  spts[[i]][sample(1:nrow(spts[[i]]), msims, replace  =  T),]
  subsp$row  =  1:nrow(subsp)
  sp  =  dlply(subsp, 'row', function(x) x[,1:2])
  simorder  =  as.numeric(morder[i,])

  print(paste0('simulating ', length(sp), ' tracks starting in ', month.name[i]))

    test  =  sim_tracks_par(par_array  =  simpar
                                 , boxmat  =  boxmat
                                 , simorder  =  rep(simorder, nyears)
                                 , sp  =  sp
                                 , bath  =  NULL
                                 , sstmat  =  sstmat
                                 , seaslen  =  npmon
                                 , sstol  =  sstol
                                 , mcoptions  =  mcoptions)
  simdat[[i]]  =  test
  runtime  =  Sys.time()-stime
  print(paste0('elapsed time: ', runtime))
}

stopCluster(cl)
rm(test, sp, subsp)

# makes each list element a simulation 
simdat  =  unlist(simdat, recursive  =  F)

## ----make simdatdf data frame, eval  =  T, fig.cap  =  'Posterior distribution of simulated tracks, 1-minute cell size, by season'-------------------------------
# MAKE A DATA FRAME OF THE RESULTS
simdatdf = simdat |> bind_rows()

seasons  =  c("Winter", "Spring", "Summer", "Fall")
# seasidx  =  make.seas.idx(simdatdf)
simdatdf$seas[simdatdf$Month%in%c(1,2,3)]  =  1
simdatdf$seas[simdatdf$Month%in%c(4,5,6)]  =  2
simdatdf$seas[simdatdf$Month%in%c(7,8,9)]  =  3
simdatdf$seas[simdatdf$Month%in%c(10,11,12)]  =  4

#---------------------------------------------------------------#
# Make Rasters of Results

mycol  =  colorRampPalette(c("lightcyan", "royalblue", "blue", "lemonchiffon", "orange", "red"), space  =  "Lab")

sr  =  dlply(simdatdf, 'seas', function(x) make.sim.raster(x, boxsize  =  150))

sr = lapply(sr, ras2df)

names(sr) = seasons

for(i in 1:4){
  sr[[i]]$season = factor(seasons[i])
}

sr = do.call(rbind, sr)

sr  %>%  
  ggplot()+
  geom_tile(mapping = aes(x,y, fill = val/max(val, na.rm = T)))+
  geom_sf(myworld, mapping = aes(), fill = 'grey90', col = 'grey50')+
  scale_fill_gradientn(colors = mycol(256), na.value = NA)+
  facet_wrap(~season)+
  geom_sf(my_boxes %>%  sf::st_as_sf(), mapping = aes(), col = 'salmon', lwd = 1, fill = 'transparent')+
   geom_text(data = my_boxes@data, inherit.aes = F, mapping = aes(rep(lpts[,1],4), rep(lpts[,2],4)), cex = 8, label =  rep(my_boxes@data$ID, 4), col = 'darkred')+
    coord_sf(xlim = c(-100, 40), ylim = c(0, 70), crs = NULL, datum = sf::st_crs(4326)) +
  xlab('')+
  ylab('')+
  theme_bw()+
  theme(strip.text.x = element_text(size = 12), strip.text.y = element_text(size = 12))+
  labs(fill = 'Density')
  


## ----get_transmat_rate, eval  =  T, caption  =  'Transitions expressed as a rate'--------------------------------------------------------------------------------

# try new version of function... it works!!
# datbox  =  get.first.box(simdat, 2000, my_boxes, seas.len  =  npmon*3) # season length is number per month*3

# source('get_first_box_mod.R')

datbox  =  get_first_box_mod(simdat, 2000, my_boxes, seas.len  =  npmon*3) # season length is number per month*3

boxtrans  =  get.trans.prob(datbox, nyears = 100, adims  =  c(6, 6, 4), perc  =  T)

names(boxtrans)  =  seasons

plot.boxtrans(boxtrans, text.size = 5, strip.text = 14, axis.text = 15, text.col = 'black', palette = 'Greens')

## ----plot_simdf_bymonth, eval  =  T, fig.cap  =  'Posterior distribution of simulated tracks, 1-minute cell size, by month'--------------------------------------

sr <- simdatdf |> 
  split(simdatdf$Month) |> 
  map(~ make.sim.raster(.x, boxsize = 150))

sr = lapply(sr, ras2df)

names(sr) = month.name

for(i in 1:12){
  sr[[i]]$Month = month.name[i]
}

sr = do.call(rbind, sr)

# make month a factor so it plots in order
sr$Month = factor(sr$Month, levels = month.name)

sr  %>%  
  ggplot()+
  geom_tile(mapping = aes(x,y, fill = val/max(val, na.rm = T)))+
  geom_sf(myworld, mapping = aes(), fill = 'grey90', col = 'grey50')+
  scale_fill_gradientn(colors = mycol(256), na.value = NA)+
  facet_wrap(~Month)+
  geom_sf(myworld, mapping = aes(), fill = 'grey90', col = 'grey50')+
  geom_sf(my_boxes %>%  sf::st_as_sf(), mapping = aes(), col = 'salmon', lwd = 1, fill = 'transparent')+
    coord_sf(xlim = c(-100, 40), ylim = c(0, 70), crs = NULL, datum = sf::st_crs(4326)) +
  xlab('')+
  ylab('')+
  theme_bw()+
  theme(strip.text.x = element_text(size = 12), strip.text.y = element_text(size = 12))+
  labs(fill = 'Density')
  