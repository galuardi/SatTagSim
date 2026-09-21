# ==============================================================================
# Modernized Dolphinfish Simulation Workflow (sim_ns_dolphinfish_modern.R)
# ==============================================================================

# User working directory
setwd('C:/Users/benjamin.galuardi/Documents/MYPROJECTS/MAHI-TMP/')

# Required Libraries
library(SatTagSim)
library(tidyverse)
library(sf)
library(raster)
library(fields)
library(rnaturalearth)
library(parallel)
library(doParallel)

# Helper function: convert RasterLayer to tibble for ggplot2
ras2df <- function(r) {
  rpts <- raster::coordinates(r)
  tibble::as_tibble(rpts) |>
    dplyr::mutate(val = as.vector(raster::values(r)))
}

# ------------------------------------------------------------------------------
# 1. Base Data & Land Polygon
# ------------------------------------------------------------------------------
data(myramps, package = "SatTagSim")
data(woasst, package = "SatTagSim")

myworld <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

seasons <- c("Winter", "Spring", "Summer", "Fall")
cols <- setNames(month.colors[, 2], month.colors[, 1])

# ------------------------------------------------------------------------------
# 2. Temperature Preference Raster Mask (rmask)
# ------------------------------------------------------------------------------
# Filter WOA SST between 18°C and 32°C for dolphinfish
rmask1 <- woasst
rmask1$sst[rmask1$sst > 32 | rmask1$sst < 18] <- NA
rmask1$sst[!is.na(rmask1$sst)] <- 1

# Convert 3D array slices to raster stack
my_rmask_layers <- lapply(seq_len(dim(rmask1$sst)[3]), function(i) {
  m <- rmask1$sst[, , i]
  # Rotate and orient properly
  raster::raster(
    x = t(m)[ncol(m):1, , drop = FALSE],
    xmn = min(rmask1$lon),
    xmx = max(rmask1$lon),
    ymn = min(rmask1$lat),
    ymx = max(rmask1$lat),
    crs = "+proj=longlat +datum=WGS84 +no_defs"
  )
})

my_rmask <- raster::stack(my_rmask_layers)
names(my_rmask) <- month.name

# ------------------------------------------------------------------------------
# 3. Stratification Boxes & Tag Data
# ------------------------------------------------------------------------------
my_boxes <- sf::read_sf("dolphinfish_boxes.shp")
if (!"ID" %in% names(my_boxes) && "Id" %in% names(my_boxes)) {
  my_boxes$ID <- my_boxes$Id
}
my_boxes_sp <- sf::as_Spatial(my_boxes)

# dolphinfish tracking observations
dlfish <- read.csv("dlfish.csv", header = TRUE, stringsAsFactors = FALSE) |>
  sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE) |>
  dplyr::mutate(
    seas = dplyr::case_when(
      Month %in% c(1, 2, 3) ~ "Winter",
      Month %in% c(4, 5, 6) ~ "Spring",
      Month %in% c(7, 8, 9) ~ "Summer",
      Month %in% c(10, 11, 12) ~ "Fall"
    ),
    Month = factor(Month, levels = 1:12),
    seas = factor(seas, levels = seasons)
  )

# Plot observations with boxes and land
ggplot() +
  geom_sf(data = myworld, fill = "grey90", color = "grey50") +
  geom_sf(data = my_boxes, fill = NA, color = "salmon", linewidth = 0.8) +
  geom_sf_text(data = my_boxes, aes(label = ID), size = 5, color = "#7f0e0e") +
  geom_sf(data = dlfish, aes(color = Month), size = 1.2, alpha = 0.8) +
  scale_color_manual(values = cols, na.value = NA) +
  coord_sf(xlim = c(-100, -15), ylim = c(10, 50), crs = 4326) +
  theme_bw() +
  labs(title = "Dolphinfish Electronic Tag Observations")

# ------------------------------------------------------------------------------
# 4. Rasterized Tag Density by Season
# ------------------------------------------------------------------------------
make.tag.raster <- function(simdat, xmn = -100, xmx = 30, ymn = 0, ymx = 60, boxsize = 60) {
  r <- raster::raster(
    nrow = (ymx - ymn) * 60 / boxsize,
    ncol = (xmx - xmn) * 60 / boxsize,
    xmn = xmn, xmx = xmx, ymn = ymn, ymx = ymx
  )
  simdat$CID <- 1
  raster::rasterize(simdat, r, field = "CID", fun = "count")
}

rdf <- dlfish |>
  split(dlfish$seas) |>
  purrr::map(make.tag.raster) |>
  purrr::map(ras2df) |>
  dplyr::bind_rows(.id = "season") |>
  dplyr::rename(locations = val) |>
  dplyr::mutate(season = factor(season, levels = seasons))

ggplot(rdf) +
  geom_tile(aes(x, y, fill = locations)) +
  scale_fill_viridis_c(na.value = "transparent") +
  geom_sf(data = myworld, fill = "grey90", color = "grey50") +
  coord_sf(xlim = c(-100, -20), ylim = c(10, 50), crs = 4326) +
  facet_wrap(~season) +
  theme_bw() +
  labs(title = "Tag Density by Season", x = "", y = "")

# ------------------------------------------------------------------------------
# 5. World Ocean Atlas Climatology & Thermal Preference
# ------------------------------------------------------------------------------
sstr <- raster::flip(
  raster::raster(
    xmn = min(woasst$lon), xmx = max(woasst$lon),
    ymn = min(woasst$lat), ymx = max(woasst$lat),
    resolution = c(diff(woasst$lon)[1], diff(woasst$lat)[1]),
    vals = t(apply(woasst$sst, 1:2, mean, na.rm = TRUE))
  ),
  direction = "y"
)

# Convert my_rmask to properly aligned 3D sstmat array
sstmat <- SatTagSim::rmask2array(my_rmask)

# 3-Month rolling sum of suitable habitat
sstdata2 <- sstmat$data
for (i in 1:12) {
  idx <- if (i == 1) c(12, 1, 2) else if (i == 12) c(11, 12, 1) else c(i - 1, i, i + 1)
  sstdata2[, , i] <- apply(sstmat$data[, , idx], 1:2, sum, na.rm = TRUE) + sstmat$data[, , i]
}
sstdata2[is.na(sstdata2)] <- 0
sstmat$data <- sstdata2

# ------------------------------------------------------------------------------
# 6. Parameter Array Estimation
# ------------------------------------------------------------------------------
# Extract coordinates and ensure Month is numeric
xy <- sf::st_coordinates(dlfish)
dlfish_df <- sf::st_drop_geometry(dlfish) |>
  dplyr::mutate(
    Longitude = xy[, 1],
    Latitude = xy[, 2],
    Month = as.numeric(as.character(Month))
  )

# Add spoof record for January if data are missing
spoof <- dlfish_df[1, ]
spoof[] <- NA
spoof$Year <- 2025
spoof$Month <- 1
spoof$Day <- 1
spoof$Longitude <- 0
spoof$Latitude <- 0
dlfish_sp <- dplyr::bind_rows(dlfish_df, spoof) |>
  sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) |>
  sf::as_Spatial()

simpar <- SatTagSim::make.par.array(
  tracks = dlfish_sp,
  inbox = my_boxes_sp,
  rasbox = NULL,
  rrows = 26 * 5,
  rcols = 29 * 5,
  use_wts = NULL,
  missvec = c(1),
  fillvec = c(2)
  # missvec = c(1, 3, 4, 6),
  # fillvec = c(2, 2, 5, 5)
)

# ------------------------------------------------------------------------------
# 7. Simulation Setup & Starting Points
# ------------------------------------------------------------------------------
msims <- 50    # simulations started per month
npmon <- 4     # steps per month
nyears <- 1    # years per track
sstol <- 2     # minimum suitable months threshold

# Release month ordering matrix
morder <- array(rep(1:12, 12), dim = c(12, 12))
for (i in 1:12) {
  morder[i, ] <- if (i == 1) 1:12 else c(i:12, 1:(i - 1))
}
rownames(morder) <- month.abb
colnames(morder) <- paste0("m", 1:12)

# Starting points extraction
ds <- as.data.frame(dlfish_sp) |>
  dplyr::rename('Longitude' = 'coords.x1', 'Latitude' = 'coords.x2') |> 
  dplyr::select(TagID, Day, Month, Year, Longitude, Latitude)

spts <- SatTagSim::get.start.pts(ds, msims, months = 2:12, posnames = c("Longitude", "Latitude"))

# Spoof January start points from empirical distribution
spts_spoof <- dplyr::bind_rows(spts) |>
  dplyr::slice_sample(n = 50, replace = TRUE) |>
  as.data.frame()

spts <- c(list(spts_spoof), spts)
names(spts) <- 1:12

# Box raster matrix for simulation
rasbox <- SatTagSim::make.rasbox(my_boxes_sp, raster = TRUE)
boxmat <- list(
  lon = unique(raster::coordinates(rasbox)[, 1]),
  lat = sort(unique(raster::coordinates(rasbox)[, 2])),
  box = t(as.matrix(raster::flip(rasbox, direction = 2)))
)

# ------------------------------------------------------------------------------
# 8. Parallel Simulations
# ------------------------------------------------------------------------------
mcoptions <- SatTagSim::setup.parallel()

cat("Simulating", length(spts) * msims, "tracks for", nyears, "year(s)...\n")
stime <- Sys.time()
simdat <- vector("list", 12)

ncores <- max(1, floor(parallel::detectCores() / 2))
cl <- parallel::makeCluster(ncores, type = ifelse(.Platform$OS.type == "windows", "SOCK", "FORK"))
doParallel::registerDoParallel(cl, cores = ncores)

for (i in 1:12) {
  subsp <- spts[[i]][sample(nrow(spts[[i]]), msims, replace = TRUE), ]
  subsp$row <- seq_len(nrow(subsp))
  sp <- split(subsp[, 1:2], subsp$row)
  simorder <- as.numeric(morder[i, ])

  cat("Simulating", length(sp), "tracks starting in", month.name[i], "...\n")

  test <- SatTagSim::sim_tracks_par(
    par_array = simpar,
    boxmat = boxmat,
    simorder = rep(simorder, nyears),
    sp = sp,
    bath = NULL,
    sstmat = sstmat,
    seaslen = npmon,
    sstol = sstol,
    mcoptions = mcoptions
  )
  simdat[[i]] <- test
}

parallel::stopCluster(cl)
cat("Simulations complete. Elapsed time:", format(Sys.time() - stime), "\n")

# Flatten into track list
simdat <- unlist(simdat, recursive = FALSE)

# ------------------------------------------------------------------------------
# 9. Simulation Post-Processing & Rasterization
# ------------------------------------------------------------------------------
simdatdf <- dplyr::bind_rows(simdat)
if ("lon," %in% names(simdatdf)) {
  names(simdatdf)[names(simdatdf) == "lon,"] <- "lon"
}

simdatdf <- simdatdf |>
  dplyr::mutate(
    seas = dplyr::case_when(
      Month %in% c(1, 2, 3) ~ 1,
      Month %in% c(4, 5, 6) ~ 2,
      Month %in% c(7, 8, 9) ~ 3,
      Month %in% c(10, 11, 12) ~ 4
    )
  )

mycol <- colorRampPalette(c("lightcyan", "royalblue", "blue", "lemonchiffon", "orange", "red"), space = "Lab")

sr <- split(simdatdf, simdatdf$seas) |>
  purrr::map(~ SatTagSim::make.sim.raster(.x, boxsize = 150)) |>
  purrr::map(ras2df) |>
  dplyr::bind_rows(.id = "season_idx") |>
  dplyr::mutate(
    season = factor(seasons[as.numeric(season_idx)], levels = seasons),
    norm_val = val / max(val, na.rm = TRUE)
  )

ggplot(sr) +
  geom_tile(aes(x, y, fill = norm_val)) +
  geom_sf(data = myworld, fill = "grey90", color = "grey50") +
  geom_sf(data = my_boxes, fill = "transparent", color = "salmon", linewidth = 0.8) +
  geom_sf_text(data = my_boxes, aes(label = ID), size = 5, color = "darkred") +
  scale_fill_gradientn(colors = mycol(256), na.value = NA) +
  coord_sf(xlim = c(-100, 40), ylim = c(0, 70), crs = 4326) +
  facet_wrap(~season) +
  theme_bw() +
  labs(title = "Simulated Track Density by Season", x = "", y = "", fill = "Density")

# ------------------------------------------------------------------------------
# 10. Seasonal Transition Probability Matrices
# ------------------------------------------------------------------------------
# npmon * 3 = steps per season (e.g. 4 steps/month * 3 = 12 steps/season)
datbox <- SatTagSim::get.first.box(
  simdat = simdat,
  syear = 2000,
  boxes = my_boxes_sp,
  seas.len = npmon * 3
)

nboxes <- nrow(my_boxes)
boxtrans <- SatTagSim::get.trans.prob(
  datbox = datbox,
  nyears = 100,
  adims = c(nboxes, nboxes, 4),
  perc = TRUE
)

names(boxtrans) <- seasons

# Plot movement transition rates
SatTagSim::plot.boxtrans(
  boxtrans = boxtrans,
  text.size = 5,
  strip.text = 14,
  axis.text = 15,
  text.col = "black",
  palette = "Greens"
)

