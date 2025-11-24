# Automated test: compare R-only vs Rcpp-patched versions
# Requires: Rcpp::sourceCpp("rcpp_helpers.cpp") already run, and functions:
#   make.sim.track.par2      (R-only refactor)
#   make.sim.track.par2_rcpp (Rcpp-patched)
# Also requires SatTagSim, MASS, foreach, doParallel, microbenchmark.

install_if_missing <- function(pkgs){
  for(p in pkgs) if(!requireNamespace(p, quietly=TRUE)) install.packages(p)
}
install_if_missing(c("microbenchmark","doParallel","foreach","MASS","Rcpp","geosphere","SatTagSim"))

library(microbenchmark)
library(doParallel)
library(foreach)
library(MASS)
library(geosphere)
library(SatTagSim)

# --- Synthetic data (small but realistic) ---
set.seed(123)
lon <- seq(-180, 180, length.out = 60)   # coarse grid
lat <- seq(-90, 90, length.out = 30)
nlon <- length(lon); nlat <- length(lat); nmon <- 12

# bath grid: ocean negative, land positive
bath_grid <- list(lon = lon, lat = lat,
                  data = matrix(sample(c(-4000, 100), nlon*nlat, replace = TRUE, prob = c(.85,.15)),
                                nrow = nlat, ncol = nlon))

# boxmat: spatial strata indices
box_idx_mat <- matrix(sample(1:20, nlon*nlat, replace=TRUE), nrow = nlon, ncol = nlat)
boxmat <- list(lon = lon, lat = lat, box = box_idx_mat)

# sstmat: dims [lon, lat, month]
sst_array <- array(sample(0:3, nlon*nlat*nmon, replace=TRUE, prob=c(.5,.2,.2,.1)),
                   dim = c(nlon, nlat, nmon))
sstmat <- list(lon = lon, lat = lat, data = sst_array)

# par_array: [parbox, month, 6]
parbox_names <- as.character(1:20)
par_array <- array(runif(20 * nmon * 6, -2, 2), dim = c(20, nmon, 6),
                   dimnames = list(parbox_names, NULL, NULL))

# starting points: pick 20 ocean points
coords <- expand.grid(lon, lat)
bath_vals_vec <- as.vector(bath_grid$data)
ocean_idx <- which(bath_vals_vec < 0)
sample_idx <- sample(ocean_idx, 20)
spts <- lapply(sample_idx, function(i) as.numeric(coords[i, ]))

simorder <- sample(1:12, 4)  # 4-month track
seaslen <- 15                # shorter for quick tests

# Ensure Rcpp helpers are compiled
if(!exists("find_next_sst_cpp")) {
  if(file.exists("rcpp_helpers.cpp")) {
    Rcpp::sourceCpp("rcpp_helpers.cpp")
  } else stop("rcpp_helpers.cpp not found; place it in working dir or compile helpers first.")
}

# Ensure both functions exist
if(!exists("make.sim.track.par2")) stop("R-only function make.sim.track.par2 not found.")
if(!exists("make.sim.track.par2_rcpp")) stop("Rcpp-patched function make.sim.track.par2_rcpp not found.")

# Small wrapper to standardize mcoptions for local single-threaded runs
mcoptions_local <- list()  # depends on your setup; empty works for sequential backend

# Warm-up both functions once
cat("Warm-up runs...\n")
invisible(make.sim.track.par2(par_array = par_array, simorder = simorder, sp = spts,
                              bath = bath_grid, sstmat = sstmat, boxmat = boxmat,
                              seaslen = seaslen, sstol = 2, mcoptions = mcoptions_local))
invisible(make.sim.track.par2_rcpp(par_array = par_array, simorder = simorder, sp = spts,
                                   bath = bath_grid, sstmat = sstmat, boxmat = boxmat,
                                   seaslen = seaslen, sstol = 2, mcoptions = mcoptions_local))

# Single-run timing (system.time)
cat("Timing single run (system.time)...\n")
t_r <- system.time(out_r <- make.sim.track.par2(par_array = par_array, simorder = simorder, sp = spts,
                                                 bath = bath_grid, sstmat = sstmat, boxmat = boxmat,
                                                 seaslen = seaslen, sstol = 2, mcoptions = mcoptions_local))
t_rcpp <- system.time(out_rcpp <- make.sim.track.par2_rcpp(par_array = par_array, simorder = simorder, sp = spts,
                                                           bath = bath_grid, sstmat = sstmat, boxmat = boxmat,
                                                           seaslen = seaslen, sstol = 2, mcoptions = mcoptions_local))
print(list(R_only = t_r, Rcpp_patched = t_rcpp))

# Microbenchmark (3 repeats)
cat("Running microbenchmark (3 reps)...\n")
mb <- microbenchmark(
  R_only = make.sim.track.par2(par_array = par_array, simorder = simorder, sp = spts,
                               bath = bath_grid, sstmat = sstmat, boxmat = boxmat,
                               seaslen = seaslen, sstol = 2, mcoptions = mcoptions_local),
  Rcpp_patched = make.sim.track.par2_rcpp(par_array = par_array, simorder = simorder, sp = spts,
                                          bath = bath_grid, sstmat = sstmat, boxmat = boxmat,
                                          seaslen = seaslen, sstol = 2, mcoptions = mcoptions_local),
  times = 3, unit = "s"
)
print(mb)

# Correctness checks
cat("Correctness checks:\n")
check_lengths <- function(A, B) {
  la <- length(A); lb <- length(B)
  list(len_equal = la == lb, lenA = la, lenB = lb)
}
len_check <- check_lengths(out_r, out_rcpp)
print(len_check)

# Per-track compare: row counts and mean lon/lat differences
compare_tracks <- function(A, B) {
  n <- min(length(A), length(B))
  res <- data.frame(track = seq_len(n),
                    nrow_A = sapply(A[1:n], function(x) if(is.data.frame(x)) nrow(x) else NA),
                    nrow_B = sapply(B[1:n], function(x) if(is.data.frame(x)) nrow(x) else NA),
                    lon_mean_diff = sapply(seq_len(n), function(i) {
                      a <- A[[i]]; b <- B[[i]]
                      if(is.null(a) || is.null(b) || nrow(a)==0 || nrow(b)==0) return(NA)
                      mean(a$lon, na.rm=TRUE) - mean(b$lon, na.rm=TRUE)
                    }),
                    lat_mean_diff = sapply(seq_len(n), function(i) {
                      a <- A[[i]]; b <- B[[i]]
                      if(is.null(a) || is.null(b) || nrow(a)==0 || nrow(b)==0) return(NA)
                      mean(a$lat, na.rm=TRUE) - mean(b$lat, na.rm=TRUE)
                    }),
                    stringsAsFactors = FALSE)
  res
}
track_cmp <- compare_tracks(out_r, out_rcpp)
print(track_cmp)

# Save results
write.csv(track_cmp, file = "track_comparison.csv", row.names = FALSE)
write.csv(as.data.frame(mb), file = "microbenchmark_details.csv", row.names = FALSE)

cat("Done. Summary:\n")
cat(sprintf("R-only elapsed: %.2fs\nRcpp-patched elapsed: %.2fs\n", t_r["elapsed"], t_rcpp["elapsed"]))
cat("Per-track comparison saved to track_comparison.csv; microbenchmark details saved to microbenchmark_details.csv\n")
