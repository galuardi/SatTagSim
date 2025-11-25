# Required packages
install_if_missing <- function(pkgs){
  for(p in pkgs) if(!requireNamespace(p, quietly=TRUE)) install.packages(p)
}
install_if_missing(c("microbenchmark","foreach","doParallel","geosphere","MASS"))

library(microbenchmark)
library(doParallel)
library(foreach)
library(geosphere)
library(MASS)
devtools::install()
library(SatTagSim)
# 

# --- Replace these with your original and refactored function names ---
orig_fun <- make.sim.track.par2  # define/assign original version in your environment
refac_fun <- make.sim.track.par3         # refactored version from previous message

# --- Create realistic synthetic data ---
set.seed(42)
# grid
lon <- seq(-180, 180, length.out = 180)
lat <- seq(-90, 90, length.out = 90)
nlon <- length(lon); nlat <- length(lat); nmon <- 12

# bath: simple depth matrix (negative = ocean, positive = land)
bath_grid <- list(lon = lon, lat = lat,
                  data = matrix(sample(c(-5000, 100), nlon*nlat, replace = TRUE, prob=c(.85,.15)),
                                nrow = nlat, ncol = nlon))

# boxmat: spatial strata indices (just tile indexes)
box_idx_mat <- matrix(sample(1:50, nlon*nlat, replace=TRUE), nrow = nlon, ncol = nlat)
boxmat <- list(lon = lon, lat = lat, box = box_idx_mat)

# sstmat: 3D array months x lon x lat (we follow original dimension order used there)
sst_array <- array(sample(0:3, nlon*nlat*nmon, replace=TRUE, prob=c(.5,.2,.2,.1)),
                   dim = c(nlon, nlat, nmon))
sstmat <- list(lon = lon, lat = lat, data = sst_array)

# par_array: array [parbox, month, params(6)], create for 50 boxes
parbox_names <- as.character(1:50)
par_array <- array(runif(50 * nmon * 6, -2, 2), dim = c(50, nmon, 6),
                   dimnames = list(parbox_names, NULL, NULL))

# starting points: sample 100 points on ocean (non-land)
coords <- expand.grid(lon, lat)
bath_vals_vec <- as.vector(bath_grid$data)
ocean_idx <- which(bath_vals_vec < 0)
sample_idx <- sample(ocean_idx, 100)
spts <- lapply(sample_idx, function(i) as.numeric(coords[i, ]))

# small simorder and seaslen for quicker runs — adjust for longer realism
simorder <- 1:12   # six-month track
seaslen <- 30                 # days per month

# helper to ensure the original refactor functions are in scope:
# If your original function is defined in file, source it before running.

# --- Benchmark configuration ---
n_rep <- 3   # repeated runs for each function (increase for more stable timings)
registerDoParallel(cores = parallel::detectCores(logical = FALSE))

# Warm-up run to ensure JIT, packages loaded on workers, etc.
cat("Warm-up runs...\n")
invisible(refac_fun(par_array = par_array, simorder = simorder, sp = spts,
                     bath = bath_grid, sstmat = sstmat, boxmat = boxmat,
                     seaslen = seaslen, sstol = 2
                    #  , mcoptions = list()
                     ))

invisible(orig_fun(par_array = par_array, simorder = simorder, sp = spts,
                   bath = bath_grid, sstmat = sstmat, boxmat = boxmat,
                   seaslen = seaslen, sstol = 2
                  #  , mcoptions = list()
                   )
                   )

# Microbenchmark (may be slow if each call is heavy). We time a single outer call (full simulation)
cat("Running microbenchmark (this will take time)...\n")
mb <- microbenchmark(
  original = orig_fun(par_array = par_array, simorder = simorder, sp = spts,
                      bath = bath_grid, sstmat = sstmat, boxmat = boxmat,
                      seaslen = seaslen, sstol = 2
                      # , mcoptions = list()
                      ),
  refactored = refac_fun(par_array = par_array, simorder = simorder, sp = spts,
                         bath = bath_grid, sstmat = sstmat, boxmat = boxmat,
                         seaslen = seaslen, sstol = 2
                        #  , mcoptions = list()
                        ),
  times = n_rep, unit = "s"
)

print(mb)
summary(mb)


# original 

print(paste0('simulating ', length(spts)*msims, ' tracks for ', nyears, ' years'))
stime  =  Sys.time()
simdat  =  list()

ncores  =  detectCores()/2
cl  =  makeCluster(ncores, type = 'SOCK') # if on Linux, use the FORK!

mcoptions  =  setup.parallel()

# registerDoParallel(cl, cores  =  ncores)

mb1 = microbenchmark(
for(i in 1:12){
  subsp  =  spts[[i]][sample(1:nrow(spts[[i]]), msims, replace  =  T),]
  subsp$row  =  1:nrow(subsp)
  sp  =  dlply(subsp, 'row', function(x) x[,1:2])
  simorder  =  as.numeric(morder[i,])
  
  print(paste0('simulating ', length(sp), ' tracks starting in ', month.name[i]))
  
  # test  =  make.sim.track.par(tpar  =  simpar, morder  =  rep(simorder, nyears), sp  =  sp, bath  =  bath, sstmat  =  sstmat, seaslen  =  npmon, sstol  =  sstol, mcoptions  =  mcoptions)
    test  =  make.sim.track.par2(par_array  =  simpar
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
, times = 1
)

stopCluster(cl)
rm(test, sp, subsp)

simdat  =  unlist(simdat, recursive  =  F)

# gpt version 

mb2 = microbenchmark(
for(i in 1:12){
  subsp  =  spts[[i]][sample(1:nrow(spts[[i]]), msims, replace  =  T),]
  subsp$row  =  1:nrow(subsp)
  sp  =  dlply(subsp, 'row', function(x) x[,1:2])
  simorder  =  as.numeric(morder[i,])
  
  print(paste0('simulating ', length(sp), ' tracks starting in ', month.name[i]))
  
  # test  =  make.sim.track.par(tpar  =  simpar, morder  =  rep(simorder, nyears), sp  =  sp, bath  =  bath, sstmat  =  sstmat, seaslen  =  npmon, sstol  =  sstol, mcoptions  =  mcoptions)
    test  =  make.sim.track.par3(par_array  =  simpar
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
, times = 1
)

stopCluster(cl)
rm(test, sp, subsp)

simdat  =  unlist(simdat, recursive  =  F)



# Also capture system.time and memory for one run each
cat("\nSystem time original:\n")
t_orig <- system.time(oout <- orig_fun(par_array = par_array, simorder = simorder, sp = spts,
                                       bath = bath_grid, sstmat = sstmat, boxmat = boxmat,
                                       seaslen = seaslen, sstol = 2, mcoptions = list()))
print(t_orig)

cat("\nSystem time refactored:\n")
t_ref <- system.time(rout <- refac_fun(par_array = par_array, simorder = simorder, sp = spts,
                                       bath = bath_grid, sstmat = sstmat, boxmat = boxmat,
                                       seaslen = seaslen, sstol = 2, mcoptions = list()))
print(t_ref)

# Basic equivalence checks
compare_basic <- function(A, B){
  if(length(A) != length(B)) return(list(equal=FALSE, reason="length differs"))
  # compare per-track row counts and basic stats
  res <- lapply(seq_along(A), function(i){
    a <- A[[i]]; b <- B[[i]]
    if(is.null(a) || is.null(b)) return(list(equal=FALSE, reason="null track"))
    ok <- nrow(a) == nrow(b) &&
          all(names(a) == names(b)) &&
          all(is.finite(range(a$lon, na.rm=TRUE))) &&
          all(is.finite(range(b$lon, na.rm=TRUE)))
    list(equal = ok, nrow_a = nrow(a), nrow_b = nrow(b),
         lon_mean_diff = if(ok) mean(a$lon, na.rm=TRUE) - mean(b$lon, na.rm=TRUE) else NA)
  })
  res
}
cmp <- compare_basic(oout, rout)
print(head(cmp, 5))

# Save results
res_summary <- data.frame(
  function = c("original","refactored"),
  system_time_user = c(t_orig["user.self"], t_ref["user.self"]),
  system_time_elapsed = c(t_orig["elapsed"], t_ref["elapsed"]),
  stringsAsFactors = FALSE
)
write.csv(res_summary, file = "benchmark_summary.csv", row.names = FALSE)
write.csv(as.data.frame(mb), file = "microbenchmark_details.csv", row.names = FALSE)

cat("Benchmark complete. Summary written to benchmark_summary.csv and microbenchmark_details.csv\n")
