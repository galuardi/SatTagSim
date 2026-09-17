test_that("simm.kf generates expected simulation matrix dimensions", {
  set.seed(42)
  sim <- simm.kf(n = 50, u = c(5, 1), v = c(2, 1), D = c(100, 20), sp = c(-70, 40))

  expect_true(is.matrix(sim))
  expect_equal(nrow(sim), 51)
  expect_equal(ncol(sim), 2)
  expect_equal(colnames(sim), c("lon", "lat"))
  expect_equal(unname(sim[1, "lon"]), -70, tolerance = 1e-4)
  expect_equal(unname(sim[1, "lat"]), 40, tolerance = 1e-4)
})

test_that("rmask2array converts raster stack to 3D matrix without matlab package", {
  data("rmask", package = "SatTagSim")
  arr <- rmask2array(rmask)

  expect_type(arr, "list")
  expect_named(arr, c("lon", "lat", "data"))
  expect_equal(length(dim(arr$data)), 3)
  expect_equal(dim(arr$data)[1], length(arr$lon))
  expect_equal(dim(arr$data)[2], length(arr$lat))
})

test_that("get.box.vals assigns points to spatial polygons correctly", {
  data("box7", package = "SatTagSim")
  pts <- data.frame(
    lon = c(-64.21, -65.0, -50.0),
    lat = c(44.20, 43.0, 30.0),
    Month = c(10, 10, 10)
  )

  bvals <- get.box.vals(pts, box7)
  expect_s3_class(bvals, "data.frame")
  expect_true("box" %in% names(bvals))
  expect_equal(bvals$box[1], 3)
})

test_that("get.first.box processes track list and transitions correctly", {
  data("sim_example", package = "SatTagSim")
  data("box7", package = "SatTagSim")

  res <- get.first.box(simdat[1:2], syear = 2000, boxes = box7, seas.len = 90)
  expect_s3_class(res, "data.frame")
  expect_true(all(c("Year", "season", "TagID", "pbox", "btrans", "cbox") %in% names(res)))

  # Check alias get_first_box_mod
  res_alias <- get_first_box_mod(simdat[1:2], syear = 2000, boxes = box7, seas.len = 90)
  expect_equal(res, res_alias)
})

test_that("get.trans.prob and make.markov.chain compute transitions and state sequence", {
  data("sim_example", package = "SatTagSim")
  data("box7", package = "SatTagSim")

  fbox <- get.first.box(simdat[1:3], syear = 2000, boxes = box7, seas.len = 90)
  tmat <- get.trans.prob(fbox, nyears = 1, adims = c(7, 7, 4), perc = TRUE)

  expect_type(tmat, "list")
  expect_length(tmat, 4)
  expect_named(tmat, c("Winter", "Spring", "Summer", "Fall"))
  # Probabilities per row should sum to 1 (fillone applied)
  for (m in tmat) {
    expect_true(all(abs(rowSums(m) - 1) < 1e-6))
  }

  chain <- make.markov.chain(tmat, s.init = 3, sorder = rep(1:4, 25))
  expect_length(chain, 100)
  expect_true(all(chain >= 1 & chain <= 7))
})
