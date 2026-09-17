test_that("get.uv computes advection parameters from coordinates and dates", {
  track <- data.frame(
    Day = 1:5,
    Month = rep(6, 5),
    Year = rep(2020, 5),
    Longitude = seq(-70, -68, length.out = 5),
    Latitude = seq(40, 42, length.out = 5)
  )

  uv <- get.uv(track)
  expect_length(uv, 2)
  expect_true(all(is.finite(uv)))
})

test_that("get.kfD summarizes diffusion parameters with group aggregation", {
  data("nsfish", package = "SatTagSim")
  df <- as.data.frame(nsfish)

  res <- get.kfD(df[1:50, ])
  expect_s3_class(res, "data.frame")
  expect_true(all(c("Month", "TagID", "D", "Dsd", "nrec") %in% names(res)))
  expect_false(any(is.na(res$D)))
  expect_false(any(is.na(res$Dsd)))
})

test_that("get.allpar and merge.par work together correctly", {
  data("nsfish", package = "SatTagSim")
  df <- as.data.frame(nsfish)
  # Take subset of tags
  df_sub <- df[df$TagID %in% unique(df$TagID)[1:2], ]

  uvpar <- get.allpar(df_sub)
  expect_s3_class(uvpar, "data.frame")
  expect_true(all(c("TagID", "Month", "u", "v", "D", "nrec") %in% names(uvpar)))

  Dpar <- get.kfD(df_sub)
  merged_all <- merge.par(uvpar, Dpar, return.mean = FALSE)
  expect_s3_class(merged_all, "data.frame")
  expect_equal(nrow(merged_all), nrow(uvpar))

  merged_mean <- merge.par(uvpar, Dpar, return.mean = TRUE)
  expect_s3_class(merged_mean, "data.frame")
  expect_true(all(c("Month", "u", "v", "D", "sd.u", "sd.v", "sd.D") %in% names(merged_mean)))
})

