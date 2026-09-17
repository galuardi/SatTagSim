test_that("weighted.var computes correctly and handles edge cases", {
  x <- c(1, 2, 3, 4, 5)
  w <- c(1, 1, 1, 1, 1)
  expect_equal(weighted.var(x, w), var(x))

  # Weights with NAs and na.rm = TRUE
  x_na <- c(1, 2, 3, NA, 5)
  w_na <- c(1, 2, 1, 1, 1)
  expect_true(is.finite(weighted.var(x_na, w_na, na.rm = TRUE)))

  # Single observation or degenerate weights returns NA_real_
  expect_true(is.na(weighted.var(c(5), c(1))))
})

test_that("make.seas.idx correctly partitions months into four seasons", {
  df <- data.frame(Month = 1:12)
  sidx <- make.seas.idx(df)

  expect_named(sidx, c("winter", "spring", "summer", "fall"))
  expect_equal(which(sidx$winter), 1:3)
  expect_equal(which(sidx$spring), 4:6)
  expect_equal(which(sidx$summer), 7:9)
  expect_equal(which(sidx$fall), 10:12)
})

test_that("col2cpt generates valid color palette output", {
  cols <- c("#FF0000", "#00FF00", "#0000FF")
  tmp <- tempfile(fileext = ".cpt")
  col2cpt(nmin = 0, nmax = 10, clist = cols, outfile = tmp, length = 10)
  expect_true(file.exists(tmp))
  unlink(tmp)
})
