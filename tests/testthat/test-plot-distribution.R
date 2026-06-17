# regression tests for the multi-run diagnostic plotting fixes:
#  * get_block_numbers() must return one number per block for a multi-block
#    object without tripping the `if (is.na(<vector>))` length-> 1 error on
#    R >= 4.2 (this errored before the fix and broke ALL multi-block plotting
#    and bidsify reports)
#  * plot_pupil_distribution() must render visible bars even when the data
#    spread yields many (thin) Freedman-Diaconis bins -- the raw pupil step was
#    rendering blank because the white bar outline covered the thin fills

make_multiblock_obj <- function(n_blocks = 3) {
  ts <- stats::setNames(
    lapply(seq_len(n_blocks), function(i) {
      data.frame(time_secs = seq_len(10), pupil_raw = as.numeric(seq_len(10)))
    }),
    paste0("block_", seq_len(n_blocks))
  )
  obj <- list(timeseries = ts)
  class(obj) <- "eyeris"
  obj
}

test_that("get_block_numbers returns one number per block for multi-block data", {
  obj <- make_multiblock_obj(3)
  # must not error on R >= 4.2 (previously: "the condition has length > 1")
  expect_silent(res <- get_block_numbers(obj))
  expect_equal(sort(as.numeric(res)), c(1, 2, 3))
})

test_that("get_block_numbers handles single-block objects", {
  obj <- make_multiblock_obj(1)
  expect_equal(as.numeric(get_block_numbers(obj)), 1)
})

test_that("get_block_numbers falls back for un-numbered block names", {
  # names that produce NA on as.numeric() should fall back rather than error
  expect_equal(as.numeric(get_block_numbers("block_foo")), 1)
})

test_that("plot_pupil_distribution renders wide-spread data without erroring", {
  # wide spread + outliers => many Freedman-Diaconis bins (the case that
  # rendered blank before the border fix)
  set.seed(1)
  d <- c(rnorm(20000, 8000, 400), 1600, 1700, 11000, 11200)
  tmp <- tempfile(fileext = ".png")
  grDevices::png(tmp)
  on.exit(unlink(tmp), add = TRUE)
  expect_silent(plot_pupil_distribution(
    d,
    color = "black",
    main = "raw",
    xlab = "pupil"
  ))
  grDevices::dev.off()
  expect_true(file.exists(tmp) && file.info(tmp)$size > 0)
})

test_that("plot_pupil_distribution tolerates NA/degenerate inputs", {
  tmp <- tempfile(fileext = ".png")
  grDevices::png(tmp)
  on.exit(unlink(tmp), add = TRUE)
  # all-NA, single value, and constant signals must not error
  expect_silent(plot_pupil_distribution(rep(NA_real_, 5), "black", "m", "x"))
  expect_silent(plot_pupil_distribution(c(NA, 5), "black", "m", "x"))
  expect_silent(plot_pupil_distribution(rep(5, 100), "black", "m", "x"))
  grDevices::dev.off()
})
