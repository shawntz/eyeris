# regression test: glassbox() must preserve the per-block detrend coefficients
# computed during the detrend step. They are stored on the internal per-block
# `temp_file` inside pipeline_handler(), and the block-recombine step must copy
# them back onto the returned object (previously they were silently dropped).

test_that("glassbox() preserves detrend_coefs per block", {
  demo <- eyeris::eyelink_asc_demo_dataset()

  out <- eyeris::glassbox(
    demo,
    lpfilt = FALSE,
    detrend = TRUE,
    zscore = FALSE,
    verbose = FALSE
  )

  # the detrend coefficients survive the per-block recombine
  expect_false(is.null(out$detrend_coefs))
  expect_true("block_1" %in% names(out$detrend_coefs))

  # linear detrending fits an intercept + a slope on time (2 coefficients)
  coefs <- out$detrend_coefs$block_1
  expect_length(coefs, 2)
  expect_true(all(is.finite(coefs)))
})

test_that("glassbox() does not set detrend_coefs when detrend is disabled", {
  demo <- eyeris::eyelink_asc_demo_dataset()

  out <- eyeris::glassbox(
    demo,
    lpfilt = FALSE,
    detrend = FALSE,
    zscore = FALSE,
    verbose = FALSE
  )

  expect_null(out$detrend_coefs)
})

test_that("glassbox() clears detrend_coefs inherited from the input object", {
  demo <- eyeris::eyelink_asc_demo_dataset()
  obj <- eyeris::load_asc(demo)

  # simulate a pre-loaded object that already carries detrend coefficients
  # (e.g., the output of a prior detrend()/glassbox() run fed back in)
  obj$detrend_coefs <- list(block_1 = c("(Intercept)" = 1, timeseries = 2))

  out <- eyeris::glassbox(
    obj,
    lpfilt = FALSE,
    detrend = FALSE,
    zscore = FALSE,
    verbose = FALSE
  )

  # the stale, inherited coefficients must not survive a run where detrend is
  # disabled -- only coefficients produced in the current run are retained
  expect_null(out$detrend_coefs)
})
