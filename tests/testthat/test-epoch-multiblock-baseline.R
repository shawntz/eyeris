# regression tests for https://github.com/shawntz/eyeris/issues/426:
# epoch(baseline = TRUE) failed on multi-block recordings with
# "recursive indexing failed at level 2" because the per-block `latest`
# pointer list was passed whole into compute_baseline()

make_multiblock_sim <- function(n_blocks = 3, latest = "pupil_raw") {
  sims <- lapply(seq_len(n_blocks), function(b) {
    simulate_eyeris(
      seed = b,
      params = sim_params(duration_secs = 30),
      verbose = FALSE
    )
  })

  x <- sims[[1]]
  for (b in seq_len(n_blocks)[-1]) {
    bn <- paste0("block_", b)
    x$timeseries[[bn]] <- sims[[b]]$timeseries[[1]]
    x$timeseries[[bn]]$block <- b
    x$events[[bn]] <- sims[[b]]$events[[1]]
    x$events[[bn]]$block <- b
    x$blinks[[bn]] <- sims[[b]]$blinks[[1]]
    x$latest[[bn]] <- latest
  }

  x
}

test_that("resolve_block_latest() returns a single pointer per block", {
  expect_identical(resolve_block_latest("pupil_raw", "block_1"), "pupil_raw")
  expect_identical(
    resolve_block_latest(list(block_1 = "pupil_raw"), "block_1"),
    "pupil_raw"
  )
  expect_identical(
    resolve_block_latest(
      list(block_1 = "pupil_a", block_2 = "pupil_b", block_3 = "pupil_c"),
      "block_2"
    ),
    "pupil_b"
  )
  expect_error(
    resolve_block_latest(list(block_1 = "a", block_2 = "b"), "block_9"),
    "block_9"
  )
})

test_that("epoch(baseline = TRUE) works on multi-block recordings", {
  x <- make_multiblock_sim(3) |> deblink() |> zscore()

  expect_length(x$latest, 3)

  out <- x |>
    epoch(
      events = "STIM_ONSET_*",
      limits = c(0, 1),
      label = "stim",
      baseline = TRUE,
      baseline_type = "sub",
      baseline_events = "STIM_ONSET_*",
      baseline_period = c(-0.5, 0),
      verbose = FALSE
    )

  expect_named(out$epoch_stim, c("block_1", "block_2", "block_3"))

  bline_col <- "pupil_raw_deblink_sub_bline_STIM_ONSET_"
  for (bn in names(out$epoch_stim)) {
    expect_true(bline_col %in% names(out$epoch_stim[[bn]]))
    expect_true(paste0(bline_col, "_z") %in% names(out$epoch_stim[[bn]]))
  }

  bline_id <- paste0("baseline_", bline_col, "_epoch_stim")
  expect_named(out[[bline_id]], c("block_1", "block_2", "block_3"))
})
