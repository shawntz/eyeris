test_that("log messages print interpolated values that contain braces", {
  events <- "PROBE_START_{trial}"
  expect_message(
    log_info("Found baseline events: {events}", verbose = TRUE),
    "PROBE_START_\\{trial\\}"
  )
  expect_message(
    log_success("Literal {{braces}} and {events}", verbose = TRUE),
    "Literal \\{braces\\} and PROBE_START_\\{trial\\}"
  )
  expect_error(log_error("Could not match {events}"), "PROBE_START_\\{trial\\}")
})
