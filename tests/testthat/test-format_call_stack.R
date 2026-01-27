test_that("format_call_stack omits epoch parameters to avoid memory issues", {
  # Create a mock call stack with epoch-related parameters
  large_events <- list(
    data.frame(time = 1:1000, msg = paste0("event_", 1:1000)),
    data.frame(time = 1:1000, msg = paste0("event_end_", 1:1000))
  )
  
  mock_callstack <- list(
    epoch = list(
      call_stack = quote(epoch(eyeris, events = events, limits = c(-0.5, 1.5))),
      parameters = list(
        events = large_events,
        limits = c(-0.5, 1.5),
        label = NULL,
        baseline = FALSE,
        baseline_type = "sub",
        baseline_events = large_events,
        baseline_period = NULL,
        hz = 1000,
        verbose = TRUE
      )
    )
  )
  
  # Format the call stack
  result <- format_call_stack(mock_callstack)
  
  # Check that result is a data frame
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_true("parameters" %in% names(result))
  
  # Check that epoch-related parameters are omitted
  param_str <- result$parameters[1]
  expect_true(grepl("events = <omitted>", param_str, fixed = TRUE))
  expect_true(grepl("baseline_events = <omitted>", param_str, fixed = TRUE))
  
  # Check that other parameters are still present
  expect_true(grepl("limits =", param_str))
  expect_true(grepl("baseline = FALSE", param_str, fixed = TRUE))
  expect_true(grepl("hz = 1000", param_str, fixed = TRUE))
  
  # Ensure the parameter string is not excessively long
  # (Without the fix, it would be thousands of characters)
  expect_lt(nchar(param_str), 500)
})

test_that("format_call_stack handles regular parameters normally", {
  # Create a mock call stack without epoch parameters
  mock_callstack <- list(
    lpfilt = list(
      call_stack = quote(lpfilt(eyeris, cutoff = 4, order = 3)),
      parameters = list(
        cutoff = 4,
        order = 3,
        verbose = TRUE
      )
    )
  )
  
  # Format the call stack
  result <- format_call_stack(mock_callstack)
  
  # Check that result is correct
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  
  # Check that regular parameters are deparsed normally
  param_str <- result$parameters[1]
  expect_true(grepl("cutoff = 4", param_str))
  expect_true(grepl("order = 3", param_str))
  expect_true(grepl("verbose = TRUE", param_str, fixed = TRUE))
  
  # Should not have <omitted> for non-epoch parameters
  expect_false(grepl("<omitted>", param_str, fixed = TRUE))
})

test_that("format_call_stack handles 'call' structure in addition to 'call_stack'", {
  # Test the first branch (call instead of call_stack)
  mock_callstack <- list(
    epoch = list(
      call = quote(epoch(eyeris, events = events)),
      parameters = list(
        events = list(data.frame(time = 1:100, msg = rep("test", 100))),
        limits = NULL
      )
    )
  )
  
  result <- format_call_stack(mock_callstack)
  
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_true(grepl("events = <omitted>", result$parameters[1], fixed = TRUE))
})
