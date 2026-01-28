test_that("format_call_stack omits epoch parameters to avoid memory issues", {
  # Create a mock call stack with epoch-related parameters that are lists
  large_events <- list(
    data.frame(time = 1:1000, msg = paste0("event_", 1:1000)),
    data.frame(time = 1:1000, msg = paste0("event_end_", 1:1000))
  )
  
  mock_callstack <- list(
    epoch = list(
      call_stack = quote(epoch(eyeris, events = events, limits = c(-0.5, 1.5))),
      parameters = list(
        events = large_events, # list - should be omitted
        limits = c(-0.5, 1.5), # vector - should NOT be omitted
        label = NULL,
        baseline = FALSE,
        baseline_type = "sub",
        baseline_events = large_events, # list - should be omitted
        baseline_period = NULL,
        hz = 1000,
        verbose = TRUE,
        epoch_length = 100 # scalar - should NOT be omitted
      )
    )
  )
  
  # Format the call stack
  result <- format_call_stack(mock_callstack)
  
  # Check that result is a data frame
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_true("parameters" %in% names(result))
  
  # Check that epoch-related list/data.frame parameters are omitted
  param_str <- result$parameters[1]
  expect_true(grepl("events = <omitted>", param_str, fixed = TRUE))
  expect_true(grepl("baseline_events = <omitted>", param_str, fixed = TRUE))
  
  # Check that other parameters are still present
  expect_true(grepl("limits =", param_str))
  expect_true(grepl("baseline = FALSE", param_str, fixed = TRUE))
  expect_true(grepl("hz = 1000", param_str, fixed = TRUE))
  expect_true(grepl("epoch_length = 100", param_str, fixed = TRUE))
  
  # Ensure the parameter string is not excessively long
  # (Without the fix, it would be thousands of characters)
  expect_lt(nchar(param_str), 500)
})

test_that("format_call_stack handles regular parameters normally", {
  # Create a mock call stack without epoch parameters
  mock_callstack <- list(
    lpfilt = list(
      call_stack = quote(lpfilt(eyeris, cutoff = 4, order = 3)),
      parameters = list(cutoff = 4, order = 3, verbose = TRUE)
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
        limits = NULL,
        epoch_count = 5 # scalar with "epoch" in name - should NOT be omitted
      )
    )
  )
  
  result <- format_call_stack(mock_callstack)
  
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_true(grepl("events = <omitted>", result$parameters[1], fixed = TRUE))
  expect_true(grepl("epoch_count = 5", result$parameters[1], fixed = TRUE))
})

test_that("format_call_stack only omits complex epoch objects, not scalars", {
  # Create a mock call stack with scalar epoch-related parameters
  mock_callstack <- list(
    test = list(
      call_stack = quote(test(epoch_length = 100, epoch_data = data)),
      parameters = list(
        epoch_length = 100, # scalar - should NOT be omitted
        epoch_count = 5, # scalar - should NOT be omitted
        epoch_data = list(data.frame(x = 1:100)) # list - should be omitted
      )
    )
  )
  result <- format_call_stack(mock_callstack)
  param_str <- result$parameters[1]
  
  # Scalars with "epoch" in name should NOT be omitted
  expect_true(grepl("epoch_length = 100", param_str, fixed = TRUE))
  expect_true(grepl("epoch_count = 5", param_str, fixed = TRUE))
  # Complex objects with "epoch" in name should be omitted
  expect_true(grepl("epoch_data = <omitted>", param_str, fixed = TRUE))
})

test_that("format_call_stack uses case-insensitive matching for events/baseline_events", {
  # Test case variations of "events" and "baseline_events"
  mock_callstack <- list(
    test = list(
      call_stack = quote(test(Events = e1, EVENTS = e2, baseline_Events = e3)),
      parameters = list(
        Events = list(data.frame(x = 1:100)),
        EVENTS = list(data.frame(x = 1:100)),
        baseline_Events = list(data.frame(x = 1:100))
      )
    )
  )
  result <- format_call_stack(mock_callstack)
  param_str <- result$parameters[1]
  # All case variations should be omitted
  expect_true(grepl("Events = <omitted>", param_str, fixed = TRUE))
  expect_true(grepl("EVENTS = <omitted>", param_str, fixed = TRUE))
  expect_true(grepl("baseline_Events = <omitted>", param_str, fixed = TRUE))
})
