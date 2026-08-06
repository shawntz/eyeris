#' The opinionated "glass box" `eyeris` pipeline
#'
#' This `glassbox` function (in contrast to a "black box" function where you run
#' it and get a result but have no (or little) idea as to how you got from input
#' to output) has a few primary benefits over calling each exported function
#' from `eyeris` separately.
#'
#' First, this `glassbox` function provides a highly opinionated prescription of
#' steps and starting parameters we believe any pupillometry researcher should
#' use as their defaults when preprocessing pupillometry data.
#'
#' Second, and not mutually exclusive from the first point, using this function
#' should ideally reduce the probability of accidental mishaps when
#' "reimplementing" the steps from the preprocessing pipeline both within and
#' across projects. We hope to streamline the process in such a way that you
#' could collect a pupillometry dataset and within a few minutes assess the
#' quality of those data while simultaneously running a full preprocessing
#' pipeline in 1-ish line of code!
#'
#' Third, `glassbox` provides an "interactive" framework where you can evaluate
#' the consequences of the parameters within each step on your data in real
#' time, facilitating a fairly easy-to-use workflow for parameter optimization
#' on your particular dataset. This process essentially takes each of the
#' opinionated steps and provides a pre-/post-plot of the time series data for
#' each step so you can adjust parameters and re-run the pipeline until you are
#' satisfied with the choices of your parameters and their consequences on your
#' pupil time series data.
#'
#' @section What `glassbox()` does, step by step:
#' In plain language, `glassbox()` runs the following `eyeris` steps in order on
#' your pupil time series. Steps marked **(default: on)** run automatically;
#' steps marked **(default: off)** are skipped unless you explicitly enable
#' them. Most preprocessing steps can be turned off by passing `<step> = FALSE`
#' (except `load_asc`, which always runs when `file` is an `.asc` path). Steps
#' that accept parameters can be
#' customized by passing `<step> = list(...)` with values you want to override
#' (for example, `deblink = list(extend = 40)`).
#'
#' 1. **Load the data** (`load_asc`, default: on) -- Reads and parses the
#' EyeLink `.asc` file into an `eyeris` object, automatically splitting the
#' recording into blocks and, for binocular recordings, handling each eye
#' separately. See [eyeris::load_asc()]. This step is skipped when a pre-loaded
#' `eyeris` object is passed as `file` (see the `file` parameter).
#' 2. **Resample onto a uniform grid** (`resample`, default: on) -- Places each
#' block on the expected uniform sampling grid. For hardware that *drops*
#' samples (instead of zero-filling) when pupil data is missing, this
#' interpolates local sub-period timing jitter and inserts `NA` rows at the
#' dropped timestamps, so the rate-dependent steps that follow stay valid. A
#' guaranteed no-op for already-uniform data (e.g., EyeLink). See
#' [eyeris::resample()].
#' 3. **Remove blinks** (`deblink`, default: on) -- Replaces the missing data
#' around blinks with `NA`s, extending each gap by `50` ms on either side so
#' that the rapid dips and spikes that surround a blink are removed too. See
#' [eyeris::deblink()].
#' 4. **Remove transient artifacts** (`detransient`, default: on) -- Rejects
#' pupil samples that change faster than is physiologically plausible, using a
#' speed-based median absolute deviation (MAD) threshold. See
#' [eyeris::detransient()].
#' 5. **Interpolate missing samples** (`interpolate`, default: on) -- Fills the
#' `NA` gaps left by the resample, deblink, and detransient steps using linear
#' interpolation, producing a continuous, gap-free time series. See
#' [eyeris::interpolate()].
#' 6. **Smooth the signal** (`lpfilt`, default: on) -- Applies a low-pass
#' filter (default `4` Hz passband) to remove high-frequency noise while
#' preserving the slower pupil dynamics of interest. See [eyeris::lpfilt()].
#' 7. **Downsample** (`downsample`, default: off) -- Optionally lowers the
#' sampling rate using an anti-aliasing filter, which preserves the temporal
#' dynamics of the signal. Cannot be combined with `bin`. See
#' [eyeris::downsample()].
#' 8. **Bin** (`bin`, default: off) -- Optionally lowers the sampling rate by
#' averaging samples within equal-width time bins. Cannot be combined with
#' `downsample`. See [eyeris::bin()].
#' 9. **Detrend** (`detrend`, default: off) -- Optionally fits a model of
#' `pupil ~ time` and returns the residuals (along with the fitted trend) to
#' remove slow drift. By default (`detrend = TRUE`) a straight-line
#' (`method = "linear"`) trend is removed; pass
#' `detrend = list(method = "spline", spline_df = 5)` to instead remove a
#' smooth, potentially nonlinear trend via a natural cubic spline of time. Use
#' with care -- see [eyeris::detrend()] for when this is appropriate.
#' 10. **Z-score** (`zscore`, default: on) -- Rescales the pupil time series to a
#' mean of `0` and a standard deviation of `1`, making values comparable across
#' participants and recordings. See [eyeris::zscore()].
#'
#' After preprocessing, `glassbox()` calls [eyeris::summarize_confounds()] to compute
#' per-step confound metrics (e.g., missingness and gaze statistics) and store them
#' in `$confounds`.
#'
#' Crucially, each step *adds a new column* to the time series rather than
#' overwriting the previous one, so every intermediate stage is preserved inside
#' the returned `eyeris` object. This is what makes the pipeline a "glass box":
#' you can inspect, plot, and compare the data before and after each
#' transformation (for example, `plot(output, steps = c(1, 5))`).
#'
#' @param file Either an SR Research EyeLink `.asc` file generated by the
#' official EyeLink `edf2asc` command, or a pre-constructed `eyeris` object. When
#' an `.asc` path is supplied, the `load_asc` step reads and parses it. When a
#' pre-loaded `eyeris` object is supplied instead (for example, the output of
#' [eyeris::load_generic()] for non-EyeLink trackers, or an existing
#' [eyeris::load_asc()] object), the load step is skipped and the remaining
#' pipeline runs on the object as-is -- enabling `load_generic(...) |> glassbox()`
#' @param interactive_preview A flag to indicate whether to run the `glassbox`
#' pipeline autonomously all the way through (set to `FALSE` by default), or to
#' interactively provide a visualization after each pipeline step, where you
#' must also indicate "(y)es" or "(n)o" to either proceed or cancel the
#' current `glassbox` pipeline operation (set to `TRUE`)
#' @param preview_n Number of random example "epochs" to generate for
#' previewing the effect of each preprocessing step on the pupil time series
#' @param preview_duration Time in seconds of each randomly selected preview
#' @param preview_window The start and stop raw timestamps used to subset the
#' preprocessed data from each step of the `eyeris` workflow for visualization.
#' Defaults to NULL, meaning random epochs as defined by `preview_n` and
#' `preview_duration` will be plotted. To override the random epochs, set
#' `preview_window` here to a vector with relative start and stop times (in
#' seconds), for example -- `c(5,6)` -- to indicate the raw data from 5-6 secs
#' on data that were recorded at 1000 Hz). Note, the start/stop time values
#' indicated here are in seconds because `eyeris` automatically computes the
#' indices for the supplied range of seconds using the `$info$sample.rate`
#' metadata in the `eyeris` S3 class object
#' @param verbose A logical flag to indicate whether to print status messages to
#' the console. Defaults to `TRUE`. Set to `FALSE` to suppress messages about
#' the current processing step and run silently
#' @param ... Additional arguments to override the default, prescribed settings
#' @param confirm **(Deprecated)** Use `interactive_preview` instead
#' @param num_previews **(Deprecated)** Use `preview_n` instead
#' @param detrend_data **(Deprecated)** A flag to indicate whether to run the
#' `detrend` step (set to `FALSE` by default). Detrending your pupil time series
#' can have unintended consequences; we thus recommend that users understand the
#' implications of detrending -- in addition to whether detrending is
#' appropriate for the research design and question(s) -- before using this
#' function
#' @param skip_detransient **(Deprecated)** A flag to indicate whether to skip
#' the `detransient` step (set to `FALSE` by default). In most cases, this
#' should remain `FALSE`. For a more detailed description about likely edge
#' cases that would prompt you to set this to `TRUE`, see the docs for
#' [eyeris::detransient()]
#'
#' @return Preprocessed pupil data contained within an object of class `eyeris`
#'
#' @seealso [lifecycle::deprecate_warn()]
#'
#' @examples
#' demo_data <- eyelink_asc_demo_dataset()
#'
#' # (1) examples using the default prescribed parameters and pipeline recipe
#'
#' ## (a) run an automated pipeline with no real-time inspection of parameters
#' output <- eyeris::glassbox(demo_data)
#'
#' start_time <- min(output$timeseries$block_1$time_secs)
#' end_time <- max(output$timeseries$block_1$time_secs)
#'
#' # by default, verbose = TRUE. To suppress messages, set verbose = FALSE.
#' plot(
#'   output,
#'   steps = c(1, 5),
#'   preview_window = c(start_time, end_time),
#'   seed = 0
#' )
#'
#' ## (b) run a interactive workflow (with confirmation prompts after each step)
#' \donttest{
#' output <- eyeris::glassbox(demo_data, interactive_preview = TRUE, seed = 0)
#' }
#'
#' # (2) examples of overriding the default parameters
#' output <- eyeris::glassbox(
#'   demo_data,
#'   interactive_preview = FALSE, # TRUE to visualize each step in real-time
#'   deblink = list(extend = 40),
#'   # only interpolate gaps up to 100 ms; longer gaps are left as NA
#'   interpolate = list(max_gap_ms = 100),
#'   lpfilt = list(plot_freqz = TRUE) # overrides verbose parameter
#' )
#'
#' # to suppress messages, set verbose = FALSE in plot():
#' plot(output, seed = 0, verbose = FALSE, preview_window = c(10, 12))
#'
#' # (3) examples of disabling certain steps
#' output <- eyeris::glassbox(
#'   demo_data,
#'   detransient = FALSE,
#'   detrend = FALSE,
#'   zscore = FALSE
#' )
#'
#' plot(output, seed = 0, preview_window = c(10, 12))
#'
#' @export
glassbox <- function(
  file,
  interactive_preview = FALSE,
  preview_n = 3,
  preview_duration = 5,
  preview_window = NULL,
  verbose = TRUE,
  ...,
  confirm = deprecated(),
  num_previews = deprecated(),
  detrend_data = deprecated(),
  skip_detransient = deprecated()
) {
  original_call <- match.call()

  # reset per-run gap-related notices (the interpolation behavior-change notice
  # and the filter-over-gaps warning) so each fires at most once per glassbox()
  # run (not once per R session)
  reset_gap_notices()

  # handle deprecated parameters
  if (is_present(confirm)) {
    deprecate_warn(
      "1.1.0",
      "glassbox(confirm)",
      "glassbox(interactive_preview)"
    )
    interactive_preview <- confirm
  }

  if (is_present(num_previews)) {
    deprecate_warn("1.1.0", "glassbox(num_previews)", "glassbox(preview_n)")
    preview_n <- num_previews
  }

  if (is_present(detrend_data)) {
    deprecate_warn(
      "1.1.0",
      "glassbox(detrend_data)",
      details = paste(
        "The `detrend_data` argument is no longer used",
        "and will be ignored."
      )
    )

    detrend_data <- NULL
  }

  if (is_present(skip_detransient)) {
    deprecate_warn(
      "1.1.0",
      "glassbox(skip_detransient)",
      details = paste(
        "The `skip_detransient` argument is no longer used",
        "and will be ignored."
      )
    )

    skip_detransient <- NULL
  }

  # a pre-constructed eyeris object (e.g., from load_generic() for non-EyeLink
  # trackers, or an already-loaded load_asc() object) may be passed directly:
  # we detect it here and skip the file-loading step below, running the
  # remaining pipeline on the object as-is.
  preloaded_eyeris <- inherits(file, "eyeris")

  # the default glassbox pipeline parameters
  default_params <- list(
    load_asc = list(block = "auto", binocular_mode = "average"),
    resample = TRUE,
    deblink = list(extend = 50),
    detransient = list(n = 16, mad_thresh = NULL),
    interpolate = TRUE,
    lpfilt = list(wp = 4, ws = 8, rp = 1, rs = 35, plot_freqz = verbose),
    downsample = FALSE,
    bin = FALSE,
    detrend = FALSE,
    zscore = TRUE,
    seed = 123
  )

  # override defaults
  params <- utils::modifyList(default_params, list(...))

  # if a pre-loaded eyeris object was supplied, skip the file-loading step and
  # run the remaining pipeline directly on the object
  if (preloaded_eyeris) {
    params$load_asc <- FALSE
    log_info(
      paste(
        "Received a pre-loaded `eyeris` object; skipping the load step and",
        "running the remaining pipeline on it directly."
      ),
      verbose = verbose
    )
  }

  # handle method parameter for bin operation
  if (
    "method" %in%
      names(list(...)) &&
      !is.null(params$bin) &&
      is.list(params$bin)
  ) {
    params$bin$method <- list(...)$method
  }

  # guard params that accept lists in the event a boolean is supplied
  # (skipped for a pre-loaded eyeris object, where load_asc stays disabled so a
  # caller-supplied `load_asc = TRUE` cannot restore file loading on an object)
  if (
    !preloaded_eyeris &&
      "load_asc" %in% names(list(...)) &&
      isTRUE(list(...)$load_asc)
  ) {
    log_warn(
      "`load_asc` expects a list of args (not a boolean)... using default: `list(block = 'auto')`",
      verbose = TRUE
    )
    params$load_asc <- default_params$load_asc
  }

  if ("deblink" %in% names(list(...)) && isTRUE(list(...)$deblink)) {
    log_warn(
      "`deblink` expects a list of args (not a boolean)... using default: `list(extend = 50)`",
      verbose = TRUE
    )
    params$deblink <- default_params$deblink
  }

  if ("detransient" %in% names(list(...)) && isTRUE(list(...)$detransient)) {
    log_warn(
      "`detransient` expects a list of args (not a boolean)... using default: `list(n = 16, mad_thresh = NULL)`",
      verbose = TRUE
    )
    params$detransient <- default_params$detransient
  }

  if ("lpfilt" %in% names(list(...)) && isTRUE(list(...)$lpfilt)) {
    log_warn(
      "`lpfilt` expects a list of args (not a boolean)... using default: `list(wp = 4, ws = 8, rp = 1, rs = 35, plot_freqz = verbose)`",
      verbose = TRUE
    )
    params$lpfilt <- default_params$lpfilt
  }

  if ("downsample" %in% names(list(...)) && isTRUE(list(...)$downsample)) {
    log_warn(
      "`downsample` expects a list of args (not a boolean)... using default: `list(target_fs = 100, plot_freqz = verbose)`",
      verbose = TRUE
    )
    params$downsample <- default_params$downsample
  }

  if ("bin" %in% names(list(...)) && isTRUE(list(...)$bin)) {
    log_warn(
      "`bin` expects a list of args (not a boolean)... using default: `list(bins_per_second = 10, method = 'mean')`",
      verbose = TRUE
    )
    params$bin <- default_params$bin
  }

  # abort if both downsample and bin are enabled
  step_status <- evaluate_pipeline_step_params(list(
    downsample = params$downsample,
    bin = params$bin
  ))

  if (
    !is.null(params$downsample) &&
      !is.null(params$bin) &&
      step_status[1] &&
      step_status[2]
  ) {
    log_error(
      "Both 'downsample' and 'bin' steps are enabled. You cannot use both downsampling and binning in the same glassbox. Please enable only one (or neither) of these steps."
    )
  }

  # evaluate which steps of pipeline to run
  which_steps <- evaluate_pipeline_step_params(params)

  if (
    which_steps[["detrend"]] &&
      !any(which_steps[c("deblink", "detransient", "interpolate", "lpfilt")])
  ) {
    log_warn(
      "Detrend is enabled but no other preprocessing steps are enabled. This may cause plotting issues since there will be no pupil columns to detrend against. Consider enabling at least one preprocessing step before detrending, or disable detrending if you want to work with raw data.",
      verbose = TRUE
    )
  }

  # eyeris workflow data structure
  pipeline <- list(
    load_asc = function(data, params, original_call) {
      if (which_steps[["load_asc"]]) {
        call_info <- list(
          call = original_call,
          parameters = list(
            block = params$load_asc$block,
            binocular_mode = params$load_asc$binocular_mode
          )
        )
        result <- eyeris::load_asc(
          data,
          block = params$load_asc$block,
          binocular_mode = params$load_asc$binocular_mode,
          verbose = verbose
        )
        if (!is.list(result$params)) {
          result$params <- list()
        }
        result$params[["load_asc"]] <- call_info
        result
      } else {
        log_error("No data loaded... the glassbox pipeline cannot proceed.")
      }
    },
    deblink = function(data, params, original_call) {
      if (which_steps[["deblink"]]) {
        call_info <- list(
          call = original_call,
          parameters = list(extend = params$deblink$extend)
        )
        eyeris::deblink(
          data,
          extend = params$deblink$extend,
          call_info = call_info
        )
      } else {
        data
      }
    },
    detransient = function(data, params, original_call) {
      if (which_steps[["detransient"]]) {
        call_info <- list(
          call = original_call,
          parameters = list(
            n = params$detransient$n,
            mad_thresh = params$detransient$mad_thresh
          )
        )
        eyeris::detransient(
          data,
          n = params$detransient$n,
          mad_thresh = params$detransient$mad_thresh,
          call_info = call_info
        )
      } else {
        data
      }
    },
    interpolate = function(data, params, original_call) {
      if (which_steps[["interpolate"]]) {
        max_gap_ms <- if (
          is.list(params$interpolate) &&
            "max_gap_ms" %in% names(params$interpolate)
        ) {
          params$interpolate$max_gap_ms
        } else {
          250
        }
        # validate up front so malformed input aborts clearly (and an explicit
        # NULL is normalized to Inf) instead of failing deep inside the step
        max_gap_ms <- validate_max_gap_ms(max_gap_ms)
        call_info <- list(
          call = original_call,
          parameters = list(max_gap_ms = max_gap_ms, verbose = verbose)
        )
        eyeris::interpolate(
          data,
          max_gap_ms = max_gap_ms,
          verbose = verbose,
          call_info = call_info
        )
      } else {
        data
      }
    },
    lpfilt = function(data, params, original_call) {
      if (which_steps[["lpfilt"]]) {
        call_info <- list(
          call = original_call,
          parameters = list(
            wp = params$lpfilt$wp,
            ws = params$lpfilt$ws,
            rp = params$lpfilt$rp,
            rs = params$lpfilt$rs,
            plot_freqz = params$lpfilt$plot_freqz
          )
        )
        eyeris::lpfilt(
          data,
          wp = params$lpfilt$wp,
          ws = params$lpfilt$ws,
          rp = params$lpfilt$rp,
          rs = params$lpfilt$rs,
          plot_freqz = params$lpfilt$plot_freqz,
          call_info = call_info
        )
      } else {
        data
      }
    },
    downsample = function(data, params, original_call) {
      if (which_steps[["downsample"]]) {
        if (is.null(params$downsample$plot_freqz)) {
          params$downsample$plot_freqz <- verbose
        }
        if (is.null(params$downsample$rp)) {
          params$downsample$rp <- 1
        }
        if (is.null(params$downsample$rs)) {
          params$downsample$rs <- 35
        }
        call_info <- list(
          call = original_call,
          parameters = list(
            target_fs = params$downsample$target_fs,
            plot_freqz = params$downsample$plot_freqz,
            rp = params$downsample$rp,
            rs = params$downsample$rs
          )
        )
        eyeris::downsample(
          data,
          target_fs = params$downsample$target_fs,
          plot_freqz = params$downsample$plot_freqz,
          rp = params$downsample$rp,
          rs = params$downsample$rs,
          call_info = call_info
        )
      } else {
        data
      }
    },
    bin = function(data, params, original_call) {
      if (which_steps[["bin"]]) {
        call_info <- list(
          call = original_call,
          parameters = list(
            bins_per_second = params$bin$bins_per_second,
            method = params$bin$method
          )
        )
        eyeris::bin(
          data,
          bins_per_second = params$bin$bins_per_second,
          method = params$bin$method,
          call_info = call_info
        )
      } else {
        data
      }
    },
    detrend = function(data, params, original_call) {
      if (which_steps[["detrend"]]) {
        detrend_opts <- if (is.list(params$detrend)) {
          params$detrend
        } else {
          list()
        }
        method <- if (!is.null(detrend_opts$method)) {
          detrend_opts$method
        } else {
          "linear"
        }
        spline_df <- if (!is.null(detrend_opts$spline_df)) {
          detrend_opts$spline_df
        } else {
          5
        }
        call_info <- list(
          call = original_call,
          parameters = list(method = method, spline_df = spline_df)
        )
        eyeris::detrend(
          data,
          method = method,
          spline_df = spline_df,
          call_info = call_info
        )
      } else {
        data
      }
    },
    zscore = function(data, params, original_call) {
      if (which_steps[["zscore"]]) {
        call_info <- list(call = original_call, parameters = list())
        eyeris::zscore(data, call_info = call_info)
      } else {
        data
      }
    }
  )

  seed <- params$seed
  step_counter <- 1
  only_linear_trend <- FALSE
  next_step <- c()

  if (which_steps[["load_asc"]]) {
    log_success("Running eyeris::load_asc()", verbose = verbose)
    file <- pipeline[["load_asc"]](file, params, original_call)

    if (interactive_preview) {
      plot_with_seed(
        file = file,
        step_counter = 1,
        seed = seed,
        preview_n = preview_n,
        preview_duration = preview_duration,
        preview_window = preview_window,
        only_linear_trend = only_linear_trend,
        next_step = NULL,
        verbose = verbose
      )

      if (!prompt_user()) {
        log_info(
          "Process cancelled after loading data. Adjust your parameters and re-run!",
          verbose = verbose
        )
        return(file)
      }
    }
  }

  # resample onto the expected uniform sampling grid before any rate-dependent
  # step, so that dropped samples become NA gaps that later steps (e.g.,
  # interpolate) can handle consistently. Runs once on the full object (whether
  # freshly loaded above or passed in pre-loaded), ahead of any binocular split
  # (resample() recurses into both eyes). For already-uniform data (e.g.,
  # EyeLink) this is a no-op.
  if (which_steps[["resample"]]) {
    log_success("Running eyeris::resample()", verbose = verbose)
    call_info <- list(
      call = original_call,
      parameters = list(verbose = verbose)
    )
    file <- eyeris::resample(file, verbose = verbose, call_info = call_info)
  }

  # handle binocular objects (whether freshly loaded above or passed in
  # pre-loaded) by processing the left and right eyes separately
  if (is_binocular_object(file)) {
    log_info(
      "Detected binocular data - processing left and right eyes separately",
      verbose = verbose
    )

    # process left eye
    left_result <- glassbox_internal(
      file$left,
      interactive_preview,
      preview_n,
      preview_duration,
      preview_window,
      verbose,
      params,
      original_call,
      seed
    )

    # process right eye
    right_result <- glassbox_internal(
      file$right,
      interactive_preview,
      preview_n,
      preview_duration,
      preview_window,
      verbose,
      params,
      original_call,
      seed
    )

    # return combined structure
    list_out <- list(
      left = left_result,
      right = right_result,
      original_file = file$original_file,
      raw_binocular_object = file$raw_binocular_object
    )

    class(list_out) <- "eyeris"

    return(list_out)
  }

  has_multiple_blocks <- is.list(file$timeseries) && length(file$timeseries) > 0

  # process each block individually through all steps (except load_asc)
  if (has_multiple_blocks) {
    block_names <- names(file$timeseries)
    processed_blocks <- list()

    # clear any detrend coefficients inherited from a pre-loaded input object
    # (e.g., an object already processed by detrend()/glassbox()) before the
    # per-block recombine below, so the returned object retains only the
    # coefficients produced in this run -- and none survive when detrending is
    # disabled or fails
    file$detrend_coefs <- NULL

    # store orig latest pointer to restore it later
    original_latest <- file$latest
    final_latest <- NULL
    block_states <- list()
    # collect params from all blocks
    all_params <- list()

    for (block_name in block_names) {
      log_info("Processing block: {block_name}", verbose = verbose)

      temp_file <- file
      temp_file$timeseries <- list(file$timeseries[[block_name]])
      names(temp_file$timeseries) <- block_name

      # set latest pointer for current block
      if (is.list(original_latest)) {
        # multiblock: use pointer for current block
        temp_file$latest <- list()
        temp_file$latest[[block_name]] <- original_latest[[block_name]]
      } else {
        # single block converted to multiblock: use original pointer
        temp_file$latest <- list()
        temp_file$latest[[block_name]] <- original_latest
      }

      # init block state
      block_states[[block_name]] <- list(
        latest_pointer = temp_file$latest[[block_name]],
        steps_completed = 0,
        has_errors = FALSE
      )

      block_step_counter <- 2

      for (step_name in names(pipeline)[-1]) {
        action <- "Running "
        skip_plot <- FALSE

        if (!which_steps[[step_name]]) {
          action <- "Skipping "
          block_step_counter <- block_step_counter - 1
          skip_plot <- TRUE

          if (!is.null(temp_file$latest[[block_name]])) {
            expected_col <- paste0(
              temp_file$latest[[block_name]],
              "_",
              step_name
            )
            block_data <- temp_file$timeseries[[block_name]]
            if (expected_col %in% colnames(block_data)) {
              temp_file$latest[[block_name]] <- expected_col
            }
          }
        } else {
          if (step_name == "detrend") {
            only_linear_trend <- TRUE
          }
        }

        if (action == "Running ") {
          log_success(
            "{action}eyeris::{step_name}() for {block_name}",
            verbose = verbose
          )
        } else {
          log_warn(
            "Skipping eyeris::{step_name}() for {block_name}",
            verbose = verbose
          )
        }

        step_to_run <- pipeline[[step_name]]
        err_thrown <- FALSE

        temp_file <- tryCatch(
          {
            step_to_run(temp_file, params, original_call)
          },
          error = function(e) {
            if (!which_steps[["interpolate"]] && which_steps[["detrend"]]) {
              log_warn(
                "Because missing pupil samples were not interpolated, there is a mismatch in the number of samples in the detrended data. Please set `interpolate` to `TRUE` before detrending data OR disable detrending by setting `detrend` to `FALSE`.",
                verbose = TRUE
              )
            }

            log_warn(
              "Skipping eyeris::{step_name}() for {block_name}: {e$message}",
              verbose = verbose
            )
            err_thrown <<- TRUE
            block_step_counter <<- block_step_counter - 1

            # mark current block as having errors
            block_states[[block_name]]$has_errors <- TRUE

            # reset latest pointer to prevent corruption from propagating
            # find last valid column name in current block
            block_data <- temp_file$timeseries[[block_name]]
            pupil_cols <- grep("^pupil_", colnames(block_data), value = TRUE)
            if (length(pupil_cols) > 0) {
              # use last valid pupil column
              temp_file$latest[[block_name]] <- pupil_cols[length(pupil_cols)]
              block_states[[block_name]]$latest_pointer <- temp_file$latest[[
                block_name
              ]]
            } else {
              # fallback to original pointer for this block
              if (is.list(original_latest)) {
                temp_file$latest[[block_name]] <- original_latest[[block_name]]
              } else {
                temp_file$latest[[block_name]] <- original_latest
              }
              block_states[[block_name]]$latest_pointer <- temp_file$latest[[
                block_name
              ]]
            }

            temp_file
          }
        )

        if (
          verbose &&
            action == "Running " &&
            (step_name == "downsample" || step_name == "bin")
        ) {
          log_success(
            "Decimating sampling rate from {temp_file$info$sample.rate} Hz --> {temp_file$decimated.sample.rate} Hz...",
            verbose = verbose
          )
        }

        if (interactive_preview && !err_thrown && !skip_plot) {
          pupil_steps <- grep(
            "^pupil_",
            colnames(temp_file$timeseries[[block_name]]),
            value = TRUE
          )

          if (block_step_counter + 1 <= length(names(pipeline))) {
            next_step <- c(next_step, pupil_steps[block_step_counter])
          } else {
            next_step <- NULL
          }

          plot_with_seed(
            file = temp_file,
            step_counter = block_step_counter,
            seed = seed,
            preview_n = preview_n,
            preview_duration = preview_duration,
            preview_window = preview_window,
            only_linear_trend = only_linear_trend,
            next_step = next_step,
            block_name = block_name,
            verbose = verbose
          )

          if (step_name == "detrend") {
            only_linear_trend <- FALSE
          }

          if (step_name != "zscore") {
            if (!prompt_user()) {
              log_info(
                "Process cancelled after running the {step_name} step for {block_name}. Adjust your parameters and re-run!",
                verbose = verbose
              )
              break
            }
          }
        }

        block_step_counter <- block_step_counter + 1
      }

      processed_blocks[[block_name]] <- temp_file$timeseries[[block_name]]

      # preserve detrend coefficients from processed blocks; these are computed
      # per block on temp_file inside pipeline_handler() but, unlike the other
      # per-block artifacts below, were previously never copied back onto
      # `file`, so glassbox(detrend = TRUE) silently dropped `$detrend_coefs`
      if (!is.null(temp_file$detrend_coefs[[block_name]])) {
        if (is.null(file$detrend_coefs)) {
          file$detrend_coefs <- list()
        }
        file$detrend_coefs[[block_name]] <- temp_file$detrend_coefs[[
          block_name
        ]]
      }

      # preserve decimated.sample.rate from processed blocks
      if (!is.null(temp_file$decimated.sample.rate)) {
        file$decimated.sample.rate <- temp_file$decimated.sample.rate
      }

      # preserve full-resolution (pre-decimation) data for diagnostic plotting
      if (!is.null(temp_file$timeseries_pre_decimation[[block_name]])) {
        if (is.null(file$timeseries_pre_decimation)) {
          file$timeseries_pre_decimation <- list()
        }
        file$timeseries_pre_decimation[[
          block_name
        ]] <- temp_file$timeseries_pre_decimation[[block_name]]
      }

      # track latest pointer from successfully processed blocks
      if (
        !is.null(temp_file$latest[[block_name]]) &&
          !grepl("_([^_]+)_\\1", temp_file$latest[[block_name]])
      ) {
        final_latest <- temp_file$latest[[block_name]]
      }

      # update block state with final state
      block_states[[block_name]]$latest_pointer <- temp_file$latest[[
        block_name
      ]]
      block_states[[block_name]]$steps_completed <- block_step_counter - 1

      # update main file's latest pointer for current block
      if (is.list(file$latest)) {
        file$latest[[block_name]] <- temp_file$latest[[block_name]]
      } else {
        # convert to list if it wasn't already
        file$latest <- list()
        file$latest[[block_name]] <- temp_file$latest[[block_name]]
      }

      # collect params from this block
      if (!is.null(temp_file$params) && is.list(temp_file$params)) {
        all_params <- modifyList(all_params, temp_file$params)
      }
    }

    # recombine processed blocks
    file$timeseries <- processed_blocks

    # preserve params from processed blocks
    if (length(all_params) > 0) {
      file$params <- all_params
    }

    log_info("Block processing summary:", verbose = verbose)
    for (block_name in names(block_states)) {
      state <- block_states[[block_name]]
      status <- if (state$has_errors) "ERRORS" else "OK"
      log_info(
        "{block_name}: {status} (steps: {state$steps_completed}, latest: {state$latest_pointer})",
        verbose = verbose
      )
    }
  } else {
    log_error("No data blocks found error.")
  }

  # generate confounds after all other steps
  log_success("Running eyeris::summarize_confounds()", verbose = verbose)

  file <- eyeris::summarize_confounds(file)

  return(file)
}

#' Plot with seed handling for glassbox pipeline
#'
#' Internal function to handle plotting with consistent seed management
#' for the glassbox pipeline interactive previews.
#'
#' @param file The `eyeris` object to plot
#' @param step_counter Current step counter
#' @param seed A random seed for reproducible plotting
#' @param preview_n Number of preview epochs
#' @param preview_duration Duration of each preview in seconds
#' @param preview_window Preview window specification
#' @param only_linear_trend A flag to indicate whether to show only linear
#' trend
#' @param next_step Next step information
#' @param block_name Block name (optional, for multi-block processing)
#' @param verbose A flag to indicate whether to show verbose output
#'
#' @keywords internal
plot_with_seed <- function(
  file,
  step_counter,
  seed,
  preview_n,
  preview_duration,
  preview_window,
  only_linear_trend,
  next_step,
  block_name = NULL,
  verbose = TRUE
) {
  if (is.null(seed)) {
    seed <- rlang::`%||%`(seed, sample.int(.Machine$integer.max, 1))
  }

  withr::with_seed(seed, {
    if (!is.null(block_name)) {
      bn <- get_block_numbers(block_name)
      plot(
        file,
        steps = step_counter,
        preview_n = preview_n,
        seed = seed,
        preview_duration = preview_duration,
        preview_window = preview_window,
        only_linear_trend = only_linear_trend,
        next_step = next_step,
        block = bn,
        suppress_prompt = FALSE,
        verbose = verbose
      )
    } else {
      plot(
        file,
        steps = step_counter,
        preview_n = preview_n,
        seed = seed,
        preview_duration = preview_duration,
        preview_window = preview_window,
        only_linear_trend = only_linear_trend,
        next_step = next_step,
        suppress_prompt = FALSE,
        verbose = verbose
      )
    }
  })
}

#' Prompt user for continuation
#'
#' Prompts the user to continue or cancel the current operation.
#'
#' @return A logical flag indicating whether the user chose to continue
#'
#' @keywords internal
prompt_user <- function() {
  resp <- readline(prompt = "Continue? [Yes/No]: ")
  tolower(resp) == "yes" | tolower(resp) == "y"
}

#' Evaluate pipeline step parameters
#'
#' Converts pipeline step parameters to logical values for evaluation.
#'
#' @param params A list of pipeline step parameters
#'
#' @return A logical vector indicating which steps should be executed
#'
#' @keywords internal
evaluate_pipeline_step_params <- function(params) {
  sapply(params, function(x) {
    if (is.logical(x)) {
      isTRUE(x)
    } else {
      !identical(x, FALSE)
    }
  })
}

#' Internal glassbox function for processing individual eyes
#'
#' @param file The `eyeris` object to process
#' @param interactive_preview A flag to indicate whether to show interactive previews
#' @param preview_n Number of preview epochs
#' @param preview_duration Duration of each preview in seconds
#' @param preview_window Preview window specification
#' @param verbose A flag to indicate whether to show verbose output
#' @param params A list of pipeline step parameters
#' @param original_call The original call to the glassbox function
#' @param seed A random seed for reproducible plotting
#'
#' @return An `eyeris` object with the processed data lists
#'
#' @keywords internal
glassbox_internal <- function(
  file,
  interactive_preview = FALSE,
  preview_n = 3,
  preview_duration = 5,
  preview_window = NULL,
  verbose = TRUE,
  params,
  original_call,
  seed
) {
  # the default glassbox pipeline parameters
  default_params <- list(
    load_asc = list(block = "auto", binocular_mode = "average"),
    resample = TRUE,
    deblink = list(extend = 50),
    detransient = list(n = 16, mad_thresh = NULL),
    interpolate = TRUE,
    lpfilt = list(wp = 4, ws = 8, rp = 1, rs = 35, plot_freqz = verbose),
    downsample = FALSE,
    bin = FALSE,
    detrend = FALSE,
    zscore = TRUE,
    seed = 123
  )

  # override defaults
  params <- utils::modifyList(default_params, params)

  # evaluate which steps of pipeline to run
  which_steps <- evaluate_pipeline_step_params(params)

  # eyeris workflow data structure
  pipeline <- list(
    deblink = function(data, params, original_call) {
      if (which_steps[["deblink"]]) {
        call_info <- list(
          call = original_call,
          parameters = list(extend = params$deblink$extend)
        )
        eyeris::deblink(
          data,
          extend = params$deblink$extend,
          call_info = call_info
        )
      } else {
        data
      }
    },
    detransient = function(data, params, original_call) {
      if (which_steps[["detransient"]]) {
        call_info <- list(
          call = original_call,
          parameters = list(
            n = params$detransient$n,
            mad_thresh = params$detransient$mad_thresh
          )
        )
        eyeris::detransient(
          data,
          n = params$detransient$n,
          mad_thresh = params$detransient$mad_thresh,
          call_info = call_info
        )
      } else {
        data
      }
    },
    interpolate = function(data, params, original_call) {
      if (which_steps[["interpolate"]]) {
        max_gap_ms <- if (
          is.list(params$interpolate) &&
            "max_gap_ms" %in% names(params$interpolate)
        ) {
          params$interpolate$max_gap_ms
        } else {
          250
        }
        # validate up front so malformed input aborts clearly (and an explicit
        # NULL is normalized to Inf) instead of failing deep inside the step
        max_gap_ms <- validate_max_gap_ms(max_gap_ms)
        call_info <- list(
          call = original_call,
          parameters = list(max_gap_ms = max_gap_ms, verbose = verbose)
        )
        eyeris::interpolate(
          data,
          max_gap_ms = max_gap_ms,
          verbose = verbose,
          call_info = call_info
        )
      } else {
        data
      }
    },
    lpfilt = function(data, params, original_call) {
      if (which_steps[["lpfilt"]]) {
        call_info <- list(
          call = original_call,
          parameters = list(
            wp = params$lpfilt$wp,
            ws = params$lpfilt$ws,
            rp = params$lpfilt$rp,
            rs = params$lpfilt$rs,
            plot_freqz = params$lpfilt$plot_freqz
          )
        )
        eyeris::lpfilt(
          data,
          wp = params$lpfilt$wp,
          ws = params$lpfilt$ws,
          rp = params$lpfilt$rp,
          rs = params$lpfilt$rs,
          plot_freqz = params$lpfilt$plot_freqz,
          call_info = call_info
        )
      } else {
        data
      }
    },
    downsample = function(data, params, original_call) {
      if (which_steps[["downsample"]]) {
        if (is.null(params$downsample$plot_freqz)) {
          params$downsample$plot_freqz <- verbose
        }
        if (is.null(params$downsample$rp)) {
          params$downsample$rp <- 1
        }
        if (is.null(params$downsample$rs)) {
          params$downsample$rs <- 35
        }
        call_info <- list(
          call = original_call,
          parameters = list(
            target_fs = params$downsample$target_fs,
            plot_freqz = params$downsample$plot_freqz,
            rp = params$downsample$rp,
            rs = params$downsample$rs
          )
        )
        eyeris::downsample(
          data,
          target_fs = params$downsample$target_fs,
          plot_freqz = params$downsample$plot_freqz,
          rp = params$downsample$rp,
          rs = params$downsample$rs,
          call_info = call_info
        )
      } else {
        data
      }
    },
    bin = function(data, params, original_call) {
      if (which_steps[["bin"]]) {
        call_info <- list(
          call = original_call,
          parameters = list(
            bins_per_second = params$bin$bins_per_second,
            method = params$bin$method
          )
        )
        eyeris::bin(
          data,
          bins_per_second = params$bin$bins_per_second,
          method = params$bin$method,
          call_info = call_info
        )
      } else {
        data
      }
    },
    detrend = function(data, params, original_call) {
      if (which_steps[["detrend"]]) {
        detrend_opts <- if (is.list(params$detrend)) {
          params$detrend
        } else {
          list()
        }
        method <- if (!is.null(detrend_opts$method)) {
          detrend_opts$method
        } else {
          "linear"
        }
        spline_df <- if (!is.null(detrend_opts$spline_df)) {
          detrend_opts$spline_df
        } else {
          5
        }
        call_info <- list(
          call = original_call,
          parameters = list(method = method, spline_df = spline_df)
        )
        eyeris::detrend(
          data,
          method = method,
          spline_df = spline_df,
          call_info = call_info
        )
      } else {
        data
      }
    },
    zscore = function(data, params, original_call) {
      if (which_steps[["zscore"]]) {
        call_info <- list(call = original_call, parameters = list())
        eyeris::zscore(data, call_info = call_info)
      } else {
        data
      }
    }
  )

  step_counter <- 1
  only_linear_trend <- FALSE
  next_step <- c()

  has_multiple_blocks <- is.list(file$timeseries) && length(file$timeseries) > 0

  # process each block individually through all steps (except load_asc)
  if (has_multiple_blocks) {
    block_names <- names(file$timeseries)
    processed_blocks <- list()

    # clear any detrend coefficients inherited from a pre-loaded input object
    # (e.g., an object already processed by detrend()/glassbox()) before the
    # per-block recombine below, so the returned object retains only the
    # coefficients produced in this run -- and none survive when detrending is
    # disabled or fails
    file$detrend_coefs <- NULL

    # store orig latest pointer to restore it later
    original_latest <- file$latest
    final_latest <- NULL
    block_states <- list()
    # collect params from all blocks
    all_params <- list()

    for (block_name in block_names) {
      log_info("Processing block: {block_name}", verbose = verbose)

      temp_file <- file
      temp_file$timeseries <- list(file$timeseries[[block_name]])
      names(temp_file$timeseries) <- block_name

      # set latest pointer for current block
      if (is.list(original_latest)) {
        # multiblock: use pointer for current block
        temp_file$latest <- list()
        temp_file$latest[[block_name]] <- original_latest[[block_name]]
      } else {
        # single block converted to multiblock: use original pointer
        temp_file$latest <- list()
        temp_file$latest[[block_name]] <- original_latest
      }

      # init block state
      block_states[[block_name]] <- list(
        latest_pointer = temp_file$latest[[block_name]],
        steps_completed = 0,
        has_errors = FALSE
      )

      block_step_counter <- 2

      for (step_name in names(pipeline)) {
        action <- "Running "
        skip_plot <- FALSE

        if (!which_steps[[step_name]]) {
          action <- "Skipping "
          block_step_counter <- block_step_counter - 1
          skip_plot <- TRUE

          if (!is.null(temp_file$latest[[block_name]])) {
            expected_col <- paste0(
              temp_file$latest[[block_name]],
              "_",
              step_name
            )
            block_data <- temp_file$timeseries[[block_name]]
            if (expected_col %in% colnames(block_data)) {
              temp_file$latest[[block_name]] <- expected_col
            }
          }
        } else {
          if (step_name == "detrend") {
            only_linear_trend <- TRUE
          }
        }

        if (action == "Running ") {
          log_success(
            "{action}eyeris::{step_name}() for {block_name}",
            verbose = verbose
          )
        } else {
          log_warn(
            "Skipping eyeris::{step_name}() for {block_name}",
            verbose = verbose
          )
        }
        step_to_run <- pipeline[[step_name]]
        err_thrown <- FALSE

        temp_file <- tryCatch(
          {
            step_to_run(temp_file, params, original_call)
          },
          error = function(e) {
            if (!which_steps[["interpolate"]] && which_steps[["detrend"]]) {
              log_warn(
                "Because missing pupil samples were not interpolated, there is a mismatch in the number of samples in the detrended data. Please set `interpolate` to `TRUE` before detrending data OR disable detrending by setting `detrend` to `FALSE`.",
                verbose = TRUE
              )
            }

            log_warn(
              "Skipping eyeris::{step_name}() for {block_name}: {e$message}",
              verbose = verbose
            )
            err_thrown <<- TRUE
            block_step_counter <<- block_step_counter - 1

            # mark current block as having errors
            block_states[[block_name]]$has_errors <- TRUE

            # reset latest pointer to prevent corruption from propagating
            # find last valid column name in current block
            block_data <- temp_file$timeseries[[block_name]]
            pupil_cols <- grep("^pupil_", colnames(block_data), value = TRUE)
            if (length(pupil_cols) > 0) {
              # use last valid pupil column
              temp_file$latest[[block_name]] <- pupil_cols[length(pupil_cols)]
              block_states[[block_name]]$latest_pointer <- temp_file$latest[[
                block_name
              ]]
            } else {
              # fallback to original pointer for this block
              if (is.list(original_latest)) {
                temp_file$latest[[block_name]] <- original_latest[[block_name]]
              } else {
                temp_file$latest[[block_name]] <- original_latest
              }
              block_states[[block_name]]$latest_pointer <- temp_file$latest[[
                block_name
              ]]
            }

            temp_file
          }
        )

        if (
          verbose &&
            action == "Running " &&
            (step_name == "downsample" || step_name == "bin")
        ) {
          log_success(
            "Decimating sampling rate from {temp_file$info$sample.rate} Hz --> {temp_file$decimated.sample.rate} Hz...",
            verbose = verbose
          )
        }

        if (interactive_preview && !err_thrown && !skip_plot) {
          pupil_steps <- grep(
            "^pupil_",
            colnames(temp_file$timeseries[[block_name]]),
            value = TRUE
          )

          if (block_step_counter + 1 <= length(names(pipeline))) {
            next_step <- c(next_step, pupil_steps[block_step_counter])
          } else {
            next_step <- NULL
          }

          plot_with_seed(
            file = temp_file,
            step_counter = block_step_counter,
            seed = seed,
            preview_n = preview_n,
            preview_duration = preview_duration,
            preview_window = preview_window,
            only_linear_trend = only_linear_trend,
            next_step = next_step,
            block_name = block_name,
            verbose = verbose
          )

          if (step_name == "detrend") {
            only_linear_trend <- FALSE
          }

          if (step_name != "zscore") {
            if (!prompt_user()) {
              log_info(
                "Process cancelled after running the {step_name} step for {block_name}. Adjust your parameters and re-run!",
                verbose = verbose
              )
              break
            }
          }
        }

        block_step_counter <- block_step_counter + 1
      }

      processed_blocks[[block_name]] <- temp_file$timeseries[[block_name]]

      # preserve detrend coefficients from processed blocks; these are computed
      # per block on temp_file inside pipeline_handler() but, unlike the other
      # per-block artifacts below, were previously never copied back onto
      # `file`, so glassbox(detrend = TRUE) silently dropped `$detrend_coefs`
      if (!is.null(temp_file$detrend_coefs[[block_name]])) {
        if (is.null(file$detrend_coefs)) {
          file$detrend_coefs <- list()
        }
        file$detrend_coefs[[block_name]] <- temp_file$detrend_coefs[[
          block_name
        ]]
      }

      # preserve decimated.sample.rate from processed blocks
      if (!is.null(temp_file$decimated.sample.rate)) {
        file$decimated.sample.rate <- temp_file$decimated.sample.rate
      }

      # preserve full-resolution (pre-decimation) data for diagnostic plotting
      if (!is.null(temp_file$timeseries_pre_decimation[[block_name]])) {
        if (is.null(file$timeseries_pre_decimation)) {
          file$timeseries_pre_decimation <- list()
        }
        file$timeseries_pre_decimation[[
          block_name
        ]] <- temp_file$timeseries_pre_decimation[[block_name]]
      }

      # track latest pointer from successfully processed blocks
      if (
        !is.null(temp_file$latest[[block_name]]) &&
          !grepl("_([^_]+)_\\1", temp_file$latest[[block_name]])
      ) {
        final_latest <- temp_file$latest[[block_name]]
      }

      # update block state with final state
      block_states[[block_name]]$latest_pointer <- temp_file$latest[[
        block_name
      ]]
      block_states[[block_name]]$steps_completed <- block_step_counter - 1

      # update main file's latest pointer for current block
      if (is.list(file$latest)) {
        file$latest[[block_name]] <- temp_file$latest[[block_name]]
      } else {
        # convert to list if it wasn't already
        file$latest <- list()
        file$latest[[block_name]] <- temp_file$latest[[block_name]]
      }

      # collect params from this block
      if (!is.null(temp_file$params) && is.list(temp_file$params)) {
        all_params <- modifyList(all_params, temp_file$params)
      }
    }

    # recombine processed blocks
    file$timeseries <- processed_blocks

    # preserve params from processed blocks
    if (length(all_params) > 0) {
      file$params <- all_params
    }

    log_info("Block processing summary:", verbose = verbose)
    for (block_name in names(block_states)) {
      state <- block_states[[block_name]]
      status <- if (state$has_errors) "ERRORS" else "OK"
      log_info(
        "{block_name}: {status} (steps: {state$steps_completed}, latest: {state$latest_pointer})",
        verbose = verbose
      )
    }
  } else {
    log_error("No data blocks found error.")
  }

  # generate confounds after all other steps
  log_success("Running eyeris::summarize_confounds()", verbose = verbose)

  file <- eyeris::summarize_confounds(file)

  return(file)
}
