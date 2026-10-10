args <- commandArgs(trailingOnly = TRUE)
dir.create(args[1], recursive = TRUE, showWarnings = FALSE)
make <- function() {
  n <- 12000L
  signal <- sin(seq(0, 10, length.out = n)) + 4000
  signal[4501] <- 9500
  signal[5001:5100] <- NA_real_
  frame <- function(event, trial) data.frame(
    timebin = seq(0, 2, length.out = n), time_orig = seq_len(n),
    pupil_raw = signal + 100, pupil_raw_lpfilt = signal,
    matched_event = event, trial = trial, block = 1L,
    arbitrary_metadata = rep(c('quoted "value"', 'comma, value'), length.out = n)
  )
  structure(list(epoch_probe = list(block_1 = rbind(frame('REPEATED_EVENT', 7), frame('REPEATED_EVENT', 7), frame('EVENT_OTHER', 8)),
    info = list(block_1 = list(epoch_limits = c(-1, 1))))), class = 'eyeris')
}
x <- make()
saveRDS(x, file.path(args[1], 'sub-001_task-memory.rds'))
# Epochs with different amounts of missing data at each stage: 30.8% raw and
# 5.8% final, 0.8% in both, and 60% in both.
z <- make()
rows <- function(i) seq.int((i - 1L) * 12000L + 1L, i * 12000L)
z$epoch_probe$block_1$pupil_raw[rows(1)[1:3600]] <- NA
z$epoch_probe$block_1$pupil_raw_lpfilt[rows(1)[1:600]] <- NA
z$epoch_probe$block_1$pupil_raw[rows(3)[1:7200]] <- NA
z$epoch_probe$block_1$pupil_raw_lpfilt[rows(3)[1:7200]] <- NA
saveRDS(z, file.path(args[1], 'sub-005_task-memory.rds'))
x$epoch_probe$block_1$pupil_raw[1] <- -200
saveRDS(x, file.path(args[1], 'sub-002_task-memory.rds'))
# BIDS entities in the filename give the session, task and run.
y <- x
y$epoch_probe$block_1$pupil_raw[2] <- -300
saveRDS(y, file.path(args[1], 'sub-002_ses-02_task-memory_run-3.rds'))
# A run and label with overlapping trial numbers must not collide.
x$epoch_second <- x$epoch_probe
x$epoch_probe$block_7 <- x$epoch_probe$block_1
saveRDS(x, file.path(args[1], 'sub-003_task-memory.rds'))
saveRDS(list(unrelated = TRUE), file.path(args[1], 'invalid.rds'))
large <- structure(list(epoch_probe = list(block_1 = data.frame(
  timebin = rep(c(0, 1), 10001), pupil_raw = rep(c(100, 110), 10001),
  matched_event = 'REPEATED_EVENT', trial = rep(seq_len(10001), each = 2)
))), class = 'eyeris')
saveRDS(large, file.path(args[1], 'sub-large.rds'))
