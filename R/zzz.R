utils::globalVariables(c(
  "time_orig",
  "time_secs",
  "pupil",
  "blink",
  "blink.lag",
  "time",
  "blink.start",
  "blink.lead",
  "blink.end",
  "pupil_deblink",
  "event",
  "ps",
  "xp",
  "yp",
  "pupil_raw",
  "type",
  "block",
  "timebin",
  "step",
  "eyeris",
  "pupil_size",
  "matched_event",
  "matching_pattern",
  "s",
  "e",
  "text_unique",
  "verbose",
  "last_col"
))

.onAttach <- function(libname, pkgname) {
  pkg_version <- utils::packageVersion(pkgname)
  nickname <- "Lumpy Space Princess"
  packageStartupMessage(
    paste0(
      "\n",
      cli::col_cyan(cli::style_bold(pkgname)),
      " v",
      pkg_version,
      " - ",
      cli::col_br_magenta(nickname),
      " ",
      cli::col_br_magenta("\uA4B0\u2022\u1D17\u2022\uFF61\uA4B1\u06F6"),
      "\n",
      "Welcome! Type ",
      cli::col_yellow("?`", pkgname, "`"),
      " to get started.\n"
    )
  )
}
