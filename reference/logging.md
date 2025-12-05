# Standardized logging functions for eyeris

These functions provide a consistent logging interface with automatic
timestamping, glue-style string interpolation, and support for multiple
string arguments.

## Arguments

- ...:

  Character strings to be logged. Will be collapsed with spaces.
  Supports glue-style interpolation with curly braces.

- verbose:

  Logical. Whether to actually print the log message.

- wrap:

  Logical. Whether to wrap long messages (default TRUE).

- .envir:

  Environment for glue interpolation (default: parent frame).
