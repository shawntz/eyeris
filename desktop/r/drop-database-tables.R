# Drop the tables of removed recordings from the project's DuckDB database.
# eyeris names tables <type>_<subject>_<session>_<task>_run<NN>[_<label>...] and
# finds a recording's tables by that part of the name when it replaces them;
# each further argument is one such part, such as _01_ret_clamp_run.
args <- commandArgs(trailingOnly = TRUE)
lib <- Sys.getenv("EYERIS_PACKAGE_LIBRARY")
if (nzchar(lib) && dir.exists(lib)) .libPaths(c(lib, .libPaths()))
drop_tables <- function(target, patterns) {
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = target)
  # Shutting down checkpoints the database, so the file is complete on its own.
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  tables <- DBI::dbGetQuery(
    con,
    "SELECT table_name FROM duckdb_tables() WHERE schema_name = 'main' AND database_name = current_database()"
  )$table_name
  matched <- tables[vapply(
    tables,
    function(table) any(vapply(patterns, grepl, logical(1), x = table, fixed = TRUE)),
    logical(1)
  )]
  DBI::dbBegin(con)
  for (table in matched) {
    DBI::dbExecute(con, paste0("DROP TABLE ", DBI::dbQuoteIdentifier(con, table)))
  }
  DBI::dbCommit(con)
  length(matched)
}
tryCatch(cat("Dropped", drop_tables(args[1], args[-1]), "tables\n"), error = function(e) {
  message("Dropping database tables failed: ", conditionMessage(e))
  quit(status = 1)
})
