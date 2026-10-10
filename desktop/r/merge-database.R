# Copy tables from a processing job's DuckDB database into the project's
# database. eyeris names tables by subject, session, task and run, so a table that
# already exists belongs to the same published run and is left unchanged.
args <- commandArgs(trailingOnly = TRUE)
lib <- Sys.getenv("EYERIS_PACKAGE_LIBRARY")
if (nzchar(lib) && dir.exists(lib)) .libPaths(c(lib, .libPaths()))
merge_database <- function(source, target) {
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = target)
  # Shutting down checkpoints the database, so the file is complete on its own.
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  DBI::dbExecute(con, paste0("ATTACH ", DBI::dbQuoteString(con, source), " AS job (READ_ONLY)"))
  tables <- function(database) {
    DBI::dbGetQuery(con, paste0(
      "SELECT table_name FROM duckdb_tables() WHERE schema_name = 'main' AND database_name = ",
      database
    ))$table_name
  }
  added <- setdiff(tables("'job'"), tables("current_database()"))
  DBI::dbBegin(con)
  for (table in added) {
    name <- DBI::dbQuoteIdentifier(con, table)
    DBI::dbExecute(con, paste0("CREATE TABLE ", name, " AS SELECT * FROM job.main.", name))
  }
  DBI::dbCommit(con)
  DBI::dbExecute(con, "DETACH job")
  length(added)
}
tryCatch(cat("Merged", merge_database(args[1], args[2]), "tables\n"), error = function(e) {
  message("Database merge failed: ", conditionMessage(e))
  quit(status = 1)
})
