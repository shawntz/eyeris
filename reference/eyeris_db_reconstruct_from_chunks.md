# Reconstruct eyerisdb from chunked files

Merges multiple chunked eyerisdb files back into a single database file.
Uses the reconstruction metadata file created by
[`eyeris_db_split_for_sharing()`](https://eyeris.shawnschwartz.com/reference/eyeris_db_split_for_sharing.md)
to ensure proper reconstruction.

## Usage

``` r
eyeris_db_reconstruct_from_chunks(
  chunked_dir,
  output_path,
  reconstruction_file = NULL,
  verbose = TRUE
)
```

## Arguments

- chunked_dir:

  Directory containing the chunked database files and reconstruction
  metadata

- output_path:

  Full path for the reconstructed database (e.g.,
  "/path/to/reconstructed.eyerisdb")

- reconstruction_file:

  Path to the reconstruction metadata JSON file. If NULL (default),
  searches for "\*\_reconstruction_info.json" in chunked_dir

- verbose:

  Whether to print progress messages (default: TRUE)

## Value

List containing information about the reconstruction process

## Examples

``` r
if (FALSE) { # \dontrun{
# Reconstruct database from chunked files
reconstruction_info <- eyeris_db_reconstruct_from_chunks(
  chunked_dir = "/path/to/chunked_db/project-name",
  output_path = "/path/to/reconstructed-project.eyerisdb"
)

# Specify custom reconstruction file location
reconstruction_info <- eyeris_db_reconstruct_from_chunks(
  chunked_dir = "/path/to/chunked_db/project-name",
  output_path = "/path/to/reconstructed-project.eyerisdb",
  reconstruction_file = "/path/to/custom_reconstruction_info.json"
)
} # }
```
