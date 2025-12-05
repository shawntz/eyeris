# Split eyerisdb for data sharing and distribution

Creates multiple smaller eyerisdb files from a single large database for
easier distribution via platforms with file size limits (GitHub, OSF,
data repositories, etc.). Data can be chunked by data type, by number of
chunks, or by maximum file size. Includes metadata to facilitate
reconstruction of the original database.

## Usage

``` r
eyeris_db_split_for_sharing(
  bids_dir,
  db_path = "my-project",
  output_dir = NULL,
  chunk_strategy = "by_data_type",
  n_chunks = 4,
  max_chunk_size_mb = 100,
  data_types = NULL,
  group_by_epoch_label = TRUE,
  include_metadata = TRUE,
  verbose = TRUE
)
```

## Arguments

- bids_dir:

  Path to the BIDS directory containing the source database

- db_path:

  Source database name (defaults to "my-project", becomes
  "my-project.eyerisdb")

- output_dir:

  Directory to save chunked databases (defaults to
  bids_dir/derivatives/chunked_db)

- chunk_strategy:

  Strategy for chunking: "by_data_type", "by_count", or "by_size"
  (default: "by_data_type")

- n_chunks:

  Number of chunks to create when chunk_strategy = "by_count" (default:
  4)

- max_chunk_size_mb:

  Maximum size per chunk in MB when chunk_strategy = "by_size" (default:
  100)

- data_types:

  Vector of data types to include. If NULL (default), includes all
  available

- group_by_epoch_label:

  If TRUE (default), processes epoch-related data types separately by
  epoch label

- include_metadata:

  Whether to include eyeris metadata columns in chunked databases
  (default: TRUE)

- verbose:

  Whether to print progress messages (default: TRUE)

## Value

List containing information about created chunked databases and
reconstruction instructions

## Examples

``` r
if (FALSE) { # \dontrun{
# These examples require an existing eyeris database

# Chunk by data type (each data type gets its own database file)
chunk_info <- eyeris_db_split_for_sharing(
  bids_dir = "/path/to/bids",
  db_path = "large-project",
  chunk_strategy = "by_data_type"
)

# Chunk into 6 files by count
chunk_info <- eyeris_db_split_for_sharing(
  bids_dir = "/path/to/bids",
  db_path = "large-project",
  chunk_strategy = "by_count",
  n_chunks = 6
)

# Chunk by size (max 50MB per file)
chunk_info <- eyeris_db_split_for_sharing(
  bids_dir = "/path/to/bids",
  db_path = "large-project",
  chunk_strategy = "by_size",
  max_chunk_size_mb = 50
)
} # }
```
