# Package index

## 🧠 Core Pipeline

Start here with the full pipeline wrapper to quickly and easily call and
customize the opinionated `glassbox` pipeline in `eyeris`.

- [`glassbox()`](https://shawnschwartz.com/eyeris/reference/glassbox.md)
  :

  The opinionated "glass box" `eyeris` pipeline

- [`eyelogger()`](https://shawnschwartz.com/eyeris/reference/eyelogger.md)
  :

  Run `eyeris` commands with automatic logging of R console's stdout and
  stderr

## ⏱️ Epoching Pupil Data

Conveniently extract tidy trial-based epochs with optional baseline
correction.

- [`epoch()`](https://shawnschwartz.com/eyeris/reference/epoch.md) :
  Epoch (and baseline) pupil data based on custom event message
  structure

## 📤 Export & Visualize

Save out BIDS-style derivatives, generate diagnostic HTML reports, and
interactively plot and explore your pupil data.

- [`bidsify()`](https://shawnschwartz.com/eyeris/reference/bidsify.md) :
  Save out pupil time series data in a BIDS-like structure

- [`boilerplate()`](https://shawnschwartz.com/eyeris/reference/boilerplate.md)
  : Generate a reproducible, copy-and-paste-ready methods boilerplate

- [`summarize_confounds()`](https://shawnschwartz.com/eyeris/reference/summarize_confounds.md)
  : Extract confounding variables calculated separately for each pupil
  data file

- [`plot(`*`<eyeris>`*`)`](https://shawnschwartz.com/eyeris/reference/plot.eyeris.md)
  :

  Plot pre-processed pupil data from `eyeris`

- [`plot_gaze_heatmap()`](https://shawnschwartz.com/eyeris/reference/plot_gaze_heatmap.md)
  : Create gaze heatmap of eye coordinates

- [`plot_binocular_correlation()`](https://shawnschwartz.com/eyeris/reference/plot_binocular_correlation.md)
  : Plot binocular correlation between left and right eye data

- [`eyeris_color_palette()`](https://shawnschwartz.com/eyeris/reference/eyeris_color_palette.md)
  : Default color palette for eyeris plotting functions

## 🗄 Database Storage & Analysis

High-performance database storage and querying powered by DuckDB.
Scalable alternative to CSV files for large studies, cloud computing,
and collaborative research.

- [`eyeris_db_collect()`](https://shawnschwartz.com/eyeris/reference/eyeris_db_collect.md)
  : Extract and aggregate eyeris data across subjects from database
- [`eyeris_db_summary()`](https://shawnschwartz.com/eyeris/reference/eyeris_db_summary.md)
  : Get summary statistics for eyeris database
- [`eyeris_db_connect()`](https://shawnschwartz.com/eyeris/reference/eyeris_db_connect.md)
  : Connect to eyeris project database (user-facing)
- [`eyeris_db_disconnect()`](https://shawnschwartz.com/eyeris/reference/eyeris_db_disconnect.md)
  : Disconnect from eyeris database (user-facing)
- [`eyeris_db_read()`](https://shawnschwartz.com/eyeris/reference/eyeris_db_read.md)
  : Read eyeris data from database
- [`eyeris_db_list_tables()`](https://shawnschwartz.com/eyeris/reference/eyeris_db_list_tables.md)
  : List available tables in eyeris database
- [`eyeris_db_to_chunked_files()`](https://shawnschwartz.com/eyeris/reference/eyeris_db_to_chunked_files.md)
  : Export eyeris database to chunked files
- [`eyeris_db_to_parquet()`](https://shawnschwartz.com/eyeris/reference/eyeris_db_to_parquet.md)
  : Split eyeris database into N parquet files by data type
- [`read_eyeris_parquet()`](https://shawnschwartz.com/eyeris/reference/read_eyeris_parquet.md)
  : Read parquet files back into R
- [`process_chunked_query()`](https://shawnschwartz.com/eyeris/reference/process_chunked_query.md)
  : Process large database query in chunks
- [`eyeris_db_split_for_sharing()`](https://shawnschwartz.com/eyeris/reference/eyeris_db_split_for_sharing.md)
  : Split eyerisdb for data sharing and distribution
- [`eyeris_db_reconstruct_from_chunks()`](https://shawnschwartz.com/eyeris/reference/eyeris_db_reconstruct_from_chunks.md)
  : Reconstruct eyerisdb from chunked files

## 🔧 Preprocessing Steps

Modular functions used by the `glassbox` pipeline for cleaning and
transforming pupil data.

- [`load_asc()`](https://shawnschwartz.com/eyeris/reference/load_asc.md)
  :

  Load and parse SR Research EyeLink `.asc` files

- [`deblink()`](https://shawnschwartz.com/eyeris/reference/deblink.md) :
  NA-pad blink events / missing data

- [`detransient()`](https://shawnschwartz.com/eyeris/reference/detransient.md)
  : Remove pupil samples that are physiologically unlikely

- [`interpolate()`](https://shawnschwartz.com/eyeris/reference/interpolate.md)
  : Interpolate missing pupil samples

- [`lpfilt()`](https://shawnschwartz.com/eyeris/reference/lpfilt.md) :
  Lowpass filtering of time series data

- [`downsample()`](https://shawnschwartz.com/eyeris/reference/downsample.md)
  : Downsample pupil time series with anti-aliasing filtering

- [`bin()`](https://shawnschwartz.com/eyeris/reference/bin.md) : Bin
  pupil time series by averaging within time bins

- [`detrend()`](https://shawnschwartz.com/eyeris/reference/detrend.md) :
  Detrend the pupil time series

- [`zscore()`](https://shawnschwartz.com/eyeris/reference/zscore.md) :
  Z-score pupil time series data

## 🧩 Build Your Own Extensions

Advanced tools for creating custom steps that plug into the glassbox
pipeline.

- [`pipeline_handler()`](https://shawnschwartz.com/eyeris/reference/pipeline_handler.md)
  :

  Build a generic operation (extension) for the `eyeris` pipeline

## 📈 Demo Datasets

Example eye-tracking / pupil datasets for testing and demonstrating
pipeline functionality.

- [`eyelink_asc_demo_dataset()`](https://shawnschwartz.com/eyeris/reference/eyelink_asc_demo_dataset.md)
  : Access example EyeLink .asc demo dataset file provided by the eyeris
  package.
- [`eyelink_asc_binocular_demo_dataset()`](https://shawnschwartz.com/eyeris/reference/eyelink_asc_binocular_demo_dataset.md)
  : Access example EyeLink .asc binocular mock dataset file provided by
  the eyeris package.
