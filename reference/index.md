# Package index

## 🧠 Core Pipeline

Start here with the full pipeline wrapper to quickly and easily call and
customize the opinionated `glassbox` pipeline in `eyeris`.

- [`glassbox()`](https://eyeris.shawnschwartz.com/reference/glassbox.md)
  :

  The opinionated "glass box" `eyeris` pipeline

- [`eyelogger()`](https://eyeris.shawnschwartz.com/reference/eyelogger.md)
  :

  Run `eyeris` commands with automatic logging of R console's stdout and
  stderr

## ⏱️ Epoching Pupil Data

Conveniently extract tidy trial-based epochs with optional baseline
correction.

- [`epoch()`](https://eyeris.shawnschwartz.com/reference/epoch.md) :
  Epoch (and baseline) pupil data based on custom event message
  structure

## 📤 Export & Visualize

Save out BIDS-style derivatives, generate diagnostic HTML reports, and
interactively plot and explore your pupil data.

- [`bidsify()`](https://eyeris.shawnschwartz.com/reference/bidsify.md) :
  Save out pupil time series data in a BIDS-like structure

- [`boilerplate()`](https://eyeris.shawnschwartz.com/reference/boilerplate.md)
  : Generate a reproducible, copy-and-paste-ready methods boilerplate

- [`summarize_confounds()`](https://eyeris.shawnschwartz.com/reference/summarize_confounds.md)
  : Extract confounding variables calculated separately for each pupil
  data file

- [`plot(`*`<eyeris>`*`)`](https://eyeris.shawnschwartz.com/reference/plot.eyeris.md)
  :

  Plot pre-processed pupil data from `eyeris`

- [`plot_gaze_heatmap()`](https://eyeris.shawnschwartz.com/reference/plot_gaze_heatmap.md)
  : Create gaze heatmap of eye coordinates

- [`plot_binocular_correlation()`](https://eyeris.shawnschwartz.com/reference/plot_binocular_correlation.md)
  : Plot binocular correlation between left and right eye data

- [`eyeris_color_palette()`](https://eyeris.shawnschwartz.com/reference/eyeris_color_palette.md)
  : Default color palette for eyeris plotting functions

## 🗄 Database Storage & Analysis

High-performance database storage and querying powered by DuckDB.
Scalable alternative to CSV files for large studies, cloud computing,
and collaborative research.

- [`eyeris_db_collect()`](https://eyeris.shawnschwartz.com/reference/eyeris_db_collect.md)
  : Extract and aggregate eyeris data across subjects from database
- [`eyeris_db_summary()`](https://eyeris.shawnschwartz.com/reference/eyeris_db_summary.md)
  : Get summary statistics for eyeris database
- [`eyeris_db_connect()`](https://eyeris.shawnschwartz.com/reference/eyeris_db_connect.md)
  : Connect to eyeris project database (user-facing)
- [`eyeris_db_disconnect()`](https://eyeris.shawnschwartz.com/reference/eyeris_db_disconnect.md)
  : Disconnect from eyeris database (user-facing)
- [`eyeris_db_read()`](https://eyeris.shawnschwartz.com/reference/eyeris_db_read.md)
  : Read eyeris data from database
- [`eyeris_db_list_tables()`](https://eyeris.shawnschwartz.com/reference/eyeris_db_list_tables.md)
  : List available tables in eyeris database
- [`eyeris_db_to_chunked_files()`](https://eyeris.shawnschwartz.com/reference/eyeris_db_to_chunked_files.md)
  : Export eyeris database to chunked files
- [`eyeris_db_to_parquet()`](https://eyeris.shawnschwartz.com/reference/eyeris_db_to_parquet.md)
  : Split eyeris database into N parquet files by data type
- [`read_eyeris_parquet()`](https://eyeris.shawnschwartz.com/reference/read_eyeris_parquet.md)
  : Read parquet files back into R
- [`process_chunked_query()`](https://eyeris.shawnschwartz.com/reference/process_chunked_query.md)
  : Process large database query in chunks
- [`eyeris_db_split_for_sharing()`](https://eyeris.shawnschwartz.com/reference/eyeris_db_split_for_sharing.md)
  : Split eyerisdb for data sharing and distribution
- [`eyeris_db_reconstruct_from_chunks()`](https://eyeris.shawnschwartz.com/reference/eyeris_db_reconstruct_from_chunks.md)
  : Reconstruct eyerisdb from chunked files

## 🔧 Preprocessing Steps

Modular functions used by the `glassbox` pipeline for cleaning and
transforming pupil data.

- [`load_asc()`](https://eyeris.shawnschwartz.com/reference/load_asc.md)
  :

  Load and parse SR Research EyeLink `.asc` files

- [`load_generic()`](https://eyeris.shawnschwartz.com/reference/load_generic.md)
  : Load pupillometry data from a non-EyeLink eye tracker

- [`resample()`](https://eyeris.shawnschwartz.com/reference/resample.md)
  : Resample an irregularly-sampled pupil timeseries onto a uniform grid

- [`deblink()`](https://eyeris.shawnschwartz.com/reference/deblink.md) :
  NA-pad blink events / missing data

- [`detransient()`](https://eyeris.shawnschwartz.com/reference/detransient.md)
  : Remove pupil samples that are physiologically unlikely

- [`interpolate()`](https://eyeris.shawnschwartz.com/reference/interpolate.md)
  : Interpolate missing pupil samples

- [`lpfilt()`](https://eyeris.shawnschwartz.com/reference/lpfilt.md) :
  Lowpass filtering of time series data

- [`downsample()`](https://eyeris.shawnschwartz.com/reference/downsample.md)
  : Downsample pupil time series with anti-aliasing filtering

- [`bin()`](https://eyeris.shawnschwartz.com/reference/bin.md) : Bin
  pupil time series by averaging within time bins

- [`detrend()`](https://eyeris.shawnschwartz.com/reference/detrend.md) :
  Detrend the pupil time series

- [`zscore()`](https://eyeris.shawnschwartz.com/reference/zscore.md) :
  Z-score pupil time series data

## 🧩 Build Your Own Extensions

Advanced tools for creating custom steps that plug into the glassbox
pipeline.

- [`pipeline_handler()`](https://eyeris.shawnschwartz.com/reference/pipeline_handler.md)
  :

  Build a generic operation (extension) for the `eyeris` pipeline

## 📈 Demo Datasets

Example eye-tracking / pupil datasets for testing and demonstrating
pipeline functionality.

- [`eyelink_asc_demo_dataset()`](https://eyeris.shawnschwartz.com/reference/eyelink_asc_demo_dataset.md)
  : Access example EyeLink .asc demo dataset file provided by the eyeris
  package.
- [`eyelink_asc_binocular_demo_dataset()`](https://eyeris.shawnschwartz.com/reference/eyelink_asc_binocular_demo_dataset.md)
  : Access example EyeLink .asc binocular mock dataset file provided by
  the eyeris package.
