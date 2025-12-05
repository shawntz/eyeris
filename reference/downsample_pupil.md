# Internal function to downsample pupil data

This function downsamples pupil data by applying an anti-aliasing filter
before decimation. Unlike binning, downsampling preserves the original
temporal dynamics without averaging within bins.

This function is called by the exposed wrapper
[`downsample()`](https://shawnschwartz.com/eyeris/reference/downsample.md).

## Usage

``` r
downsample_pupil(x, prev_op, target_fs, plot_freqz, current_fs, rp, rs)
```

## Arguments

- x:

  A data frame containing pupil data with columns `time_secs` and the
  previous operation's pupil column

- prev_op:

  The name of the previous operation's pupil column

- target_fs:

  The target sampling frequency in Hz after downsampling

- plot_freqz:

  A flag to indicate whether to display the filter frequency response.
  Defaults to `FALSE`

- current_fs:

  The current sampling frequency in Hz. Defaults to `NULL`

- rp:

  Passband ripple in dB. Defaults to `1`

- rs:

  Stopband attenuation in dB. Defaults to `35`

## Value

A list containing the downsampled data and the decimated sample rate
