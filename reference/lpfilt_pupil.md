# Internal function to lowpass filter pupil data

This function lowpass filters pupil data using a Butterworth filter.

This function is called by the exposed wrapper
[`lpfilt()`](https://shawnschwartz.com/eyeris/reference/lpfilt.md)

## Usage

``` r
lpfilt_pupil(x, prev_op, wp, ws, rp, rs, fs, plot_freqz)
```

## Arguments

- x:

  A data frame containing pupil data

- prev_op:

  The name of the previous operation in the pipeline

- wp:

  The end of passband frequency in Hz (desired lowpass cutoff)

- ws:

  The start of stopband frequency in Hz (required lowpass cutoff)

- rp:

  Required maximal ripple within passband in dB

- rs:

  Required minimal attenuation within stopband in dB

- fs:

  The sample rate of the data

- plot_freqz:

  A flag to indicate whether to display the filter frequency response

## Value

A vector of filtered pupil data
