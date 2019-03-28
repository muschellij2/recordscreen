
[![Travis build
status](https://travis-ci.com/muschellij2/recordscreen.svg?branch=master)](https://travis-ci.com/muschellij2/recordscreen)
[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/github/muschellij2/recordscreen?branch=master&svg=true)](https://ci.appveyor.com/project/muschellij2/recordscreen)
[![Coverage
status](https://codecov.io/gh/muschellij2/recordscreen/branch/master/graph/badge.svg)](https://codecov.io/gh/muschellij2/recordscreen)
<!-- README.md is generated from README.Rmd. Please edit that file -->

# recordscreen Package:

The goal of `recordscreen` is to provide functions to reccord the screen
with `ffmpeg` to be able to show script execution. A system installation
`ffmpeg` is required, run to see if `ffmpeg` is in your `PATH`:

``` r
Sys.which("ffmpeg")
```

## Installation

You can install `recordscreen` from GitHub with:

``` r
# install.packages("remotes")
remotes::install_github("muschellij2/recordscreen")
```
