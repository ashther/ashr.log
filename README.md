# ashr.log
[![Travis build status](https://travis-ci.org/ashther/ashr.log.svg?branch=master)](https://travis-ci.org/ashther/ashr.log)
[![R Style Guide: Good Parts](https://img.shields.io/badge/code%20style-goodparts-blue.svg)](http://adv-r.had.co.nz/Style.html)

The goal of ashr.log is to provide simple rotate log function, which is a basic and 
import utility, but I don't see it in `luzlogr` or `futile.logger`.

## Installation

You can install the released version of ashr.log with:

``` r
# install.packages('devtools')
devtools::install_github('ashther/ashr.log')
```

## Example
You can specify the rotating log with max file size and backup file number, for 
example, we want to limit each bakcup log file size only to 200kB, and 5 log files
at most:
```r
rotatelog('log/log', 'this is a test msg', max_bytes = 200*1000, backup_n = 5)
```
