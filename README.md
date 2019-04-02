# ashr.log
[![Travis build status](https://travis-ci.org/ashther/ashr.log.svg?branch=master)](https://travis-ci.org/ashther/ashr.log)
[![R Style Guide: Good Parts](https://img.shields.io/badge/code%20style-goodparts-blue.svg)](http://adv-r.had.co.nz/Style.html)
[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

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
setLogName('log/log')
setMaxBytes(200*1024)
setBackupN(5)

printlog('this message will be output directly to log file')
rotatelog('before output message to log file, the file size and backup number will be checked first')
```

## TODO
- add log level error info debug
- make log message as json string
- use openlog instead of setMaxSize setBackupN
- use printlog instead of rotatelog dailylog, set rotate type in openlog
- use infolog instead of getLogName getMaxSize getBackupN
