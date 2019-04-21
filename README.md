# ashr.log
[![Travis build status](https://travis-ci.org/ashther/ashr.log.svg?branch=master)](https://travis-ci.org/ashther/ashr.log)
[![R Style Guide: Good Parts](https://img.shields.io/badge/code%20style-goodparts-blue.svg)](http://adv-r.had.co.nz/Style.html)
[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

The goal of ashr.log is to provide simple rotate log functionality, which is a basic and 
important utility, but I don't see it in `luzlogr` or `futile.logger`.

## Installation

You can install the released version of ashr.log with:

``` r
# install.packages('devtools')
devtools::install_github('ashther/ashr.log')
```

## Example
When `ashr.log` is loaded, the global log configuration will be set, such as log max size, log rotate type, etc. 
Open the log file, opening the file connection for outputing log content meanwhile.
``` r
openlog('log/log') # with default configuration
# TODO more configuration setting
```

Print the log content to log file, with the rotate type(or not) which is in global configuration.
``` r
printlog(msg = 'normal message', id = id)
# TODO more printlog style
```

Close log connection, and delete the log file name in global configuration.
``` r
closelog()
```

We often need to analysis script running history, API error response, long-running task process, etc. A handful and simple functionality about reading log files in, cleaning and manipulating is very imporatant.
``` r
readlog() # use default argument, and the log file name in global configuration
# TODO more readlog type
```

## TODO
- readlog with custom log file name, not in global configuration
- print log to console while outputing to log file
- more unit test
