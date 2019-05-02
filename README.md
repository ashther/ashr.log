# ashr.log
[![Travis build status](https://travis-ci.org/ashther/ashr.log.svg?branch=master)](https://travis-ci.org/ashther/ashr.log)
[![Coverage status](https://codecov.io/gh/ashther/ashr.log/branch/master/graph/badge.svg)](https://codecov.io/github/ashther/ashr.log?branch=master)
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
openlog('log/log', log_level = ERROR) # only check log when error happened
openlog('log/log', rotate = 'daily') # save log daily
openlog('log/log', max_size = 3, units = 'Mb') # a litter bigger log file
openlog('log/log', backup_n = 10)
openlog('log/log', as_json = FALSE)
```

Print the log content to log file, with the rotate type(or not) which is in global configuration.
``` r
printlog(msg = 'normal message', id = id)

# if as_json is TRUE in configuration, the log content will be like '[plain message]'
printlog('plain message')  
```

Close log connection, and delete the log file name in global configuration.
``` r
closelog()
```

We often need to analysis script running history, API error response, long-running task process, etc. A handful and simple functionality about reading log files in, cleaning and manipulating is very imporatant.
``` r
readlog() # use default argument, and the log file name in global configuration
readlog(FALSE) # don't parse log content as json
readlog(TRUE, 'today')

readlog(.time = 'yesterday')
readlog(.time = '2 days')
readlog(.time = 'this minute')
readlog(.time = 'this min')
readlog(.time = '3 minutes')
readlog(.time = '1 hour')
readlog(.time = 'this hour')
readlog(.time = '3 weeks')
readlog(.time = 'this week')
readlog(.time = 'this month')
readlog(.time = 'this mon')
readlog(.time = '2 months')
readlog(.time = 'this year')
readlog(.time = '2 years')

readlog(log_name = 'log/log') # custom log files
readlog(log_name = 'log/') # read all log files in log direcotry
```
