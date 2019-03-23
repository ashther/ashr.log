
extractNumeric <- function(s, pattern, now, n) {
  x <- as.numeric(gsub(paste0(pattern, 's*$'), '', s, ignore.case = TRUE))
  if (is.na(x))
    stop('invalid log read time limit format!', call. = FALSE)
  now - x * n
}

periodToTime <- function(time_string) {
  now <- Sys.time()
  day_1 <- 24*3600

  if (time_string %in% c('today', 'TODAY'))
    return(strftime(now, '%Y-%m-%d 00:00:00'))

  if (time_string %in% c('yesterday', 'YESTERDAY'))
    return(strftime(now - day_1, '%Y-%m-%d 00:00:00'))

  if (time_string %in% c('this week', 'THIS WEEK'))
    return(as.POSIXct(cut(now, 'week')))

  if (time_string %in% c('this month', 'THIS MONTH', 'this mon', 'THIS MON'))
    return(strftime(now, '%Y-%m-01 00:00:00'))

  if (time_string %in% c('this year', 'THIS YEAR'))
    return(strftime(now, '%Y-01-01 00:00:00'))

  if (time_string %in% c('this hour', 'THIS HOUR'))
    return(strftime(now, '%Y-%m-%d %H:00:00'))

  if (time_string %in% c('this minute', 'THIS MINUTE', 'this min', 'THIS MIN'))
    return(strftime(now, '%Y-%m-%d %H:%M:00'))

  if (!grepl('^[0-9.]+\\s', time_string, ignore.case = TRUE))
    stop('invalid log read time limit format!', call. = FALSE)

  if (grepl('^[0-9.]+\\sdays*$', time_string, ignore.case = TRUE))
    return(extractNumeric(time_string, 'day', now, day_1))

  if (grepl('^[0-9.]+\\sweeks*$', time_string, ignore.case = TRUE))
    return(extractNumeric(time_string, 'week', now, day_1 * 7))

  if (grepl('^[0-9.]+\\smonths*$', time_string, ignore.case = TRUE))
    return(extractNumeric(time_string, 'month', now, day_1 * 30))

  if (grepl('^[0-9.]+\\syears*$', time_string, ignore.case = TRUE))
    return(extractNumeric(time_string, 'year', now, day_1 * 365))

  if (grepl('^[0-9.]+\\shours*$', time_string, ignore.case = TRUE))
    return(extractNumeric(time_string, 'hour', now, 3600))

  if (grepl('^[0-9.]+\\sminutes*$', time_string, ignore.case = TRUE))
    return(extractNumeric(time_string, 'minute', now, 60))

  stop('invalid log read time limit format!', call. = TRUE)
}

readSingleLog <- function(log_file, .json) {

}

readlog <- function(.time = 'today', .json = TRUE) {
  stopifnot(is.character(.time))
  stopifnot(is.logical(.json))

  time_limit <- period_to_time(.time)
  log_name <- getLogName()
  log_files_path <- dirname(log_name)
  log_files <- list.files(
    log_files_path,
    pattern = paste0(
      '^', basename(log_name), c('$', '\\.\\d+$', '\\.\\d{4}-\\d{2}-\\d{2}$'),
      collapse = '|'
    ),
    full.names = TRUE
  )
  log_files <- sort(log_files)

  if (length(log_files) == 0) {
    warning('no log files to read!')
    return(tibble(timestamp = as.POSIXct(NA), log = character(0)))
  }

  i <- 1
  res <- tibble()
  while (TRUE) {
    res_temp <- readSingleLog(log_files[i], .json = .json)
    res <- rbind(res, res_temp)

    # TODO it should be min(res$timestamp) < time_limit, and make this
    # in res_temp would be fine
    if (res$timestamp > time_limit | i >= length(log_files))
      break()

    i <- i + 1
  }

  res
}
