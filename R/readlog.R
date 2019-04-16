
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

readSingleLog <- function(log_file) {
  x <- readLines(log_file)
  x <- strsplit(x, '^\\[\\s*|\\s*]\\s+')

  if (length(x) == 0)
    return(dplyr::tibble(timestamp = as.POSIXct(NA), log = character(0)))

  do.call(rbind, lapply(x, function(y) {
    dplyr::tibble(timestamp = as.POSIXct(y[2]), log = y[3])
  }))
}

# TODO fix bug about no names vector as log content
fromJSONLog <- function(log_df) {
  log_temp <- lapply(log_df$log, jsonlite::fromJSON)
  log_temp <- dplyr::bind_rows(log_temp)
  dplyr::as_tibble(
    cbind(log_df[, 'timestamp', drop = FALSE], log_temp)
  )
}

#' @title read log files
#'
#' @description read log files, and convert to data frame
#'
#' @param .time the min timestamp in log files
#' @param as_json if convert log column to multiple columns
#'
#' @return log data frame
#' @export
#'
#' @examples
#' \dontrun{
#' readlog('today', TRUE)
#' }
readlog <- function(.time = 'today', as_json = TRUE) {
  stopifnot(is.character(.time))
  stopifnot(is.logical(as_json))

  # got all log files in the log directory, including rotate log and daily log
  time_limit <- periodToTime(.time)
  log_name <- .config$log_name
  log_files_path <- dirname(log_name)
  log_files <- list.files(
    log_files_path,
    pattern = paste0(
      # pattern match:
      # 1. log
      # 2. log.1
      # 3. log.2000-01-01
      '^', basename(log_name), c('$', '\\.\\d+$', '\\.\\d{4}-\\d{2}-\\d{2}$'),
      collapse = '|'
    ),
    full.names = TRUE
  )
  log_files <- sort(log_files)

  if (length(log_files) == 0) {
    warning('no log files to read!')
    return(dplyr::tibble(timestamp = as.POSIXct(NA), log = character(0)))
  }

  # loop log files, rbind one by one, until
  # 1. run out all of log files
  # 2. current log file's min timestamp is samller than time limit, cause the
  # timestamp is older when log files are older, no need to read older log files
  i <- 1
  res <- dplyr::tibble()
  while (TRUE) {
    res_temp <- readSingleLog(log_files[i])
    res <- rbind(res, res_temp)

    if (i >= length(log_files) |
        (nrow(res_temp) > 0 & min(res$timestamp, na.rm = TRUE) < time_limit))
      break()

    i <- i + 1
  }
  res <- res[res$timestamp >= time_limit, , drop = FALSE]

  # convert log column to multiple columns
  if (as_json)
    res <- fromJSONLog(res)

  res[order(res$timestamp, decreasing = TRUE), , drop = FALSE]
}
