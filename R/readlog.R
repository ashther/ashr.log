
extractNumeric <- function(s, pattern, now, n) {
  x <- as.numeric(gsub(paste0(pattern, 's*$'), '', s, ignore.case = TRUE))
  if (is.na(x))
    stop('invalid log read time limit format!', call. = FALSE)
  now - x * n
}

periodToTime <- function(time_string) {
  if (time_string == 'all')
    return(0)

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
  if (Sys.info()['sysname'] == 'Windows') {
    temp <- iconv(x, 'utf-8', 'gbk')
    if (all(!is.na(temp))) {
      x <- temp
    } else {
      x_na <- x[is.na(temp)]
      if (length(x_na) > 3) {
        x_na <- c(x_na[c(1:3)], '...')
      }
      warning(
        'there may be encoding issue, following log can\'t be iconv\n',
        sprintf("%s\n", x_na),
        call. = FALSE
      )
    }
  }

  x <- strsplit(
    x, '^\\[\\s*(?=\\d{4}-\\d{2}-\\d{2})|\\]\\s+(?=[\\{\\[])',
    perl = TRUE
  )

  if (length(x) == 0)
    return(dplyr::tibble(timestamp = as.POSIXct(NA), log = character(0)))

  x <- unlist(x)
  id_timestamp <- seq(2, length(x), by = 3)
  id_log <- id_timestamp + 1
  dplyr::tibble(
    timestamp = as.POSIXct(x[id_timestamp]),
    log = x[id_log]
  )
}

fromJSONLog <- function(log_df) {
  idx <- grepl('(\\}|\\])\\s*$', log_df$log, perl = TRUE) &
    grepl('^\\s*(\\{|\\[)', log_df$log, perl = TRUE)
  if (any(!idx)) {
    log_df <- log_df[idx, ]
    warning(sprintf('remove %s non-json', sum(!idx)), call. = FALSE)
  }

  log_temp <- paste0(log_df$log, collapse = ',')
  log_temp <- paste0('[', log_temp, ']')
  log_temp <- jsonlite::fromJSON(log_temp)
  if (is.list(log_temp) & !is.data.frame(log_temp)) {
    log_temp <- dplyr::bind_rows(lapply(log_temp, function(x) {
      if (is.null(names(x)))
        return(list(log = paste0(x, collapse = ' ')))
      x
    }))
  }
  dplyr::as_tibble(
    cbind(log_df[, 'timestamp', drop = FALSE], log_temp)
  )
}

#' @title read log files
#'
#' @description read log files, and convert to data frame
#'
#' @param log_name log file name will be used if this argument is missing, otherwise
#' `readlog` will read `log_name`, `log_name` can be either file or directory
#' @param .time the min timestamp in log files
#' @param as_json if convert log column to multiple columns, it will use `as_json`
#' in configuration if this is not provided
#'
#' @return log data frame
#' @export
#'
#' @importFrom utils file_test
#'
#' @examples
#' \dontrun{
#' readlog() # use default argument, and the log file name in global configuration
#' readlog(as_json = FALSE) # don't parse log content as json
#' readlog(.time = 'today', as_json = TRUE)
#'
#' readlog(.time = 'yesterday')
#' readlog(.time = '2 days')
#' readlog(.time = 'this minute')
#' readlog(.time = 'this min')
#' readlog(.time = '3 minutes')
#' readlog(.time = '1 hour')
#' readlog(.time = 'this hour')
#' readlog(.time = '3 weeks')
#' readlog(.time = 'this week')
#' readlog(.time = 'this month')
#' readlog(.time = 'this mon')
#' readlog(.time = '2 months')
#' readlog(.time = 'this year')
#' readlog(.time = '2 years')
#'
#' readlog(log_name = 'log/log') # custom log files
#' readlog(log_name = 'log/') # read all log files in log direcotry
#' }
readlog <- function(log_name, .time = 'all', as_json) {

  stopifnot(is.character(.time))
  time_limit <- periodToTime(.time)

  if (missing(as_json))
    as_json <- .config$as_json
  stopifnot(is.logical(as_json))

  # got all log files in the log directory, including rotate log and daily log
  if (missing(log_name))
    log_name <- dirname(.config$log_name)

  if (is.null(log_name)) {
    warning('no log name to read!', call. = FALSE)
    return(dplyr::tibble(timestamp = as.POSIXct(NA), log = character(0)))
  }

  if (file_test('-d', log_name)) {
    log_files <- list.files(log_name, full.names = TRUE)
    ord <- order(file.info(log_files)$mtime, decreasing = TRUE)
    log_files <- log_files[ord]
  } else {
    log_files <- log_name
  }

  is_mix_logtype <- any(grepl('\\.\\d+$', basename(log_files))) &
    any(grepl('\\.\\d{4}-\\d{2}-\\d{2}$', basename(log_files)))

  if (length(log_files) == 0) {
    warning('no log files to read!', call. = FALSE)
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

    if (i >= length(log_files))
      break()
    if (!is_mix_logtype &
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
