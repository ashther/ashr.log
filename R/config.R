
# configuration enviorment for log file path, max_bytes, backup_n, etc...
.config <- new.env(parent = emptyenv())

#' @title set log file
#'
#' @description set log file name, if the file or directory doesn't exist, it will be created
#'  and open file connection
#'
#' @param log_name log file name
#' @param log_level `ERROR`, `INFO` or `DEBUG`, constant in this package
#' @param rotate `size` or `daily`, the type of rotate log
#' @param max_size the max file size in bytes
#' @param units the max size units, should be one of `Kb`, `b`, `Mb`, `Gb`, `Tb`, `Pb`
#' @param backup_n the backup log files number, must be non-negative
#' @param as_json if all log input will be convert to json, default is true
#' @param verbose if print success message
#'
#' @export
#'
#' @examples
#' \dontrun{
#' openlog('log/log')
#' }
openlog <- function(log_name, log_level = INFO,
                    rotate = c('size', 'daily'), max_size = 100, backup_n = 5L,
                    units = c('Kb', 'b', 'Mb', 'Gb', 'Tb', 'Pb'),
                    as_json = TRUE, verbose = FALSE) {
  rotate <- match.arg(rotate)
  # log_level <- as.numeric(log_level)
  # if (is.na(log_level)) stop('log_level must be numeric!')
  as_json <- as.logical(as_json)
  if (is.na(as_json)) stop('as_json must be logical!')
  verbose <- as.logical(verbose)
  if (is.na(verbose)) stop('verbose must be logical!')

  if (length(log_name) > 1) {
    warning('multiple log files provided, use the frist one!', call. = FALSE)
    log_name <- log_name[1]
  }

  .config$log_name <- log_name
  createIfNotExist()

  .config$log_level <- log_level
  .config$rotate <- rotate
  .config$max_size <- setMaxSize(max_size, units, FALSE)
  .config$backup_n <- setBackupN(backup_n, FALSE)
  .config$as_json <- as_json

  if (verbose)
    message(sprintf(
      'log file was set to %s, \n
      log level is %s, \n
      rotate type is %s, \n
      backup number is %s, \n
      convert inpu to json is %s',
      log_name, names(log_level), rotate, backup_n, as_json
    ))

  invisible(TRUE)
}

# check if the file connection is opened
isOpenCon <- function() {
  con <- getLogCon()

  if (is.null(con))
    return(FALSE)

  suppressWarnings(
    !'try-error' %in% class(try(isOpen(con), silent = TRUE))
  )
}

#' @title close log file connection
#'
#' @param verbose if print success message
#'
#' @export
#'
#' @examples
#' \dontrun{
#' closelog()
#' }
closelog <- function(verbose = TRUE) {
  if (is.null(getLogName())) {
    warning("log file wasn't set already!", call. = FALSE)
    return(invisible())
  }

  if (isOpenCon()) {
    close(getLogCon())
    if (verbose)
      message('log file connection closed.')
    return(invisible(TRUE))
  }

  warning('log file connection is not opened!', call. = FALSE)
}

#' @title set log file size
#'
#' @description if the log file size is larger than `max_sizes`, it will be rename like log.1,
#' and the log message will be output to new log
#'
#' @param max_size the max file size in bytes
#' @param units the max size units, should be one of `Kb`, `b`, `Mb`, `Gb`, `Tb`, `Pb`
#' @param verbose if print success message
#'
#' @examples
#' \dontrun{
#' setMaxBytes(100*1024)
#' }
setMaxSize <- function(max_size, units = c('Kb', 'b', 'Mb', 'Gb', 'Tb', 'Pb'),
                       verbose = TRUE) {

  units <- match.arg(units)
  max_size <- as.numeric(max_size)
  if (is.na(max_size))
    stop("the max size parameter of log file must can't be converted to numeric!",
         call. = FALSE)

  units <- switch(
    units,
    Kb = 1024,
    b = 1,
    Mb = 1024^2,
    Gb = 1024^3,
    Tb = 1024^4,
    Pb = 1024^5
  )
  .config$max_size <- max_size * units

  if (verbose)
    message(sprintf(
      'the max size of log file was set to %s', getMaxSize()
    ))

  invisible(TRUE)
}

#' @title set log file backup number
#'
#' @description if the backup log file number is more than `backup_n`, the earliest log file
#' will be removed, and all log file will add 1 like 'log.1' -> 'log.2', the log message
#' will be output to new log file
#'
#' @param backup_n the max number of backup log file
#' @param verbose if print success message
#'
#' @examples
#' \dontrun{
#' setBackupN(5)
#' }
setBackupN <- function(backup_n, verbose = TRUE) {
  backup_n <- as.numeric(backup_n)
  if (is.na(backup_n))
    stop("the max backup log file number parameter can't be converted to integer!",
         call. = FALSE)

  .config$backup_n <- backup_n

  if (verbose)
    message(sprintf('the number of backup log file was set to %s', backup_n))

  invisible(TRUE)
}

# get log file name
getLogName <- function() {
  .config$log_name
}

# get log file connection
getLogCon <- function() {
  .config$log_con
}

# get log file max size
getMaxSize <- function() {
  tryCatch(
    utils:::format.object_size(.config$max_size, 'auto'),
    error = function(e) NULL
  )
}

# get log file backup number
getBackupN <- function() {
  .config$backup_n
}

getLogLevel <- function() {
  names(.config$log_level)
}

#' @title get log configuration
#'
#' @return log name, log level, rotate type, backup number, max size, if convert input to json
#' @export
#'
#' @examples
getLogInfo <- function() {
  res <- as.list(.config)
  modifyList(res, list(log_level = getLogLevel(), max_size = getMaxSize()))
}
