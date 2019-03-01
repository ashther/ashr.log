
# configuration enviorment for log file path, max_bytes, backup_n, etc...
.config <- new.env(parent = emptyenv())

#' @title set log file name
#'
#' @description set log file name, if the file or directory doesn't exist, it will be created
#'
#' @param log_name log file name
#' @param verbose if print success message
#'
#' @export
#'
#' @examples
#' \dontrun{
#' setLogName('log/log')
#' }
setLogName <- function(log_name, verbose = TRUE) {
  if (length(log_name) > 1) {
    warning('multiple log files provided, use the frist one!')
    log_name <- log_name[1]
  }

  # get dir name of log, create the dir and file it they dont exist
  log_files_path <- dirname(log_name)
  if (!dir.exists(log_files_path)) dir.create(log_files_path)
  if (!file.exists(log_name)) file.create(log_name)

  .config$log_name <- log_name

  ts <- paste('[', as.character(Sys.time()), ']')
  cat(ts, '=== log start ===', '\n', file = log_name, append = TRUE)

  if (verbose)
    message(sprintf('log file was set to %s', log_name))

  invisible(TRUE)
}

#' @title get log file name
#'
#' @return the file path of log which was set by user
#' @export
#'
#' @examples
#' \dontrun{
#' getLogName()
#' }
getLogName <- function() {
  .config$log_name
}

#' @title set log file size
#'
#' @description if the log file size is larger than `max_bytes`, it will be rename like log.1,
#' and the log message will be output to new log
#'
#' @param max_bytes the max file size in bytes
#' @param verbose if print success message
#'
#' @export
#'
#' @examples
#' \dontrun{
#' setMaxBytes(100*1024)
#' }
setMaxBytes <- function(max_bytes, verbose = TRUE) {
  max_bytes <- as.numeric(max_bytes)
  if (is.na(max_bytes))
    stop("the max size parameter of log file must can't be converted to numeric!")

  .config$max_bytes <- max_bytes

  if (verbose)
    message(sprintf('the max size of log file was set to %s bytes', max_bytes))

  invisible(TRUE)
}

#' @title get log file max size
#'
#' @return the max size of log file which was set by user
#' @export
#'
#' @examples
#' \dontrun{
#' getMaxBytes()
#' }
getMaxBytes <- function() {
  .config$max_bytes
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
#' @export
#'
#' @examples
#' \dontrun{
#' setBackupN(5)
#' }
setBackupN <- function(backup_n, verbose = TRUE) {
  backup_n <- as.numeric(backup_n)
  if (is.na(backup_n))
    stop("the max backup log file number parameter can't be converted to integer!")

  .config$backup_n <- backup_n

  if (verbose)
    message(sprintf('the number of backup log file was set to %s bytes', backup_n))

  invisible(TRUE)
}

#' @title get log file backup number
#'
#' @return the number of backup log file which was set by user
#' @export
#'
#' @examples
#' \dontrun{
#' getBackupN()
#' }
getBackupN <- function() {
  .config$backup_n
}
