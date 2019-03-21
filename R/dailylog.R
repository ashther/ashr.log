
#' @title daily log
#'
#' @description provide daily log function
#'
#' @param ... the message output to log file
#'
#' @details expecpt the main log file, other log file will be named with its
#' create day, like `log.2000-01-01`, and with only \code{backou_n} log files in this log directory
#'
#' @export
#'
#' @examples
#' \dontrun{
#' dailylog('this is a test message')
#' }
dailylog <- function(...) {

  log_name <- getLogName()
  if (is.null(log_name)) {
    warning("log file wasn't set already!", call. = FALSE)
    return(invisible())
  }

  createIfNotExist()

  day_log_create <- as.Date(file.info(log_name)[1, 'ctime'])
  # this log is not created today
  if (day_log_create != Sys.Date()) {

    # rename this log to its create-day, and open new one
    closelog(FALSE)
    file.rename(log_name, paste(log_name, day_log_create, sep = '.'))
    printlog(...)

    # 1. list all log file under this directory
    # 2. if log file number is over than backup number
    # 3. delete all log file which are beyond backup number
    log_files_path <- dirname(log_name)
    log_files <- list.files(
      log_files_path,
      pattern = paste0('^', basename(log_name), '\\.\\d{4}-\\d{2}-\\d{2}$'),
      full.names = TRUE
    )
    if (length(log_files) > getBackupN()) {
      n_delete <- length(log_files) - getBackupN()
      log_files <- sort(log_files, decreasing = TRUE)
      file.remove(log_files[seq_len(n_delete)])
    }

    invisible(TRUE)

  } else {
    # well, this log is today's log, go ahead
    printlog(...)
  }
}
