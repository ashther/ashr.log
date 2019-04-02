
#' @title daily log
#'
#' @description provide daily log function
#'
#' @param ... the message output to log file
#'
#' @details expecpt the main log file, other log file will be named with its
#' last modify day, like `log.2000-01-01`, and with only \code{backou_n} log files in this log directory
#'
#' @export
#'
#' @examples
#' \dontrun{
#' dailylog('this is a test message')
#' }
dailylog <- function(..., .level = 0) {

  log_name <- getLogName()
  if (is.null(log_name)) {
    warning("log file wasn't set already!", call. = FALSE)
    return(invisible())
  }

  createIfNotExist()

  day_last_modify <- as.Date(file.info(log_name)$mtime, tz = Sys.timezone())
  # this last modify day of this file is not today
  if (day_last_modify != Sys.Date()) {

    # rename this log to its create-day, and open new one
    closelog(FALSE)
    file.rename(log_name, paste(log_name, day_last_modify, sep = '.'))
    printlog(..., .level = .level)

  } else {
    # well, this log is today's log, go ahead
    printlog(..., .level = .level)
  }

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
    log_files <- sort(log_files)
    file.remove(log_files[seq_len(n_delete)])
  }

  invisible(TRUE)
}
