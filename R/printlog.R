
#' @title create diretory or/and file if log file path doesn't exits
createIfNotExist <- function() {
  # get dir name of log, create the dir and file it they dont exist
  log_name <- getLogName()

  log_files_path <- dirname(log_name)
  if (!dir.exists(log_files_path)) dir.create(log_files_path, recursive = TRUE)
  if (!file.exists(log_name)) file.create(log_name)

  if (!isOpenCon())
    .config$log_con <- file(log_name, open = 'a')

  invisible(TRUE)
}

#' @title cat message to log
#'
#' @description just like `cat`, with a timestamp
#'
#' @param ... content to be loged, cause we use `cat` function behind the hood, you can put any
#' R object here
#'
#' @export
#'
#' @examples
#' \dontrun{
#' printlog('this is a test message')
#' }
printlog <- function(...) {

  # TODO add log level error info debug
  # TODO make log message as json string

  if (!isOpenCon()) {
    if (is.null(getLogName())) {
      warning("log file connection wasn't opened!", call. = FALSE)
      return(invisible())
    }
    createIfNotExist()
  }

  ts <- paste('[', as.character(Sys.time()), ']')
  cat(ts, ..., '\n', file = getLogCon(), append = TRUE)
}
