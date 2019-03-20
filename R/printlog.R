
#' @title create diretory or/and file if log file path doesn't exits
createIfNotExist <- function() {
  # get dir name of log, create the dir and file it they dont exist
  log_files_path <- dirname(.config$log_name)
  if (!dir.exists(log_files_path)) dir.create(log_files_path)
  if (!file.exists(.config$log_name)) file.create(.config$log_name)

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
  if (is.null(.config$log_name)) {
    warning("log file wasn't set already!", call. = FALSE)
    return(invisible())
  }

  # TODO maybe log file was removed after setLogName, so we check it exist-status
  # maybe we should put the create-new-connectin function in createIfNotExist?
  # and add a onfiguration in .config like .config$log_conn, and use it in cat
  createIfNotExist()

  ts <- paste('[', as.character(Sys.time()), ']')
  cat(ts, ..., '\n', file = .config$log_name, append = TRUE)
}
