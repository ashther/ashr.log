
#' @title create diretory or/and file if log file path doesn't exits
createIfNotExist <- function() {
  # get dir name of log, create the dir and file it they dont exist
  log_name <- .config$log_name

  log_files_path <- dirname(log_name)
  if (!dir.exists(log_files_path)) dir.create(log_files_path, recursive = TRUE)
  if (!file.exists(log_name)) file.create(log_name)

  if (!isOpenCon())
    .config$log_con <- file(log_name, open = 'a')

  invisible(TRUE)
}

# core function to print log
.printlog <- function(..., .level = 0) {

  if (.level > .config$log_level)
    return(invisible())

  if (!isOpenCon()) {
    if (is.null(.config$log_name)) {
      warning("log file wasn't set already!", call. = FALSE)
      return(invisible())
    }
    createIfNotExist()
  }

  ts <- paste0('[', as.character(Sys.time()), ']')
  if (.config$as_json)
    cat(ts, jsonlite::toJSON(list(...), auto_unbox = TRUE),
        '\n', file = .config$log_con, append = TRUE)
  else
    cat(ts, ..., '\n', file = .config$log_con, append = TRUE)
}

#' @title cat message to log
#'
#' @description just like `cat`, with a timestamp
#'
#' @param ... content to be loged, cause we use `cat` function behind the hood, you can put any
#' R object here
#' @param .level the log message level
#'
#' @export
#'
#' @examples
#' \dontrun{
#' printlog('this is a test message')
#' }
printlog <- function(..., .level = 0) {
  if (.config$rotate == 'size')
    rotatelog(..., .level = .level)

  if (.config$rotate == 'daily')
    dailylog(..., .level = .level)

  if (.config$rotate == 'none')
    .printlog(..., .level = .level)
}
