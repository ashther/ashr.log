
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
  if (is.null(.config$log_name))
    stop("log file wasn't set already!")

  ts <- paste('[', as.character(Sys.time()), ']')
  cat(ts, ..., '\n', file = .config$log_name, append = TRUE)
}
