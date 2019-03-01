
#' @title cat message to log
#'
#' @description just like `cat`, with a timestamp
#'
#' @param log_name log file name
#' @param ... content to be loged, cause we use `cat` function behind the hood, you can put any
#' R object here
#'
#' @export
#'
#' @examples
#' \dontrun{
#' printlog('log/log', 'this is a test message')
#' }
printlog <- function(log_name, ...) {
  ts <- paste('[', as.character(Sys.time()), ']')
  cat(ts, ..., '\n', file = log_name, append = TRUE)
}
