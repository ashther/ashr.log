
# rotate log function ----------------------------------------------------
#' @title ratate log
#'
#' @describeIn provide rotate log function
#'
#' @param log_name the main log file name, all other backup log file will be in
#' the same directory
#' @param msg the message output to log file
#' @param max_bytes the max log file size, the unit is bytes
#' @param backup_n the max number of log files, include main log file
#'
#' @details each log file will be size of \code{max_bytes}
#' at most, and with only \code{backou_n} log files in this log directory
#'
#' @export
#'
#' @examples
#' \dontrun{
#' rotatelog('log/log', 'this is a test message', max_bytes = 200*1000, backup_n = 5)
#' }
rotatelog <- function(log_name, ..., max_bytes = 200*1000, backup_n = 5) {

  if (length(log_name) > 1)
    warning('multiple log files provided, use the frist one!')
  max_bytes <- as.numeric(max_bytes)
  if (is.na(max_bytes))
    stop("the max size parameter of log file must can't be converted to numeric!")
  backup_n <- as.integer(backup_n)
  if (is.na(backup_n))
    stop("the max backup log file number parameter can't be converted to integer!")

  # get dir name of log, create the dir and file it they dont exist
  log_files_path <- dirname(log_name)
  if (!dir.exists(log_files_path)) dir.create(log_files_path)
  if (!file.exists(log_name)) file.create(log_name)

  if (file.size(log_name) >= max_bytes) {

    log_files <- list.files(
      log_files_path,
      pattern = paste0(basename(log_name), '\\.?\\d*'),
      full.names = TRUE
    )
    ind <- gregexpr('\\d+$', log_files)
    ind <- vapply(seq_along(ind), FUN = function(i) {
      match_length <- attr(ind[[i]], 'match.length')
      match_start <- as.integer(ind[[i]])
      if (match_length > 0)
        as.integer(substr(log_files[i], match_start, match_start + match_length))
      else
        NA_integer_
    }, FUN.VALUE = integer(1))

    # delete extra log files bigger than backup_n
    file.remove(log_files[!is.na(ind) & ind >= backup_n])

    # rename log files, add 1 to everyone which already in log.1 type
    valid_ind <- !is.na(ind) & ind >= 1 & ind < backup_n
    if (any(valid_ind)) {
      files_from <- log_files[valid_ind]
      files_to <- file.path(
        log_files_path, paste0(basename(log_name), '.', ind[valid_ind] + 1)
      )
      ind_order <- order(ind[valid_ind], decreasing = TRUE)
      files_from <- files_from[ind_order]
      files_to <- files_to[ind_order]

      for (i in seq_along(files_from)) {
        file.rename(files_from[i], files_to[i])
      }
    }

    # rename log file to log.1, and re-create new log file
    file.rename(log_name, paste0(log_name, '.1'))
    printlog(log_name, ...)

  } else {
    printlog(log_name, ...)
  }
}
