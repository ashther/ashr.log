
# rotate log function ----------------------------------------------------
# @title ratate log
#
# @description provide rotate log function
#
# @param ... the message output to log file
# @param .level the log message level
#
# @details each log file will be size of \code{max_size}
# at most, and with only \code{backou_n} log files in this log directory
#
# @examples
# \dontrun{
# rotatelog('this is a test message')
# }
rotatelog <- function(..., .level = 0) {

  if (is.null(.config$log_name)) {
    warning("log file wasn't set already!", call. = FALSE)
    return(invisible())
  }

  createIfNotExist()
  log_files_path <- dirname(.config$log_name)

  if (file.size(.config$log_name) >= .config$max_size) {

    log_files <- list.files(
      log_files_path,
      pattern = paste0(basename(.config$log_name), '\\.?\\d*'),
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
    file.remove(log_files[!is.na(ind) & ind >= .config$backup_n])

    # rename log files, add 1 to everyone which already in log.1 type
    valid_ind <- !is.na(ind) & ind >= 1 & ind < .config$backup_n
    if (any(valid_ind)) {
      files_from <- log_files[valid_ind]
      files_to <- file.path(
        log_files_path, paste0(basename(.config$log_name), '.', ind[valid_ind] + 1)
      )
      ind_order <- order(ind[valid_ind], decreasing = TRUE)
      files_from <- files_from[ind_order]
      files_to <- files_to[ind_order]

      for (i in seq_along(files_from)) {
        file.rename(files_from[i], files_to[i])
      }
    }

    # rename log file to log.1, and re-create new log file
    .l <- getLogInfo()
    closelog(FALSE)
    file.rename(.l$log_name, paste0(.l$log_name, '.1'))
    reopenlog(.l)
    .printlog(..., .level = .level)

  } else {
    .printlog(..., .level = .level)
  }
}
