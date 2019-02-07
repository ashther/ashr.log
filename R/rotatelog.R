
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
#' @importFrom  luzlogr openlog closelog printlog
#' @importFrom stringr str_extract str_replace
#' @importFrom fs file_info path_dir dir_ls file_delete file_move
#'
#' @export
rotatelog <- function(log_name, msg, max_bytes = 200*1000, backup_n = 5) {

  if (file_info(log_name)$size >= max_bytes) {

    log_files_path <- path_dir(log_name)
    log_files <- dir_ls(log_files_path, regexp = paste0(basename(log_name), '\\.?\\d*'))

    ind <- as.integer(str_extract(log_files, '\\d+$'))

    # delete extra log files bigger than backup_n
    file_delete(log_files[!is.na(ind) & ind >= backup_n])

    # rename log files, add 1 to everyone which already in log.1 type
    valid_ind <- !is.na(ind) & ind >= 1 & ind < backup_n
    files_from <- log_files[valid_ind]
    files_to <- str_replace(files_from, '\\d+$', as.character(ind[valid_ind] + 1))
    file_move(rev(files_from), rev(files_to))

    # rename log file to log.1, and re-create new log file
    closelog(FALSE)
    file_move(log_name, paste0(log_name, '.1'))
    openlog(log_name, append = TRUE, sink = TRUE)
    printlog(msg)

  } else {
    printlog(msg)
  }
}
