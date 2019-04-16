
# configuration enviorment for log file path, max_bytes, backup_n, etc...
.config <- new.env(parent = emptyenv())

#' @title set log file
#'
#' @description set log file name, if the file or directory doesn't exist, it will be created
#'  and open file connection
#'
#' @param log_name log file name
#' @param log_level `ERROR`, `INFO` or `DEBUG`, constant in this package
#' @param rotate `size` or `daily`, the type of rotate log
#' @param max_size the max file size in bytes
#' @param units the max size units, should be one of `Kb`, `b`, `Mb`, `Gb`, `Tb`, `Pb`
#' @param backup_n the backup log files number, must be non-negative
#' @param as_json if all log input will be convert to json, default is true
#' @param verbose if print success message
#'
#' @export
#'
#' @examples
#' \dontrun{
#' openlog('log/log')
#' }
openlog <- function(log_name, log_level = INFO,
                    rotate = c('size', 'daily', 'none'), max_size = 100, backup_n = 5L,
                    units = c('Kb', 'b', 'Mb', 'Gb', 'Tb', 'Pb'),
                    as_json = TRUE, verbose = FALSE) {
  rotate <- match.arg(rotate)
  if (is.na(as.numeric(log_level))) stop('log_level must be numeric!')
  as_json <- as.logical(as_json)
  if (is.na(as_json)) stop('as_json must be logical!')
  verbose <- as.logical(verbose)
  if (is.na(verbose)) stop('verbose must be logical!')

  if (length(log_name) > 1) {
    warning('multiple log files provided, use the frist one!', call. = FALSE)
    log_name <- log_name[1]
  }

  backup_n <- as.integer(backup_n)
  if (is.na(backup_n))
    stop("the max backup log file number parameter can't be converted to integer!",
         call. = FALSE)

  .config$log_name <- log_name
  createIfNotExist()

  .config$log_level <- log_level
  .config$rotate <- rotate
  setMaxSize(max_size, units, FALSE)
  .config$backup_n <- backup_n
  .config$as_json <- as_json

  if (verbose)
    message(sprintf(
      'log file was set to %s, \nlog level is %s, \nrotate type is %s, \nmax size is %s, \nbackup number is %s, \nconvert input to json is %s',
      .config$log_name, names(.config$log_level), .config$rotate,
      getMaxSize(), .config$backup_n, .config$as_json
    ))

  invisible(TRUE)
}

appendVal <- function(val, params, .l) {
  val <- val[1]
  if (!names(val) %in% names(.l)) {
    params <- append(params, val)
  } else {
    params <- append(params, .l[names(val)])
  }
  params
}

# for rotatelog and dailylog, close and reopen
reopenlog <- function(.l) {
  params <- list()

  if (!'log_name' %in% names(.l)) {
    stop('no log name in configuration!', call. = FALSE)
  } else {
    params <- append(params, .l['log_name'])
  }

  params <- appendVal(list(rotate = 'size'), params, .l)
  params <- appendVal(list(backup_n = 5), params, .l)
  params <- appendVal(list(as_json = TRUE), params, .l)
  params <- append(params, list(verbose = FALSE))

  if (!'log_level' %in% names(.l)) {
    params <- append(params, list(log_level = INFO))
  } else {
    params <- append(params, list(log_level = get(.l$log_level)))
  }

  if (!'max_size' %in% names(.l)) {
    params <- append(params, list(max_size = 100, units = 'Kb'))
  } else {
    max_size <- unlist(strsplit(.l$max_size, ' '))
    max_size_value <- as.numeric(max_size[1])
    max_size_units <- switch(
      max_size[2],
      bytes = 'b',
      Kb = 'Kb',
      Mb = 'Mb',
      Gb = 'Gb',
      Tb = 'Tb',
      Pb = 'Pb'
    )
    params <- append(
      params, list(max_size = max_size_value, units = max_size_units)
    )
  }

  do.call(openlog, params)

}

# check if the file connection is opened
isOpenCon <- function() {
  con <- .config$log_con

  if (is.null(con))
    return(FALSE)

  suppressWarnings(
    !'try-error' %in% class(try(isOpen(con), silent = TRUE))
  )
}

#' @title close log file connection
#'
#' @param verbose if print success message
#'
#' @export
#'
#' @examples
#' \dontrun{
#' closelog()
#' }
closelog <- function(verbose = TRUE) {
  if (is.null(.config$log_name)) {
    warning("log file wasn't set already!", call. = FALSE)
    return(invisible())
  }

  if (isOpenCon()) {
    close(.config$log_con)
    .config$log_name <- NULL
    if (verbose)
      message('log file connection closed.')
    return(invisible(TRUE))
  }

  warning('log file connection is not opened!', call. = FALSE)
}

#' @title set log file size
#'
#' @description if the log file size is larger than `max_sizes`, it will be rename like log.1,
#' and the log message will be output to new log
#'
#' @param max_size the max file size in bytes
#' @param units the max size units, should be one of `Kb`, `b`, `Mb`, `Gb`, `Tb`, `Pb`
#' @param verbose if print success message
#'
#' @examples
#' \dontrun{
#' setMaxBytes(100*1024)
#' }
setMaxSize <- function(max_size, units = c('Kb', 'b', 'Mb', 'Gb', 'Tb', 'Pb'),
                       verbose = TRUE) {

  units <- match.arg(units)
  max_size <- as.numeric(max_size)
  if (is.na(max_size))
    stop("the max size parameter of log file must can't be converted to numeric!",
         call. = FALSE)

  units <- switch(
    units,
    Kb = 1024,
    b = 1,
    Mb = 1024^2,
    Gb = 1024^3,
    Tb = 1024^4,
    Pb = 1024^5
  )
  .config$max_size <- max_size * units

  if (verbose)
    message(sprintf(
      'the max size of log file was set to %s', getMaxSize()
    ))

  invisible(TRUE)
}

# get log file max size
getMaxSize <- function() {
  tryCatch(
    utils:::format.object_size(.config$max_size, 'auto'),
    error = function(e) NULL
  )
}

#' @title get log configuration
#'
#' @return log name, log level, rotate type, backup number, max size, if convert input to json
#'
#' @importFrom utils modifyList
#' @export
#'
#' @examples
#' \dontrun{
#' getLogInfo()
#' }
getLogInfo <- function() {
  res <- as.list(.config)
  modifyList(res, list(log_level = names(.config$log_level),
                       log_con = isOpenCon(),
                       max_size = getMaxSize()))
}
