context("test-ashr")

test_that("setting and getting", {

  log_name <- file.path(tempdir(), 'log/log')
  openlog(log_name)
  temp <- getloginfo()
  expect_equal(temp$log_name, log_name)

  expect_true(ashr.log:::isOpenCon())
  # can't test on linux
  # file.remove(log_name)
  # expect_true(file.exists(log_name))

  closelog()
  unlink(dirname(log_name), TRUE)

  expect_output(
    openlog(log_name, log_level = DEBUG, rotate = 'daily', max_size = 666,
            units = 'b', backup_n = 6, as_json = FALSE, is_print = TRUE),
    regexp = 'open log'
  )
  temp <- getloginfo()
  expect_equal(temp$backup_n, 6)
  expect_false(temp$as_json)
  expect_equal(temp$log_level, 'DEBUG')
  expect_equal(temp$max_size, '666 bytes')
  expect_equal(temp$rotate, 'daily')
  expect_equal(temp$is_print, TRUE)

  setMaxSize(1, units = 'Kb')
  expect_equal(getMaxSize(), '1 Kb')

  setMaxSize(0.1)
  expect_equal(getMaxSize(), '102.4 bytes')

  setMaxSize(10000)
  expect_equal(getMaxSize(), '9.8 Mb')

  setMaxSize(1, units = 'b')
  expect_equal(getMaxSize(), '1 bytes')

  setMaxSize(1, units = 'Mb')
  expect_equal(getMaxSize(), '1 Mb')

  openlog(log_name, is_print = TRUE)
  expect_output(closelog(), 'close log')

  unlink(dirname(log_name), TRUE)
})

test_that('log level', {

  log_name <- file.path(tempdir(), 'log/log')
  openlog(log_name, as_json = FALSE)

  printlog(msg = 'this is info message', .level = INFO)
  printlog(msg = 'this is error message', .level = ERROR)
  printlog(msg = 'this is debug message', .level = DEBUG)

  temp <- readLines(log_name)
  temp <- sapply(
    strsplit(temp, '(?<=\\d{2}:\\d{2}:\\d{2})\\](?=\\s)', perl = TRUE),
    `[[`, 2
  )
  temp <- trimws(temp)
  expect_equal(temp, c('open log', 'this is info message', 'this is error message'))

  closelog()
  unlink(dirname(log_name), TRUE)

  openlog(log_name, as_json = FALSE, log_level = ERROR)

  printlog(msg = 'this is info message', .level = INFO)
  printlog(msg = 'this is error message', .level = ERROR)
  printlog(msg = 'this is debug message', .level = DEBUG)

  temp <- readLines(log_name)
  temp <- sapply(
    strsplit(temp, '(?<=\\d{2}:\\d{2}:\\d{2})\\](?=\\s)', perl = TRUE),
    `[[`, 2
  )
  temp <- trimws(temp)
  expect_equal(temp, c('open log', 'this is error message'))

  closelog()
  unlink(dirname(log_name), TRUE)

  openlog(log_name, as_json = FALSE, log_level = DEBUG)

  printlog(msg = 'this is info message', .level = INFO)
  printlog(msg = 'this is error message', .level = ERROR)
  printlog(msg = 'this is debug message', .level = DEBUG)

  temp <- readLines(log_name)
  temp <- sapply(
    strsplit(temp, '(?<=\\d{2}:\\d{2}:\\d{2})\\](?=\\s)', perl = TRUE),
    `[[`, 2
  )
  temp <- trimws(temp)
  expect_equal(temp, c('open log', 'this is info message', 'this is error message',
                       'this is debug message'))

  closelog()
  unlink(dirname(log_name), TRUE)
})

test_that('print to log file', {

  log_name <- file.path(tempdir(), 'log/log')

  openlog(log_name, as_json = FALSE, rotate = 'none')
  printlog('this is a test plain message')
  temp <- readLines(log_name)
  temp <- strsplit(temp, '(?<=\\d{2}:\\d{2}:\\d{2})\\](?=\\s)', perl = TRUE)[[2]][2]
  temp <- trimws(temp)
  expect_equal(temp, 'this is a test plain message')

  closelog()
  unlink(dirname(log_name), TRUE)

  openlog(log_name, as_json = TRUE, rotate = 'none')
  printlog('this is a array message')
  temp <- readLines(log_name)
  temp <- strsplit(temp, '(?<=\\d{2}:\\d{2}:\\d{2})\\](?=\\s)', perl = TRUE)[[2]][2]
  temp <- trimws(temp)
  expect_equal(temp, "[\"this is a array message\"]")

  printlog(msg = 'this is a test message')
  temp <- readLines(log_name)[3]
  temp <- strsplit(temp, '(?<=\\d{2}:\\d{2}:\\d{2})\\](?=\\s)', perl = TRUE)[[1]][2]
  temp <- trimws(temp)
  expect_equal(temp, "{\"msg\":\"this is a test message\"}")

  closelog()
  expect_warning(printlog('test'), "log file wasn't set already!")
  unlink(dirname(log_name), TRUE)
})

test_that('rotate log', {

  log_name <- file.path(tempdir(), 'log/log')

  openlog(log_name, rotate = 'size', max_size = 1, units = 'Kb', backup_n = 3,
          as_json = FALSE, is_print = TRUE)
  # setMaxSize(1, units = 'Kb')
  # setBackupN(3)
  printlog(paste(rep(letters, 10), collapse = ''))
  temp <- list.files(dirname(log_name))
  expect_equal(length(temp), 1)
  expect_equal(temp, 'log')

  invisible(lapply(1:5, printlog, ... = paste(rep(letters, 10), collapse = '')))
  temp <- list.files(dirname(log_name))
  expect_equal(length(temp), 2)
  expect_equal(temp, c('log', 'log.1'))

  invisible(lapply(1:5, printlog, ... = paste(rep(letters, 10), collapse = '')))
  temp <- list.files(dirname(log_name))
  expect_equal(length(temp), 3)
  expect_equal(temp, c('log', 'log.1', 'log.2'))

  invisible(lapply(1:10, printlog, ... = paste(rep(letters, 10), collapse = '')))
  temp <- list.files(dirname(log_name))
  expect_equal(length(temp), 4)
  expect_equal(temp, c('log', 'log.1', 'log.2', 'log.3'))

  invisible(lapply(1:5, printlog, ... = paste(rep(letters, 10), collapse = '')))
  temp <- list.files(dirname(log_name))
  expect_equal(length(temp), 4)
  expect_equal(temp, c('log', 'log.1', 'log.2', 'log.3'))

  expect_output(printlog(msg = 'output ok'), 'output ok')

  closelog()
  unlink(dirname(log_name), TRUE)
})

test_that('daily log', {
  msg_test <- 'write message to an old log file'

  log_name <- file.path(tempdir(), 'log/log')
  openlog(log_name, rotate = 'daily', as_json = FALSE)

  printlog(msg_test)
  temp <- readLines(log_name)
  temp <- strsplit(temp, '(?<=\\d{2}:\\d{2}:\\d{2})\\](?=\\s)', perl = TRUE)[[2]][2]
  temp <- trimws(temp)
  expect_equal(temp, msg_test)

  os_test <- Sys.info()['sysname']
  if (os_test %in% c('Windows', 'Linux')) {
    if (os_test == 'Windows')
      cmd <- "powershell -command (Get-ChildItem %s).LastWriteTime = '06/22/1986 12:42AM'"
    else
      cmd <- 'touch -d "1986-06-22 00:42:00" %s'

    res <- system(sprintf(cmd, log_name), intern = TRUE)
    expect_equal(res, character(0))

    printlog(msg_test)
    temp <- list.files(dirname(log_name))
    expect_equal(temp, c('log', 'log.1986-06-22'))
    file.remove(file.path(dirname(log_name), 'log.1986-06-22'))
  }
  closelog()

  openlog(log_name, rotate = 'daily', backup_n = 2)
  file.create(file.path(dirname(log_name), 'log.2000-01-01'))
  file.create(file.path(dirname(log_name), 'log.2000-01-02'))
  file.create(file.path(dirname(log_name), 'log.2000-01-03'))
  file.create(file.path(dirname(log_name), 'log.2000-01-04'))
  # setBackupN(2)
  printlog(msg_test)
  temp <- list.files(dirname(log_name))
  expect_equal(temp, c('log', 'log.2000-01-03', 'log.2000-01-04'))

  closelog()
  unlink(dirname(log_name), TRUE)
})

test_that('read log', {

  log_name <- file.path(tempdir(), 'log/log')
  # different file size between windows and linux
  openlog(log_name, max_size = 100, units = 'b', backup_n = 5)
  # setMaxSize(100, 'b')
  # setBackupN(5)

  printlog(x = paste0(letters, collapse = ''))
  invisible(lapply(1:10, function(x) {
    printlog(y = paste0(1:10, collapse = ''))
  }))

  temp <- readlog(file.path(dirname(log_name), 'log.1'), as_json = FALSE)
  temp_all <- readlog(dirname(log_name), as_json = FALSE)
  expect_lt(nrow(temp), nrow(temp_all))

  temp <- readlog(as_json = FALSE)
  expect_is(temp, 'tbl_df')
  expect_equal(colnames(temp), c('timestamp', 'log'))
  temp <- readlog()
  expect_is(temp, 'tbl_df')
  expect_equal(colnames(temp), c('timestamp', 'msg', 'y', 'x'))

  closelog()

  openlog(log_name, as_json = FALSE)
  temp <- readlog()
  expect_is(temp, 'tbl_df')
  expect_equal(colnames(temp), c('timestamp', 'log'))

  closelog()

  openlog(log_name)
  printlog('message without names')
  expect_warning({temp <- readlog()}, 'remove \\d+ non-json')
  expect_is(temp, 'tbl_df')
  expect_equal(colnames(temp), c('timestamp', 'msg', 'y', 'log', 'x'))

  closelog()

  expect_warning({temp <- readlog(log_name = log_name)}, 'remove \\d+ non-json')
  expect_is(temp, 'tbl_df')
  expect_equal(colnames(temp), c('timestamp', 'msg', 'y', 'log'))

  expect_warning({temp <- readlog(log_name = dirname(log_name))}, 'remove \\d+ non-json')
  expect_is(temp, 'tbl_df')
  expect_equal(colnames(temp), c('timestamp', 'msg', 'y', 'log', 'x'))

  unlink(dirname(log_name), TRUE)
})
