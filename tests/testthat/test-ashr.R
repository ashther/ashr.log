context("test-ashr")

test_that("setting and getting", {

  log_name <- file.path(tempdir(), 'log/log')
  openlog(log_name)
  expect_equal(getLogName(), log_name)

  expect_true(ashr.log:::isOpenCon())
  # can't test on linux
  # file.remove(log_name)
  # expect_true(file.exists(log_name))

  closelog()
  unlink(dirname(getLogName()), TRUE)

  setBackupN(6)
  expect_equal(getBackupN(), 6)

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
})

test_that('print to log file', {

  log_name <- file.path(tempdir(), 'log/log')

  openlog(log_name)
  printlog('this is a test plain message')
  temp <- readLines(getLogName())
  temp <- strsplit(temp, '\\s\\]\\s')[[1]][2]
  temp <- trimws(temp)
  expect_equal(temp, 'this is a test plain message')

  closelog()
  unlink(dirname(getLogName()), TRUE)
})

test_that('rotate log', {

  log_name <- file.path(tempdir(), 'log/log')

  openlog(log_name)
  setMaxSize(1, units = 'Kb')
  setBackupN(3)
  rotatelog(paste(rep(letters, 10), collapse = ''))
  temp <- list.files(dirname(getLogName()))
  expect_equal(length(temp), 1)
  expect_equal(temp, 'log')

  invisible(lapply(1:5, rotatelog, ... = paste(rep(letters, 10), collapse = '')))
  temp <- list.files(dirname(getLogName()))
  expect_equal(length(temp), 2)
  expect_equal(temp, c('log', 'log.1'))

  invisible(lapply(1:5, rotatelog, ... = paste(rep(letters, 10), collapse = '')))
  temp <- list.files(dirname(getLogName()))
  expect_equal(length(temp), 3)
  expect_equal(temp, c('log', 'log.1', 'log.2'))

  invisible(lapply(1:10, rotatelog, ... = paste(rep(letters, 10), collapse = '')))
  temp <- list.files(dirname(getLogName()))
  expect_equal(length(temp), 4)
  expect_equal(temp, c('log', 'log.1', 'log.2', 'log.3'))

  invisible(lapply(1:5, rotatelog, ... = paste(rep(letters, 10), collapse = '')))
  temp <- list.files(dirname(getLogName()))
  expect_equal(length(temp), 4)
  expect_equal(temp, c('log', 'log.1', 'log.2', 'log.3'))

  closelog()
  unlink(dirname(getLogName()), TRUE)
})

test_that('daily log', {
  msg_test <- 'write message to an old log file'

  log_name <- file.path(tempdir(), 'log/log')
  openlog(log_name)

  dailylog(msg_test)
  temp <- readLines(getLogName())
  temp <- strsplit(temp, '\\s\\]\\s')[[1]][2]
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

    dailylog(msg_test)
    temp <- list.files(dirname(getLogName()))
    expect_equal(temp, c('log', 'log.1986-06-22'))
    file.remove(file.path(dirname(log_name), 'log.1986-06-22'))
  }

  file.create(file.path(dirname(log_name), 'log.2000-01-01'))
  file.create(file.path(dirname(log_name), 'log.2000-01-02'))
  file.create(file.path(dirname(log_name), 'log.2000-01-03'))
  file.create(file.path(dirname(log_name), 'log.2000-01-04'))
  setBackupN(2)
  dailylog(msg_test)
  temp <- list.files(dirname(getLogName()))
  expect_equal(temp, c('log', 'log.2000-01-03', 'log.2000-01-04'))

  closelog()
  unlink(dirname(getLogName()), TRUE)
})

test_that('read log', {

  log_name <- file.path(tempdir(), 'log/log')
  openlog(log_name)
  setMaxSize(10, 'b')
  setBackupN(5)

  rotatelog(jsonlite::toJSON(list(x = paste0(letters, collapse = '')), auto_unbox = TRUE))
  invisible(lapply(1:10, function(x) {
    rotatelog(jsonlite::toJSON(list(y = paste0(1:10, collapse = '')), auto_unbox = TRUE))
  }))
  temp <- readlog(as_json = FALSE)
  expect_is(temp, 'tbl_df')
  expect_equal(colnames(temp), c('timestamp', 'log'))
  temp <- readlog(as_json = TRUE)
  expect_is(temp, 'tbl_df')
  expect_equal(colnames(temp), c('timestamp', 'y', 'x'))

  closelog()
  unlink(dirname(getLogName()), TRUE)
})
