context("test-ashr")

test_that("setting and getting", {
  setLogName('log/test')
  expect_equal(getLogName(), 'log/test')

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

  setLogName(log_name)
  printlog('this is a test plain message')
  temp <- readLines(getLogName())
  temp <- strsplit(temp, '\\s\\]\\s')[[1]][2]
  temp <- trimws(temp)
  expect_equal(temp, 'this is a test plain message')

  unlink(dirname(getLogName()), TRUE)
})

test_that('rotate log', {

  log_name <- file.path(tempdir(), 'log/log')

  setLogName(log_name)
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

  unlink(dirname(getLogName()), TRUE)
})
