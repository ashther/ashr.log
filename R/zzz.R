
.onLoad <- function(libname, pkgname) {
  setMaxSize(100, verbose = FALSE)
  setBackupN(5, verbose = FALSE)
}

.onAttach <- function(libname, pkgname) {
  pkgversion <- read.dcf(
    system.file("DESCRIPTION", package = pkgname), fields = "Version"
  )
  msg <- sprintf(
    "max size: %s \nbackup: %s", getMaxSize(), .config$backup_n
  )
  packageStartupMessage(msg)
}
