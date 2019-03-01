
.onLoad <- function(libname, pkgname) {
  setMaxBytes(100 * 1024, verbose = FALSE)
  setBackupN(5, verbose = FALSE)
}

.onAttach <- function(libname, pkgname) {
  pkgversion <- read.dcf(system.file("DESCRIPTION", package = pkgname),
                         fields = "Version")
  #TODO use better format
  msg <- sprintf(
    "max size: %s \nbackup: %s", .config$max_bytes, .config$backup_n
  )
  packageStartupMessage(msg)
}
