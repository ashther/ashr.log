
.onLoad <- function(libname, pkgname) {
  setMaxBytes(100 * 1024, verbose = FALSE)
  setBackupN(5, verbose = FALSE)
}

.onAttach <- function(libname, pkgname) {
  pkgversion <- read.dcf(system.file("DESCRIPTION", package = pkgname),
                         fields = "Version")
  msg <- sprintf(
    "max size: %s \n backup: %s", .config$max_bytes, .config$backup_n
  )
  packageStartupMessage(msg)
}
