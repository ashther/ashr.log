
.onLoad <- function(libname, pkgname) {
  setMaxSize(100)
  .config$backup_n <- 5
  .config$log_level <- INFO
  .config$rotate <- 'size'
  .config$as_json <- TRUE
  .config$is_print <- FALSE
}

.onAttach <- function(libname, pkgname) {
  pkgversion <- read.dcf(
    system.file("DESCRIPTION", package = pkgname), fields = "Version"
  )
  msg <- sprintf(
    "=== WITH GREAT LOG, COMES WITH GREAT TRASH ==="
  )
  packageStartupMessage(msg)
}
