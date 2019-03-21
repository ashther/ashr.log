% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/config.R
\name{openlog}
\alias{openlog}
\title{set log file}
\usage{
openlog(log_name, verbose = TRUE)
}
\arguments{
\item{log_name}{log file name}

\item{verbose}{if print success message}
}
\description{
set log file name, if the file or directory doesn't exist, it will be created
 and open file connection
}
\examples{
\dontrun{
openlog('log/log')
}
}