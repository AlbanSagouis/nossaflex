% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/renaming.R
\name{renaming_nossaflex}
\alias{renaming_nossaflex}
\title{Batch renaming picture files with the names provided by the `analog` app}
\usage{
renaming_nossaflex(filenames, files, copy = FALSE)
}
\arguments{
\item{filenames}{a character vector of file names provided by
\code{\link{reading_nossaflex}}. With or without file extensions.}

\item{files}{a character vector of paths with complete file.names like what
\code{\link{list.files}} when used with `full.names = TRUE` or
\code{\link[tools:fileutils]{tools::list_files_with_exts}} gives}

\item{copy}{logical, FALSE by default.}
}
\value{
These functions return a logical vector indicating which
  operation succeeded for each of the files attempted.  Using a missing
  value for a file or path name will always be regarded as a failure.

  If \code{showWarnings = TRUE}, \code{file.create} will give a warning
  for an unexpected failure.
}
\description{
By default, the `filenames` provided by `analog` do not have file extensions.
   If the `filenames` do not have an extension, the extension of `files` is
   used. If `files` do not have an extension, an error is thrown.

   By default, the `filenames` provided by `analog` do not have path.
   If the `filenames` do not have a complete path, the path of `files` is
   used. If `files` do not have an path, an error is as they cannot be found.
}
\examples{
\dontrun{
files <- list.files(path = "tests/testthat/testdata/",
                  pattern = "jpg",
                  full.names = TRUE)
files <- tools::list_files_with_exts(dir = "tests/testthat/testdata/",
                    exts = "jpg", full.names = TRUE)
filenames = c("NO01_SS1s_A5.6_FL35_EX-2", "NO02_SS125_A5.6_FL35_EX+1")
renaming_nossaflex(filenames, files, copy = TRUE)
}
}
