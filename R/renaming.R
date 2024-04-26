#' Batch renaming picture files with the names provided by the `analog` app
#' @param filenames a character vector of file names provided by
#'     \code{\link{reading_nossaflex}}. With or without file extensions.
#' @param files a character vector of paths with complete file.names like what
#'     \code{\link{list.files}} when used with `full.names = TRUE` or
#'     \code{\link[tools:fileutils]{tools::list_files_with_exts}} gives
#' @param copy logical, FALSE by default.
#' @inherit base::files return
#' @description
#'    By default, the `filenames` provided by `analog` do not have file extensions.
#'    If the `filenames` do not have an extension, the extension of `files` is
#'    used. If `files` do not have an extension, an error is thrown.
#'
#'    By default, the `filenames` provided by `analog` do not have path.
#'    If the `filenames` do not have a complete path, the path of `files` is
#'    used. If `files` do not have an path, an error is as they cannot be found.
#' @export
#' @examples
#' \dontrun{
#' files <- list.files(path = "tests/testthat/testdata/",
#'                   pattern = "jpg",
#'                   full.names = TRUE)
#' files <- tools::list_files_with_exts(dir = "tests/testthat/testdata/",
#'                     exts = "jpg", full.names = TRUE)
#' filenames = c("NO01_SS1s_A5.6_FL35_EX-2", "NO02_SS125_A5.6_FL35_EX+1")
#' renaming_nossaflex(filenames, files, copy = TRUE)
#' }

renaming_nossaflex <- function(filenames, files, copy = FALSE) {
  base::stopifnot("files must have an extension" = grepl(
    x = files, pattern = "\\.[A-Za-z0-9]{1,3}$"))
  base::stopifnot("files must all have the same extension" =
                    length(
                      unique(grepl(x = files, pattern = "\\.[A-Za-z0-9]{1,3}$"))
                    ) == 1L)
  checkmate::assert_access(files,     access = "r")
  checkmate::assert_logical(copy, len = 1L, null.ok = FALSE)

  if (is.element(el = ".", set = dirname(filenames))) {
    filenames <- paste0(dirname(files), "/", filenames)
  }
  checkmate::assert_access(dirname(filenames), access = "w")

  if (all(grepl(x = filenames, pattern = "\\.[A-Za-z0-9]{1,3}$"))) {
    base::stopifnot("filenames must all have the same extension" =
                      length(
                        unique(grepl(x = files, pattern = "\\.[A-Za-z0-9]{1,3}$"))
                      ) == 1L)
    base::stopifnot("filenames must all have the same extension has files" =
                      unique(stringi::stri_extract_first_regex(
                        str = filenames,
                        pattern = "\\.[A-Za-z0-9]{1,3}$")) ==
                      unique(stringi::stri_extract_first_regex(
                        str = files,
                        pattern = "\\.[A-Za-z0-9]{1,3}$"))
    )
  } else { # if `filenames` don't have extensions
    file_extension <- unique(stringi::stri_extract_first_regex(
      str = files,
      pattern = "\\.[A-Za-z0-9]{1,3}$"))
    filenames <- paste0(filenames, file_extension)
  }

  if (isTRUE(copy)) {
    checkmate::assert_access(files, access = "w")

    base::file.copy(
      from = files,
      to = stringi::stri_replace_first_regex(
        str = files,
        pattern = "(?=\\.jpg$)",
        replacement = "_copy"),
      copy.date = TRUE, recursive = FALSE)
  }

  return(stats::setNames(
    base::file.rename(from = files, to = filenames),
    filenames))
}
