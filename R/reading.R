#' Reading nossaflex file names
#' @param path if null, reading from clipboard
#' @returns a vector of file names without extension
#' @export

reading_nossaflex <- function(path) {
  checkmate::assert_access(path, access = "r")

  stringi::stri_read_lines(con = path)
}
