# Parsing
#' Parsing nossaflex names into a data.frame
#' @param filenames a character vector of file names following the structure:
#'     NO01_SS125_A5.6_FL35_EX0
#'     where
#'     NO = Number
#'     SS = Shutter Speed
#'     A = Aperture
#'     FL = Focal Length
#'     EX = Exposure (Over/under)
#' @returns a data.frame with standard columns from nossaflex convention.
#' @export
#' @examples
#' filenames = c("NO01_SS1s_A5.6_FL50_EX-2", "NO02_SS125_A5.6_FL50_EX+1")
#' parsing_nossaflex(filenames)

parsing_nossaflex <- function(filenames) {
  stringi::stri_split_regex(str = filenames,
                            pattern = "_") |>
    lapply(function(split_filename) {
      stringi::stri_replace_all_regex(split_filename,
                                      pattern = "[A-Z]",
                                      replacement = "") |>
        stats::setNames(stringi::stri_extract_all_regex(split_filename,
                                                        pattern = "[A-Z]{1,2}")) |>
        as.list()
    }) |>
    data.table::rbindlist(fill = TRUE)
}

#' Parsing `filenames` with a consistent structure different from the classic
#' `NOSSAFLEX` structure into a data.frame
#' @inheritParams parsing_nossaflex
#' @param format A character string. Used to extract the desired parts of the
#'    NOSSAFLEX file names. default is "NO%NO_SS%SS_A%A_FL%FL_EX%EX" which
#'    corresponds to the standard NOSSAFLEX structure as in
#'    `NO01_SS125_A5.6_FL35_EX+1`.
#' @details
#' Inspired by \code{\link[base]{strptime}}
#'
parsing_custom <- function(filenames, format = "NO%NO_SS%SS_A%A_FL%FL_EX%EX") {}
