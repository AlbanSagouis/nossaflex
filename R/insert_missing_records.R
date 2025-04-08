#' Insert rows in metadata data.tables
#' @inheritParams editing_exif
#' @param i An integer or integer vectors of row numbers where new values should
#' be inserted.
#' @param value A data.frame or data.table with column names matching names in
#' `metadata` and values to insert, probably Roll_Name and Camera_Brand for
#' instance.
#' @param extrapolate_data Logical, FALSE by default. If TRUE, the function
#' computes average Date_Time_Original, Latitude and Longitude values. Hence
#' works only with JSON data.
#' @importFrom data.table :=
#' @export
#' @examples
#' \dontrun{
#' # NOSSAFLEX data
#' metadata <- reading_nossaflex("tests/testthat/testdata/nossaflex_filenames.txt") |>
#'     parsing_nossaflex()
#' value <- data.table::data.table(T = "est roll", NO = "02", SS = "125",
#'                                 A = "1.4", FL = "50", EX = "0")
#' insert_missing_records(metadata, 2, value)
#' # JSON data
#' metadata <- parsing_json("tests/testthat/testdata/nossaflex_Vito_70_test-week.txt")
#' value <- data.table::data.table(Roll_Name = rep("Test-week", 3),
#'                                 Roll_Number = rep("005", 3),
#'                                 Camera_Brand = rep("Voigtlaender", 3),
#'                                 Camera_Model = rep("Vito 70", 3), NO = 5:7,
#'                                 SS = rep("auto", 3), A = rep("auto", 3))
#' insert_missing_records(metadata = metadata, i = 5:7, value = value,
#'                        extrapolate_data = TRUE)
#' }


insert_missing_records <- function(metadata, i, value,
                                   extrapolate_data = FALSE) {
  # if value is a list, make it a data.table
  checkmate::assert_class(metadata, "data.table")
  checkmate::assert_integerish(i, lower = 1L)
  checkmate::assert_class(value, "data.table")

  value_row_counter <- 1L

  for (I in i) {
    if (isTRUE(extrapolate_data)) {
      mean_values <- data.table::data.table(
        # fails if we add at the end of the roll. Shouldn't.
        stats::setNames(
          metadata[
          i = c(I, I + 1L),
          j = paste(sep = " ",
                    mean(data.table::as.IDate(Date_Time_Original)),
                    mean(data.table::as.ITime(Date_Time_Original)))
        ], "Date_Time_Original"),
        metadata[
          i = c(I, I + 1L),
          j = lapply(.SD, function(x) mean(as.numeric(x))),
          .SDcols = c("Latitude", "Longitude")
        ],
        metadata[
          i = c(I, I + 1L),
          j = lapply(.SD, function(x) data.table::fifelse(
            test = identical(x[[1L]], x[[2L]]),
            yes = x[[1L]],
            no = NA_character_)),
          .SDcols = c("Northing", "Easting")
        ]
      )

      data.table::set(x = value,
                      i = value_row_counter,
                      j = c("Date_Time_Original",
                            "Latitude", "Longitude",
                            "Northing", "Easting"),
                      value = mean_values
      )
    }

    #   if (nrow(value) > 1L) warning("value is longer than i and only the first element was used")
    # if else (length(i) == nrow(value)) {

    metadata <- data.table::rbindlist(list(
      metadata[seq(from = 1L, to = I - 1L)],
      value[value_row_counter],
      metadata[seq(from = I, to = .N)]),
      fill = TRUE
    )
    value_row_counter <- value_row_counter + 1L
  }

  # Fixing NO
  metadata[j = NO := stringi::stri_pad_left(1L:.N, width = 2, pad = "0")]

  # if length(i) >1 but value == 1, recycle value. if value != 1 && value != i, stop
  # }
  return(metadata[])
}
