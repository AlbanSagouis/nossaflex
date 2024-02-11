#' Batch editing of exif data
#' @inheritParams renaming_nossaflex
#' @param metadata a data.frame as provided by \code{\link{parsing_nossaflex}}.
#' @importFrom data.table :=
#' @export
#' @examples
#' \dontrun{
#' files <- tools::list_files_with_exts(dir = "tests/testthat/testdata/",
#'                     exts = "jpg", full.names = TRUE)
#' metadata <- reading_nossaflex("tests/testthat/testdata/nossaflex_filenames.txt") |>
#'     parsing_nossaflex()
#' editing_exif(files, metadata)
#' }
#'
editing_exif <- function(files, metadata) {
  base::stopifnot("metadata must have as many rows as the length of files" =
                    length(files) == nrow(metadata))

  # Deleting columns with unknown tags ----
  exif_tags <- c("NO", "SS", "A", "FL", "EX")
  metadata[j = colnames(metadata)[!is.element(colnames(metadata), exif_tags)] := NULL]

  # Converting values ----
  ## ShutterSpeedValue
  metadata[j = "SS" := data.table::fifelse(grepl("s", x = metadata$SS, fixed = TRUE),
                                           sub("s", "", x = metadata$SS, fixed = FALSE),
                                           paste0("1/", metadata$SS))]
  ## ApertureValue

  # Editing exif
  for (i in seq_along(files)) {
    arguments <- metadata[i, ]

    data.table::setnames(x = arguments,
                         old = exif_tags,
                         new = c("ImageNumber", "ShutterSpeedValue",
                                 "ApertureValue", "FocalLength",
                                 "ExposureCompensation")[match(
                                   x = names(arguments),
                                   table = exif_tags,
                                   nomatch = 0L)]
    )
    arguments <- sapply(seq_along(arguments),
                        function(j) stringi::stri_join("-", names(arguments)[[j]],
                                                       "=", arguments[[j]]))

    exiftoolr::exif_call(path = files[[i]],
                         args = arguments,
                         quiet = FALSE)
  }
}

# args = c("-Photographer=Alban",
#          "-ImageNumber=1",
#          "-ShutterSpeedValue=1", # APEX unit?? ShutterSpeedValue
#          "-iso=200",
#          "-ApertureValue=1.4", #(displayed as an F number, but stored as an APEX value)
#          "-FocalLength=50",
#          "-ExposureTime=1/250",
#          "-ExposureCompensation=+1",# better than ExposureBiasValue ?
#          "-model=Nikon FA",
#          "-LensMake=Nikon",
#          "-LensModel=50mm 1.4d"),
# Camera ----
# Model
# Lens ----
# MaxApertureValue rational64u displayed as an F number, but stored as an APEX value)
#Artist 	string
# 0xa432 	LensInfo 	rational64u[4] 	ExifIFD 	(4 rational values giving focal and aperture ranges, called LensSpecification by the EXIF spec.)
# 0xa433 	LensMake 	string 	ExifIFD
# 0xa434 	LensModel 	string 	ExifIFD
# 0xa435 	LensSerialNumber 	string 	ExifIFD
# Lens 	string/
# FocalLength
# 0x0083 	LensType 	int8u 	Bit 0 = MF
# Bit 1 = D
# Bit 2 = G
# Bit 3 = VR
# Bit 4 = 1
# Bit 5 = FT-1
# Bit 6 = E
# Bit 7 = AF-P
