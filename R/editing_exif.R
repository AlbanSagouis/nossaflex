#' Batch editing of exif data
#' @inheritParams renaming_nossaflex
#' @param metadata a data.frame as provided by \code{\link{parsing_nossaflex}},
#' \code{\link{parsing_custom}} or \code{\link{parsing_json}}.
#' @param extra_tags A Named vector of exif tags to modify.
#' @importFrom data.table :=
#' @export
#' @examples
#' \dontrun{
#' files <- tools::list_files_with_exts(dir = "tests/testthat/testdata/",
#'                     exts = "jpg", full.names = TRUE)
#' metadata <- reading_nossaflex("tests/testthat/testdata/nossaflex_filenames.txt") |>
#'     parsing_nossaflex()
#' editing_exif(files, metadata)
#'
#' metadata <- data.table::data.table(NO = c(1, 2), SS = c(2s, 4000), A = c(1.4, 2.8),
#'  FL = c(50, 50), EX = c("+2", "-1"),
#'  Northing = c("N", "N"), Easting = c("E", "E"),
#'  Latitude = c(54.321, 54.321), Longitude = c(12.345, 12.345),
#'  Date_Time_Original = c("2024-03-19 21:40:40 +0000", "2024-04-20 12:20:10 +0000"),
#'  Camera_Brand = c("Nikon", "Nikon"), Camera_Model = c("FA", "FA"),
#'  Lens_Brand = c("Nikon", "Nikon"),
#'  Lens_Model = c("Nikkor AF 50mm d f/1.4", "Nikkor AF 50mm d f/1.4"),
#'  Lens_Focal_Length = c(50, 50), Lens_Maximum_Aperture = c(1.4, 1.4)
#' )
#' }
#'
#'
#'
editing_exif <- function(files, metadata, extra_tags) {
  base::stopifnot("metadata must have as many rows as the length of files" =
                    length(files) == nrow(metadata))

  # Deleting columns with unknown tags ----
  exif_tags <- c(
    # Shot
    "NO", "SS", "A",
    "FL", "EX",
    "Northing", "Easting",
    "Latitude", "Longitude",
    "Date_Time_Original",
    #Camera
    "Camera_Brand", "Camera_Model",
    # Lens
    "Lens_Brand", "Lens_Model", "Lens_Focal_Length", "Lens_Maximum_Aperture"
  )

  exif_names <- c(
    # Shot
    "ImageNumber", "ShutterSpeedValue", "FNumber",
    "FocalLength", "ExposureCompensation",
    "GPSLatitudeRef", "GPSLongitudeRef",
    "GPSLatitude", "GPSLongitude",
    "DateTimeOriginal",
    # Camera
    "Make","Model",
    # Lens
    "LensMake", "LensModel", "MaxFocalLength", "MaxApertureValue")
  metadata[, colnames(metadata)[!is.element(colnames(metadata), exif_tags)] := NULL]

  # Converting values ----
  ## Excluding "auto" values ----
  variables <- c("SS","FL","A")
  if (any(metadata[j = ..variables] == "auto")) {
    message('"auto" values in SS, A and FL are turned into "".')
    metadata[j = (variables) := lapply(.SD,
                                       function(SDcolumn) base::replace(
                                         x = SDcolumn,
                                         list = grep(
                                           pattern = "auto",
                                           x = SDcolumn,
                                           fixed = TRUE
                                         ),
                                         values = ""
                                       )),
             .SDcols = variables]

  }

  ## ShutterSpeedValue ----
  metadata[j = "SS" := data.table::fcase(
    grepl("s", x = SS, fixed = TRUE), sub("s", "", x = SS, fixed = TRUE),
    !is.na(as.numeric(SS)), as.character(1 / as.numeric(SS)),
    default = ""
  )]



  # if aperture is auto and SS no, mode is S
  # if aperture and SS is auto, mode is P
  # if aperture is given and SS is auto, mode is A




  # Editing exif ----
  for (i in seq_along(files)) {
    arguments <- metadata[i, ]

    arguments <- stats::setNames(object = arguments,
                                 exif_names[match(
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
#          "-FNumber=1.4",
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
# FNumber
#Artist or Photographer	string
# 0xa432 	LensInfo 	rational64u[4] 	ExifIFD 	(4 rational values giving focal and aperture ranges, called LensSpecification by the EXIF spec.)
# Value 1 : = Minimum focal length (unit: mm)
# Value 2 : = Maximum focal length (unit: mm)
# Value 3 : = Minimum F number in the minimum focal length
# Value 4 : = Minimum F number in the maximum focal length
#
# So, just making up numbers, if you set
# exiftool -LensInfo="5 10 100 200" FILE.JPG
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

# CreateDate
# GPSLatitude 10.23456
# GPSLatitudeRef "N"
# GPSLongitude 10.23456
# GPSLongitudeRef "E"

# Film roll possible exif tags: ImageDescription, ImageHistory, UserComment,
# Title, CameraFirmware, ProfileName, CameraLabel, DocumentName

# 0xa300 	FileSource 	undef 	ExifIFD 	1 = Film Scanner
# 2 = Reflection Print Scanner
# 3 = Digital Camera
# "\x03\x00\x00\x00" = Sigma Digital Camera
