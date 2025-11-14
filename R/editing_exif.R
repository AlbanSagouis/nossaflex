#' Batch editing of exif data
#' @inheritParams renaming_nossaflex
#' @param metadata a data.frame as provided by \code{\link{parsing_nossaflex}},
#' \code{\link{parsing_custom}} or \code{\link{parsing_json}}.
#' @param extra_tags A Named vector of exif tags to modify.
#' @param overwrite_original logical, if TRUE, no copy is created by `exiftool`. See <https://exiftool.org/forum/index.php?topic=13191.msg71304#msg71304>
#' @param verbose passes `-v2` argument to `exiftool`
#' @details
#' Editing the `maker`, `model` and various `makerNotes` tags before or during darktable editing will most likely render the file unsuable for darktable since it uses these fields for parameterising the editing treatments.
#'
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

###################
# IDEA
# 1) taking advantage of exiftool ability to read tags in a csv directly
# import from CSV file https://exiftool.org/faq.html#Q26
# exiftool -csv="c:\Users\Phil\test.csv" "c:\Users\Phil\Images"
##############
# 2) taking advantage of exiftool batch execution abilities
editing_exif <- function(
  files,
  metadata,
  extra_tags,
  overwrite_original = FALSE,
  verbose = TRUE
) {
  base::stopifnot(
    "metadata must have as many rows as the length of files" = length(files) ==
      nrow(metadata)
  )

  # Deleting columns with unknown tags ----
  exif_tags <- c(
    # Shot
    "NO",
    "SS",
    #"ShutterSpeed", # not writable
    "ExposureTime",
    "A",
    "FNumber",
    "FL",
    "FocalLengthIn35mmFormat",
    #"FocalLength35efl", # not writable
    "EX",
    "Northing",
    "Easting",
    "Latitude",
    "Longitude",
    "Date_Time_Original",
    "Focus_Mode",
    #Camera
    "Camera_Brand",
    "Camera_Model",
    # Lens
    "Lens",
    "Lens_Brand",
    "Lens_Model",
    "Lens_Focal_Length",
    "Lens_Maximum_Aperture",
    # Nikon lens
    "Nikon:LensIDNumber",
    "Nikon:LensFStops",
    "Nikon:MinFocalLength",
    "Nikon:MaxFocalLength",
    "Nikon:MaxApertureAtMinFocal",
    "Nikon:MaxApertureAtMaxFocal",
    "Nikon:MCUVersion",
    "Nikon:LensType",
    #"Nikon:LensSpec", # composite
    #"Nikon:LensID", # composite
    # "Lens_ID", # composite
    # Film stock
    "Stock",
    "ISO",
    # Flash
    "Flash"
  )

  exif_names <- c(
    # Shot
    "ImageNumber",
    "ShutterSpeedValue",
    #"ShutterSpeed", # not writable
    "ExposureTime",
    "Aperture",
    "FNumber",
    "FocalLength",
    "FocalLengthIn35mmFormat",
    # "FocalLength35efl", # not writable
    "ExposureCompensation",
    "GPSLatitudeRef",
    "GPSLongitudeRef",
    "GPSLatitude",
    "GPSLongitude",
    "DateTimeOriginal",
    "FocusMode",
    # Camera
    "Make",
    "Model",
    # Lens
    "XMP:Lens",
    "LensMake",
    "LensModel",
    "MaxFocalLength",
    "MaxApertureValue",
    # Nikon lens
    "Nikon:LensIDNumber",
    "Nikon:LensFStops",
    "Nikon:MinFocalLength",
    "Nikon:MaxFocalLength",
    "Nikon:MaxApertureAtMinFocal",
    "Nikon:MaxApertureAtMaxFocal",
    "Nikon:MCUVersion",
    "Nikon:LensType",
    #"Nikon:LensSpec", # composite
    #"Nikon:LensID", # composite
    # "LensID", # composite
    # Film stock
    "ProfileName",
    "ISO",
    # Flash
    "Flash"
  )

  checkmate::assert_true(length(exif_tags) == length(exif_names))

  metadata[
    j = colnames(metadata)[!is.element(colnames(metadata), exif_tags)] := NULL
  ]

  # Converting values ----
  ## Excluding "auto" values ----
  variables <- c("SS", "FL", "A")
  if (any(metadata[j = ..variables] == "auto")) {
    message('"auto" values in SS, A and FL are turned into "".')
    metadata[
      j = (variables) := lapply(
        .SD,
        function(SDcolumn)
          base::replace(
            x = SDcolumn,
            list = grep(
              pattern = "auto",
              x = SDcolumn,
              fixed = TRUE
            ),
            values = ""
          )
      ),
      .SDcols = variables
    ]
  }

  ## ShutterSpeedValue ----
  metadata[
    j = "SS" := data.table::fcase(
      grepl("/", x = SS, fixed = TRUE),
      SS,
      grepl("s", x = SS, fixed = TRUE),
      sub("s", "", x = SS, fixed = TRUE),
      !is.na(as.numeric(SS)),
      as.character(1 / as.numeric(SS)),
      default = ""
    )
  ]
  ## ExposureTime & ShutterSpeed----
  metadata[
    j = "ExposureTime" := data.table::fcase(
      grepl("/", x = SS, fixed = TRUE),
      (1 / sub("1/", "", ExposureTime, fixed = TRUE) |> as.integer()) |>
        as.character(),
      grepl("s", x = SS, fixed = TRUE),
      sub("s", "", x = SS, fixed = TRUE),
      default = ""
    )
  ]

  # if aperture is auto and SS no, mode is S
  # if aperture and SS is auto, mode is P
  # if aperture is given and SS is auto, mode is A

  # Editing exif ----
  for (i in seq_along(files)) {
    arguments <- metadata[i, ]

    arguments <- stats::setNames(
      object = arguments,
      exif_names[match(
        x = names(arguments),
        table = exif_tags,
        nomatch = 0L
      )]
    )
    arguments <- sapply(
      seq_along(arguments),
      function(j)
        stringi::stri_join("-", names(arguments)[[j]], "=", arguments[[j]])
    )

    if (isTRUE(overwrite_original)) arguments <- c("-overwrite_original", arguments)

    if (isTRUE(verbose)) arguments <- c("-v2", arguments)

    print(arguments)

    exiftoolr::exif_call(path = files[[i]], args = arguments, quiet = FALSE)
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
#          "-LensModel=50mm 1.4D"),
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
