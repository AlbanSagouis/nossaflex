#' Nikon AF 28-105 D
#' @details
#'    To help `exiftoolr` to guess the lend ID, we can provide a lens of favourite
#'    lenses in a config file:
#'    # ~/.ExifTool_config
#'    # A special 'Lenses' list can be defined to give priority to specific lenses
#'    # in the logic to determine a lens model for the Composite:LensID tag
#'    @Image::ExifTool::UserDefined::Lenses = (
#'        'AF Zoom-Nikkor 28-105mm f/3.5-4.5D IF',
#'        'AF Nikkor 50mm f/1.4D',
#'        'AF Nikkor 85mm f/1.8',
#'        'AF Nikkor 24mm f/2.8D'
#'    );
#'
#'   Additional lenses can be added here, the information listed here can be
#'   found in the exif data of pictures taken with the additional lenses.
#'
#' @seealso See <https://exiftool.org/TagNames/Nikon.html#LensID> for the list
#'    of standard lens names
#' @rdname adding_lens_info
#' @export
add_lens_data_nikon_AF_28_105_D <- function() {
  data.frame(
    `Nikon:LensIDNumber` = 95,
    `Nikon:LensFStops` = 5.333,
    `Nikon:MinFocalLength` = 28.28,
    `Nikon:MaxFocalLength` = 106.8,
    `Nikon:MaxApertureAtMinFocal` = 3.564,
    `Nikon:MaxApertureAtMaxFocal` = 4.49,
    `Nikon:MCUVersion` = 101,
    `Nikon:LensType` = 2,
    `Nikon:LensSpec` = "28 105 3.6 4.5 2",
    `Nikon:LensID` = "AF Zoom-Nikkor 28-105mm f/3.5-4.5D IF",
  check.names = FALSE
  )
}

#' Nikon AF 24 D
#' @rdname adding_lens_info
#'
#' @export
add_lens_data_nikon_AF_24_D <- function() {
  data.frame(
    `Nikon:LensIDNumber` = 54,
    `Nikon:LensFStops` = 6,
    `Nikon:MinFocalLength` = 24.48,
    `Nikon:MaxFocalLength` = 24.48,
    `Nikon:MaxApertureAtMinFocal` = 2.828,
    `Nikon:MaxApertureAtMaxFocal` = 2.828,
    `Nikon:MCUVersion` = 52,
    `Nikon:LensType` = 2,
    `Nikon:LensSpec` = "24 24 2.8 2.8 2",
    `Nikon:LensID` = "AF Nikkor 24mm f/2.8D",
    check.names = FALSE
  )
}

#' Nikon AF 50 D
#' @rdname adding_lens_info
#' @export
add_lens_data_nikon_AF_50_D <- function() {
  data.frame(
    `Nikon:LensIDNumber` = 67,
    `Nikon:LensFStops` = 7,
    `Nikon:MinFocalLength` = 50.4,
    `Nikon:MaxFocalLength` = 50.4,
    `Nikon:MaxApertureAtMinFocal` = 1.414,
    `Nikon:MaxApertureAtMaxFocal` = 1.414,
    `Nikon:MCUVersion` = 70,
    `Nikon:LensType` = 2,
    `Nikon:LensSpec` = "25 50 1.4 1.4 2",
    `Nikon:LensID` = "AF Nikkor 50mm f/1.4D",
    check.names = FALSE
  )
}

#' Nikon AF 85
#' @rdname adding_lens_info
#' @export
add_lens_data_nikon_AF_85 <- function() {
  data.frame(
    `Nikon:LensIDNumber` = 21,
    `Nikon:LensFStops` = 6.333,
    `Nikon:MinFocalLength` = 84.76,
    `Nikon:MaxFocalLength` = 84.76,
    `Nikon:MaxApertureAtMinFocal` = 1.782,
    `Nikon:MaxApertureAtMaxFocal` = 1.782,
    `Nikon:MCUVersion` = 12,
    `Nikon:LensType` = 0,
    `Nikon:LensSpec` = "85 85 1.8 1.8",
    `Nikon:LensID` = "AF Nikkor 85mm f/1.8",
    check.names = FALSE
  )
}

#' Nikon AF 105 D
#' @rdname adding_lens_info
#' @export
add_lens_data_nikon_AF_105_D <- function() {
  data.frame(
    `Nikon:LensIDNumber` = 50,
    `Nikon:LensFStops` = 7,
    `Nikon:MinFocalLength` = 106.8,
    `Nikon:MaxFocalLength` = 106.8,
    `Nikon:MaxApertureAtMinFocal` = 2.828,
    `Nikon:MaxApertureAtMaxFocal` = 2.828,
    `Nikon:MCUVersion` = 53,
    `Nikon:LensType` = 2,
    `Nikon:LensSpec` = "105 105 2.8 2.8 2",
    `Nikon:LensID` = "AF Micro-Nikkor 105mm f/2.8D",
    check.names = FALSE
  )
}
