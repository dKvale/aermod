#' Projection options
#'
#' Create an input table of AERMOD projection options.
#' @param projection Coordinate projection for AERMOD run.
#' @param zone UTM zone of coordinates.
#' @keywords projection options aermod input
#' @export
#' @examples
#' proj_tbl(projection  = "Coordinate System UTM")
#

proj_tbl <- function(projection  = "Coordinate System UTM",
                     description = "UTM: Universal Transverse Mercator",
                     datum       = "North American datum 1983",
                     dtmrgn      = "conus",
                     units       = "m",
                     zone        = "15", 
                     zoneinx     = "0"
) {
  
  df <- tibble::tibble(projection   = projection,
                       description  = description,
                       datum        = datum,
                       dtmrgn       = dtmrgn,
                       units        = units,
                       zone         = zone, 
                       zoneinx      = zoneinx)
  
  return(df)
}

##