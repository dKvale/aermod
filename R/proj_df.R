#' Projection details
#'
#' Create a data frame of projection details for AERMOD receptors.
#' @param projection Name of coordinate projection used for receptors.
#' @param description Extended description of coordinate projection used for receptors.
#' @param zone UTM zone of coordinates.
#' @keywords projection options aermod output
#' @export
#' @examples
#' proj_df(projection  = "Coordinate System UTM")
#

proj_df <- function(projection  = "Coordinate System UTM",
                    description = "UTM: Universal Transverse Mercator",
                    datum       = "North American datum 1983",
                    dtmrgn      = "conus",
                    units       = "m",
                    zone        = "15", 
                    zone_inx     = "0"
) {
  
  df <- tibble::tibble(projection   = projection,
                       description  = description,
                       datum        = datum,
                       dtmrgn       = dtmrgn,
                       units        = units,
                       zone         = zone, 
                       zone_inx     = zone_inx)
  
  return(df)
}

##
