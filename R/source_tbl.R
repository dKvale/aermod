#' Source options
#'
#' Create an input table of AERMOD source options.
#' @param id Source ID. For multiple sources use a vector: \code{c("SV01", "SV02")}.
#' @param elevation_m Source elevation in meters. For multiple sources use a vector: \code{c(333, 334)}.
#' @param x_coord X coordinates of source. For multiple sources use a vector: \code{c(461874.9, 471977.5)}.
#' @param y_coord Y coordinates of source. For multiple sources use a vector: \code{c(5631367, 5811367)}.
#' @param type Type of source to be modeled as in AERMOD. 
#'             Options: (1) "point", (2) "volume", or (3) "area". 
#'             For multiple sources use a vector: \code{c("point", "volumne")}.
#' @param emissions Emission rate for each source. For multiple sources use a vector: \code{c(1, 2)}.
#' @param emis_units Units of measurement for \code{emissions}.  
#'                   Options: (1) "g/s", (2) "lb/hr", or (3) "tons/yr". 
#'                   For multiple sources use a vector: \code{c("g/s", "lb/hr")}.
#' @param height_m Height of source in meters. For multiple sources use a vector: \code{c(10, 10)}   
#' @param temp_k Exit temperature of air emissions in Kelvin. For multiple sources use a vector: \code{c(610, 600)}.           
#' @param velocity_ms Exit velocity of air emissions in in m/s.  
#'                    Options: (1) "g/s", (2) "lb/hr", or (3) "tons/yr". 
#'                    For multiple sources use a vector: \code{c("g/s", "lb/hr")}.
#' @param diameter_m Source diameter in meters. For multiple sources use a vector: \code{c(1, 1.1)}.                    
#' @param group_id Group IDs assigned to source. 
#'                 Separate multiple groups with a comma: \code{"All, SV01, SV01_to_SV03"}.
#'                 If blank, source is assigned to group "All".
#' @keywords source aermod input
#' @export
#' @examples
#' source_tbl(id = "Stack_1", elevation = 300)
# 
# 

source_tbl <- function(id            = c("SV01", "SV02"),
                       description   = c("boiler1", "boiler2"),
                       elevation_m   = c(361.33, 361.33),
                       x_coord       = c(461874.9, 471977.5),
                       y_coord       = c(4631367, 4811367),
                       type          = c("point", "point"),
                       emissions     = c(1, 1),
                       emis_unit     = "g/s",
                       height_m      = c(10, 12),
                       temp_k        = c(600, 610),
                       velocity_ms   = c(30, 25),
                       diameter_m    = c(2, 2),
                       downwash_file = "",
                       group_id      = ""
) {
  df <- tibble::tibble(id            = id,
                       description   = description,
                       elevation_m   = elevation_m,
                       x_coord       = x_coord,
                       y_coord       = y_coord,
                       type          = type,
                       emissions     = emissions,
                       emis_unit     = "g/s",
                       height_m      = height_m,
                       temp_k        = temp_k,
                       velocity_ms   = velocity_ms,
                       diameter_m    = diameter_m,
                       downwash_file = downwash_file,
                       group_id      = ifelse(nchar(group_ids) < 1, "ALL", group_id))
  return(df)
 
}

##
  
  
 
