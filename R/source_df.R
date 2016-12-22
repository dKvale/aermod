#' Source options
#'
#' Create a data frame of AERMOD source options.
#' @param source_id Emission source ID or name.
#' @param description Short description of emission source as a text string. 
#' @param elevation_m Source elevation in meters.
#' @param x_coord X coordinates of source. 
#' @param y_coord Y coordinates of source. 
#' @param type Type of source to be modeled in AERMOD. 
#'             Options: (1) "point", (2) "volume", or (3) "area".
#' @param emit_gs Emission rate for source in \code{grams/second}.
#' @param height_m Height of source in meters.
#' @param temp_k Exit temperature of air emissions in degrees \code{Kelvin}.           
#' @param velocity_ms Exit velocity of air emissions in in \code{m/s}. 
#' @param diameter_m Internal diameter of stack in \code{meters}.
#' @param downwash_file Path to downwash parameters for each emission source.
#' @param urban_pop Urban population for source. Requires a value of at least \code{100}.  
#'                  If blank or a value less than \code{100}, source is modeled with default \code{rural} dispersion coefficients. 
#' @param group_id Group IDs assigned to source. 
#'                 To assign a source to multiple groups use a vector: \code{c("ALL", "SV01", "SV01_to_SV03"}.
#'                 If blank, source is assigned to group "All".
#' @keywords source aermod input
#' @export
#' @examples
#' source_df(source_id     = c("SV01"),
#'           description   = c("boiler1"),
#'           elevation_m   = c(361.33),
#'           x_coord       = c(461874.9),
#'           y_coord       = c(4631367),
#'           type          = c("point"),
#'           emit_gs       = c(1),
#'           height_m      = c(10),
#'           temp_k        = c(600),
#'           velocity_ms   = c(30),
#'           diameter_m    = c(2),
#'           downwash_file = as.character(NA),
#'           urban_pop     = 60000,
#'           group_id      = as.character(NA))
# 
# 

source_df <- function(source_id     = as.character(NA),
                      description   = as.character(NA),
                      elevation_m   = as.numeric(NA),
                      x_coord       = as.numeric(NA),
                      y_coord       = as.numeric(NA),
                      type          = c("point"),
                      emit_gs       = as.numeric(NA),
                      height_m      = as.numeric(NA),
                      temp_k        = as.numeric(NA),
                      velocity_ms   = as.numeric(NA),
                      diameter_m    = as.numeric(NA),
                      downwash_file = as.character(NA),
                      urban_pop     = as.numeric(NA),
                      group_id      = as.character(NA)
) {
  df <- tibble::tibble(source_id     = source_id,
                       description   = description,
                       elevation_m   = elevation_m,
                       x_coord       = x_coord,
                       y_coord       = y_coord,
                       type          = type,
                       emit_gs       = emit_gs ,
                       height_m      = height_m,
                       temp_k        = temp_k,
                       velocity_ms   = velocity_ms,
                       diameter_m    = diameter_m,
                       downwash_file = downwash_file,
                       urban_pop     = urban_pop,
                       group_id      = ifelse(is.null(group_id) || is.na(group_id) || nchar(group_id) < 1, list("ALL"), list(group_id)))
  return(df)
 
}

##
  
  
 
