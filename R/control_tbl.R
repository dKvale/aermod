#' Control options
#'
#' Create an input table of AERMOD control options.
#' @param title_one Title.
#' @param title_two Subtitle.
#' @param model_opt Model run options. 
#'                  Separate multiple options with a comma: \code{"DFAULT, CONC"}. 
#'                  For a complete list of options, see 'MODELOPT' in the AERMOD reference guide at 
#'                  https://www3.epa.gov/ttn/scram/models/aermod/aermod_quick-reference-guide.pdf. 
#' @param avg_time Dispersion factor averaging times. Separate multiple options with a comma: \code{"1, 24, ANNUAL"}.
#' @param urban_opt Urban options for AERMOD run. Separate multiple options with a comma: \code{"500000 Minneapolis"}. 
#' @param pollutant_id Pollutant ID. Options: (1) "NO2" (2)... 
#' @param flagpole Flagpole options.
#' @param event_file File to write AERMOD 'events' to.
#' @param error_file File to write AERMOD errors to.
#' @param multi_year Options for performing multiple "year" AERMOD runs. 
#'                   Used for identifying the highest annual value across multiple years of met data.
#' @param debug_opt Automatic debugging options performed by AERMOD.
#' @keywords control options aermod input
#' @export
#' @examples
#' control_tbl(title_one = "Demo Energy Station")
#'
#

control_tbl <- function(title_one     = "Demo Energy Station",
                        title_two     = "Default Run: Hour, Day, Annual",
                        model_opt     = "DFAULT, CONC",
                        avg_time      = "1, 24, ANNUAL",
                        urban_opt     = "",
                        pollutant_id  = "OTHER",
                        flagpole      = "",
                        event_file    = "",
                        error_file    = "",
                        multi_year    = "",
                        debug_opt     = ""
) {
 
  df <- tibble::tibble(title_one     = title_one, 
                       title_two     = title_two,
                       model_opt     = model_opt,
                       avg_time      = avg_time,
                       urban_opt     = urban_opt, 
                       pollutant_id  = pollutant_id,
                       flagpole      = flagpole,
                       event_file    = event_file,
                       error_file    = error_file,
                       multi_year    = multi_year,
                       debug_opt     = debug_opt
                       )
  
  return(df)
}


