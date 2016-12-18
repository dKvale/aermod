#' Control options
#'
#' Create a data frame of AERMOD control options.
#' @param title Title of modeling run. `TITLEONE` of AERMOD input file.
#' @param subtitle Subtitle of modeling run. `TITLETWO` of AERMOD input file.
#' @param model_opt Model run options. 
#'                  Enter multiple options as a vector: \code{c("DFAULT", "CONC")}. 
#'                  For a complete list of options, see 'MODELOPT' in the AERMOD reference guide at 
#'                  https://www3.epa.gov/ttn/scram/models/aermod/aermod_quick-reference-guide.pdf. 
#' @param avg_time Dispersion factor averaging times. Enter multiple options as a vector: \code{c("1", "24", "ANNUAL")}.
#' @param urban_opt Urban options for AERMOD run. Enter multiple options as a vector: \code{c("400000", "Minneapolis")}. 
#' @param pollutant_id Pollutant ID. Options: (1) "NO2"  (2) "SO2"  (3) "OTHER"... 
#' @param flagpole Flagpole options.
#' @param event_file File to write AERMOD 'events' to.
#' @param error_file File to write AERMOD errors to.
#' @param multi_year Options for performing multiple "year" AERMOD runs. 
#'                   Use for identifying the highest annual value across multiple years of met data.
#' @param debug_opt Automatic debugging options performed by AERMOD.
#' @keywords control options aermod input
#' @export
#' @examples
#' control_tbl(title = "My New Space Station")
#'
#

control_tbl <- function(title         = "Demo Energy Station",
                        subtitle      = "Default Run: Hour, Day, Annual",
                        model_opt     = c("DFAULT", "CONC"),
                        avg_time      = c("1", "24", "ANNUAL"),
                        urban_opt     = as.character(NA),
                        pollutant_id  = "OTHER",
                        flagpole      = as.character(NA),
                        event_file    = as.character(NA),
                        error_file    = as.character(NA),
                        multi_year    = as.character(NA),
                        debug_opt     = as.character(NA)
) {
 
  df <- tibble::tibble(title         = title, 
                       subtitle      = subtitle,
                       model_opt     = list(model_opt),
                       avg_time      = list(avg_time),
                       urban_opt     = list(urban_opt), 
                       pollutant_id  = pollutant_id,
                       flagpole      = flagpole,
                       event_file    = event_file,
                       error_file    = error_file,
                       multi_year    = multi_year,
                       debug_opt     = debug_opt
                       )
  
  return(df)
}


