#' Create AERMOD input tables
#'
#' Create default AERMOD input data frames: 
#' (1) control 
#' (2) sources
#' (3) meteorology
#' (4) output_options
#' @param control Name for control options data frame. Default is "control".
#' @param sources Name for emission source data frame. Default is "sources".
#' @param meteorology Name for meteorology options data frame. Default is "met".
#' @param output Name for output options data frame. Default is "output".
#' @keywords aermod input new start tables
#' @export
#' @examples
#' 
#' 
#' new_aermod()
# 
#


new_aermod <- function(control     = "control",
                       sources     = "sources",
                       meteorology = "met",
                       output      = "out") {
  
  # 1 - CONTROL OPTIONS
  assign(control, control_tbl(), pos = 1)

  # 2 - SOURCES
  assign(sources, source_tbl())
  
  # 3- METEOROLOGY OPTIONS
  assign(meteorology, met_tbl())

  # 4 - OUTPUT OPTIONS
  assign(output, out_tbl())
  
}