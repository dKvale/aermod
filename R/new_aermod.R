#' Create AERMOD input tables
#'
#' Create default AERMOD input data frames: 
#' (1) Control options 
#' (2) Source parameters
#' (3) Meteorology data
#' (4) Output options
#' @param control Name for control options data frame. Default is "control".
#' @param sources Name for emission source data frame. Default is "sources".
#' @param met Name for meteorology options data frame. Default is "met".
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
                       met         = "met",
                       output      = "out") {
  
  # 1 - CONTROL OPTIONS
  assign(control, control_tbl(), pos = 1)

  # 2 - SOURCES
  assign(sources, source_tbl())
  
  # 3- METEOROLOGY OPTIONS
  assign(met, met_tbl())

  # 4 - OUTPUT OPTIONS
  assign(output, out_tbl())
  
}
