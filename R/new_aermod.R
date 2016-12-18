#' Create AERMOD input tables
#'
#' Create default AERMOD input data frames: 
#' (1) Control options 
#' (2) Source parameters
#' (3) Receptor parameters
#' (4) Meteorology options
#' (5) Output options
#' @param as_one_df \code{TRUE} or \code{FALSE}. 
#'                    Return all inputs in a single wide data frame. 
#'                    If \code{FALSE}, return 5 data frames in a list: 
#'                         (1) control
#'                         (2) sources
#'                         (3) receptors
#'                         (4) meteorology
#'                         (5) output
#' @param add_to_envir \code{TRUE} or \code{FALSE}. 
#'                     Exports tables directly to the Global Environment.
#' @param input_df Name for the joined input data frame added to Global Environment. 
#'                 Default is "aermod_inp". Ignored if \code{as_one_df} is \code{FALSE}. 
#' @param control Name for control options data frame added to Global Environment. 
#'                Default is "control". Ignored if \code{as_one_df} is \code{TRUE}. 
#' @param sources Name for emission source data frame added to Global Environment. 
#'                Default is "sources". Ignored if \code{as_one_df} is \code{TRUE}.
#' @param receptors Name for receptor data frame added to Global Environment. 
#'                  Default is "receptors". Ignored if \code{as_one_df} is \code{TRUE}.
#' @param met Name for meteorology options data frame added to Global Environment. 
#'            Default is "met". Ignored if \code{as_one_df} is \code{TRUE}.
#' @param out Name for output options data frame added to Global Environment. 
#'            Default is "output". Ignored if \code{as_one_df} is \code{TRUE}.
#' @keywords aermod input new start tables
#' @export
#' @examples
#' input_df <- new_aermod(as_one_df = TRUE)
#' 
#' new_aermod(as_one_df = FALSE, add_to_envir = TRUE, met = "rochester_met")
#' 
#' input_list <- new_aermod(as_one_df = FALSE, met = "rochester_met")
# 
#

new_aermod <- function(input_df      = "aermod_inp",
                       as_one_df     = TRUE,
                       control       = "control",
                       sources       = "sources",
                       receptors     = "receptors",
                       met           = "met",
                       out           = "out",
                       add_to_envir  = FALSE) {
  
  if(as_one_df) {
  
    aermod_inp <- cbind(control_tbl(), 
                        source_tbl(), 
                        receptor_tbl(), 
                        met_tbl(), 
                        out_tbl())
    
    if(add_to_envir) assign(input_df, aermod_inp, pos = 1)
    
  } else {
    
    if(add_to_envir) {
      
    # 1 - CONTROL OPTIONS
    assign(control, control_tbl(), pos = 1)
    
    # 2 - SOURCES
    assign(sources, source_tbl(), pos = 1)
    
    # 3- RECEPTORS
    assign(receptors, receptor_tbl(), pos = 1)
    
    # 4- METEOROLOGY OPTIONS
    assign(met, met_tbl(), pos = 1)
    
    # 5- OUTPUT OPTIONS
    assign(out, out_tbl(), pos = 1)
    }
    
    aermod_inp <- list(control     = control_tbl(),
                       sources     = source_tbl(),
                       receptors   = receptor_tbl(),
                       met         = met_tbl(),
                       out         = out_tbl())
    
    names(aermod_inp) <- c(control, sources, receptors, met, out)
  }
  
  return(aermod_inp)
  
}
