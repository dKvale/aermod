#' Create AERMOD input tables
#'
#' Create default AERMOD input data frames: 
#' (1) Control options 
#' (2) Source parameters
#' (3) Receptor parameters
#' (4) Meteorology options
#' (5) Output options
#' @param input_df Name for the joined input data frame added to Global Environment. 
#'                 Default is "aermod_inp". Ignored if \code{as_one_df} is \code{FALSE}. 
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
                       add_to_envir  = FALSE,
                       control       = "control",
                       sources       = "sources",
                       receptors     = "receptors",
                       met           = "met",
                       out           = "out") {
  
  # Create tables
  co   <- control_df()
  so   <- source_df()
  re   <- receptor_df()
  me   <- met_df()
  ou   <- out_df()
  
  # Output
  if(as_one_df) {
  
    aermod_inp <- tibble::as_data_frame(cbind(co, so, re, me, ou)) 
    
    if(add_to_envir) assign(input_df, aermod_inp, pos = 1)
    
  } else {
    
    if(add_to_envir) {
      
    # 1 - CONTROL OPTIONS
    assign(control, co, pos = 1)
    
    # 2 - SOURCES
    assign(sources, so, pos = 1)
    
    # 3- RECEPTORS
    assign(receptors, re, pos = 1)
    
    # 4- METEOROLOGY OPTIONS
    assign(met, me, pos = 1)
    
    # 5- OUTPUT OPTIONS
    assign(out, ou, pos = 1)
    }
    
    aermod_inp <- list(control     = co,
                       sources     = so,
                       receptors   = re,
                       met         = me,
                       out         = ou)
    
    names(aermod_inp) <- c(control, sources, receptors, met, out)
  }
  
  return(aermod_inp)
  
}
