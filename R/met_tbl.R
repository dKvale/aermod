#' Meteorology options
#'
#' Create an input table of AERMOD meteorology options.
#' @param surf_file Surface met data file location.
#' @param surf_site_info Optional surface met site description: \code{"Station_no Year City/Name"}.
#' @param prof_file Profile met data file location.
#' @param prof_site_info Optional profile met site description: \code{"Station_no Year City/Name"}.
#' @param base_elev_m Base elevation in meters above mean sea level of temperature profile data.
#' @param start_met Start date of meteorology data to be modeled. 
#'                  Enter as text: \code{"year month day hour"}
#'                  If blank, set to first entry of \code{surf_file}.
#' @param end_met End date of meteorology data to be modeled. 
#'                Enter as text : \code{"year month day hour"}
#'                If blank, set to last entry of \code{surf_file}.
#' @keywords meteorology weather surface aermod input
#' @export
#' @examples
#' met_tbl(surf_file      = "..\\MET\\RSTMPX5Y.SFC",
#'         surf_site_info = "14925 2006 ROCHESTER/MUNICIPAL_ARPT",
#'         prof_file      = "..\\MET\\RSTMPX5Y.PFL",
#'         prof_site_info = "94983 2006 ROCHESTER/MUNICIPAL_ARPT",
#'         base_elev_m    = 396.0,
#'         start_met      = "2009 1 1 1",
#'         end_met        = "2013 12 31 24")
# 
#

met_tbl <- function(surf_file      = "RSTMPx5Y.SFC",
                    surf_site_info = "14925 2006 ROCHESTER/MUNICIPAL_ARPT",
                    prof_file      = "RSTMPx5Y.PFL",
                    prof_site_info = "94983 2006",
                    base_elev_m    = 396.0,
                    start_met      = "2009 1 1 1",
                    end_met        = "2013 12 31 24"
) {
  
  if(nchar(start_met) < 1 & nchar(surf_file) > 1) {
    
    surf_met  <- readLines(surf_file)
    
    start_met <- strsplit(surf_met[grep("date", surf_met)], " ")[[1]][1]
  }
  
  if(nchar(start_met) < 1 & nchar(surf_file) > 1) {
    if(is.null(surf_met)) surf_met  <- readLines(surf_file)
    
    end_met <- strsplit(surf_met[max(grep("date", surf_met))], " ")[[1]][1]
  }
  
  df <- tibble::tibble(surf_file      = surf_file, 
                       surf_site_info = surf_site_info,
                       prof_file      = prof_file,
                       prof_site_info = prof_site_info,
                       base_elev_m    = base_elev_m,
                       start_met      = start_met,
                       end_met        = end_met)
  return(df)
}

##
