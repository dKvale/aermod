#' Meteorology options
#'
#' Create an input table of AERMOD meteorology options.
#' @param surf_file Surface met data file location.
#' @param surf_site_info Optional surface met site description: \code{"Station_no Year City/Name"}.
#' @param prof_file Profile met data file location.
#' @param prof_site_info Optional profile met site description: \code{"Station_no Year City/Name"}.
#' @param base_elev_m Base elevation above mean sea level of met site in meters.
#' @param start_met Start date of meteorology data to model. Enter as text: \code{"start_year start_month start_day start_hour"}
#' @param end_met End date of meteorology data to model. Enter as text : \code{"end_year end_month end_day end_hour"}
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

met_tbl <- function(surf_file      = "..\\MET\\RSTMPX5Y.SFC",
                    surf_site_info = "14925 2006 ROCHESTER/MUNICIPAL_ARPT",
                    prof_file      = "..\\MET\\RSTMPX5Y.PFL",
                    prof_site_info = "94983 2006",
                    base_elev      = 396.0,
                    start_met      = "2009 1 1 1",
                    end_met        = "2013 12 31 24"
) {
  
  df <- tibble::tibble(surf_file      = surf_file, 
                       surf_site_info = surf_site_info,
                       prof_file      = prof_file,
                       prof_site_info = prof_site_info,
                       base_elev      = base_elev,
                       start_met      = start_met,
                       end_met        = end_met)
  return(df)
}

##