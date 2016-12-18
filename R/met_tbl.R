#' Meteorology options
#'
#' Create a data frame of AERMOD meteorology options.
#' @param surf_file Surface met data file location.
#' @param surf_site_info Optional surface met site description: \code{"Station_no Year City/Name"}.
#' @param prof_file Profile met data file location.
#' @param upper_air_info Optional upper air site description: \code{"Station_no Year City/Name"}.
#' @param base_elev_m Base elevation in meters above mean sea level of temperature profile data.
#' @param start_date Start date of meteorology data to be modeled. 
#'                   Enter as text: \code{"year month day hour"}
#'                   If blank, set to the first entry of \code{surf_file}.
#' @param end_date End date of meteorology data to be modeled. 
#'                 Enter as text : \code{"year month day hour"}
#'                 If blank, set to the last entry of \code{surf_file}.
#' @keywords meteorology weather surface aermod input
#' @export
#' @examples
#' met_tbl(surf_file      = "..\\MET\\RSTMPX5Y.SFC",
#'         surf_site_info = "14925 2006 ROCHESTER/MUNICIPAL_ARPT",
#'         prof_file      = "..\\MET\\RSTMPX5Y.PFL",
#'         upper_air_info = "94983 2006 ROCHESTER/MUNICIPAL_ARPT",
#'         base_elev_m    = 396.0,
#'         start_date     = "2009 1 1 1",
#'         end_date       = "2013 12 31 24")
# 
#

met_tbl <- function(surf_file      = as.character(NA),
                    surf_site_info = as.character(NA),
                    prof_file      = as.character(NA),
                    upper_air_info = as.character(NA),
                    base_elev_m    = as.numeric(NA),
                    start_date     = as.character(NA),
                    end_date       = as.character(NA)
) {
  
  # Find start_date if only end_date provided
  if((!is.null(end_date) & !is.na(end_date) & nchar(end_date) > 1) & 
     (is.null(start_date) || is.na(start_date) || nchar(start_date) < 1)) {
    
    surf_met   <- try(readLines(surf_file), silent = TRUE)
    
    #surf_met   <- readLines("M:\\KME Files\\RASSUpdate2015\\AERSCREEN vs. AERMOD\\AERSCREEN (new version)\\aerscreen_02_01.sfc")
    
    if(class(surf_met) != "try-error") {
      
      # Remove blank rows
      surf_met   <- surf_met[grepl("[1:9]", surf_met)]
      
      # First date
      start_date <- substring(surf_met[[2]], 1, 20)
      
      start_date <- gsub("\\s+", " ", start_date)
      
      start_date <- trimws(start_date)
      
      start_date <- strsplit(start_date, " ")[[1]][c(1:3,5)]
      
      start_date[1] <- ifelse(as.numeric(start_date[1]) <= format(Sys.Date(), "%y"), 
                              paste0("20", start_date[1]),  paste0("19", start_date[1]))
      
      start_date <- paste(start_date, collapse = " ")
      
    }
  }
      
  # Find end_date if only start_date provided
  if((!is.null(start_date) & !is.na(start_date) & nchar(start_date) > 1) & 
     (is.null(end_date) || is.na(end_date) || nchar(end_date) < 1)) {
    
    if(is.null(surf_met)) {
      surf_met   <- try(readLines(surf_file), silent = TRUE)
    }
    
    if(class(surf_met) != "try-error") {
      
      # Remove blank rows
      surf_met <- surf_met[grepl("[1:9]", surf_met)]
      
      # First date
      end_date <- substring(surf_met[[length(surf_met)]], 1, 20)
      
      end_date <- gsub("\\s+", " ", end_date)
      
      end_date <- trimws(end_date)
      
      end_date <- strsplit(end_date, " ")[[1]][c(1:3,5)]
      
      end_date[1] <- ifelse(as.numeric(end_date[1]) <= format(Sys.Date(), "%y"), 
                              paste0("20", end_date[1]),  paste0("19", end_date[1]))
      
      end_date <- paste(end_date, collapse = " ")
    }
  }
  
  df <- tibble::tibble(surf_file      = surf_file, 
                       surf_site_info = surf_site_info,
                       prof_file      = prof_file,
                       upper_air_info = upper_air_info,
                       base_elev_m    = base_elev_m,
                       start_date     = start_date,
                       end_date       = end_date)
  return(df)
}

##
