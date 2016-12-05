#' Meteorology options
#'
#' Create an input table of AERMOD meteorology options.
#' @param SURFFILE Surface met data file location.
#' @param PROFFILE Profile met data file location.
#' @keywords meteorology weather surface aermod input
#' @export
#' @examples
#' met_tbl(SURFFILE = "..\\MET\\RSTMPX5Y.SFC")
# 
#

met_tbl <- function(SURFFILE = "..\\MET\\RSTMPX5Y.SFC",
                    PROFFILE = "..\\MET\\RSTMPX5Y.PFL",
                    SURFDATA = "14925 2006 ROCHESTER/MUNICIPAL_ARPT",
                    UAIRDATA = "94983 2006",
                    PROFBASE = "396.0 METERS",
                    STARTEND = "2009 1 1 1, 2013 12 31 24"
) {
  
  df <- data.frame(SURFFILE = SURFFILE, 
                   PROFFILE = PROFFILE,
                   SURFDATA = SURFDATA,
                   UAIRDATA = UAIRDATA,
                   PROFBASE = PROFBASE,
                   STARTEND = STARTEND,
                   stringsAsFactors = F)
  return(df)
}

##