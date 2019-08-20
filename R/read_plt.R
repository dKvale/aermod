#' Read AERMOD PLOT Output Files
#' 
#' Read a table of AERMOD receptor concentration data from the
#' PLOTFILE option into a \code{\link{data.frame}}.
#' @param infile Input pos file to read.
#' @keywords plt plot
#' @export
#' @examples read_plt("project/pm25_srcgrp1.PLT")

read_plt <- function(infile){
  
  library(dplyr)
  
  # Read in header to get number of receptors.
  nhdrlines <- 8
  plt_header <- readLines(infile, n = nhdrlines)
  
  #Check to see if plot file is for high n-th high (ie pm10) or nth-highest max daily averaged over x years (ie pm25 or nox)
  #The header and number of columns varies depending on which type of results are contained in the file.
  string <- plt_header[4]
  if( grepl("PLOT FILE OF  HIGH ..... HIGH ..-HR VALUES",string) ){
    ptype <- "HNH"
  } else if(grepl("PLOT FILE OF .....-HIGHEST MAX DAILY ..-HR VALUES AVERAGED OVER",string)) {
    ptype <- "HDMAVG"
  } else {
    stop("Error: Unrecognized results type.")
  }
  
  if( ptype == "HDMAVG") {
    nyears     <- max(as.integer(substr(plt_header[4], 75, 77)), 1, na.rm = T)
  }
  nreceptors <- as.integer(substr(plt_header[5], 26, 30))
  
  #Create column name list for data frame from line 7 of the first header.
  col_names <- plt_header[7]
  
  col_names %<>%
    gsub("^\\*","", .) %>%
    gsub("AVERAGE CONC","AVERAGE_CONC", .) %>%
    gsub("YEAR NUM","YEAR_NUM", .) %>%
    gsub("NUM HRS","NUM_HRS", .) %>%
    gsub("NET ID","NET_ID", .) %>%
    gsub("AVER CONC YR","AVER_CONC_YR", .) %>%
    gsub("DATE YR","DATE_YR", .) %>%
    gsub("DATE\\(CONC\\)","DATE_CONC", .)
  
  col_names <- unlist(strsplit(trimws(col_names, which = "both"), "\\s+"))

  #Create column width list for reading the fixed format text file.  The number of 
  #columns varies depending on the number of source groups in the modeling run.
  col_widths <- c(-1,13,-1,13,-1,13,-1,8,-1,8,-1,8,-2,6,-2,8,-2,5,-5,8)
  
  if( ptype == "HDMAVG") {
    for (i in 1:nyears) {
      col_widths <- c(col_widths, -2, 13, -2, 8)
    }
  } else {
    col_widths <- c(col_widths, -2, 8)
  }
  # The data in the PLT file contain a single rank at all receptors.  
  # Therefore, there is only a single header and one chunk of data.
  plt_data <- read.fwf(infile, 
                       widths    = col_widths, 
                       header    = FALSE, 
                       skip      = nhdrlines, 
                       n         = nreceptors, 
                       col.names = col_names, 
                       stringsAsFactors = FALSE)
  
  return(plt_data)
}