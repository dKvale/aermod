#' Read AERMOD POST Output Files
#' 
#' Read a table of AERMOD post processed concentration data from the
#' POSTFILE option into a \code{\link{data.frame}}.
#' @param infile Input pos file to read.
#' @keywords pos post
#' @export
#' @examples read_pos("project/pm25_srcgrp1.POS")

read_pos <- function(infile){
  
  library(dplyr)
  
  #Read in header to get number of receptors.
  nhdrlines <- 8
  pos_header <- readLines(infile, n = nhdrlines)
  nreceptors <- as.integer(substr(pos_header[5],26,30))
  
  #Create column name list for data frame from line 7 of the first header.
  col_names <- pos_header[7]
  col_names %<>%
    gsub("^\\*","", .) %>%
    gsub("AVERAGE CONC","AVERAGE_CONC", .) %>%
    gsub("YEAR NUM","YEAR_NUM", .) %>%
    gsub("NUM HRS","NUM_HRS", .) %>%
    gsub("NET ID","NET_ID", .)
  
  col_names <- unlist(strsplit(trimws(col_names, which = "both"),"\\s+"))
  
  #Create column width list for reading the fixed format text file.  The number of 
  #columns varies depending on the number of source groups in the modeling run.
  col_widths <- c(-1,13,-1,13,-1,13,-1,8,-1,8,-1,8,-2,6,-2,8,-2,8,-2,8)
  
  #The data in the pos file is organized by year number (usually 1 through 5) and each set of data are separated by
  #header lines.  Therefore the file cannot be read at once, but must be read in segments (length of segments is number of
  #receptors.  The number of years contained in the file is not contained in the headers, but we can keep trying to read 
  #in the file until we get a data frame with zero rows.
  j <- 1
  pos_data <- data.frame()
  end_of_file <- FALSE
  while ( ! end_of_file ) {
    
    s <- j*nhdrlines + (j-1)*nreceptors
    data_chunk <- read.fwf(infile, widths = col_widths, header = FALSE, skip = s, n = nreceptors, col.names = col_names, stringsAsFactors = FALSE)
    
    if(is.data.frame(data_chunk) && nrow(data_chunk)==0){
      end_of_file <- TRUE
    } else {
      pos_data <- rbind(pos_data,data_chunk)
    }
    
    j <- j + 1
  }#end while
  
  return(pos_data)
}
