#' Read AERMOD MAXDCONT Output Files
#' 
#' Read a table of AERMOD pollutant contributions from the MAXDCONT option
#' into a \code{\link{data.frame}}.
#' @param infile Input maxdcont file to read.
#' @keywords maxdcont
#' @export
#' @examples read_maxdcont(project/pm25_maxdcont.TXT)

read_maxdcont <- function(infile){
  
  library(dplyr)
  #Read in header to get number of receptors and source groups.
  nhdrlines <- 8
  maxdcont_header <- readLines(infile, n = nhdrlines)
  nreceptors <- as.integer(substr(maxdcont_header[5],26,31))
  nsrcgroups <- as.integer(substr(maxdcont_header[5],47,50))
  
  #Create column name list for data frame from line 7 of the first header.
  col_names <- maxdcont_header[7]
  col_names %<>%
    gsub("^\\*","", .) %>%
    gsub("AVERAGE CONC","AVERAGE_CONC", .) %>%
    gsub("NET ID","NET_ID", .) %>%
    gsub("CONT ","CONT_", .)
  
  col_names <- unlist(strsplit(trimws(col_names, which = "both"),"\\s+"))
  
  #Create column width list for reading the fixed format text file.  The number of 
  #columns varies depending on the number of source groups in the modeling run.
  col_widths <- c(-1,13,-1,13,-1,13,-1,8,-1,8,-1,8,-2,6,-2,8,-2,5,-5,8)
  for (i in 1:nsrcgroups) {
    col_widths <- c(col_widths,-2,13)
  }
  
  #The data in the maxdcont file is organized by concentration ranks (eg. 4th high) and each set of ranked data are separated by
  #header lines.  Therefore the file cannot be read at once, but must be read in segments.  The number of ranks contained in the 
  #file is not contained in the headers, but we can keep trying to read in the file until we get a data frame with zero rows.
  j <- 1
  maxdcont_data <- data.frame()
  end_of_file <- FALSE
  while ( ! end_of_file ) {
    
    s <- j*nhdrlines + (j-1)*nreceptors
    data_chunk <- read.fwf(infile, widths = col_widths, header = FALSE, skip = s, n = nreceptors, col.names = col_names, stringsAsFactors = FALSE)
    
    if(is.data.frame(data_chunk) && nrow(data_chunk)==0){
      end_of_file <- TRUE
    } else {
      maxdcont_data <- rbind(maxdcont_data,data_chunk)
    }
    
    j <- j + 1
  }#end while
  
  return(maxdcont_data)
  
}
