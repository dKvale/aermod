# Reads an AERMOD output file and assigns dispersion results 
# to a data frame.

#setwd("..\\batch run")

read_aer_out <- function(file = "aermod.out") {
  
  out <- readLines(file)

  # Read dispersion tables
  results_all <- data.frame()
  
  options(digits= 10)
  
  for(line in grep("GROUP:", out)) {
    
    start <- line + grep("X-COORD", out[line:length(out)])[1] + 1
    
    end   <- start + grep("[***]", out[start:length(out)])[1] - 2
    
    df <- gsub("[[:space:]]+", ",", out[start:end])
    
    df <- read.csv(textConnection(df), header = FALSE, stringsAsFactors = FALSE)
    
    df <- df[ , -c(1, ncol(df))]
    
    n_col <- ncol(df)
    
    names(df) <- rep('x', n_col)
    
    df <- rbind(df[ , 1:(n_col/2)], df[ , (n_col/2+1):n_col])
    
    if(ncol(df) < 4) df$date <- NA
    
    names(df) <- c('x','y','concentration','date')
  
    df$type    <- strsplit(strsplit(out[line], "THE[[:space:]]+")[[1]][2], "[[:space:]]+VALUES")[[1]][1]
    
    df$group   <- strsplit(strsplit(out[line], "GROUP:[[:space:]]+")[[1]][2], "[[:space:]]+[***]")[[1]][1]
    
    df$sources <- strsplit(out[line+1], ":[[:space:]]+")[[1]][2]
    
    results_all <- rbind(results_all, df)
  
  }
  
  # Get AERMOD version
  cat(paste0("AERMOD version #", substring(out[grep("VERSION", out)][1], 25, 29), "\n\n"))
  
  # Read source names
  cat(paste0("Source list: ", paste0(substring(out[grep("LOCATION", out)], 13, 24), collapse = " ")), "\n\n")
  
  # Read AERMOD messages
  cat(paste0(out[(grep("Summary of Total", out)[2]) : (grep("FATAL ERROR", out)[2] - 3)], collapse = "\n"), "\n")
  
  return(results_all)
}
