# Reads an AERMOD output file and assigns dispersion results 
# to a data frame.

#setwd("..\\batch run")
library(RCurl)  # For https://
library(XML)
library(magrittr)
library(stringr)

file_name <- "https://raw.githubusercontent.com/dKvale/aermod/master/data-raw/aermod.out"

read_aer_out <- function(file = "aermod.out") {
  
  if(substring(file, 1,5) == "https") out <- strsplit(getURL(file), "\n")[[1]]
  else out <- readLines(file)
  
  # Read dispersion tables
  all_results <- data.frame()
  
  options(digits= 10)
  
  for(line in grep("GROUP[:]", out)) {
    
    start <- line + grep("X-COORD", out[line:length(out)])[1] + 1
    
    end   <- start + grep("[***]", out[start:length(out)])[1] - 2
    
    df <- gsub("[[:space:]]+", ", ", out[start:end])
    
    df <- read.csv(textConnection(df), header = FALSE, stringsAsFactors = FALSE)
    
    df <- df[ , -c(1, ncol(df))]
    
    n_col <- ncol(df)
    
    names(df) <- rep('x', n_col)
    
    df <- rbind(df[ , 1:(n_col/2)], df[ , (n_col/2+1):n_col])
    
    if(ncol(df) < 4) df$date <- NA
    
    names(df) <- c('rec_x','rec_y','concentration','date')
    
    df$type    <- strsplit(strsplit(out[line], "THE[[:space:]]+")[[1]][2], "[[:space:]]+VALUES")[[1]][1]
    
    df$group   <- strsplit(strsplit(out[line], "GROUP:[[:space:]]+")[[1]][2], "[[:space:]]+[***]")[[1]][1]
    
    df$sources <- strsplit(out[line+1], ":[[:space:]]+")[[1]][2] %>% 
                  gsub("[[:space:]]+, ", " ", .) %>%
                  str_trim()
    
    df$src_coords <- substring(out[grep("LOCATION", out)], 13, 80) %>% 
                     .[grep(df$sources[1], .)] %>% 
                     str_split("[[:space:]]+")
    
    df$src_x    <- df$src_coords %>% unlist() %>% .[3]
    
    df$src_y    <- df$src_coords %>% unlist() %>% .[4]
    
    df$src_elev <- df$src_coords %>% unlist() %>% .[5]
    
    df$src_coords <- df$src_coords %>% unlist() %>% .[3:4] %>% paste(collapse= " ")
    
    all_results <- rbind(all_results, df)
    
  }
  
  df <- data.frame(AERMOD_v = substring(out[grep("VERSION", out)][1], 25, 29),
                   stringsAsFactors = F)
  
  df$title <- substring(out[grep("TITLEONE", out)][1], 13, 40) %>% str_trim()
  
  df$subtitle <- substring(out[grep("TITLETWO", out)][1], 13, 40) %>% str_trim()
  
  df$pollutant <- substring(out[grep("POLLUTID", out)][1], 13, 40) %>% str_trim()
  
  df$ter_options <- substring(out[grep("MODELOPT", out)][1], 13, 40) %>% str_trim()
  
  df$met_surface <- substring(out[grep("SURFDATA", out)][1], 13, 40)
  
  df$met_upper <- substring(out[grep("UAIRDATA", out)][1], 13, 70) %>% str_trim()
  
  df$met_base <- substring(out[grep("PROFBASE", out)][1], 13, 40) %>% str_trim()
  
  df <- cbind(df, all_results)
  
  # Get AERMOD version
  #cat(paste0("AERMOD version #", substring(out[grep("VERSION", out)][1], 25, 29), "\n\n"))
  
  # Read source names
  #cat(paste0("Source list: ", paste0(substring(out[grep("LOCATION", out)], 13, 24), collapse = " ")), "\n\n")
  
  # Read AERMOD messages
  cat(paste0(out[(grep("Summary of Total", out)[2]) : (grep("FATAL ERROR", out)[2] - 3)], collapse = "\n"), "\n")
  
  return(df)
}

##
