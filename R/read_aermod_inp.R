#' Read AERMOD input file
#'
#' Read an aermod.inp file into an AERMOD input table.
#' @param file File location. Default is "aermod.inp" in the working directory.
#' @keywords read aermod input
#' @export
#' @examples
#' read_aermod_inp(file = "aermod.inp")
# 
#

read_aermod_inp <- function(file = "aermod.inp") {
  
  inp <- readLines(file)
  
  # CONTROL OPTIONS
  start <- grep("CO STARTING", inp) + 1
  end   <- grep("CO FINISHED", inp) - 1 
  
  df <- utils::read.fwf(textConnection(inp[start:end]), 
                        widths = c(12, nchar(inp[start]) - 12), 
                        header = FALSE, 
                        stringsAsFactors = FALSE)
  
  co <- control_tbl()
  
  co$title_one <- df[1, 2]
  co$title_two <- df[2, 2]
  co$model_opt <- df[3, 2]
  co$aver_time <- df[4, 2]
  co$pollut_id <- df[5, 2]
      
  # SOURCE OPTIONS
  so <- source_tbl()
  
  ## SOURCE locations
  start <- grep("LOCATION", inp)[1]
  end   <- max(grep("LOCATION", inp))
  
  df <- gsub("[[:space:]]+", ",", inp[start:end][!grepl("DESCRSRC", inp[start:end])])
  
  df <- read.csv(textConnection(df), header = FALSE, stringsAsFactors = FALSE)

  so$id        <- df[ , 3]
  so$type      <- df[ , 4]
  so$x_coord   <- df[ , 5]
  so$y_coord   <- df[ , 6]
  so$elevation <- df[ , 7]
  
  so$description <- ""
  
  # SOURCE parameters
  start <- grep("SRCPARAM", inp)[1]
  end   <- max(grep("SRCPARAM", inp))
  
  df <- gsub("[[:space:]]+", ",", inp[start:end])
  
  df <- utils::read.csv(textConnection(df), header = FALSE, stringsAsFactors = FALSE)
  
  so$emissions   <- df[ , 4]
  so$height_m    <- df[ , 5]
  so$temp_k      <- df[ , 6]
  so$velocity_ms <- df[ , 7]
  so$diameter_k  <- df[ , 8]
  
  # SOURCE downwash
  so$downwash_file <- ""
  
  # SOURCE groups
  start <- grep("SRCGROUP", inp)[1]
  end   <- max(grep("SRCGROUP", inp))
  
  df <- utils::read.fwf(textConnection(inp[start:end]), 
                        widths = c(12, 8, nchar(inp[start])-20), 
                        header = FALSE, 
                        stringsAsFactors = FALSE)
  
  so$group_ids     <- df[ , 2]
  so$group_sources <- df[ , 3]
      
  # RECEPTORS
  start <- grep("RE STARTING", inp)[1] + 1
  end   <- max(grep("RE FINISHED", inp)) - 1
  
  re <- receptor_tbl()
  
  re$rect_file <- strsplit(inp[start:end][grep("INCLUDED", inp[start:end])], "INCLUDED ")[[1]][2]
      
  # METEOROLOGY
  start <- grep("ME STARTING", inp) + 1
  end   <- grep("ME FINISHED", inp) - 1 
  
  df <- utils::read.fwf(textConnection(inp[start:end]), 
                        widths = c(12, max(nchar(inp[start:end])) - 12), 
                        header = FALSE, 
                        stringsAsFactors = FALSE)
  
  me <- met_tbl()
  
  me$surf_file      <- df[1, 2]
  me$prof_file      <- df[2, 2]
  me$surf_site_info <- df[3, 2]
  me$prof_site_info <- df[4, 2]
  me$base_elev      <- df[5, 2]
  me$start_met      <- strsplit(df[6, 2], ", ")[[1]][1]
  me$end_met        <- strsplit(df[6, 2], ", ")[[1]][2]
  
  # OUTPUT
  start <- grep("OU STARTING", inp) + 1
  end   <- grep("OU FINISHED", inp) - 1
  
  df <- read.fwf(textConnection(inp[start:end][!grepl("[**]", inp[start:end])]), 
                 widths = c(12, max(nchar(inp[start:end])) - 12), 
                 header = FALSE, 
                 stringsAsFactors = FALSE)
  
  ou <- out_tbl()
  
  ou$rect_table <- subset(df, V1 == "   RECTABLE ")[ , 2]
  ou$max_table  <- subset(df, V1 == "   MAXTABLE ")[ , 2]
  ou$day_table  <- subset(df, V1 == "   DAYTABLE ")[ , 2]
  ou$plot_file  <- subset(df, V1 == "   PLOTFILE ")[ , 2]
      
  # PROJECTION DETAILS
  #start <- grep("PROJCTN", inp)
  #end   <- grep("ZONEINX", inp)
  
  #df <- read.fwf(textConnection(inp[start:end]), 
  #               widths = c(12, max(nchar(inp[start:end])) - 12), 
  #               header = FALSE, 
  #               stringsAsFactors = FALSE)
  
  #po <- project_tbl
  
  #po$PROJCTN  <- df[1, 2]
  #po$DESCPTN  <- df[2, 2]
  #po$DATUM    <- df[3, 2]
  #po$DTMRGN   <- df[4, 2]
  #po$UNITS    <- df[5, 2]
  #po$ZONE     <- df[6, 2]
  #po$ZONEINX  <- df[7, 2]
      
  # COMBINE input tables
  aermod_inp <- cbind(co, so, re, me, ou)
  
  aermod_inp <- list(control     = co,
                     sources     = so,
                     receptors   = re,
                     meteorology = me,
                     output      = ou)
      
  return(aermod_inp)
  
}