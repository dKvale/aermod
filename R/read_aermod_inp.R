#' Read AERMOD input file
#'
#' Read an aermod.inp file into an AERMOD input table.
#' @param file File location. Defaults to "aermod.inp".
#' @param as_one_df \code{TRUE} or \code{FALSE}. 
#'                    Return all inputs in a single wide data frame. 
#'                    If \code{FALSE}, 5 data frames are returned in a list: 
#'                         (1) control
#'                         (2) sources
#'                         (3) receptors
#'                         (4) meteorology
#'                         (5) output
#' @keywords read aermod input
#' @export
#' @importFrom magrittr "%>%"
#' @examples
#' \dontrun{
#' read_aermod_inp(file = "aermod.inp")
#' }
#

read_aermod_inp <- function(file        = "aermod.inp", 
                            as_one_df   = TRUE) {
  
  inp <- readLines(file)
  
  inp <- inp[!grepl("[**]", inp)]
  
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
  co$avg_time  <- df[4, 2]
  co$pollutant_id <- df[5, 2]
      
  inp <- inp[(end+2):length(inp)]
  
  # SOURCE OPTIONS
  
  ## SOURCE locations
  start <- grep("LOCATION", inp)[1]
  end   <- max(grep("LOCATION", inp))
  
  df <- gsub("[[:space:]]+", ",", inp[start:end][!grepl("DESCRSRC", inp[start:end])])

  df <- utils:: read.csv(textConnection(df), header = FALSE, stringsAsFactors = FALSE)
  
  so <- source_tbl(source_id   = df[ , 3],
                   type        = df[ , 4],
                   x_coord     = df[ , 5],
                   y_coord     = df[ , 6],
                   elevation_m = df[ , 7],
                   description = "")
  
  inp <- inp[(end+1):length(inp)]
  
  # SOURCE parameters
  start <- grep("SRCPARAM", inp)[1]
  end   <- max(grep("SRCPARAM", inp))
  
  df <- gsub("[[:space:]]+", ",", inp[start:end])
  
  df <- utils::read.csv(textConnection(df), header = FALSE, stringsAsFactors = FALSE)
  as_one_df
  so$emissions   <- df[ , 4]
  so$height_m    <- df[ , 5]
  so$temp_k      <- df[ , 6]
  so$velocity_ms <- df[ , 7]
  so$diameter_m  <- df[ , 8]
  
  # SOURCE downwash
  end <- grep("SRCGROUP", inp)[1] - 1
  
  if(sum(grepl("INCLUDED", inp[start:end])) > 0) {
    
    downwash_file <- inp[start:end][grep("INCLUDED", inp[start:end])[1]]
     
    so$downwash_file <- substring(downwash_file, 13)
    
  } 
  
  inp <- inp[(end + 1):length(inp)]
  
  # SOURCE groups
  start <- grep("SRCGROUP", inp)[1]
  end   <- max(grep("SRCGROUP", inp))
  
  df <- utils::read.fwf(textConnection(inp[start:end]), 
                        widths = c(12, 4, 1, max(nchar(inp[start:end]))-17), 
                        header = FALSE, 
                        stringsAsFactors = FALSE)
  
  # Create a new row for each source in a group
  df <- dplyr::group_by(tibble::as_data_frame(df), V2) %>%
        dplyr::mutate(source_id = list(strsplit(V4, " ")[[1]])) %>%
        tidyr::unnest(source_id)
  
  # Collapse the groups that a source belongs to into a vector
  df <- dplyr::group_by(df[ , c("V2", "source_id")], source_id) %>%
        tidyr::nest() %>%
        dplyr::ungroup()
  
  # Unlist the group_ids
  df <- dplyr::group_by(df, source_id) %>%
        dplyr::mutate(group_id = list(unlist(data)))
  
  # Join groups to sources
  so <- dplyr::left_join(so[ , -grep("group_id", names(so))], 
                         df[ , c("source_id", "group_id")],
                         by = "source_id")
     
  inp <- inp[(end+2):length(inp)]
   
  
  # RECEPTORS
  start <- grep("RE STARTING", inp)[1] + 1
  end   <- max(grep("RE FINISHED", inp)) - 1
  
  re <- receptor_tbl()
  
  re$rect_file <- substring(inp[start:end][grep("INCLUDED", inp[start:end])], 13)
     
  inp <- inp[(end+2):length(inp)]
   
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
  me$base_elev_m    <- substring(df[5, 2], 1, 5)
  me$start_met      <- paste(strsplit(df[6, 2], " ")[[1]][1:4], collapse = " ")
  me$end_met        <- paste(strsplit(df[6, 2], " ")[[1]][5:8], collapse = " ")
  
  inp <- inp[(end+2):length(inp)]
  
  # OUTPUT
  start <- grep("OU STARTING", inp) + 1
  end   <- grep("OU FINISHED", inp) - 1
  
  df <- utils::read.fwf(textConnection(inp[start:end]), 
                        widths = c(12, max(nchar(inp[start:end])) - 12), 
                        header = FALSE, 
                        stringsAsFactors = FALSE)
  
  ou <- out_tbl()
  
  if(grepl("RECTABLE", df$V1)) ou$rect_table <- subset(df, V1 == "   RECTABLE ")$V2
  if(grepl("MAXTABLE", df$V1)) ou$max_table  <- subset(df, V1 == "   MAXTABLE ")$V2
  if(grepl("DAYTABLE", df$V1)) ou$day_table  <- subset(df, V1 == "   DAYTABLE ")$V2
  if(grepl("PLOTFILE", df$V1)) ou$plot_file  <- subset(df, V1 == "   PLOTFILE ")$V2
    
  #inp <- inp[(end+2):length(inp)]
  
  # PROJECTION DETAILS
  if(FALSE) {
  #start <- grep("PROJCTN", inp)
  #end   <- grep("ZONEINX", inp)
  
  #if(length(start) > 0 && length(end) > 0 && !is.na(start) && !is.na(end)) {
    
  #df <- utils::read.fwf(textConnection(inp[start:end]), 
  #                      widths = c(12, max(nchar(inp[start:end])) - 12), 
  #                      header = FALSE, 
  #                      stringsAsFactors = FALSE)
  #print(df) 
  #}
  
  
  #po <- project_tbl()
  
  #po$projection  <- df[1, 2]
  #po$description <- df[2, 2]
  #po$datum       <- df[3, 2]
  #po$dtmrgn      <- df[4, 2]
  #po$units       <- df[5, 2]
  #po$zone        <- df[6, 2]
  #po$zone_inx    <- df[7, 2]
  }
  
  
  # COMBINE input tables
  if(as_one_df) {
    aermod_inp <- cbind(co, so, re, me, ou)
  } else {
    aermod_inp <- list(control     = co,
                       sources     = so,
                       receptors   = re,
                       meteorology = me,
                       output      = ou)
  }
      
  return(aermod_inp)
  
}
