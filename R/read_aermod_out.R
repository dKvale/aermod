#' Read AERMOD output file
#'
#' Read an aermod.out file into a dataframe.
#' @param file File l ocation. Defaults to "aermod.out".
#' @keywords read aermod output results
#' @export
#' @examples
#' \dontrun{
#' read_aermod_out(file = "aermod.out")
#' }
#

read_aermod_out <- function(file) {
  
  out <- readLines(file)
  
  #out <- out[!grepl("[**]", out)]

  # Read dispersion tables
  results_all <- tibble::tibble()
  
  options(digits = 10)
  
  for(line in grep("GROUP:", out)) {
    
    start <- line + grep("X-COORD", out[line:length(out)])[1] + 1
    
    end   <- start + grep("[***]", out[start:length(out)])[1] - 2
    
    df <- gsub("[[:space:]]+", ",", out[start:end])
    
    df <- utils::read.csv(textConnection(df), header = FALSE, stringsAsFactors = FALSE)
    
    df <- df[ , -c(1, ncol(df))]
    
    n_col <- ncol(df)
    
    names(df) <- rep('x', n_col)
    
    df <- rbind(df[ , 1:(n_col/2)], df[ , (n_col / 2+1) : n_col])
    
    if(ncol(df) < 4) df$date <- NA
    
    names(df) <- c('x_coord','y_coord','concentration','date')
    
    df$date        <- as.Date(df$date, format = "(%y%m%d%H)")
  
    df$model_opts  <- strsplit(strsplit(out[line], "THE[[:space:]]+")[[1]][2], "[[:space:]]+VALUES")[[1]][1]
    
    df$group_id    <- strsplit(strsplit(out[line], "GROUP:[[:space:]]+")[[1]][2], "[[:space:]]+[***]")[[1]][1]
    
    # Force to tibble for storing lists
    df <- tibble::as_data_frame(df)
    
    # Source IDs
    df <- dplyr::mutate(df, source_ids = strsplit(out[line+1], ":[[:space:]]+")[[1]][2])
    
    df <- dplyr::group_by(df, dplyr::row_number())
    
    df <- dplyr::mutate(df, source_ids = list((strsplit(gsub(" ", "", df$source_ids[1]), ",")[[1]])))
    
    
    # Combine all groups
    results_all    <- rbind(results_all, df)
  
  }
  
  # Get AERMOD version
  cat(paste0("AERMOD version #", substring(out[grep("VERSION", out)][1], 25, 29), "\n\n"))
  
  # Read source names
  cat(paste0("Source list: ", paste0(substring(out[grep("LOCATION", out)], 13, 24), collapse = " ")), "\n\n")
  
  # Read AERMOD messages
  cat(paste0(out[(grep("Summary of Total", out)[2]) : (grep("FATAL ERROR", out)[2] - 3)], collapse = "\n"), "\n")
  
  return(results_all)
}
