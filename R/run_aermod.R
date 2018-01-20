#' Run AERMOD
#'
#' Call aermod.exe and run input file
#' @param input Path to input file.
#' @param out_file File name for saving model output. 
#'               Defaults to value of \code{title} in the provided input file.
#' @param exe_folder Folder containing \code{aermod.exe}.
#' @param silent Print results to screen. \code{TRUE} or \code{FALSE} 
#' @keywords aermod run dispersion model EPA
#' @export
#' @examples
#' \dontrun{
#' run_aermod(data       = "aermod.inp", 
#'            out_file   = "aermod_results", 
#'            exe_folder = "aermod_exe")
#' }
# 
run_aermod <- function(input      = "aermod.inp", 
                       out_file   = "aermod.out",
                       exe_folder = "aermod",
                       silent     = FALSE) {
  
  
  # Attach current directory to exe_folder
  if(!grepl("[:]", exe_folder)) exe_folder <- paste0(getwd(), "/", exe_folder)
  
  # Check if aermod.exe exists in folder
  is_aermod <- "aermod.exe" %in% tolower(list.files(exe_folder))
  
  if(!is_aermod) {
    stop("aermod.exe not found in ", exe_folder)
  }
  
  # Save input file if path was not provided
  if("character" %in% class(input)) {
    
    if("\n" %in% input | length(input) > 1) {
      
       writeLines(input, paste0(exe_folder, "/aermod.inp"))
      
    } else {
       
       writeLines(readLines(input), paste0(exe_folder, "/aermod.inp"))
      
    }
    
  } else if("data.frame" %in% class(input)) {
    
    write_aermod(input, paste0(exe_folder, "/aermod.inp")) 
    
  } else {
    
    stop(paste0('The entered "input" is of class ', class(input), '. \n 
                The "input" should be a text string, a path to an input file, or a data frame.'))
    
  }
  
  
  # Shell command
  relocate <- paste0(substring(getwd(), 1, 2), paste0(' & CD "', exe_folder, '"'))
  
  shell(paste0(relocate, "& aermod.exe"))
  
  # Update output file name to end with ".out"
  if(!grepl("[.]out", out_file)) out_file <- paste0(out_file, ".out")
  
  # Copy output file
  shell(paste0(relocate, ' & COPY aermod.out "', paste0(getwd(), "/", out_file, '"')))
  
  # Clean house      
  #shell(paste(relocate, "& DEL aermod.inp"))
  #shell(paste(relocate, "& DEL aermod.out"))
  
  # Read modeling results
  if(!silent) {
    invisible(readLines(out_file))
    return(paste0(getwd(), "/", out_file))
    
  } else return()
}
