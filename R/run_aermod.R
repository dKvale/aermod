#' Run AERMOD
#'
#' Call aermod.exe and run input file
#' @param input Path to input file.
#' @param out_file File name for saving model output. 
#'               Defaults to value of \code{title} in the provided input file.
#' @param exe_folder Folder containing \code{aermod.exe}.
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
                       exe_folder = "aermod") {
  
 # Check if aermod.exe exists in folder
  is_aermod <- "aermod.exe" %in% tolower(list.files(exe_folder))
  
  if(!is_aermod) {
    stop("aermod.exe not found in ", exe_folder)
    
  }
  
  # Copy input file to folder
  writeLines(print(input), paste0(exe_folder, "/aermod.inp"))
  
  # Shell command
  relocate <- paste0(substring(exe_folder, 1, 1), ": & CD ", exe_folder)
  
  shell(paste(relocate, "& aermod.exe"))
  
  # Copy output file
  shell(paste0(relocate, ' & COPY aermod.out "', out_file, '.out"'))
  
  # Clean house      
  #shell(paste(relocate, "& DEL aermod.inp"))
  #shell(paste(relocate, "& DEL aermod.out"))
  invisible(readLines("aermod.out"))
}
