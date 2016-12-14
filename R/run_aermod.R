#' Run AERMOD
#'
#' Call aermod.exe and run input file
#' @param input Path to input file.
#' @param output Filename for model results. Defaults to `title` of input file.
#' @param exe_folder Folder containing aermod.exe.
#' @keywords aermod dispersion model EPA
#' @export
#' @examples
#' \dontrun{
#' run_aermod(data       = "aermod.inp", 
#'            output     = "aermod_results", 
#'            exe_folder = "aermod_exe")
#' }
# 
run_aermod <- function(input      = "aermod.inp", 
                       output     = input$title,
                       exe_folder = "aermod") {
  
 # Check if aermod.exe exists in folder
  is_aermod <- "aermod.exe" %in% tolower(list.files(exe_folder))
  
  if(!is_aermod) {
    warning("aermod.exe was not found in ", exe_folder)
    stop()
  }
  
  # Copy input file to folder
  writeLines(print(input), paste0(exe_folder, "/aermod.inp"))
  
  # Shell command
  relocate <- paste0(substring(exe_folder, 1, 1), ": & CD ", exe_folder)
  
  shell(paste(relocate, "& aermod.exe"))
  
  # Copy output file
  shell(paste0(relocate, ' & COPY aermod.out "', output, '.out"'))
  
  # Clean house      
  #shell(paste(relocate, "& DEL aermod.inp"))
  #shell(paste(relocate, "& DEL aermod.out"))
  invisible(readLines("aermod.out"))
}
