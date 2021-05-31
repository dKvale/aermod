#' @title writeText
#' @description Write result (like those of of makeSRCPARAMtext) to a text file
#' @param path Character. Filepath and filename
#' @param inp_text Character. Result of makeSRCPARAMtext or similar
#' @export

writeText <- function(inp_text, path){
  con <- file(path)
  writeLines(inp_text, con)
  close(con)
}
