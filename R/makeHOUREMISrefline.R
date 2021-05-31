#' @title makeHOUREMISrefline
#' @description Create the line the references the .EMI file.
#' e.g.    HOUREMIS  .\Emissions\MARTIN.EMI  MC12  ED2  HL2
#' @param path Filepath
#' @param ids Character. Source IDs

makeHOUREMISrefline <- function(path = NA_character_, ids, relative = FALSE){
  if (is.na(path)) stop("Please specify path")
  if (is.null(path)) stop("path or refFilePath cannot be NULL ")
  if (relative) path <- gsub("//", "/",paste0("./", path))
  idd <- paste(ids, collapse = "  ")
  paste0("   HOUREMIS ", path, "  ", idd)
}
