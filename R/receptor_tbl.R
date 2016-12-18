#' Receptor options
#'
#' Create a data frame of AERMOD receptor options.
#' @param recept_file Receptor file location.
#' @param recept_as_text Additional receptor locations as text string.
#' @keywords receptors aermod input
#' @export
#' @examples
#' \dontrun{
#' receptor_tbl(recept_file = "..\\Receptors\\circleReceptors.ROU")
#' }
#

receptor_tbl <- function(recept_file    = as.character(NA),
                         recept_as_text = as.character(NA)
) {
  
  df <- tibble::tibble(recept_file     = recept_file, 
                       recept_as_text  = recept_as_text)
  
  return(df)
}

