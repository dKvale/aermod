#' Receptor options
#'
#' Create an input table of AERMOD receptor options.
#' @param recept_file Receptor file location.
#' @param recept_text Additional receptor locations as text string.
#' @keywords receptors aermod input
#' @export
#' @examples
#' \dontrun{
#' receptor_tbl(recept_file = "..\\Receptors\\circleReceptors.ROU")
#' }
#

receptor_tbl <- function(recept_file = NA,
                         recept_text = NA
) {
  
  df <- tibble::tibble(recept_file  = recept_file, 
                       recept_text  = recept_text)
  
  return(df)
}

