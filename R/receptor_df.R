#' Receptor options data frame
#'
#' Create a data frame of AERMOD receptor options.
#' @param receptor_file Receptor file location.
#' @param receptor_as_text Additional receptor locations as text string.
#' @keywords receptors aermod input
#' @export
#' @examples
#' \dontrun {
#' receptor_df(receptor_file = "..\\receptors\\circle_receptors.ROU")
#' }
#

receptor_df <- function(receptor_file    = as.character(NA),
                        receptor_as_text = as.character(NA)
                        ) {
  
  df <- tibble::tibble(receptor_file     = receptor_file, 
                       receptor_as_text  = receptor_as_text)
  
  return(df)
}

