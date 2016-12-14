#' Receptor options
#'
#' Create an input table of AERMOD receptor options.
#' @param rect_file Receptor file location.
#' @param rou_text Additional receptor locations as text string. Default is blank.
#' @keywords receptors aermod input
#' @export
#' @examples
#' \dontrun{
#' receptor_tbl(rect_file = "..\\Receptors\\circleReceptors.ROU")
#' }
#

receptor_tbl <- function(rect_file = "",
                         rou_text  = ""
) {
  
  df <- tibble::tibble(rect_file = rect_file, 
                       rou_text  = rou_text)
  
  return(df)
}

