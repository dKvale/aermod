#' Output options
#'
#' Create an input table of AERMOD output options.
#' @param rect_table Output options as text. Separate multiple outputs with a comma: \code{"ALLAVE 1ST, 1 1ST"} 
#' @param max_table Output options as text. 
#' @param day_table Output options as text.   
#' @param plot_file Output options as text. 
#' @keywords output aermod 
#' @export
#' @examples
#' out_tbl(rect_table = "ALLAVE 1ST, 1 1ST 8TH")
# 
#

out_tbl <- function(rect_table = "ALLAVE 1ST, 1 1ST",
                    max_table  = as.character(NA),
                    day_table  = as.character(NA),
                    plot_file  = as.character(NA)
) {

df <- tibble::tibble(rect_table = rect_table, 
                     max_table  = max_table,
                     day_table  = day_table,
                     plot_file  = plot_file)
 
return(df)

}

##