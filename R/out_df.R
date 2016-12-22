#' Output options
#'
#' Create a data frame of AERMOD output options.
#' @param rect_table Output options as text. Enter multiple options as a vector: \code{c("ALLAVE 1ST", "1 1ST")} 
#' @param max_table Output options as text. 
#' @param day_table Output options as text.   
#' @param file_from Output options as text.
#' @param rank_file Output options as text.
#' @param plot_file Output options as text. 
#' @keywords output aermod 
#' @export
#' @examples
#' out_df(rect_table = c("ALLAVE 1ST", "1 1ST 8TH"))
# 
#

out_df <- function(rect_table = c("ALLAVE 1ST", "1 1ST"),
                   max_table  = as.character(NA),
                   day_table  = as.character(NA),
                   file_form  = as.character(NA),
                   rank_file  = as.character(NA),
                   plot_file  = as.character(NA)
) {

df <- tibble::tibble(rect_table = list(rect_table), 
                     max_table  = list(max_table),
                     day_table  = list(day_table),
                     file_form  = list(file_form),
                     rank_file  = list(rank_file),
                     plot_file  = list(plot_file))
 
return(df)

}

##