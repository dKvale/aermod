#' Downwash options
#'
#' Create a data frame of AERMOD downwash options.
#' @param source_id Emission source ID or name. 
#' @keywords downwash building aermod input
#' @export
#' @examples
#' downwash_tbl(source_id = "STK_1")
# 
# 

downwash_tbl <- function(source_id     = NULL,
                         build_heights = NULL,
                         build_widths  = NULL,
                         build_lengths = NULL,
                         x_badj        = NULL,
                         y_badj        = NULL
) {

df <- tibble::tibble(source_id       = source_id ,
                     build_heights   = build_heights,
                     build_widths    = build_widths,
                     build_lengths   = build_lengths,
                     x_badj          = x_badj,
                     y_badj          = y_badj)
return(df)
}

##
