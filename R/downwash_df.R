#' Downwash options
#'
#' Create a data frame of AERMOD downwash options.
#' @param source_id Emission source ID or name. 
#' @keywords downwash building aermod input
#' @export
#' @examples
#' downwash_df(source_id = "STK_1")
# 
# 

downwash_df <- function(source_id     = as.character(NA),
                        bld_heights   = as.numeric(NA),
                        bld_widths    = as.numeric(NA),
                        bld_lengths   = as.numeric(NA),
                        x_badj        = as.numeric(NA),
                        y_badj        = as.numeric(NA)
) {

df <- tibble::tibble(source_id     = source_id ,
                     bld_heights   = bld_heights,
                     bld_widths    = bld_widths,
                     bld_lengths   = bld_lengths,
                     x_badj        = x_badj,
                     y_badj        = y_badj)
return(df)
}

##
