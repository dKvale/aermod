#' Output options
#'
#' Create an input table of AERMOD output options.
#' @param RECTABLE Output options as text. Multiple options separated by a comma: 
#' @keywords output aermod input
#' @export
#' @examples
#' out_tbl(RECTABLE = "ALLAVE 1ST, 1 1ST 8TH")
# 
#

out_tbl <- function(RECTABLE = "ALLAVE 1ST, 1 1ST",
                    MAXTABLE = "",
                    DAYTABLE = "",
                    PLOTFILE = ""
) {

df <- tibble::tibble(RECTABLE = RECTABLE, 
                     MAXTABLE = MAXTABLE,
                     DAYTABLE = DAYTABLE,
                     PLOTFILE = PLOTFILE)

return(df)

}

##