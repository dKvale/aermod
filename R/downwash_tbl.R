#' Downwash options
#'
#' Create an input table of AERMOD downwash options.
#' @param STACK Stack names. Separate multiple sources with commas.
#' @keywords downwash building aermod input
#' @export
#' @examples
#' downwash_tbl(STACK = "STK_1")
# 
# 

downwash_tbl <- function(STACK = "STK_1",
                         BUILDHGTS = "",
                         BUILDWIDS = "",
                         BUILDLENS = "",
                         XBADJ = "",
                         YBADJ = ""
) {

df <- data.frame(STACK     = STACK,
                 BUILDHGTS = BUILDHGTS,
                 BUILDWIDS = BUILDWIDS,
                 BUILDLENS = BUILDLENS,
                 XBADJ     = XBADJ,
                 YBADJ     = YBADJ,
                 stringsAsFactors = F)
return(df)
}

##