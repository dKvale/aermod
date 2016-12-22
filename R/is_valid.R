#' Test if object is valid
#'
#' Test for nchars() or nrows() of object. 
#' For lists, returns \code{TRUE} if
#' at least one member of the list passes validity test.
#' \code{NA} objects return \code{FALSE}.
#' @param x Object for length test.
#' @param n Minimum number of characters if character or numeric; 
#'          Minimum number of rows if data frame.
#' @keywords minimum valid test
#' @export
#' @examples
#' # For a character string
#' is_valid("MyName", n = 3)
#' 
#' is_valid(as.character(NA), n = 1)
#' 
#' # For a data frame
#' is_valid(data.frame(X = 1), n = 1)
#' 
#' is_valid(data.frame(X = 1), n = 2)
#' 
#' # For numeric
#' is_valid(13, n = 2)
#' 
#' # For a list
#' is_valid(list(NA, "MyName"), n = 3)
#' 
# 
is_valid <- function(x, n = 1) {
  
  x <- unlist(x)
  
  if(is.null(x) || sum(!is.na(x)) < 1) return(FALSE) 
  
  if(is.character(x) || is.numeric(x)) return(max(nchar(x), na.rm = TRUE) >= n)
  
  if(is.data.frame(x)) return(max(nrow(x), na.rm = TRUE) >= n)
}