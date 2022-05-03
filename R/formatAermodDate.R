
#' @title formatAermodDate
#' @description Take datetime format and change to Aeromod's fixed with year month day hour format used in .EMI files
#' @param d POSIXct. E.g. lubridate::ymd_hms("2011-05-01 04:00:00", tz = "Africa/Johannesburg")
#' @import lubridate

formatAermodDate <- function(d){
  
  space = " "
  
  if (!lubridate::is.POSIXct(d)) stop("d must be POSIXct")
  paste0(fw(format(d, "%y"), 3, after = TRUE),
         fw(lubridate::month(d), 2, after = FALSE),
         space,
         fw(format(d, "%e"), 2, after = FALSE),
         space,
         fw(lubridate::hour(d), 2, after = FALSE),
         space
         )
}
