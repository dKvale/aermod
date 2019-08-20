#' Convert Geodetic Coordinates (Longitude, Latitude) to UTM Coordinates
#' 
#' Convert longitude, latitude coordinates in a data frame to UTM coordinates
#' in a specified UTM zone.  The longitude and latitude coordinates should be
#' columns in the data frame passed into the function.  The user can optionally
#' specify the names of the columns to process.  The function returns the
#' dataframe with the X and Y columns bound to it.
#' @param df Input data frame.
#' @param long The name of the column containing longitude values
#' @param lat The name of the column containing latitude values
#' @param utm_zone UTM zone in which to calculate X, Y coordinates
#' @keywords utm ll
#' @export
#' @examples ll_to_utm(df, long = "LONG", lat = "LAT", utm_zone = 15)

ll_to_utm <- function(df, long = "LONG", lat = "LAT", utm_zone = 15){
library(dplyr)
library(proj4)

#Ensure input is a data frame.
if(!is.data.frame(df)) stop("df must be a data frame")

#Convert longitude and latitude coordinates to UTM Zone 15 x and y coordinates in meters.
ll <- df[,c(long,lat)]
proj_string <- paste0("+proj=utm +zone=",utm_zone," +datum=NAD83 +units=m +no_defs")
xy <- ll %>% project(proj_string)
names(xy) <- c("X","Y")
df <- cbind(df,xy)

return(df)
}