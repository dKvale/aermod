#' Convert UTM Coordinates to Geodetic Coordinates (Longitude, Latitude) 
#' 
#' Convert UTM coordinates in a specified UTM zone in a data frame to 
#' longitude, latitude coordinates. The UTM coordinates should be
#' columns in the data frame passed into the function.  The user can optionally
#' specify the names of the columns to process.  The function returns the
#' dataframe with the LONG and LAT columns bound to it.
#' @param df Input data frame.
#' @param long The name of the column containing longitude values
#' @param lat The name of the column containing latitude values
#' @param utm_zone UTM zone in which to calculate X, Y coordinates
#' @keywords utm ll
#' @export
#' @examples ll_to_utm(df, long = "LONG", lat = "LAT", utm_zone = 15)

utm_to_ll <- function(df, utm_zone = 15, x = "X", y = "Y"){
library(dplyr)
library(proj4)

#Ensure input is a data frame.
if(!is.data.frame(df)) stop("df must be a data frame")

#Convert longitude and latitude coordinates to UTM Zone 15 x and y coordinates in meters.
xy <- df[,c(x,y)]
proj_string <- paste0("+proj=utm +zone=",utm_zone," +datum=NAD83 +units=m +no_defs")
ll <- xy %>% project(proj_string, inverse = TRUE)
names(ll) <- c("LONG","LAT")
df <- cbind(df,ll)

return(df)
}