#' Check whether a point falls over a given land use type
#' 
#' Input a lat, long location and a land use category and return
#' TRUE if the point falls within that category
#' @param lat Latitude as WGS84 (e.g. 46.34) 
#' @param long Longitude as WGS84 (e.g. -90.34) 
#' @param category Land use category to check for.  Can be "developed", "undeveloped", "agriculture", "forest", "shrub", or "water".
#' @keywords is_over landuse land use
#' @export 
#' @examples 
#' is_over(lat = 45.121, long = -96.342, category = "developed")
#' is_over(lat = 46.152, long = -93.720, category = "undeveloped")
#' is_over(lat = 46.152, long = -93.720, category = "water")
#' is_over(lat = 46.139, long = -93.705, category = "water")
#' is_over(lat = 46.071, long = -93.668, category = "developed")
#' is_over(lat = 46.071, long = -93.668, category = "undeveloped")

is_over <- function(lat, long, category) {
  
  library(dplyr)
  
  if(is.null(lat)) stop("Location must include a latitude.")
  if(is.null(long)) stop("Location must include a longitude.")
  
  if(long > 180.0 || long < -180.0) stop("Longitude is out of range.")
  
  if(lat > 90.0 || lat < -90.0) stop("Latitude is out of range.")
  
  if(!(category %in% c("developed", "undeveloped", "agriculture", "forest", "water"))) stop("Landuse type can be \"developed\", \"agriculture\", \"forest\", or \"water\"")

  # Add landuse data to package
  #  nlcd_path <- system.file("data-raw", "nlcd_2011_landcover_mn_usgs1.tif", package = "aermod")
  #nlcd <- raster::raster("data-raw/nlcd_2011_landcover_mn_usgs1.tif")
  #nlcd_2011 = nlcd; save(nlcd_2011, file = "data/nlcd_2011.Rdata")
  
  nlcd <- aermod::nlcd_2011
  
  # Convert longitude and latitude coordinates to UTM Zone 15 x and y coordinates in meters.
  #nlcd_proj <- toString(nlcd@crs)
  nlcd_proj <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
  
  #xy <- c(long, lat) %>% proj4::project(nlcd_proj)
  
  xy <- sf::st_as_sf(data_frame(long2 = long, lat2 = lat), coords = c("long2", "lat2"), crs = 4326) %>% 
          sf::st_transform(nlcd_proj) %>% 
          sf::st_coordinates()
  
  lu_point <- raster::extract(nlcd, xy)
  
  lu_point <- as.integer(lu_point)
  
  lu_cat <- dplyr::case_when(
              lu_point == 0 ~ "unclassified",
              lu_point == 11 ~ "water",
              lu_point >= 21 & lu_point <=24 ~ "developed",
              lu_point == 31 ~ "barren",
              lu_point >= 41 & lu_point <= 43 ~ "forest",
              lu_point == 52 | lu_point == 71 ~ "shrub",
              lu_point == 81 | lu_point == 82 ~ "agriculture",
              lu_point == 90 | lu_point == 95 ~ "wetlands"
  )
  
  lu_check <- FALSE
  
  if (category == "undeveloped") {
    lu_check <- lu_cat != "developed"
  } else {
    lu_check <- category == lu_cat
  }
  return(lu_check)
}
