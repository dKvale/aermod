#' Get max dispersion result from AERMOD output file
#' 
#' Read maximum dispersion results from AERMOD output file into a data frame.
#' @importFrom magrittr "%>%"

get_max <- function(data              = as.character(NA),
                    property_boundary = as.numeric(NA)) {

  max_list <- data %>% 
              dplyr::mutate(dist_m = round(sqrt((rec_x - src_x)**2 + (rec_y - src_y)**2))) %>%
              dplyr::filter(dist_m >= property_boundary) %>%
              dplyr::group_by(group, type) %>%  
              dplyr::filter(concentration == max(concentration, na.rm = T))

}