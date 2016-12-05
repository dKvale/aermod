# Get max dispersion result from AERMOD output


get_max <- function(data,
                    property_boundary) {

  max_list <- data %>% 
              dplyr::mutate(dist_m = round(sqrt((rec_x - src_x)**2 + (rec_y - src_y)**2))) %>%
              dplyr::filter(dist_m >= property_boundary) %>%
              dplyr::group_by(group, type) %>%  
              dplyr::filter(concentration == max(concentration))

}