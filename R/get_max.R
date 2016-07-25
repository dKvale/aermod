# Get max dispersion result from AERMOD output

data <- read_aer_out(file_name)

# Get max outside of property boundary
max_list <- data %>% 
            mutate(dist_m = round(sqrt((rec_x - src_x)**2 + (rec_y - src_y)**2))) %>%
            filter(dist_m >= 10) %>%
            group_by(group, type) %>%  
            filter(concentration == max(concentration))

#
