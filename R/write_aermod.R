#' Write a data frame to an AERMOD input file
#'
#' Output an AERMOD input file from a data frame of AERMOD parameters.
#' @param data Data frame of AERMOD modeling parameters.
#' @param path Path to write to. Default is "aermod.inp".
#' @param control Data frame of control parameters. If blank, control parameters from \code{data} are used. 
#' @param sources Data frame of source parameters. If blank, source parameters from \code{data} are used.
#' @param receptors Data frame of receptor parameters. If blank, receptor parameters from \code{data} are used.
#' @param met Data frame of meteorology parameters. If blank, meteorology parameters from \code{data} are used. 
#' @param out Data frame of output parameters. If blank, output parameters from \code{data} are used. 
#' @keywords aermod write save input
#' @importFrom receptors "fw"
#' @export
#' @examples
#' aermod_inp <- new_aermod()
#' 
#' write_aermod(data = aermod_inp, path = "aermod.inp")
# 
# 
write_aermod <- function(data      = NULL, 
                         path      = NULL,
                         control   = NULL,
                         sources   = NULL,
                         receptors = NULL,
                         met       = NULL,
                         out       = NULL
) {

# Data tests    
if((is.null(data) || nrow(data) < 1) & (is.null(sources) || nrow(sources) < 1)) {
  stop("Data frame is empty. AERMOD requires at least 1 emission source.")
}
  
## Use pathway specific tables if provided
if(is.null(control) || !is.data.frame(control) || nrow(control) < 1) {co <- data[1, ]} else {co <- control[1, ]}

if(is.null(sources) || !is.data.frame(sources) || nrow(sources) < 1) {so <- data} else {so <- sources}

if(is.null(receptors) || !is.data.frame(receptors) || nrow(receptors) < 1) {re <- data[1, ]} else {re <- receptors[1, ]}

if(is.null(met) || !is.data.frame(met) || nrow(met) < 1) {me <- data[1, ]} else {me <- met[1, ]}

if(is.null(out) || !is.data.frame(out) || nrow(out) < 1) {ou <- data[1, ]} else {ou <- out[1, ]}

  
# Replace `NA` values with blanks to avoid printing 'NA'
co[is.na(co)] <- ""  

so[is.na(so)] <- ""  

re[is.na(re)] <- ""  

me[is.na(me)] <- ""  

ou[is.na(ou)] <- ""  


# Check if urban population at least 100
if(!is_valid(so$urban_pop, 1)) so$urban_pop <- NA

if(is_valid(so$urban_pop, 1) && min(so$urban_pop, na.rm = TRUE) < 100) {
  
  so$urban_pop <- sapply(so$urban_pop, function(x) ifelse(is_valid(x, 1) && x < 100, NA, x))
                      
  warning("A source is assigned an 'urban_pop' value below 100, the source will be modeled using rural dispersion coefficients.")
}


# Create text file

## Header
inp_text <- paste0("**\n",
                   "** AERMOD input file\n",
                   "** Created: ", format(Sys.Date(), "%m/%d/%Y"), "\n",
                   "**\n")  

## New section function
new_section  <- function(section_code   = section, 
                         section_header = section_head,
                         comment_line   = paste(paste(rep("*", 40), collapse = ""), "\n**")) {
  
  paste0(comment_line, 
           " ", section_header, "\n", 
           comment_line, "\n",
           section_code, " STARTING \n")
  }

## Control pathway ##
section <- "CO"
section_head <- "Control pathway"
    
inp_text <- paste0(inp_text, new_section())

inp_text <- paste0(inp_text, "   TITLEONE ", co$title, "\n")

inp_text <- paste0(inp_text, "   TITLETWO ", co$subtitle, "\n")

inp_text <- paste0(inp_text, 
                   "   MODELOPT ", paste(co$model_opt[[1]], collapse = " "), "\n",
                   "   AVERTIME ", paste(co$avg_time[[1]], collapse = " "), "\n")

if(is_valid(so$urban_pop, 1)) {
  
  inp_text <- paste0(inp_text,
                     paste0("   URBANOPT ",
                            if(length(so$source_id[sapply(so$urban_pop, function(x) is_valid(x))]) > 1) { 
                                   receptors::fw(substr(so$source_id[sapply(so$urban_pop, function(x) is_valid(x))], 1, 6), 7)
                              } else "",
                            so$urban_pop[sapply(so$urban_pop, function(x) is_valid(x))], 
                            collapse = "\n"), "\n")
}

inp_text <- paste0(inp_text, "   POLLUTID ", co$pollutant_id, "\n")

#if(!is.null(co$half_life) &  nchar(as.character(co$half_life)) > 0)  {
#  inp_text <- paste0(inp_text, "   HALFLIFE ", co$half_life, "\n")
#}
#if(!is.null(co$decay_coef) & nchar(as.character(co$decay_coef)) > 0) {
#  inp_text <- paste0(inp_text, "   DCAYCOEF ", co$decay_coef, "\n")
#} 

if(is_valid(co$flagpole, 1)) {
  inp_text <- paste0(inp_text, "   FLAGPOLE ", co$flagpole, "\n")
}

inp_text <- paste0(inp_text, 
                   "   RUNORNOT RUN\n",
                   section, " FINISHED \n**\n")
 

## Source pathway ##
section <- "SO"
section_head <- "Source pathway"

inp_text <- paste0(inp_text, new_section())

inp_text <- paste0(inp_text,
                   "** Source Locations **\n",
                   "**          source_id    type       x_coord     y_coord          elevation_m **\n")

inp_text <- paste0(inp_text, paste0("   LOCATION ",        
                   receptors::fw(so$source_id, 13),
                   receptors::fw(so$type,      11),
                   receptors::fw(so$x_coord,   12),
                   receptors::fw(so$y_coord,   17),
                   so$elevation_m, "\n",
                   "** DESCRSRC ", so$description, "\n", 
                   collapse = ""))

inp_text <- paste0(inp_text, 
                   "\n** Source Parameters **\n",
                   "**          source_id    g/s  ht_m     temp_K   vel_m/s   diameter_m **\n")

inp_text <- paste0(inp_text, paste0("   SRCPARAM ", 
                   receptors::fw(so$source_id,   13),
                   receptors::fw(so$emit_gs,   5),
                   receptors::fw(so$height_m,    9),
                   receptors::fw(so$temp_k,      9),
                   receptors::fw(so$velocity_ms, 10),
                   so$diameter_m, "\n",
                   collapse = ""))

if(is_valid(so$downwash_file[1], 1)) {
  
  inp_text <- paste0(inp_text, "\n** Building Downwash **\n",
                     "** The building downwash file is attached by the INCLUDED statement below.\n")

  inp_text <- paste0(inp_text, "   INCLUDED ", so$downwash_file[1], "\n")
}

inp_text <- paste0(inp_text, "\n** Source groups defined\n")

if(is_valid(so$urban_pop, 1)) {
  
  inp_text <- paste0(inp_text,
                     paste0("   URBANSRC ",
                            if(length(so$source_id[sapply(so$urban_pop, function(x) is_valid(x))]) > 1) { 
                              receptors::fw(substr(so$source_id[sapply(so$urban_pop, function(x) is_valid(x))], 1, 6), 7)
                            } else "",
                            receptors::fw(substr(so$source_id[sapply(so$urban_pop, function(x) is_valid(x))], 1, 6), 7),
                           collapse = "\n"), "\n")
}

# Split multiple group_ids
if(is.list(so$group_id)) so <- tidyr::unnest(so[ , c("source_id", "group_id")])

# Paste together sources assigned to the same group_id
for(group_x in unique(so$group_id)) {
  
  inp_text <- paste0(inp_text, paste0("   SRCGROUP ", 
                     receptors::fw(toupper(group_x), 5),
                     ifelse(toupper(group_x) == "ALL", "", toupper(paste(subset(so, group_id == group_x)$source_id))),
                     "\n", 
                     collapse = ""))
}

inp_text <- paste0(inp_text, section, " FINISHED \n**\n")


## Receptor pathway ##
section <- "RE"
section_head <- "Receptor pathway"

inp_text <- paste0(inp_text, new_section())

if(is_valid(re$recept_file, 1)) {
  
   inp_text <- paste0(inp_text, 
                      "** The receptor file is attached by the INCLUDED statement below.\n",
                      "   INCLUDED ", re$recept_file, "\n")
}

if(is_valid(re$recept_as_text, 1)) {
  
  inp_text <- paste0(inp_text, "** Locations of additional receptors are shown below.\n",
                     re$recept_as_text, "\n")
}

inp_text <- paste0(inp_text, section, " FINISHED \n**\n")


## Meteorology Pathway ##
section <- "ME"
section_head <- "Meteorology Pathway"

inp_text <- paste0(inp_text, new_section())

inp_text <- paste0(inp_text, 
                   "   SURFFILE ", me$surf_file, "\n",
                   "   PROFFILE ", me$prof_file, "\n",
                   "   SURFDATA ", me$surf_site_info, "\n",
                   "   UAIRDATA ", me$upper_air_info, "\n",
                   "   PROFBASE ", me$base_elev_m, "\n")

# Check for start and end date
if(is_valid(me$start_date, 6)) {
  if(is_valid(me$end_date, 6)) {
    inp_text <- paste0(inp_text, "   STARTEND ", paste(me$start_date, me$end_date), "\n")
  }
}

inp_text <- paste0(inp_text, section, " FINISHED \n**\n")


## Output Pathway ##
section <- "OU"
section_head <- "Output Pathway"

inp_text <- paste0(inp_text, new_section())

inp_text <- paste0(inp_text, 
                   ifelse(is_valid(ou$rect_table), paste0("   RECTABLE ", ou$rect_table, "\n"), ""),
                   ifelse(is_valid(ou$max_table),  paste0("   MAXTABLE ", ou$max_table, "\n"), ""),
                   ifelse(is_valid(ou$day_table),  paste0("   DAYTABLE ", ou$day_table, "\n"), ""),
                   ifelse(is_valid(ou$file_form),  paste0("   FILEFORM ", ou$file_form, "\n"), ""),
                   ifelse(is_valid(ou$rank_file),  paste0("   RANKFILE ", ou$rank_file, "\n"), ""),
                   ifelse(is_valid(ou$plot_file),  paste0("   PLOTFILE ", ou$plot_file, "\n"), ""))

inp_text <- paste0(inp_text, section, " FINISHED \n**\n")


# Return results
cat("\nGenerated input file: \n\n")
invisible(writeLines(inp_text))
  
if(!is_valid(path)) {
  
  return(inp_text)
  
} else  writeLines(inp_text, path)
  
}

