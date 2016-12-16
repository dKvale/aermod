#' Write a data frame to an AERMOD input file.
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
#' @export
#' @examples
#' aermod_inp <- new_aermod()
#' 
#' write_aermod(data = aermod_inp, path = "aermod.inp")
# 
# 

write_aermod <- function(data      = data.frame(), 
                         path      = "aermod.inp",
                         control   = data.frame(),
                         sources   = data.frame() ,
                         receptors = data.frame(),
                         met       = data.frame(),
                         out       = data.frame()
) {

# Data tests    
if((is.null(data) & is.null(sources)) || (nrow(data) < 1 & nrow(sources) < 1)) {
  return("Data frame is empty. AERMOD requires at least 1 emission source")
}
  
# Create temp tables
if(is.null(control) || nrow(control) < 1) {co <- data[1, ]} else {co <- control}

if(is.null(sources) || nrow(sources) < 1) {so <- data} else {so <- sources}

if(is.null(receptors) || nrow(receptors) < 1) {re <- data[1, ]} else {re <- receptors}

if(is.null(met) || nrow(met) < 1) {me <- data[1, ]} else {me <- met}

if(is.null(out) || nrow(out) < 1) {ou <- data[1, ]} else {ou <- out}
  
  
# Create text file
comment_line <- "****************************************\n**"

indent       <- "   "
  

## Control pathway ##
section <- "CO"

section_head <- "Control pathway"
    
inp_text <- paste0(comment_line, " ", section_head, "\n", comment_line, "\n")

inp_text <- paste0(inp_text, section, " STARTING \n")

inp_text <- paste0(inp_text, "   TITLEONE ", co$title, "\n")

if(!is.null(co$subtitle) & !is.na(co$subtitle) & nchar(co$subtitle) > 0) {
  inp_text <- paste0(inp_text, "   TITLETWO ", co$subtitle, "\n")
}

inp_text <- paste0(inp_text, 
                   "   MODELOPT ", paste(co$model_opt, collapse = " "), "\n",
                   "   AVERTIME ", paste(co$avg_time, collapse = " "), "\n")

if(!is.null(co$urban_opt) & !is.na(co$urban_opt) & nchar(co$urban_opt) > 0) {
  inp_text <- paste0(inp_text, "   URBANOPT ", paste(co$urban_opt, collapse = " "), "\n")
}

inp_text <- paste0(inp_text, "   POLLUTID ", co$pollutant_id, "\n")

#if(!is.na(co$half_life) &  nchar(as.character(co$half_life)) > 0)  {
#  inp_text <- paste0(inp_text, "   HALFLIFE ", co$half_life, "\n")
#}
#if(!is.na(co$decay_coef) & nchar(as.character(co$decay_coef)) > 0) {
#  inp_text <- paste0(inp_text, "   DCAYCOEF ", co$decay_coef, "\n")
#} 

if(!is.null(co$flagpole) & !is.na(co$flagpole) & nchar(as.character(co$flagpole)) > 0) {
  inp_text <- paste0(inp_text, "   FLAGPOLE ", co$flagpole, "\n")
}

inp_text <- paste0(inp_text, 
                   "   RUNORNOT RUN\n",
                   section, " FINISHED \n**\n")
 

## Source pathway ##
section <- "SO"

section_head <- "Source pathway"

inp_text <- paste0(inp_text, comment_line, " ", section_head, "\n", comment_line, "\n")

inp_text <- paste0(inp_text, section, " STARTING \n")

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
                   receptors::fw(so$emissions,   5),
                   receptors::fw(so$height_m,    9),
                   receptors::fw(so$temp_k,      9),
                   receptors::fw(so$velocity_ms, 10),
                   so$diameter_m, "\n",
                   collapse = ""))

if(!is.null(so$downwash_file) & !is.na(so$downwash_file) & nchar(as.character(so$downwash_file)) > 0) {
  
  inp_text <- paste0(inp_text, "\n** Building Downwash **\n",
                     "** The building downwash file is attached by the INCLUDED statement below.\n")

  inp_text <- paste0(inp_text, "   INCLUDED ", so$downwash_file, "\n")
}

inp_text <- paste0(inp_text, "\n** Source groups defined\n")

# Test table
#so <- source_tbl() %>% rbind(source_tbl()) 
#so[2, c(1,14)] <- c("SV02", "SV02")
#so <- mutate(so, group_id = list(c("ALL","SV01"), c("ALL", "SV02")))

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

inp_text <- paste0(inp_text, comment_line, " ", 
                   section_head, "\n", 
                   comment_line, "\n")

inp_text <- paste0(inp_text, section, " STARTING \n")

if(!is.null(re$recept_file) & !is.na(re$recept_file) & nchar(as.character(re$recept_file)) > 0) {
  
   inp_text <- paste0(inp_text, 
                      "** The receptor file is attached by the INCLUDED statement below.\n",
                      "   INCLUDED ", re$recept_file, "\n")
}

if(!is.null(re$recept_as_text) & !is.na(re$recept_as_text) & nchar(as.character(re$recept_as_text)) > 0) {
  
  inp_text <- paste0(inp_text, "** Locations of additional receptors are shown below.\n")
  
  inp_text <- paste0(inp_text, re$recept_as_text, "\n")
}

inp_text <- paste0(inp_text, section, " FINISHED \n**\n")


## Meteorology Pathway ##
section <- "ME"

section_head <- "Meteorology Pathway"

inp_text <- paste0(inp_text, comment_line, " ", section_head, "\n", comment_line, "\n")

inp_text <- paste0(inp_text, section, " STARTING \n",
      
                   "   SURFFILE ", me$surf_file, "\n",

                   "   PROFFILE ", me$prof_file, "\n",

                   "   SURFDATA ", me$surf_site_info , "\n",

                   "   UAIRDATA ", me$upper_air_info, "\n",

                   "   PROFBASE ", me$base_elev_m, "\n")

# Check for start and end date
if(!is.null(me$start_date) & !is.na(me$start_date) & nchar(me$start_date) > 6) {
  if(!is.null(me$end_date) & !is.na(me$end_date) & nchar(me$end_date) > 6) {
    
  inp_text <- paste0(inp_text, "   STARTEND ", paste(me$start_date, me$end_date), "\n")
  }
}

inp_text <- paste0(inp_text, section, " FINISHED \n**\n")


## Output Pathway ##
section <- "OU"

section_head <- "Output Pathway"

inp_text <- paste0(inp_text, comment_line, " ", section_head, "\n", comment_line, "\n")

inp_text <- paste0(inp_text, section, " STARTING \n")

inp_text <- paste0(inp_text, 
                   "   RECTABLE ", ou$rect_table, "\n",
                   "   MAXTABLE ", ou$max_table, "\n",
                   "   DAYTABLE ", ou$day_table, "\n",
                   "   PLOTFILE ", ou$plot_file, "\n")

inp_text <- paste0(inp_text, section, " FINISHED \n**\n")


# Return results
cat("\nGenerated input file: \n\n")
invisible(writeLines(inp_text))
  
if(is.null(path) | nchar(path) < 1) {
  return(inp)
} else  writeLines(inp_text, path)
  
}

