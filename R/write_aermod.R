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
  return("Data frame is empty. AERMOD requires at least 1 emission source.")
}
  
## Use pathway specific tables if provided
if(is.null(control) || !is.data.frame(control) || nrow(control) < 1) {co <- data[1, ]} else {co <- control}

if(is.null(sources) || !is.data.frame(sources) || nrow(sources) < 1) {so <- data} else {so <- sources}

if(is.null(receptors) || !is.data.frame(receptors) || nrow(receptors) < 1) {re <- data[1, ]} else {re <- receptors}

if(is.null(met) || !is.data.frame(met) || nrow(met) < 1) {me <- data[1, ]} else {me <- met}

if(is.null(out) || !is.data.frame(out) || nrow(out) < 1) {ou <- data[1, ]} else {ou <- out}

  
# Replace `NA` values with blanks to avoid printing 'NA'
co[is.na(co)] <- ""  

so[is.na(so)] <- ""  

re[is.na(re)] <- ""  

me[is.na(me)] <- ""  

ou[is.na(ou)] <- ""  


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

## Test length function
is_min_length <- function(x, length = 1) {
  
  if(is.null(x) || is.na(x)) return(FALSE) 
  
  if(is.character(x)) return(nchar(x) >= length)
  
  if(is.data.frame(x)) return(nrow(x) >= length)
}

## Control pathway ##
section <- "CO"
section_head <- "Control pathway"
    
inp_text <- paste0(inp_text, new_section())

inp_text <- paste0(inp_text, "   TITLEONE ", co$title[1], "\n")

inp_text <- paste0(inp_text, "   TITLETWO ", co$subtitle[1], "\n")

inp_text <- paste0(inp_text, 
                   "   MODELOPT ", paste(co$model_opt[1], collapse = " "), "\n",
                   "   AVERTIME ", paste(co$avg_time[1], collapse = " "), "\n")

if(is_min_length(co$urban_opt[1], 1)) {
  inp_text <- paste0(inp_text, "   URBANOPT ", paste(co$urban_opt[1], collapse = " "), "\n")
}

inp_text <- paste0(inp_text, "   POLLUTID ", co$pollutant_id[1], "\n")

#if(!is.null(co$half_life) &  nchar(as.character(co$half_life)) > 0)  {
#  inp_text <- paste0(inp_text, "   HALFLIFE ", co$half_life, "\n")
#}
#if(!is.null(co$decay_coef) & nchar(as.character(co$decay_coef)) > 0) {
#  inp_text <- paste0(inp_text, "   DCAYCOEF ", co$decay_coef, "\n")
#} 

if(is_min_length(co$flagpole[1], 1)) {
  inp_text <- paste0(inp_text, "   FLAGPOLE ", co$flagpole[1], "\n")
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
                   receptors::fw(so$emissions,   5),
                   receptors::fw(so$height_m,    9),
                   receptors::fw(so$temp_k,      9),
                   receptors::fw(so$velocity_ms, 10),
                   so$diameter_m, "\n",
                   collapse = ""))

if(is_min_length(so$downwash_file[1], 1)) {
  
  inp_text <- paste0(inp_text, "\n** Building Downwash **\n",
                     "** The building downwash file is attached by the INCLUDED statement below.\n")

  inp_text <- paste0(inp_text, "   INCLUDED ", so$downwash_file[1], "\n")
}

inp_text <- paste0(inp_text, "\n** Source groups defined\n")

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

if(is_min_length(re$recept_file[1], 1)) {
  
   inp_text <- paste0(inp_text, 
                      "** The receptor file is attached by the INCLUDED statement below.\n",
                      "   INCLUDED ", re$recept_file[1], "\n")
}

if(is_min_length(re$recept_as_text[1], 1)) {
  
  inp_text <- paste0(inp_text, "** Locations of additional receptors are shown below.\n",
                     re$recept_as_text[1], "\n")
}

inp_text <- paste0(inp_text, section, " FINISHED \n**\n")


## Meteorology Pathway ##
section <- "ME"
section_head <- "Meteorology Pathway"

inp_text <- paste0(inp_text, new_section())

inp_text <- paste0(inp_text, 
                   "   SURFFILE ", me$surf_file[1], "\n",
                   "   PROFFILE ", me$prof_file[1], "\n",
                   "   SURFDATA ", me$surf_site_info[1], "\n",
                   "   UAIRDATA ", me$upper_air_info[1], "\n",
                   "   PROFBASE ", me$base_elev_m[1], "\n")

# Check for start and end date
if(is_min_length(me$start_date[1], 6)) {
  if(is_min_length(me$end_date[1], 6)) {
    inp_text <- paste0(inp_text, "   STARTEND ", paste(me$start_date[1], me$end_date[1]), "\n")
  }
}

inp_text <- paste0(inp_text, section, " FINISHED \n**\n")


## Output Pathway ##
section <- "OU"
section_head <- "Output Pathway"

inp_text <- paste0(inp_text, new_section())

inp_text <- paste0(inp_text, 
                   ifelse(is_min_length(ou$rect_table[1]), paste0("   RECTABLE ", ou$rect_table[1], "\n"), ""),
                   ifelse(is_min_length(ou$max_table[1]),  paste0("   MAXTABLE ", ou$max_table[1], "\n"), ""),
                   ifelse(is_min_length(ou$day_table[1]),  paste0("   DAYTABLE ", ou$day_table[1], "\n"), ""),
                   ifelse(is_min_length(ou$plot_file[1]),  paste0("   PLOTFILE ", ou$plot_file[1], "\n"), ""))

inp_text <- paste0(inp_text, section, " FINISHED \n**\n")


# Return results
cat("\nGenerated input file: \n\n")
invisible(writeLines(inp_text))
  
if(!is_min_length(path)) {
  
  return(inp_text)
  
} else  writeLines(inp_text, path)
  
}

