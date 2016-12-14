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
if(nrow(data) < 1 & nrow(sources) < 1) return("Data frame is empty. AERMOD requires at least 1 emission source")

  
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

if(nchar(co$subtitle) > 0) {
  inp_text <- paste0(inp_text, "   TITLETWO ", co$subtitle, "\n")
}

inp_text <- paste0(inp_text, "   MODELOPT ", paste(co$model_opt, collapse = " "), "\n")
inp_text <- paste0(inp_text, "   AVERTIME ", paste(co$avg_time, collapse = " "), "\n")

if(!is.na(co$urban_opt) & nchar(co$urban_opt) > 0) {
  inp_text <- paste0(inp_text, "   URBANOPT ", paste(co$urban_opt, collapse = " "), "\n")
}

inp_text <- paste0(inp_text, "   POLLUTID ", co$pollutant_id, "\n")

#if(!is.na(co$half_life) &  nchar(as.character(co$half_life)) > 0)  {
#  inp_text <- paste0(inp_text, "   HALFLIFE ", co$half_life, "\n")
#}

#if(!is.na(co$decay_coef) & nchar(as.character(co$decay_coef)) > 0) {
#  inp_text <- paste0(inp_text, "   DCAYCOEF ", co$decay_coef, "\n")
#} 

if(!is.na(co$flagpole) & nchar(as.character(co$flagpole))> 0) {
  inp_text <- paste0(inp_text, "   FLAGPOLE ", co$flagpole, "\n")
}

inp_text <- paste0(inp_text, "   RUNORNOT RUN\n")
  
inp_text <- paste0(inp_text, section, " FINISHED \n")
 

## Source pathway ##
section <- "SO"

section_head <- "Source pathway"

inp_text <- paste0(inp_text, comment_line, " ", section_head, "\n", comment_line, "\n")

inp_text <- paste0(inp_text, section, " STARTING \n")

op <- paste0(op, "** Source Locations **\n")
op <- paste0(op, "**          SourceID     Type       X Coord     Y Coord          Elevat **\n")
op <- paste0(op, paste0("   LOCATION ",        
                        fw(object@ID, width = 13),
                        fw(object@TYPE, width = 11),
                        fw(object@XCOORD, width = 12),
                        fw(object@YCOORD, width = 17),
                        object@ELEV, "\n",
                        "** DESCRSRC ", object@DESCRSRC, "\n", 
                        collapse=""))

op <- paste0(op, "\n** Source Parameters **\n")
op <- paste0(op, "**          SourceID     g/s  (ht,m)   (temp,K) (vel,m/s) (diam,m) **\n")
op <- paste0(op, paste0("   SRCPARAM ", 
                        fw(object@ID, width = 13),
                        fw(object@EMISS, width = 5),
                        fw(object@HEIGHT, width = 9),
                        fw(object@TEMPK, width = 9),
                        fw(object@VELOCITY, width = 10),
                        object@DIAMETER, "\n",
                        collapse=""))

op <- paste0(op, "\n** Building Downwash **\n",
             "** The building downwash file is attached by the INCLUDED statement\n")
op <- paste0(op, "   INCLUDED ", object@DOWNFILE, "\n")

op <- paste0(op, "\n** Source Groups Defined\n")
op <- paste0(op, paste0("   SRCGROUP ", 
                        fw(object@GROUPID, width = 5),
                        ifelse(object@GROUPID == "ALL", "", object@GROUPSRC),"\n", 
                        collapse=""))

inp_text <- paste0(inp_text, section, " FINISHED \n")


## Receptor pathway ##
section <- "RE"

section_head <- "Receptor pathway"

inp_text <- paste0(inp_text, comment_line, " ", section_head, "\n", comment_line, "\n")

inp_text <- paste0(inp_text, section, " STARTING \n")



inp_text <- paste0(inp_text, section, " FINISHED \n")


## Meteorology Pathway ##
section <- "ME"

section_head <- "Meteorology Pathway"

inp_text <- paste0(inp_text, comment_line, " ", section_head, "\n", comment_line, "\n")

inp_text <- paste0(inp_text, section, " STARTING \n")



inp_text <- paste0(inp_text, section, " FINISHED \n")


## Output Pathway ##
section <- "OU"

section_head <- "Output Pathway"

out <- paste0(out, comment_line, " ", section_head, "\n", comment_line, "\n")

out <- paste0(out, section, " STARTING \n")



out <- paste0(out, section, " FINISHED \n")



# Return results
cat("\nGenerated input file: \n\n")
invisible(writeLines(inp))
  
if(is.null(path) | nchar(path) < 1) {
  return(inp)
} else  writeLines(inp, path)
  
}

