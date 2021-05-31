#' @title makeSRCPARAMtext 
#' @description Make the SRCPARAM subsection within the SO section of an Aermod input file 
#' @param so tibble resulting from source_df() function
#' @param zeros Logical. Use 0 for  g/s, temp_K, vel_m/s  to use with .EMI file (see addFileRef option in writeSRCPARAMtext)
#' @importFrom  receptors fw
#' @return Character string with SRCPARAM section

makeSRCPARAMtext <- function(so, zeros = FALSE, refFilePath = NULL, refFilePathRelative = FALSE){
  head <- paste0("\n** Source Parameters **\n", 
                 "**          source_id    g/s  ht_m     temp_K   vel_m/s   diameter_m **\n")
  out <- paste0(head, paste0("   SRCPARAM ", 
                      fw(so$source_id, 13), 
                      fw(ifelse(zeros, 0, round(so$emit_gs, 4)), 5), 
                      fw(round(so$height_m,  4), 9), 
                      fw(ifelse(zeros, 0, round(so$temp_k, 3)), 9), 
                      fw(ifelse(zeros, 0, round(so$velocity_ms, 4)), 10), 
                      round(so$diameter_m, 4), "\n", collapse = ""))
  
  if (zeros){
    refline <- makeHOUREMISrefline(path = refFilePath, 
                        ids = so$source_id, 
                        relative = refFilePathRelative)
    out <- paste0(out, "\n", refline)
  }
  out
}
