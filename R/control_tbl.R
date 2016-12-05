#' Control options
#'
#' Create an input table of AERMOD control options.
#' @param TITLEONE Title of AERMOD run.
#' @param MODELOPT AERMOD run options. Separate multiple options by comma.
#' @keywords control options aermod input
#' @export
#' @examples
#' control_tbl(TITLEONE = "Demo Energy Station")
#'
#' TITLEONE            TITLETWO   MODELOPT     AVERTIME      URBANOPT POLLUTID HALFLIFE DCAYCOEF FLAGPOLE EVENTFIL SAVEFILE INITFILE MULTYEAR DEBUGOPT ERRORFIL RUNORNOT
#' Demo Energy Station AERMOD Run DFAULT, CONC 1, 24, ANNUAL          OTHER                                                                                     TRUE 
#


control_tbl <- function(TITLEONE = "Demo Energy Station",
                        TITLETWO = "AERMOD Run",
                        MODELOPT = "DFAULT, CONC",
                        AVERTIME = "1, 24, ANNUAL",
                        URBANOPT = "",
                        POLLUTID = "OTHER",
                        HALFLIFE = "",
                        DCAYCOEF = "",
                        FLAGPOLE = "",
                        EVENTFIL = "",
                        SAVEFILE = "",
                        INITFILE = "",
                        MULTYEAR = "",
                        DEBUGOPT = "",
                        ERRORFIL = "",
                        RUNORNOT = TRUE
) {
 
  df <- data.frame(TITLEONE = TITLEONE, 
                   TITLETWO = TITLETWO,
                   MODELOPT = MODELOPT,
                   AVERTIME = AVERTIME,
                   URBANOPT = URBANOPT,
                   POLLUTID = POLLUTID,
                   HALFLIFE = HALFLIFE,
                   DCAYCOEF = DCAYCOEF,
                   FLAGPOLE = FLAGPOLE,
                   EVENTFIL = EVENTFIL,
                   SAVEFILE = SAVEFILE,
                   INITFILE = INITFILE,
                   MULTYEAR = MULTYEAR,
                   DEBUGOPT = DEBUGOPT,
                   ERRORFIL = ERRORFIL,
                   RUNORNOT = RUNORNOT,
                   stringsAsFactors = F)
  
  return(df)
}

##


