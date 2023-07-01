#' @title source_df_hour
#' @description  Create HOUREMIS file from dataframe containing date, sourceID, QS, TS, VS
#' @param df Dataframe
#' @param outname. Character
#' @import tidyverse
#' @export

source_df_hour <- function(emi){
  if (!all(colnames(emi) %in% c("date", "sourceID", "QS", "TS", "VS"))) stop('Names must be c("date", "sourceID", "QS", "TS", "VS")')
 emi %>% 
    mutate(
      pre = fw("SO HOUREMIS", 12),
      datestring = formatAermodDate(date),
      id = fw(sourceID, 14, after = TRUE),
      QS = fw(format(round(QS, 2), nsmall = 2), 11, after = TRUE),
      TS = fw(format(round(TS, 1), nsmall = 1), 10, after = TRUE),
      VS = fw(format(round(VS, 2), nsmall = 2), 5,  after = FALSE),
      all = paste0(pre, datestring, id, QS, TS, VS)
      ) %>% 
    select(all)
}
