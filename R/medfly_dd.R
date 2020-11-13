#' Medfly day degree model
#' 
#' Description to come....
#' 
#' @param temps Dataframe generated from \code{day_degree_simple}
#' @param begin.date Date to begin model runs
#' @return A formatted data.frame with model outputs for each replicate
#' @export

medfly_dd <- function (temps=temps, 
                       begin.date=Sys.Date()){
  
  
  begin.date <- as.Date(begin.date, origin="1970-01-01")
  
  egg <- cbind(day_degree_simple(9.3, 100, temps, start.date=begin.date), "Stage" ="egg")
  larva <- cbind(day_degree_simple(11.1, 100, temps, start.date=min(egg$Date[egg$CDD > 44], na.rm=T)), "Stage" ="larva")
  pupa <- cbind(day_degree_simple(8.4, 100, temps, start.date=min(larva$Date[larva$CDD > 162], na.rm=T)), "Stage" ="pupa")
  adult <- cbind(day_degree_simple(12.8, 100, temps, start.date=min(pupa$Date[pupa$CDD > 56], na.rm=T)), "Stage" ="adult")
  
  all_data <- cbind(rbind(egg[egg$Date < min(larva$Date),], 
                          larva[larva$Date < min(pupa$Date),],
                          pupa[pupa$Date < min(adult$Date),],
                          adult[adult$Date < min(adult$Date[adult$CDD > 36 ]),]))
  
  return (all_data)
}
