#' Earwig day degree simulations
#'
#' This is an example of how to run a number of day degree models with error around the parameters.
#' The names for the stages in the output are one stage along from the model names, this is because the model reflects that stage at end of development, whereas the model output reflects what stage would be observable.
#' Moult 4 is an adult earwig, so that is the stage observable following the completion of the model.
#' 
#' @param temps Dataframe generated from \code{day_degree_simple}
#' @param begin.date Date to begin model runs
#' @param date.error How many days +/- of begin.date to sample from
#' @param one.sided True/False flag for if dates can only be selected BEFORE observation
#' @param reps Number or replicates
#' @param egg True/False flag of whether to include egg development 
#' @return A formatted data.frame with model outputs for each replicate
#' @export

earwig_dd <- function (temps=temps, 
                       begin.date=Sys.Date(), 
                       date.error=2,
                       one.sided=TRUE,
                       reps=10, 
                       egg=TRUE){
  
  outdata <- data.frame()
  
  begin.date <- as.Date(begin.date, origin="1970-01-01")
  
  new.date <- as.Date(runif (1, min=begin.date-date.error, max=begin.date+date.error), origin="1970-01-01")
  if (one.sided==TRUE){
  new.date <- as.Date(runif (1, min=begin.date-date.error, max=begin.date), origin="1970-01-01")
  }
  
  pb = txtProgressBar(min = 1, max = reps, width=20, initial = 1)
  
  for (idx in 1:reps){
    
    if (egg==TRUE){
      hatchling <- cbind(day_degree_simple(rnorm(1, 5.3, 0.8), 19, tempvec=temps, start.date=new.date), "Stage" ="egg")
      moult1 <- cbind(day_degree_simple(rnorm(1, 7.5, 0.7), 23.3, temps, start.date=min(hatchling$Date[hatchling$CDD > rnorm(1, 165.7, 14.9)])), "Stage" ="hatchling")
      moult2 <- cbind(day_degree_simple(rnorm(1, 8.0, 0.5), 28, temps, start.date=min(moult1$Date[moult1$CDD > rnorm(1, 101.3, 11.5)])), "Stage" ="moult1")
      moult3 <- cbind(day_degree_simple(rnorm(1, 8.7, 0.8), 28, temps, start.date=min(moult2$Date[moult2$CDD > rnorm(1, 87, 12)])), "Stage" ="moult2")
      moult4 <- cbind(day_degree_simple(rnorm(1, 8.4, 1.7), 28, temps, start.date=min(moult3$Date[moult3$CDD > rnorm(1, 90.7, 20.2)])), "Stage" ="moult3")
      
      all_data <- cbind(rbind(hatchling[hatchling$Date < min(moult1$Date),], 
                              moult1[moult1$Date < min(moult2$Date),], 
                              moult2[moult2$Date < min(moult3$Date),],
                              moult3[moult3$Date < min(moult4$Date),],
                              moult4[moult4$Date < min(moult4$Date[moult4$CDD > rnorm(1, 141.6, 23.1)]),]), "run"= paste(idx))
    }else{
      moult1 <- cbind(day_degree_simple(rnorm(1, 7.5, 0.7), 23.3, temps, start.date=new.date), "Stage" ="hatchling")
      moult2 <- cbind(day_degree_simple(rnorm(1, 8.0, 0.5), 28, temps, start.date=min(moult1$Date[moult1$CDD > rnorm(1, 101.3, 11.5)])), "Stage" ="moult1")
      moult3 <- cbind(day_degree_simple(rnorm(1, 8.7, 0.8), 28, temps, start.date=min(moult2$Date[moult2$CDD > rnorm(1, 87, 12)])), "Stage" ="moult2")
      moult4 <- cbind(day_degree_simple(rnorm(1, 8.4, 1.7), 28, temps, start.date=min(moult3$Date[moult3$CDD > rnorm(1, 90.7, 20.2)])), "Stage" ="moult3")
      
      all_data <- cbind(rbind(moult1[moult1$Date < min(moult2$Date),], 
                              moult2[moult2$Date < min(moult3$Date),],
                              moult3[moult3$Date < min(moult4$Date),],
                              moult4[moult4$Date < min(moult4$Date[moult4$CDD > rnorm(1, 141.6, 23.1)]),]), "run"= paste(idx))
    }
    
    
    outdata <- rbind(outdata, all_data)
    setTxtProgressBar(pb,idx)
  }
  return (outdata)
}