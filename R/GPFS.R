#' Generic Pest Forecast System (GPFS)
#'
#' Model is described in Hong et al. (2015). The population index each hour is based on the sum of a development rate (scaled by
#' the number of generations to reach maximum population) while removing the proportions
#' of the population killed by high and low temperatures and by wet soil moisture.
#'
#' \code{P[n] <- (P[n-1] + (beta8 * d)) * (1-mlt) * (1-mht)}
#'
#' @param Tmin Temperature minimum
#' @param Tmax Temperature maximum
#' @param Topt1 Low optimum temperature
#' @param Topt2 High optimum temperature
#' @param Tmht Threshold - High temperature mortality
#' @param Tmlt Threshold - Low temperature mortality
#' @param beta1 Constant for low temperature mortality
#' @param beta2 Coefficient for mlt
#' @param beta3 Coefficient for mlt
#' @param beta4 Constant for mht
#' @param beta5 Coefficient of degree one for mht
#' @param beta6 Coefficient of degree two for mht
#' @param beta7 Constant for Mwsm
#' @param beta8 Reciprocal of number of hours required to reach maximum population for \code{P(n)}
#' @param Mwsm Wet soil moisture mortality
#' @param precipitation T/F flag for whether to include the moisture parameters in the model
#' @return A data.frame with 7 columns showing parameters for Ta, P, d, mlt, mht and mwsm for hourly time steps over the data provided
#' @references Hong, S. C., Magarey, R., Borchert, D. M., Vargas, R. I., & Souder, S. (2015). Site-specific temporal and spatial validation of a generic plant pest forecast system with observations of Bactrocera dorsalis (oriental fruit fly). NeoBiota, 27, 37–67. https://doi.org/10.3897/neobiota.27.5177
#' @author Matt Hill
#' @export

GPFS.model <- function(weather.df=weather.df,
                      Tmin = 13.3,
                      Topt1 = 24,
                      Topt2 = 34,
                      Tmax = 41,
                      Tmht = 33,
                      Tmlt = 13.3,
                      beta1 = 1101.7,
                      beta2 = -49892,
                      beta3 = -162.9,
                      beta4 = 25.9595,
                      beta5 = -0.4959,
                      beta6 = 0,
                      beta7 = 50.4,
                      beta8 = 0.008,
                      precipitation=FALSE){

 # dev rate component
  dev.rate <- function (T=T){
    D <- 0
    if (T < Tmin | T > Tmax){D <- 0}else{
      if (Tmin < T & T < Topt1){D <- (T-Tmin)/((Topt1 - Tmin)*24)}else{
        if (Topt1 <= T & T <= Topt2){D <- (1/24)}else{
          if (T > Topt2 & T < Tmax) {D <- (T-Topt2)/((Topt2-Tmax)*24)}
        }
      }
    }
    return(D)
  }

  # Mortalitity due to lower temps
  Mlt <- function(T=T){
    if (T < Tmlt){
      return(1/(exp(beta1 + (beta2/(T + 273.2)) + beta3 * log(T+273.2))))
    }else{return(0)}
  }

# Mortality due to higher temps
  Mht <- function(T=T){
    if (T > Tmht){
      x <- (60/(exp(beta4 + beta5 * T + beta6 * T^2)))
      x <- ifelse(x < 1, x, 1)
      return(x)
    }else{return(0)}
  }

  # Wet soil moisture mortality
if (precipitation == TRUE){
  Mwsm <- function(prec=prec){

    P <- 0.36

    if (n < 24){
      if(prec[1:24] >= 10){
        x <- P * (1/beta7)
        return(x)
      }else{x=0}
    }else{
      if(prec[n-24:n] >= 10){
        x <- P * (1/beta7)
        return(x)
      }else{x=0}
    }
    return(x)
  }

  prec.24 <- weather.df$prec
  for (i in 1:length(weather.df$prec)){
    if (i <= 24){
      prec.24[i] <- sum(weather.df$prec[1:24])}else{
        prec.24[i] <- sum(weather.df$prec[(i-24):i])
      }
  }
}

  P <- seq(from=0, to=0, length=length(weather.df$Date))
  P.init <- 0.1

  if (precipitation == TRUE){
  output.df <- as.data.frame(cbind(seq(from=1, to=length(weather.df$Date), by=1), weather.df$Ta, prec.24, P, P, P, P, P))
  colnames (output.df) <- c("Step" , "Ta", "prec", "P", "d", "mlt", "mht", "mwsm")
  }else{
    output.df <- as.data.frame(cbind(seq(from=1, to=length(weather.df$Date), by=1), weather.df$Ta, P, P, P, P, P))
    colnames (output.df) <- c("Step" , "Ta", "P", "d", "mlt", "mht", "mwsm")
  }


  for (n in 1:length(P)){

    if (n == 1){
      output.df$P[n] <- P.init
    } else{
      output.df$d[n] <- dev.rate(weather.df$Ta[n])
      output.df$mlt[n] <- Mlt(weather.df$Ta[n])
      output.df$mht[n] <- Mht(weather.df$Ta[n])
    if (precipitation==TRUE){
      output.df$mwsm[n] <- Mwsm(prec.24[n])
      }
      x <- ((output.df$P[n-1] + (beta8 * output.df$d[n])) * (1-output.df$mlt[n]) * (1-output.df$mht[n]) * (1-output.df$mwsm[n]))
      output.df$P[n] <- ifelse(x <= 1, x, 1)
    }
    n <- n+1
  }
  return (output.df)
}










# for (T in 1:50){
#   x <- dev.rate(T)
#   y <- Mlt(T)
#   z <- Mht(T)
#   print(c(T, x, y, z))
# }

#
# In the absence of soil moisture data, the soil was defined as flooded if more than
# 10 mm of rain had fallen in the previous 24 hours

# first create a vector for rain in past 24 hours
# prec.24 <- prec.vec
# for (i in 1:length(prec.vec)){
#   if (i <= 24){
#     prec.24[i] <- sum(prec.vec[1:24])}else{
#       prec.24[i] <- sum(prec.vec[(i-24):i])
#     }
# }



















