##### Weather functions

#' Simple overwintering function - vector based
#'
#' Switch for overwintering. This calculates cumulative days less than 18 degrees (5 needed to enter)
#' Currently calculated on the Ta (Average temperature for the day - based on the interpolated temp calculations)
#' @export

overwinter_vector <- function(tempvec = tempvec, datevec = datevec) {

    x <- as.data.frame(tempvec)
    colnames(x) <- "Ta"

    x$cumd <- 0
    x$cumd[x$Ta < 18] <- 1

    for (i in 2:nrow(x)) {

        if (x$cumd[i] != 0) {
            x$cumd[i] <- x$cumd[i] + x$cumd[i - 1]
        } else {
            (x$cumd[i] == 0)
        }
    }

    ## Switch for exiting overwintering. Calcualtes cumulative days above 18 degrees (5 needed to exit) Currently
    ## calculated on the Ta (Average temperature for the day - based on the interpolated temp calculations)

    x$cume <- 0
    x$cume[x$Ta >= 18] <- 1

    for (i in 2:nrow(x)) {

        if (x$cume[i] != 0) {
            x$cume[i] <- x$cume[i] + x$cume[i - 1]
        } else {
            (x$cume[i] == 0)
        }
    }

    ## Overwintering calculations. Basically runs down the dataframe, sets the condition to 1 once the 5 days below
    ## 18 are accumlated.  Then goes though, and for every day that is below the 5 days to exit overwintering, and
    ## the day before was also overwinter, these get set to overwintering as well.  The remaining days (0) reflect
    ## times after 5 days of above 18 in a row until the next event of 5 days below 18.

    # Initial condition is 0 (no overwintering)
    x$overwinter <- 0

    # First condition is set to 2 because it cannot be accumation of either, and needs to be positive for xx - 1.
    xx <- 2

    while (xx <= nrow(x)) {
        if (x$cumd[xx] >= 5 & x$cume[xx] < 5) {
            x$overwinter[xx] <- 1
        }
        if (x$cume[xx] < 5 & x$overwinter[xx - 1] == 1) {
            x$overwinter[xx] <- 1
        }
        xx <- xx + 1
    }

    x <- cbind(as.Date(datevec), tempvec, x[, 4])
    colnames(x) <- c("Date", "Temp", "Overwinter")

    return(x)
}


#' Simple day degree model
#'
#' Switch for overwintering. This calculates cumulative days less than 18 degrees (5 needed to enter)
#' Currently calculated on the Ta (Average temperature for the day - based on the interpolated temp calculations)
#' @export
day_degree_simple <- function(tlow = 4.5, thigh = 100, tempvec = tempvec, start.date = "2015-08-31") {

    degday <- tempvec[tempvec$Date > start.date, ]
    degday$AppThresLow <- tlow
    degday$AppThresHigh <- thigh

    degday$DD_low <- ifelse((degday$Tmax < degday$AppThresLow), 0, ifelse(degday$Tmin > degday$AppThresLow, ((degday$Tmax +
        degday$Tmin)/2) - degday$AppThresLow, ((degday$Tmax - degday$AppThresLow)/2) * ((degday$Tmax - degday$AppThresLow)/(degday$Tmax -
        degday$Tmin))))

    degday$DD_high <- ifelse((degday$Tmax < degday$AppThresHigh), 0, ifelse(degday$Tmin > degday$AppThresHigh,
        ((degday$Tmax + degday$Tmin)/2) - degday$AppThresHigh, ((degday$Tmax - degday$AppThresHigh)/2) * ((degday$Tmax -
            degday$AppThresHigh)/(degday$Tmax - degday$Tmin))))

    degday$DD <- degday$DD_low - degday$DD_high

    degday$CDDlow <- degday$DD_low[1]
    x = 2
    while (x <= length(degday$DD_low)) {

        y = x - 1
        degday$CDDlow[x] <- degday$CDDlow[y] + degday$DD_low[x]
        x = x + 1
    }

    degday$CDD <- degday$DD[1]
    x = 2
    while (x <= length(degday$DD)) {

        y = x - 1
        degday$CDD[x] <- degday$CDD[y] + degday$DD[x]
        x = x + 1
    }

    degday$step <- seq(from = 1, to = length(degday[, 1]), by = 1)


    return(degday)
}

#' Hourly temperature interpolation function
#'
#' This function interpolates the daily minimum and maximum temperature values to predict hourly temperature values.
#' The function uses the location, the \code{suncalc} package and a vector of dates to calculate solar noon and sunrise times (hour).
#' Adjusts the sine coefficients to the location information, and from there assumes Tmin is achieved at
#' sunrise-1 hour and Tmax is achieved at solar noon + 2 hours.
#'
#' Original calculations are found in chapter 2 of Campbell & Norman (1998)

#' @param climate A dataframe with columns of: (Date, Latitude, Longitude, Tmin, Tmax)
#' @export
#' @return An expanded data.frame of \code{climate} with 1 row per hour of each day provided, with corresponding temperature value
#' @author Madeleine Barton
#' @author Matt Hill
#' @references Campbell, G. S., & Norman, J. M. (1998). An Introduction to Environmental Biophysics. https://doi.org/10.1007/978-1-4612-1626-1


hourly_interpolate <- function(climate=climate){

    if ("DateCleared" %in% colnames (climate)){
        colnames(climate)[which(names(climate) == "DateCleared")] <- "Date"
    }

    if ("X" %in% colnames (climate)){
        colnames(climate)[which(names(climate) == "X")] <- "Longitude"
    }

    if ("Y" %in% colnames (climate)){
        colnames(climate)[which(names(climate) == "Y")] <- "Latitude"
    }

    lat.long <- c(climate$Latitude[1], climate$Longitude[1])

    #-----------------------------------------------------------------------------------------------------------
    #Convert Daily data to hourly data using the max/min temperatures and a sin equation
    #-----------------------------------------------------------------------------------------------------------
    numdays<-length(climate[,1])

    ## calculate solar noon for a give location using the suncalc package
    solar_params <- suncalc::getSunlightTimes(date = as.Date(climate$Date, origin="1970-01-01"),
                                              lat = lat.long[1], lon = lat.long[2], tz ="Australia/Sydney",
                                              keep=c("solarNoon", "sunrise"))

    climate$solar_noon <- solar_params$solarNoon
    climate$sunrise <- solar_params$sunrise

    ## calculate the time difference from solar noon
    time_diff <- climate$solar_noon

    ## set to 12:00, midday
    lubridate::hour(time_diff) <-12; lubridate::minute(time_diff) <-0; lubridate::second(time_diff) <-0

    ##calculate difference
    climate$t.adj <- as.numeric(difftime(climate$solar_noon,time_diff,units="hours"))

    climate<-climate[rep(seq_len(nrow(climate)), each=24),1:ncol(climate)]
    climate$hour<-rep((1:24),numdays)

    #total number of hours in the simulations
    numhours<-length(climate[,1])

    ## From equation 2.2
    w <- pi/12
    sin.coeff <- vector()
    for (i in 1:numhours){
        sin.coeff[i] <- 0.44-0.46*sin(w*(climate$hour[i]+climate$t.adj[i])+0.9)+0.11*sin(2*(w*(climate$hour[i]+climate$t.adj[i]))+0.9)
        print (i)
    }

    #Create vectors for processing

    Tmin<-climate$Tmin    #saving the minimum temperature as its own object, for easier use below
    Tmax<-climate$Tmax    #saving the maximum temperature as its own object, for easier use below
    hour <- climate$hour  #daily hours for iterations
    sunrise <- as.integer(format(round(climate$sunrise, units="hours"), format="%H")) # hour of sunrise - rounded to nearest hour
    solar_noon <- as.integer(format(round(climate$solar_noon, units="hours"), format="%H")) # hour of solar_noon - rounded to nearest hour

    ## Loop to iterate over all the hours in the dataset provided, and use the above vectors to calculate a "Ta" per hour

    for(i in 1:numhours){ ###START LOOP (note the + sign in the console (bottom left window))
        if(i>=25){
            j<-i-24 ## j refers to the max/min conditions of the previous day
        }else{
            j<-i} ##  the max/min are assumed to be the same as on that day

        if(i<=(numhours-24)){
            k<-i+24## 'j'refers to the  conditions of the following day (for the final day in the script, the max/min are assumed to be the same as on that day)
        }else{
            k<-i}##  the max/min are assumed to be the same as on that day
        if (hour[i]<(sunrise[i]-1)) {                                         #hours before sunrise-1
            Ta<-(sin.coeff[i]*Tmax[j])+((1-sin.coeff[i])*Tmin[i])}
        if((hour[i]>=(sunrise[i]-1)|hour[i]<=(solar_noon[i]+2))){                            #hours between sunrise-1 and solar noon + 2
            Ta<-(sin.coeff[i]*Tmax[i])+((1-sin.coeff[i])*Tmin[i])}
        if(hour[i]>(solar_noon[i]+2)){                                          #hours after solar noon + 2
            Ta<-(sin.coeff[i]*Tmax[i])+((1-sin.coeff[i])*Tmin[k])}
        if(i==1){
            hourly<-as.data.frame(Ta)
        }else{
            hourly<-rbind(hourly,Ta)
        }
        print(i)
    } ###END LOOP

    ##calculating the total number of hours in the simulation.
    julhour<-1:numhours

    ##creating a data table with the hourly temperatures and the total number of hours (NOTE: to bind columns, the length of the columns must always be the same)
    hour.climate.data<-as.data.frame(cbind(julhour, climate, hourly))

    hour.climate.data$hour <- seq(from =0, to=23, by=1)

    hour.climate.data <- hour.climate.data[,c("Date",  "Latitude", "Longitude", "Tmin", "Tmax", "Ta")]

    return (hour.climate.data)
}

#' Create a temperature vector
#'
#' Downloads Australian Bureau of Meterology data and formats into a data.frame for use in development models
#' This function uses the 3 nearest stations to patch in missing data. Uses the the \code{bomrang} package
#'
#' @param xy Coordinates (lat-long) for site to extract data from
#' @param datemin Earliest date to go back to get recrods for
#' @param datemax Latest date to get records for (default=current day)
#' @return A formatted data.frame with hourly temperature data and the date
#' @export

create_temp_vec <- function(xy = xy, datemin=Sys.Date()-365, datemax=Sys.Date()){
    get_temp_data <- function (station, datemin=datemin, datemax=datemax) {

         tmax <- as.data.frame(get_historical(station, type="max"))
        tmin <- as.data.frame(get_historical(station, type="min"))

        tmax$Date <- as.Date(paste0(tmax$year,"-", tmax$month,"-", tmax$day),"%Y-%m-%d")
        tmin$Date <- as.Date(paste0(tmin$year,"-", tmin$month,"-", tmin$day),"%Y-%m-%d")

        if (datemax == Sys.Date()){
            tmax <- tmax[tmax$Date > datemin,]
            tmin <- tmin[tmin$Date > datemin,]
        }else{
            tmax <- tmax[tmax$Date > datemin & tmax$Date < datemax,]
            tmin <- tmin[tmin$Date > datemin & tmin$Date < datemax,]
        }

        temp_df <- merge(tmax, tmin, by=c("station_number", "Date"))

        temp_df <-temp_df[,c("station_number", "Date", "min_temperature", "max_temperature")]

        colnames (temp_df) <- c("Station", "Date", "Tmin", "Tmax")
        temp_df$Date <- as.Date(temp_df$Date, origin = "1970-01-01")

        return (temp_df)

    }

    statid <- bomrang::sweep_for_stations(latlon=c(xy[[1]], xy[[2]]))$site[1:3]

    temp.dat1 <- get_temp_data(station=as.numeric(statid[1]), datemin=as.Date(datemin), datemax=as.Date(datemax))
    temp.dat2 <- get_temp_data(station=as.numeric(statid[2]), datemin=as.Date(datemin), datemax=as.Date(datemax))
    temp.dat3 <- get_temp_data(station=as.numeric(statid[3]), datemin=as.Date(datemin), datemax=as.Date(datemax))

    ## add to a list
    temp.dat.list <- list(temp.dat1, temp.dat2, temp.dat3)

    ## determine which is longest
    temp.dat <- temp.dat.list[[which.max(c(nrow(temp.dat1), nrow(temp.dat2), nrow(temp.dat3)))]]

    # fill data with nearest station
    temp.dat[match(temp.dat1$Date, temp.dat$Date), ] <- temp.dat1

    ## fill in blanks
    blanks.fill <- temp.dat2[which(temp.dat2$Date %in% temp.dat$Date[is.na(temp.dat$Tmin) | is.na (temp.dat$Tmax)]),]
    temp.dat[match(blanks.fill$Date, temp.dat$Date), ] <- blanks.fill

    ## again, just if there are still blanks
    blanks.fill <- temp.dat3[which(temp.dat3$Date %in% temp.dat$Date[is.na(temp.dat$Tmin) | is.na (temp.dat$Tmax)]),]
    temp.dat[match(blanks.fill$Date, temp.dat$Date), ] <- blanks.fill

    ## 'climate' should be a dataframe of (Date, Latitude, Longitude, Tmin, Tmax)
    climate <- cbind(temp.dat$Date, xy[1], xy[2], temp.dat[,c("Tmin", "Tmax")])
    colnames (climate) <- c("Date", "Latitude", "Longitude", "Tmin", "Tmax")

    m <- hourly_interpolate(climate)
    m$Date <- as.Date(m$Date)
    return(m)

}

#' Create a precipitation vector
#'
#' Downloads Australian Bureau of Meterology data and formats into a data.frame for use in development models
#' Uses the the \code{bomrang} package to obtain the data.
#'
#' @param xy Coordinates (lat-long) for site to extract data from
#' @param datemin Earliest date to go back to get recrods for
#' @param datemax Latest date to get records for (default=current day)
#' @return A formatted data.frame with hourly interpolated precipitation data and the dates.
#' @export


create_prec_vec <- function(xy = xy, datemin=Sys.Date()-365, datemax=Sys.Date()){
    statid <- sweep_for_stations(latlon=c(xy[[1]], xy[[2]]))$site[1:3]

    prec <- as.data.frame(get_historical(statid[1], type="rain"))


    prec$Date <- as.Date(paste0(prec$year,"-", prec$month,"-", prec$day),"%Y-%m-%d")

    if (datemax == Sys.Date()){
        prec <- prec[prec$Date > datemin,]
    }else{
        prec <- prec[prec$Date > datemin & prec$Date < datemax,]
    }

    prec.24 <- prec[rep(seq_len(nrow(prec)), each=24),1:ncol(prec)]

    for (i in unique(prec.24$Date)){
        prec.24$prec[prec.24$Date == i] <- (prec.24$rainfall[prec.24$Date== i])/24
    }

    return (prec.24)
}
