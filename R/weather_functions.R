##### Weather functions

#' Simple overwintering function
#'
#' Switch for overwintering. This calculates cumulative days less than 18 degrees (5 needed to enter)
#' Currently calculated on the Ta (Average temperature for the day - based on the interpolated temp calculations)
#' @export

overwinter <- function(tempvec = tempvec, datevec = datevec) {

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


#' Simple cherry_dev
#'
#' Switch for overwintering. This calculates cumulative days less than 18 degrees (5 needed to enter)
#' Currently calculated on the Ta (Average temperature for the day - based on the interpolated temp calculations)
#' @export
cherry_dev <- function(tlow = 4.5, thigh = 100, tempvec = tempvec, budburst = "2015-08-31") {

    degday <- tempvec[tempvec$Date > budburst, ]
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
