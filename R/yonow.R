##### Yonow model.

#' Yonow et al. (2004) model for Queensland Fruit Fly development
#'
#' Function to run the Yonow et al. (2004) model on a temp and date vector. The initial parameters are for the
#' Queensland Fruit Fly (Bactrocera tryoni).
#' @export
#' @param tempvec A vector of temperatures (preferably daily average temp (Ta))
#' @param datevec A vector of dates that is the same length as \code{tempvec}
#' @param start.date Initialisation date, in YYYY-MM-DD format
#' @param ep1 Egg development parameters
#' @param ep2 Egg development parameters
#' @param ebt Egg base temperature
#' @param lp1 Larvae development parameters
#' @param lp2 Larvae development parameters
#' @param lbt Larval base temperature
#' @param pp1 Pupae development parameters
#' @param pp2 Pupae development parameters
#' @param pbt Pupal base temperature
#' @param tf1 Teneral female development parameters
#' @param tf2 Teneral female development parameters
#' @param tfbt Teneral female base temperature
#' @param reverse Run the model in reverse
#' @param adult Include / exclude the teneral female development stage
#' @return A dataframe of daily dates, temperatures, insect development stages, and percent development
#' @author Matt Hill
#' @references Yonow, T, M.P Zalucki, R.W Sutherst, B.C Dominiak, G.F Maywald, D.A Maelzer, and D.J Kriticos. “Modelling the Population Dynamics of the Queensland Fruit Fly, Bactrocera (Dacus) Tryoni: A Cohort-Based Approach Incorporating the Effects of Weather.” Ecological Modelling 173, no. 1 (March 2004): 9–30. https://doi.org/10.1016/S0304-3800(03)00306-5.


yonow.model <- function(tempvec = tempvec, datevec = datevec, start.date = start.date, ep1 = 0.0382, ep2 = 0.4229,
    lp1 = 0.0061, lp2 = 0.0609, pp1 = 0.0061, pp2 = 0.068, tf1 = 0.0108, tf2 = 0.133, ebt = 11.1, lbt = 10, pbt = 11.2,
    tfbt = 12.3, reverse = FALSE, adult = FALSE) {

    model.out <- data.frame(cbind(datevec, tempvec))  ## output dateframe
    colnames(model.out) <- c("Date", "Temp")
    model.out$Date <- as.Date(datevec, origin = "1970-01-01")
    start.date <- as.Date(start.date, origin = "1970-01-01")

    if (reverse == FALSE) {

        j <- which(model.out$Date == start.date)  ## day counter init
        model.out$egg <- 0  ## development init
        k <- 0  ## development completion counter init


        # ### Egg development
        if (j == 1) {
            model.out$egg[j] <- ep1 * model.out$Temp[j] - ep2
            j <- j + 1
        }

        while (k < 1) {

            if (model.out$Temp[j] > ebt) {
                # temp for lower development threshold
                k <- model.out$egg[j] <- (ep1 * model.out$Temp[j] - ep2) + model.out$egg[j - 1]
            } else {
                k <- model.out$egg[j] <- model.out$egg[j - 1]
            }
            if (j == length(model.out$Date)) {
                k <- 1
            } else {
                j <- j + 1
            }
        }


        ## Larval development
        model.out$larvae <- 0
        k <- 0

        while (k < 1) {
            if (model.out$Temp[j] > lbt) {
                # temp for lower development threshold
                k <- model.out$larvae[j] <- (lp1 * model.out$Temp[j] - lp2) + model.out$larvae[j - 1]
            } else {
                k <- model.out$larvae[j] <- model.out$larvae[j - 1]
            }
            if (j == length(model.out$Date)) {
                k <- 1
            } else {
                j <- j + 1
            }
        }

        ## Pupal development
        model.out$pupae <- 0
        k <- 0
        while (k < 1) {
            if (model.out$Temp[j] > pbt) {
                # temp for lower development threshold
                k <- model.out$pupae[j] <- (pp1 * model.out$Temp[j] - pp2) + model.out$pupae[j - 1]
            } else {
                k <- model.out$pupae[j] <- model.out$pupae[j - 1]
            }
            if (j == length(model.out$Date)) {
                k <- 1
            } else {
                j <- j + 1
            }
        }
        if (adult == TRUE) {
            model.out$teneral <- 0
            k <- 0
            while (k < 1) {
                if (model.out$Temp[j] > tfbt) {
                  # temp for lower development threshold
                  k <- model.out$teneral[j] <- (tf1 * model.out$Temp[j] - tf2) + model.out$teneral[j - 1]
                } else {
                  k <- model.out$teneral[j] <- model.out$teneral[j - 1]
                }
                if (j == length(model.out$Date)) {
                  k <- 1
                } else {
                  j <- j + 1
                }
            }
        }
    }

    if (reverse == TRUE) {

        j <- which(model.out$Date == start.date)  ## day counter init
        model.out$pupae <- 0  ## development init
        k <- 0  ## development completion counter init


        if (adult == TRUE) {
            model.out$teneral <- 0
            k <- 0
            while (k < 1) {
                if (model.out$Temp[j] > tfbt) {
                  # temp for lower development threshold
                  k <- model.out$teneral[j] <- (tf1 * model.out$Temp[j] - tf2) + model.out$teneral[j + 1]
                } else {
                  k <- model.out$teneral[j] <- model.out$teneral[j + 1]
                }
                if (j == length(model.out$Date)) {
                  k <- 1
                } else {
                  j <- j - 1
                }
            }
        }


        ### pupal development
        model.out$pupae <- 0
        k <- 0
        while (k < 1) {
            if (model.out$Temp[j] > pbt) {
                # temp for lower development threshold
                k <- model.out$pupae[j] <- (pp1 * model.out$Temp[j] - pp2) + model.out$pupae[j + 1]
            } else {
                k <- model.out$pupae[j] <- model.out$pupae[j + 1]
            }
            j <- j - 1
        }

        ## Larval development
        model.out$larvae <- 0
        k <- 0

        while (k < 1) {
            if (model.out$Temp[j] > lbt) {
                # temp for lower development threshold
                k <- model.out$larvae[j] <- (lp1 * model.out$Temp[j] - lp2) + model.out$larvae[j + 1]
            } else {
                k <- model.out$larvae[j] <- model.out$larvae[j + 1]
            }
            j <- j - 1
        }

        ## egg development
        model.out$egg <- 0
        k <- 0
        while (k < 1) {
            if (model.out$Temp[j] > ebt) {
                # temp for lower development threshold
                k <- model.out$egg[j] <- (ep1 * model.out$Temp[j] - ep2) + model.out$egg[j + 1]
            } else {
                k <- model.out$egg[j] <- model.out$egg[j + 1]
            }
            j <- j - 1
        }
    }

    # Final formatting
    model.out <- reshape2::melt(model.out, id.vars = c("Date", "Temp"))
    model.out$value[model.out$value == 0] <- NA
    model.out <- model.out[complete.cases(model.out), ]

    if (reverse == TRUE) {
        model.out$value <- 1 - model.out$value
        model.out$value[model.out$value < 0] <- 0
        model.out <- model.out[order(model.out$Date), ]
        model.out$value[model.out$variable == "larvae"] <- model.out$value[model.out$variable == "larvae"] + max(model.out$value[model.out$variable ==
            "egg"])
        model.out$value[model.out$variable == "pupae"] <- model.out$value[model.out$variable == "pupae"] + max(model.out$value[model.out$variable ==
            "larvae"])
    }

    return(model.out)


}
