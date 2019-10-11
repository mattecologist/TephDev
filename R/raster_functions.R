#' Average Temp Raster
#'
#' Simple function to create an average Temp layer from Tmin and Tmax layers or Bricks.
#' @param x \code{RasterLayer} or \code{RasterBrick} object (e.g. Tmin)
#' @param y \code{RasterLayer} or \code{RasterBrick} object (e.g. Tmin)
#' @param ext \code{extent} object or vector with coordinates of extent
#' @return \code{RasterLayer} or \code{RasterBrick} object with averaged temperatures
#' @export
create_Ta <- function(x, y, ext) {
    x <- raster::crop(x, ext)
    y <- raster::crop(y, ext)
    z <- (x + y)/2
    names(z) <- names(x)
    return(z)
}

#' Raster-based Overwintering calculations
#'
#' Similar to \code{overwinter()} but spatial. Calculates number of overwintering days:
#' these are triggered by X consecutive days below a threshold temperature / broken by X days above
#' @param brick_name The \code{RasterBrick} object e.g. Tmin
#' @param t.thresh Threshold temperature in degrees
#' @param n.days Consecutive days needed at threshold to change condition
#' @return \code{RasterLayer} object showing total number of overwintering days for the period analysed
#' @author Matt Hill
#' @export

build.overwinter <- function(brick_name = Ta_range, t.thresh = 18, n.days = 5) {
    raster1 <- raster2 <- brick_name
    names_vec <- names(raster1)


    # raster1 is is to determine the days accumulated to enter overwinter
    cat("Calculating days below threshold:", t.thresh, "\n")
    for (i in 1:nlayers(raster1)) {
        # cat(i, '\n')
        raster1[[i]][!is.na(raster1[[i]])] <- 0
        raster1[[i]][brick_name[[i]] < t.thresh] <- 1
    }

    # raster2 is is to determine the days accumulated to exit overwinter note change in symbol term
    cat("Calculating days above threshold:", t.thresh, "\n")
    for (i in 1:nlayers(raster2)) {
        # cat(i, '\n')
        not_na <- !is.na(raster2[[i]])
        raster2[[i]][not_na] <- 0

        t_thresh <- brick_name[[i]] >= t.thresh
        raster2[[i]][t_thresh] <- 1
    }

    ## Switch for overwintering. This calculates cumulative days less than t.thresh degrees (n.days needed to
    ## enter) Currently calculated on the Ta (Average temperature for the day - based on the interpolated temp
    ## calculations)

    # cumulative loop for raster1
    cat("Calculating cumulative days below threshold of", n.days, "days", "\n")
    for (i in 2:nlayers(raster1)) {
        # cat(i, '\n')
        raster1[[i]][which(raster1[[i]]@data@values == 1)] <- raster1[[i]][which(raster1[[i]]@data@values == 1)] +
            raster1[[i - 1]][which(raster1[[i]]@data@values == 1)]
    }

    # cumulative loop for raster2
    cat("Calculating cumulative days above threshold of", n.days, "days", "\n")
    for (i in 2:nlayers(raster2)) {
        # cat(i, '\n')
        raster2[[i]][which(raster2[[i]]@data@values == 1)] <- raster2[[i]][which(raster2[[i]]@data@values == 1)] +
            raster2[[i - 1]][which(raster2[[i]]@data@values == 1)]
    }

    overwinter <- brick_name
    cat("Building overwinter layer", "\n")
    for (i in names(overwinter)) {
        not_na <- !is.na(overwinter[[i]])
        overwinter[[i]][not_na] <- 0
    }

    # First condition is set to 2 because it cannot be accumation of either, and needs to be positive for xx - 1.
    xx <- 2

    while (xx <= nlayers(overwinter)) {
        ## Condition 1. If the accumlation of days below 18 is greater or equal to 5, and the accumlation of days above
        ## 18 is less than 5, then its overwintering
        overwinter[[names_vec[xx]]]@data@values[raster1[[names_vec[xx]]]@data@values >= n.days] <- 1
        overwinter[[names_vec[xx]]]@data@values[raster1[[names_vec[xx]]]@data@values < n.days && overwinter[[names_vec[xx -
            1]]]@data@values == 1 && raster2[[names_vec[xx]]]@data@values < n.days] <- 1
        overwinter[[names_vec[xx]]]@data@values[raster2[[names_vec[xx]]]@data@values >= n.days] <- 0
        overwinter[[names_vec[xx]]]@data@values[raster2[[names_vec[xx]]]@data@values < n.days && overwinter[[names_vec[xx -
            1]]]@data@values == 0 && raster1[[names_vec[xx]]]@data@values < n.days] <- 0
        xx <- xx + 1
    }
    complete <- sum(overwinter)
    cat("Done!", "\n")
    return(complete)
}
