#' Create Temperature Vectors from SILO data
#' 
#' Uses the `{terra}` package to extract temperature vectors over multiple years from the gridded SILO data.
#' 
#' @param silodir Directory holding the SILO netcdf files, both tmin and tmax for relevant years
#' @param xy Coordinates of sites to extract data from
#' @param datemin Date to begin extraction from
#' @param datemax Date to end extraction from
#' @return A dataframe of Tmin and Tmax values for 
#' @author Matt Hill
#' @export

extract_SILO_data <- function(silodir="/Users/hil32c/Documents/SILO_data/",
                              site_list= as.data.frame(cbind("Site" = "Test", "x"=-33.2833, "y"=149.1000)),
                              datemin= "2015-06-30",
                              datemax= "2021-03-10"){

temp_years <- seq(from =format(as.Date(datemin, format="%Y-%m-%d"),"%Y"),
                  to = format(as.Date(datemax, format="%Y-%m-%d"),"%Y"))



Tmin <- assign(paste0("Tmin",temp_years[1]), terra::rast(paste0(silodir,temp_years[1],".min_temp.nc")))
Tmax <- assign(paste0("Tmax",temp_years[1]), terra::rast(paste0(silodir,temp_years[1],".max_temp.nc")))

for (i in 2:length(temp_years)){
  
  Tmin_temp <- assign(paste0("Tmin",temp_years[i]), terra::rast(paste0(silodir,temp_years[i],".min_temp.nc")))
  Tmin <- c(Tmin, Tmin_temp)
  
  Tmax_temp <- assign(paste0("Tmax",temp_years[i]), terra::rast(paste0(silodir,temp_years[i],".max_temp.nc")))
  Tmax <- c(Tmax, Tmax_temp)
}

names (Tmin) <- as.Date(as.POSIXct(Tmin@ptr$time, origin="1970-01-01"), format ="%Y-%m-%d")
names (Tmax) <- as.Date(as.POSIXct(Tmax@ptr$time, origin="1970-01-01"), format ="%Y-%m-%d")


x1 <- datemin
x2 <- datemax

Tmin <- Tmin[[which(names(Tmin)==x1):which(names(Tmin)==x2)]]
Tmax <- Tmax[[which(names(Tmax)==x1):which(names(Tmax)==x2)]]

xy <- site_list %>%
  st_as_sf(coords=c("y", "x"))

p <- terra::vect(xy)

tmin_hold <- as.data.frame(t(terra::extract (Tmin, p))) 
tmax_hold <- as.data.frame(t(terra::extract (Tmax, p)))

colnames(tmin_hold) <- site_list$Site
colnames(tmax_hold) <- site_list$Site

tmin_hold <- tmin_hold[-1,]
tmax_hold <- tmax_hold[-1,]

tmin_hold <- tmin_hold %>%
  tibble::rownames_to_column("Date")%>%
  reshape2::melt()%>%
  dplyr::rename(Date = Date, Site=variable, Tmin=value)

tmax_hold <- tmax_hold %>%
  tibble::rownames_to_column("Date")%>%
  reshape2::melt()%>%
  dplyr::rename(Date = Date, Site=variable, Tmax=value)

tempvec_sites <- dplyr::left_join(tmin_hold, tmax_hold)

return (tempvec_sites)

}
