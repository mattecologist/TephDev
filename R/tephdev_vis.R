#' Shiny app to demonstrate model and data integration
#'
#' I development!
#' @export
#' @return A Shiny app instance
#' @author Matt Hill

TephDev_vis <- function() {
  
  require (shiny)
  require(shinyjs)
  require (leaflet)
  require(leaflet.extras)
  require (plainview)
  require (raster)
  require (sf)
  require (tidyverse)
  require (mapview)
  require(TephDev)
  
  shinyApp(
    ui = navbarPage(
      theme = shinythemes::shinytheme("superhero"),
      title = 'TephDev visualise',
      tabPanel('Model Setup',
               useShinyjs(),
               fluidRow(
                 p("Use the following map to pan, zoom, and select a bounding box"),
                 #https://github.com/rstudio/leaflet/issues/616
                 leafletOutput("map", height=400, width=400),
                 br(),
                 textOutput("poly"),
                 br(),
                 p("And then an input to select the date to begin analysis"),
                 br(),
                 sliderInput("slider2", label = h4("Select Commencement Date"), min = min(as.Date(Tmin2015@z$Date)),
                             max = max(as.Date(Tmin2015@z$Date)),
                             value=mean(max(as.Date(Tmin2015@z$Date))),
                             timeFormat="%Y-%m-%d"),
                 p("followed by a submit button to perform task"),
                 br(),
                 actionButton('submit_button', 'Run Analysis' ),
                 br(),
                 p("For now, just a radio button to select preconstructed rasters"),
                 radioButtons( 'season', 'Select Season', choices = list( '2015/16' = 'X2015.06.01_X2015.12.31', '2016/17' = 'X2016.06.01_X2016.12.31', '2017/18'='X2017.06.01_X2017.12.31' ),inline = T )
               )),
      tabPanel('Displaying Model outputs',
               br(),
               actionButton('add_button', 'Add layers'),
               leafletOutput("mapplot", height=600, width=800) ## size here really determines speed of loading
      )
      
    ), 
    server = function( input, output, session ){
      
      Tmin2015 <- raster::brick("./SILO/2015.min_temp.nc")
      Tmax2015 <- raster::brick("./SILO/2015.max_temp.nc")
      
      datevec <- as.Date(Tmin2015@z$Date)
      
      create_Ta <- function(x, y, ext){
        x <- raster::crop (x, ext)
        y <- raster::crop (y, ext)
        z <- (x+y)/2
        names(z) <- names(x)
        return(z)
      }
      
      #QFFfree <- st_read("./app_data/qff_control/QFly_Control_Zone.shp")
      
      maxQFF <- raster("./app_data/qfly_current.tif")
      projection(maxQFF) <- "+proj=aea +lat_1=-18 +lat_2=-36 +lat_0=0 +lon_0=132 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
      
      flyobs <- read_csv("./app_data/flypoints.csv") %>%
        st_as_sf(coords = c("Longitude", "Latitude"),
                 crs = "+proj=longlat +datum=WGS84")
      
      # Select map
      output$map <- renderLeaflet({
        leaflet() %>% 
          setView (lng=133.8,
                   lat=-23.7,
                   zoom=3)%>%
          addProviderTiles("CartoDB.PositronNoLabels") %>%
          addDrawToolbar(polylineOptions = F, circleOptions = F, markerOptions = F,
                         circleMarkerOptions = F, polygonOptions = F)
      })
      
      bboxRV <- reactiveVal(NULL)
      Ta2015 <- reactiveVal(NULL)
      tempvec <- reactiveVal(NULL)
      tempdata <- reactiveVal(NULL)
      r2 <- reactiveVal(NULL)
      
      
      
      #27700 
      observeEvent(input$map_draw_new_feature, {
        feat <- input$map_draw_new_feature
        coords <- unlist(feat$geometry$coordinates)
        coords <- matrix(coords, ncol = 2, byrow = T)
        poly <- st_sf(st_sfc(st_polygon(list(coords))), crs = st_crs("+proj=longlat +datum=WGS84"))
        # use Extent not BBOX
        bbox <- extent(poly)
        bboxRV(bbox)
        TempR <- create_Ta(Tmax2015, Tmin2015, bbox)
        Ta2015(TempR)
        
        leafletProxy("map", session) %>%
          clearImages()
        print(Ta2015)
        
      })
      
      output$poly <- renderPrint({
        req(bboxRV())
        print("Spatial data selected and ready!")
      })
      
      
      ###################################################################
      #create a blank raster stack to store output to.
      observeEvent(input$submit_button, {
        
        
        # r1 <- raster::brick(Ta2015())
        # print(r1)
        
        
        # init a progress bar
        pb = txtProgressBar(min = 1, max = (nrow(Ta2015())*ncol(Ta2015())), width=20, initial = 1)
        
        test <- list()
        tempdata(Ta2015()[])
        
        # Take the raster brick, and consider each cell postion in a single layer.
        # 
        
        for (i in 1:(nrow(Ta2015())*ncol(Ta2015()))) {
          
          tempvec(as.vector(tempdata()[i, ]))
          
          if(is.na(tempvec()[1])==FALSE) {
            
            hold.dat <- yonow_model(tempvec(), datevec, start.date = as.Date("2015-01-01")) # running from 1 Jan only at this point - need to update this
            
            hold.dat$value <- 1 #give thme all a value of 1 - indicating that it was still developing on this day.
            xx <- length(tempvec())-length(hold.dat$value)
            if (xx < 0){xx <- 0}
            
            test[[paste0(i)]] <- c(hold.dat$value, rep(NA, xx))
          }
          else{
            test[[paste0(i)]] <- rep(NA, length(tempvec()))
          }
          
          setTxtProgressBar(pb,i)
        }
        
        
        # step through each vector position and assign to the appropriate raster layer
        # this changes it from requiring the number of cells * replacement to the number of days * replacement = much quicker!!
        #print(as.vector(unlist(lapply(test, `[[`, 1))))
        
        r1 <- Ta2015()
        r1[] <- NA
        
        for (ii in 1:length(tempvec())){
          r1[[ii]] <- as.vector(unlist(lapply(test, `[[`, ii)))
          print (ii)
        }
        
        r2(mask(sum(r1, na.rm=T), Ta2015()[[1]]))
        
        
      })
      ###################################################################
      
      
      
      
      
      ## Build up mapview raster objects
      
      Overwinter <-raster(paste0("./app_data/overwinter_X2015.06.01_X2015.12.31.grd"))
      
      m <- mapview(Overwinter)
      
      
      ## Leaflet rendering
      
      output$mapplot <- renderLeaflet({
        
        leaflet()%>%
          setView (lng=bboxRV()[1],
                   lat=bboxRV()[3],
                   zoom=5)%>%
          addProviderTiles("CartoDB.PositronNoLabels") %>%
          addProviderTiles("CartoDB.PositronOnlyLabels",
                           options = leafletOptions(pane = "maplabels"),
                           group = "map labels") #%>%
      })
      
      observeEvent(input$add_button, {
        leafletProxy("mapplot", session) %>%
          clearImages()%>%
          clearShapes()%>%
          addRasterImage(r2(),
                         project=TRUE)%>%
          ## Need to come up with a way to add this is a mapview object so that it scales with the map
          #mapView(m@map)%>%
          addCircles(data= flyobs,
                     layerId = NULL, group = NULL, stroke = TRUE, color = "yellow",
                     weight = 5, opacity = 0.5, fill = FALSE,
                     fillOpacity = 0.2)
      })
      
    }
  )
}