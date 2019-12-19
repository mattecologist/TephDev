#' Shiny app to demonstrate model and data integration
#'
#' In development!
#' 
#' Download the SILO data from https://www.longpaddock.qld.gov.au/silo/gridded-data/
#' For this demonstration, use the 2015 min and max temperatures and keep the same file names.
#' 
#' 
#' 
#' @export
#' @return A Shiny app instance
#' @author Matt Hill

TephDev_vis <- function(silo_dir=silo_dir) {
  
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
  
  Tmin2015 <- raster::brick(paste0(silo_dir, "2015.min_temp.nc"))
  Tmax2015 <- raster::brick(paste0(silo_dir, "2015.max_temp.nc"))

  datevec <- as.Date(Tmin2015@z$Date)
  
  # Simple function to create average (although not entirely true) daily temps.
  
  create_Ta <- function(x, y, ext){
    x <- raster::crop (x, ext)
    y <- raster::crop (y, ext)
    z <- (x+y)/2
    names(z) <- names(x)
    return(z)
  }
  
  # flyobs <- read_csv("/home/hil32c/Documents/raster_process/app_data/flypoints.csv") %>%
  #   st_as_sf(coords = c("Longitude", "Latitude"),
  #            crs = "+proj=longlat +datum=WGS84")
  
  data(flyobs)
  
## Begin app.  
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
                             value=min(as.Date(Tmin2015@z$Date)),
                             timeFormat="%Y-%m-%d"),
                 p("followed by a submit button to perform task"),
                 br(),
                 actionButton('submit_button', 'Run Analysis' ),
                 br())),
                 tabPanel('Displaying Model outputs',
                 br(),
                 actionButton('add_button', 'Add layers'),
                 leafletOutput("mapplot", height=600, width=800) ## size here really determines speed of loading
      )
      
    ), 
    server = function( input, output, session ){
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
      sim.length <- reactiveVal(NULL)

      #27700 
      observeEvent(input$map_draw_new_feature, {
        
        withProgress(message = 'Preparing spatial data', value = 0, {
        feat <- input$map_draw_new_feature
        coords <- unlist(feat$geometry$coordinates)
        coords <- matrix(coords, ncol = 2, byrow = T)
        incProgress(1/3, detail = paste("Bounding layers"))
        poly <- st_sf(st_sfc(st_polygon(list(coords))), crs = st_crs("+proj=longlat +datum=WGS84"))
        # use Extent not BBOX
        bbox <- extent(poly)
        bboxRV(bbox)
        incProgress(2/3, detail = paste("Creating Ta"))
        TempR <- create_Ta(Tmax2015, Tmin2015, bbox)
        Ta2015(TempR)
       
        incProgress(1/3, detail = paste("Preparing outputs"))
        leafletProxy("map", session) %>%
          clearImages()
        })
        
      })
      
      output$poly <- renderPrint({
        req(bboxRV())
        print("Spatial data selected and ready!")
      })
      
      
      ###################################################################
      #create a blank raster stack to store output to.
      observeEvent(input$submit_button, {
    
        # init a progress bar
        pb = txtProgressBar(min = 1, max = (nrow(Ta2015())*ncol(Ta2015())), width=20, initial = 1)
        
        test <- list()
        tempdata(Ta2015()[])
        
        sim.length(datevec >= as.Date(input$slider2))
        # Take the raster brick, and consider each cell postion in a single layer.
        # 
        withProgress(message = 'Running models', value = 0, {
        for (i in 1:(nrow(Ta2015())*ncol(Ta2015()))) {
          incProgress(1/(nrow(Ta2015())*ncol(Ta2015())), detail = paste("Cell", i, "of", (nrow(Ta2015())*ncol(Ta2015()))))
          
          tempvec(as.vector(tempdata()[i, ]))
          
          if(is.na(tempvec()[1])==FALSE) {
            
           # hold.dat <- yonow_model(tempvec(), datevec, start.date = as.Date("2015-01-01")) # running from 1 Jan only at this point - need to update this
            
            hold.dat <- yonow_model(tempvec(), datevec, start.date = as.Date(input$slider2))
            
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
        # this changes it from requiring the number of cells * replacement to the number of days * replacement = much quicker!
        r1 <- Ta2015()
        r1[] <- NA
        
        message(paste0(as.Date(input$slider2), " selected as start"))
       # for (ii in 1:length(tempvec())){
        for (ii in which(datevec >= as.Date(input$slider2))){
          incProgress(1/length(which(datevec >= as.Date(input$slider2))), detail = paste("Raster building", ii))
          r1[[ii]] <- as.vector(unlist(lapply(test, `[[`, ii)))
         
        }
        
        r2(mask(sum(r1, na.rm=T), Ta2015()[[1]]))
        })
        
      })
      ###################################################################
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
          addCircles(data= flyobs,
                     layerId = NULL, group = NULL, stroke = TRUE, color = "yellow",
                     weight = 5, opacity = 0.5, fill = FALSE,
                     fillOpacity = 0.2)
      })
      
    }
  )
}
