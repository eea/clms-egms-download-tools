#
# Load R packages
library(shiny)
library(shinyjs)
library(shinythemes)
library(shinyWidgets)
library(readr)
library(base)
library(sp)
library(raster)
library(downloader)
library(RSQLite)
library(leaflet)
library(terra)
library(sf)
library(tidyverse)
library(leaflet.extras)
library(rgdal)
library(maptools)
library(shinyFiles)
library(shinyBS)

#### Define UI ####
ui <- fluidPage(
  tags$head(
    tags$style(
      HTML(".shiny-notification {
              height: 100px;
              width: 500px;
              text-align: center;
              vertical-align: middle;
              position:fixed;
              top: calc(50% - 50px);;
              left: calc(61% - 400px);;
            }
           ")
    )
  ),
  useShinyjs(),
  # Define page title and visual style
  
  navbarPage(title=div(img(src="https://lh3.googleusercontent.com/8TJ4LGwQrF9D06LyqznyhZrR6AvhjylYawZDq6P0DFA1nB5GbKtnDuBkch1yA40JnrGvFHcRAM6RB2hTp0gZhv-EvjY7Slt7vAjMm4Xb2HeZotDMKIgXfS0R-vAhECYiKkKCjI6g4g=w2400", height = 35, width = 35, style ="margin:5px 5 px"), "EGMStream v1.0", theme = shinytheme("spacelab")),
             # Define Tab Panel title features
             tabPanel("EGMS download and store", fluid = TRUE, icon = icon("earth-europe"),
                      # Define sidebar Layout
                      sidebarLayout(
                        sidebarPanel(
                          titlePanel(h3(p("Data Storage Settings"))),
                          h4(p("Browse", a("EGMS viewer", href = "https://egms.land.copernicus.eu/", target="_blank"), "to download links")),
                          
                          br(),
                          fluidRow(column(12, fileInput("LINKS", "Upload the txt file (egms-archive-links.txt)",
                                                        multiple = F,
                                                        accept = ".txt"),
                          ), # close column
                          ), # close FliudRow
                          #hr(),
                          fluidRow(
                            column(12, 
                                   fluidRow(column(9, h5(p(strong("Draw your area of interest on map to crop PS data"),"(optional)"))),
                                            column(3, 
                                                   materialSwitch(
                                                     inputId = "DrawAoI",
                                                     label = "Crop",
                                                     right = TRUE,
                                                     value = FALSE,
                                                     status = "primary"
                                                   )
                                            ), # close column DRAW AoI
                                   ), br(), br(), # close FliudRow
                                   #hr(style = "border-top: 1px solid gray;"),
                            ), # close column Draw
                            column(12, 
                                   fluidRow(column(4, radioButtons(inputId = "FormatSelection",
                                                                   label = "Data format:",
                                                                   choices = c("Geopackage (.gpkg)" = "GPKG", "Shapefile (.shp)" = "SHP"),
                                                                   selected = "GPKG"), # Flag to choose between geopackage format or shapefile format
                                   ), # close column SHP or GPKG
                                   column(4, radioButtons(inputId = "TimeSeriesSelection",
                                                          label = "Database design:",
                                                          choices = c("With Time Series" = "YTS", "Without Time Series" = "NTS"),
                                                          selected = "YTS"), # Flag to include TS or not
                                   ), # close column YTS or NTS)
                                   conditionalPanel(
                                     condition= "input.TimeSeriesSelection == 'YTS'",
                                     column(4, radioButtons(inputId = "DataFormat",
                                                            label = "Date format:",
                                                            choices = c("Dddmmyyyy" = "dmy", "Dyyyymmdd" = "ymd"),
                                                            selected = "ymd"), # Flag to chose data format Dggmmyyyy o Dyyyymmdd
                                     ), # close column YDM or DMY
                                   ), # close conditionalPanel
                                   ), br(), br(), # close FluidRow
                            ),# close column
                            
                            # set work directory to download data
                            column(12, fluidRow(column(4, shinyDirButton(id='folder', label='Choose Folder', title='Please select a folder',
                                                                         buttonType = "default", icon=icon("folder")),),
                                                column(8, verbatimTextOutput("dir", placeholder = TRUE)),
                            ), br(), br(), # close fluidRow
                            ), # close column 12
                            
                            column(12, 
                                   fluidRow(column(5, h5(p(strong("Convert the EGMS data")))),
                                            column(7, actionButton(inputId= "RUN",
                                                                   label ="Convert", icon=icon("spinner")) # Buttons for starting conversion
                                            ), # close column RUN CONVERSION
                                   ), # close FliudRow
                            ), # close column Draw
                            #hr(style = "border-top: 1px solid gray;"),
                            br(),br(),
                            column(12,hr(style = "border-top: 1px solid gray;"),
                                   h5(p(paste0("Credits"))),
                                   h6(p("Festa D. & Del Soldato M."))),
                            column(12,h5(p(paste0("Acknowledgments"))),
                                   h6(p("EGMS © European Union, Copernicus Land Monitoring Service 2022, European Environment Agency (EEA) - " ,
                                        a("https://land.copernicus.eu/pan-european/european-ground-motion-service", href = "https://land.copernicus.eu/pan-european/european-ground-motion-service"))),
                            ),
                          ),# close column Credits # close FluidRow
                        ), # close slidePanel
                        # Main panel features with sub-panels for map visual and geoprocessing history
                        mainPanel(
                          tabsetPanel(
                            tabPanel("Data Location Preview", leafletOutput("Map", height = "81vh"),
                                     br(),
                                     h5(p("Mail to ", a(strong("egmstream@dst.unifi.it"), href = "mailto:egmstream@dst.unifi.it"), "for receiving support and to stay up-to-date on EGMStream future releases"))
                            ),
                            
                            
                            tabPanel("Processing History", 
                                     verbatimTextOutput("txtout1")
                                     ,verbatimTextOutput("txtout2"),
                            )
                          )
                        )
                      )
             ),
             
             #New Tab Panel for ReadMe section: quickstart guide to interface and acknowledgments
             tabPanel("ReadMe", fluid = TRUE, icon = icon("info-circle"),
                      fluidRow(
                        column(12,
                               h3(p(div(img(src="https://lh3.googleusercontent.com/8TJ4LGwQrF9D06LyqznyhZrR6AvhjylYawZDq6P0DFA1nB5GbKtnDuBkch1yA40JnrGvFHcRAM6RB2hTp0gZhv-EvjY7Slt7vAjMm4Xb2HeZotDMKIgXfS0R-vAhECYiKkKCjI6g4g=w2400", height = 65, width = 65, style ="margin:5px 5px"),"About the", strong("EGMStream"), "project"))),
                               br(),
                               h5(p("The European Ground Motion Service (EGMS) data are available for download since November 2022.")),
                               h5(p("The presented app is intended as a supporting tool for the EGMS data downstream.")),
                               h5(p("In the framework of optimally disseminating radar data from Copernicus Sentinel-1 satellite mission, an open source value-adding service able to adapt EGMS products to users\' need is provided.")),
                               h5(p("Thanks to an intuitive and user-friendly interface, it is possible to automatically manage, transform and store EGMS data into geospatial databases departsng from EGMS data download links available at", a("EGMS explorer", href = "https://egms.land.copernicus.eu/"), ".")),
                               br(),
                               h5(p("", strong("EGMStream"), "v1.0 is realised through the R package Shiny and corresponds to the release version 1.0")),
                               hr(),
                               h5(p("Mail to ", a(strong("egmstream@dst.unifi.it"), href = "mailto:egmstream@dst.unifi.it"), "for receiving support and to stay up-to-date on EGMStream future releases")),
                               hr(),
                               h3(p("Quickstart guide to user interface")),
                               br(),
                               h5(p("This section will briefly explain the key input requirements for", strong("EGMStream"), "app to download, visualize and set the frame of the EGMS-related geospatial databases to be stored.")),
                               br(),
                               h4(p(strong(em(tags$ol("App Layout"))))),
                               br(),
                               column(12,img(src="https://lh3.googleusercontent.com/RGJ8djFvcm1xWRW8cbbj7TYaLjUaZUbl5TzKSBQj1aY2U7SD5duf542QfV3wqAaTalJRk1dwY2ONKjBkHfmSoRQcVf3ijaN64Fvs7VQnC2QcfwLJQ89hQJQh4S_QzhpMXF7KsPODMA=w2400", height="60%", width="60%", align="left")),
                               br(),
                               h5(p(strong(tags$ul(em("- EGMS download and store"), "(main panel):")))),
                               h5(p(tags$li(em("Data Storage settings"), "(side bar): from the upper part of the side bar, is possible to start the process by feeding the App with the EGMS download links related to the data selected within the EGMS explorer;\n 
                                            the lower part of the side panel can be used to adjust and set the desired characteristics of the geospatial databases that will be created."))),
                               h5(p(tags$li(em("Data location preview"), "(tab panel): map viewer from where it is possible to draw an Area of Interest (AoI) covering the uploaded EGMS data."))),
                               h5(p(tags$li(em("Processing History"), "(tab panel): panel containing the recorded tracking information of the processes run by the App."))),
                               h5(p(strong(tags$ul(em("- ReadMe"), "(secondary panel):"))), tags$li("section containing info about the background and objectives of the work, a quickstart guide and credits for the EGMStream app and the underlying code.")),
                               br(),
                               h4(p(strong(em(tags$ol("Get started"))))),
                               br(),
                               column(12,img(src="https://lh3.googleusercontent.com/bv9ezAbeeROs0KJNwxlTeGBSKJY4FPJxNoOx07xXpU3J3bXoCOYuogpZ97KXeI8D0ZzzZnWYdFcBi1LE9pOli48xkN_vL1boMFtel1WVJMBRPopmoKyF31qEJolPbTbcfOXycSV3lw=w2400", height="50%", width="50%", align="left")),
                               br(),
                               h5(p(tags$li("1. Browse", a("EGMS explorer", href = "https://egms.land.copernicus.eu/"), "to search for the area of interest;"), 
                                    tags$li("2. Download EGMS archive links related to either BASIC, CALIBRATED, and ORTHO data covering the area of interest;"), 
                                    tags$li("3. Store the generated .txt file in a folder with a known path."))),
                               #h5(p(strong("1)"), "Browse", a("EGMS explorer", href = "https://egms.land.copernicus.eu/"), "to search for the area of interest", strong("2)"), "download EGMS archive links related to either BASIC, CALIBRATED, and ORTHO data covering the area of interest", strong("3)"), "store the generated .txt file in a folder with a known path")),
                               br(),
                               h4(p(strong(tags$ol(em("Steps to EGMStream implementation"))))),
                               br(),
                               column(12,img(src="https://lh3.googleusercontent.com/9MVllHeRvUgHAKGq7Srtb8vk-Ww9pC1RFvZhsxcdiwsP5shXZeL_J6aQAWtLnV4kUpmCL9lplX-aY7s5LXDpJYCZb6tNhHXBzsRUoFQD9bD37o2RMWBjPrLRE18W9FfMGVgZj5DGHQ=w2400", height="40%", width="40%", align="left")),
                               br(),
                               #h5(p(tags$ol(""))),
                               h5(p(tags$li("1. Browse folder location where the  archive links were previously stored and select the .txt file containing the hyperlinks;"),
                                    tags$li("2. Optionally design your Area of Interest (AoI) by enabling the draw toolbar (swipe right on the Crop button);"),
                                    tags$li("3. Adjust data storage settings according to the user needs;"),
                                    tags$li("4. Choose folder where to store the converted EGMS data;"),
                                    tags$li("5. Run conversion by triggering the 'Convert' action button;"),
                                    tags$li("6. Follow conversion progress through the appearing popup notifications and eventually check the processing history tab panel."))),
                               br(),
                               hr(),
                               h4(p("Credits")),
                               h5(p("Festa D. & Del Soldato M.")),
                               h4(p("Acknowledgments")),
                               h5(p("EGMS © European Union, Copernicus Land Monitoring Service 2022, European Environment Agency (EEA) - " ,
                                    a("https://land.copernicus.eu/pan-european/european-ground-motion-service", href = "https://land.copernicus.eu/pan-european/european-ground-motion-service"))),
                               br(),
                               h6(p("You may not use EGMStream except in compliance with the Apache License, Version 2.0"))
                               #h5(p("The source code for this Shiny app is available ", a("on github", href = "https://github.com/maybedave"), "."))
                        ) # close column
                      )
                      
             )
  )
)

#### Define server function ####  
server <- function(input, output, session) {
  
  #### FUNCTIONS ####
  Ddmy <- function(date) {
    for (a in 1:length(date)) {
      date[a] <- paste0("D", substr(date[a], 7, 8), substr(date[a], 5, 6), substr(date[a], 1, 4))
    } # close for date
    return(date)
  }	
  
  Dymd <- function(date) {
    for (a in 1:length(date)) {
      date[a] <- paste0("D", substr(date[a], 1, 4), substr(date[a], 5, 6), substr(date[a], 7, 8))
    } # close for date
    return(date)
  }
  
  Write_SHP <- function(PS_data, path, namefile) {
    parts <- as.integer(nrow(PS_data)/500000)+1
    if (parts == 1) {
      pathfile_shp <- paste0(path, namefile, ".shp")
      WGS_Table_PS <- as.data.frame(PS_data)
      WGS_Table_PS[,2:length(WGS_Table_PS)] <- as.double(as.matrix(WGS_Table_PS[,2:length(WGS_Table_PS)]))
      WGS_Table_PS <- sf::st_as_sf(WGS_Table_PS, coords = c("easting", "northing"), remove=FALSE, crs = "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs")
      WGS_Table_PS <- sf::st_transform(WGS_Table_PS, crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
      WGS_Table_PS <- sf:::as_Spatial(WGS_Table_PS)
      raster::shapefile(WGS_Table_PS, pathfile_shp)
    } else {
      for (p in 1:parts) {
        if (p == 1) {
          dummy <- PS_data[1:as.integer(nrow(PS_data)*p/parts),]
          namefile_shp <- paste0(namefile, "_part", p)
          pathfile_shp <- paste0(path, namefile_shp, ".shp")
          WGS_Table_PS <- as.data.frame(dummy)
          WGS_Table_PS[,2:length(WGS_Table_PS)] <- as.double(as.matrix(WGS_Table_PS[,2:length(WGS_Table_PS)]))
          WGS_Table_PS <- sf::st_as_sf(WGS_Table_PS, coords = c("easting", "northing"), remove=FALSE, crs = "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs")
          WGS_Table_PS <- sf::st_transform(WGS_Table_PS, crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
          WGS_Table_PS <- sf:::as_Spatial(WGS_Table_PS)
          raster::shapefile(WGS_Table_PS, pathfile_shp)
        } else  {
          dummy <- PS_data[as.integer((nrow(PS_data)*(p-1)/parts)+1):as.integer(nrow(PS_data)*p/parts),]
          namefile_shp <- paste0(namefile, "_part", p)
          pathfile_shp <- paste0(path, namefile_shp, ".shp")
          WGS_Table_PS <- as.data.frame(dummy)
          WGS_Table_PS[,2:length(WGS_Table_PS)] <- as.double(as.matrix(WGS_Table_PS[,2:length(WGS_Table_PS)]))
          WGS_Table_PS <- sf::st_as_sf(WGS_Table_PS, coords = c("easting", "northing"), remove=FALSE, crs = "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs")
          WGS_Table_PS <- sf::st_transform(WGS_Table_PS, crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
          WGS_Table_PS <- sf:::as_Spatial(WGS_Table_PS)
          raster::shapefile(WGS_Table_PS, pathfile_shp)
        } # close if p = 1
        rm(dummy)
        rm(namefile_shp)
      } # close for parts
      rm(parts)
    } # close if parts
  }
  
  Write_track_SHP <- function(PS_data, path, namefile, track) {
    parts <- as.integer(nrow(PS_data)/500000)+1
    if (parts == 1) {
      pathfile_shp <- paste0(path, namefile, "_", track, ".shp")
      WGS_Table_PS <- as.data.frame(PS_data)
      WGS_Table_PS[,2:length(WGS_Table_PS)] <- as.double(as.matrix(WGS_Table_PS[,2:length(WGS_Table_PS)]))
      WGS_Table_PS <- sf::st_as_sf(WGS_Table_PS, coords = c("easting", "northing"), remove=FALSE, crs = "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs")
      WGS_Table_PS <- sf::st_transform(WGS_Table_PS, crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
      WGS_Table_PS <- sf:::as_Spatial(WGS_Table_PS)
      raster::shapefile(WGS_Table_PS, pathfile_shp)
    } else {
      for (p in 1:parts) {
        if (p == 1) {
          dummy <- PS_data[1:as.integer(nrow(PS_data)*p/parts),]
          namefile_shp <- paste0(namefile, "_", track, "_part", p)
          pathfile_shp <- paste0(path, namefile_shp, ".shp")
          WGS_Table_PS <- as.data.frame(dummy)
          WGS_Table_PS[,2:length(WGS_Table_PS)] <- as.double(as.matrix(WGS_Table_PS[,2:length(WGS_Table_PS)]))
          WGS_Table_PS <- sf::st_as_sf(WGS_Table_PS, coords = c("easting", "northing"), remove=FALSE, crs = "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs")
          WGS_Table_PS <- sf::st_transform(WGS_Table_PS, crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
          WGS_Table_PS <- sf:::as_Spatial(WGS_Table_PS)
          raster::shapefile(WGS_Table_PS, pathfile_shp)
        } else  {
          dummy <- PS_data[as.integer((nrow(PS_data)*(p-1)/parts)+1):as.integer(nrow(PS_data)*p/parts),]
          namefile_shp <- paste0(namefile, "_", track, "_part", p)
          pathfile_shp <- paste0(path, namefile_shp, ".shp")
          WGS_Table_PS <- as.data.frame(dummy)
          WGS_Table_PS[,2:length(WGS_Table_PS)] <- as.double(as.matrix(WGS_Table_PS[,2:length(WGS_Table_PS)]))
          WGS_Table_PS <- sf::st_as_sf(WGS_Table_PS, coords = c("easting", "northing"), remove=FALSE, crs = "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs")
          WGS_Table_PS <- sf::st_transform(WGS_Table_PS, crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
          WGS_Table_PS <- sf:::as_Spatial(WGS_Table_PS)
          raster::shapefile(WGS_Table_PS, pathfile_shp)
        } # close if p = 1
        rm(dummy)
        rm(namefile_shp)
      } # close for parts
      rm(parts)
    } # close if parts
  }
  
  Write_GPKG <- function(PS_data, path, namefile) {
    parts <- as.integer(nrow(PS_data)/500000)+1
    if (parts == 1) {
      pathfile_gpkg <- paste0(path, namefile, ".gpkg")
      WGS_Table_PS <- as.data.frame(PS_data)
      WGS_Table_PS[,2:length(WGS_Table_PS)] <- as.double(as.matrix(WGS_Table_PS[,2:length(WGS_Table_PS)]))
      WGS_Table_PS <- sf::st_as_sf(WGS_Table_PS, coords = c("easting", "northing"), remove=FALSE, crs = "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs")
      WGS_Table_PS <- sf::st_transform(WGS_Table_PS, crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
      sf::st_write(WGS_Table_PS, pathfile_gpkg, driver="GPKG", append=F)
    } else {
      for (p in 1:parts) {
        if (p == 1) {
          dummy <- PS_data[1:as.integer(nrow(PS_data)*p/parts),]
          namefile_gpkg <- paste0(namefile, "_part", p)
          pathfile_gpkg <- paste0(path, namefile_gpkg, ".gpkg")
          WGS_Table_PS <- as.data.frame(dummy)
          WGS_Table_PS[,2:length(WGS_Table_PS)] <- as.double(as.matrix(WGS_Table_PS[,2:length(WGS_Table_PS)]))
          WGS_Table_PS <- sf::st_as_sf(WGS_Table_PS, coords = c("easting", "northing"), remove=FALSE, crs = "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs")
          WGS_Table_PS <- sf::st_transform(WGS_Table_PS, crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
          sf::st_write(WGS_Table_PS, pathfile_gpkg, driver="GPKG")
        } else  {
          dummy <- PS_data[as.integer((nrow(PS_data)*(p-1)/parts)+1):as.integer(nrow(PS_data)*p/parts),]
          namefile_gpkg <- paste0(namefile, "_part", p)
          pathfile_gpkg <- paste0(path, namefile_gpkg, ".gpkg")
          WGS_Table_PS <- as.data.frame(dummy)
          WGS_Table_PS[,2:length(WGS_Table_PS)] <- as.double(as.matrix(WGS_Table_PS[,2:length(WGS_Table_PS)]))
          WGS_Table_PS <- sf::st_as_sf(WGS_Table_PS, coords = c("easting", "northing"), remove=FALSE, crs = "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs")
          WGS_Table_PS <- sf::st_transform(WGS_Table_PS, crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
          sf::st_write(WGS_Table_PS, pathfile_gpkg, driver="GPKG")
        } # close if p = 1
        rm(dummy)
        rm(namefile_gpkg)
      } # close for parts
      rm(parts)
    } # close if parts
  }
  
  Write_track_GPKG <- function(PS_data, path, namefile, track) {
    parts <- as.integer(nrow(PS_data)/500000)+1
    if (parts == 1) {
      pathfile_gpkg <- paste0(path, namefile, "_", track, ".gpkg")
      WGS_Table_PS <- as.data.frame(PS_data)
      WGS_Table_PS[,2:length(WGS_Table_PS)] <- as.double(as.matrix(WGS_Table_PS[,2:length(WGS_Table_PS)]))
      WGS_Table_PS <- sf::st_as_sf(WGS_Table_PS, coords = c("easting", "northing"), remove=FALSE, crs = "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs")
      WGS_Table_PS <- sf::st_transform(WGS_Table_PS, crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
      sf::st_write(WGS_Table_PS, pathfile_gpkg, driver="GPKG", append=F)
    } else {
      for (p in 1:parts) {
        if (p == 1) {
          dummy <- PS_data[1:as.integer(nrow(PS_data)*p/parts),]
          namefile_gpkg <- paste0(namefile, "_", track, "_part", p)
          pathfile_gpkg <- paste0(path, namefile_gpkg, ".gpkg")
          WGS_Table_PS <- as.data.frame(dummy)
          WGS_Table_PS[,2:length(WGS_Table_PS)] <- as.double(as.matrix(WGS_Table_PS[,2:length(WGS_Table_PS)]))
          WGS_Table_PS <- sf::st_as_sf(WGS_Table_PS, coords = c("easting", "northing"), remove=FALSE, crs = "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs")
          WGS_Table_PS <- sf::st_transform(WGS_Table_PS, crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
          sf::st_write(WGS_Table_PS, pathfile_gpkg, driver="GPKG")
        } else  {
          dummy <- PS_data[as.integer((nrow(PS_data)*(p-1)/parts)+1):as.integer(nrow(PS_data)*p/parts),]
          namefile_gpkg <- paste0(namefile, "_", track, "_part", p)
          pathfile_gpkg <- paste0(path, namefile_gpkg, ".gpkg")
          WGS_Table_PS <- as.data.frame(dummy)
          WGS_Table_PS[,2:length(WGS_Table_PS)] <- as.double(as.matrix(WGS_Table_PS[,2:length(WGS_Table_PS)]))
          WGS_Table_PS <- sf::st_as_sf(WGS_Table_PS, coords = c("easting", "northing"), remove=FALSE, crs = "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs")
          WGS_Table_PS <- sf::st_transform(WGS_Table_PS, crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
          sf::st_write(WGS_Table_PS, pathfile_gpkg, driver="GPKG")
        } # close if p = 1
        rm(dummy)
        rm(namefile_gpkg)
      } # close for parts
      rm(parts)
    } # close if parts
  }
  
  PS_aoiL2_dmy <- function(csv_list, path, aoi) {
  for (i in 1:length(csv_list)) {
    pathfile <- paste0(path, csv_list[i])
    Table_PS_all <- read_csv(pathfile)
    Table_PS <- Table_PS_all[1]
    column_ok <- c("pid", "mp_type", "latitude", "longitude", "easting", "northing", "height_wgs84", "temporal_coherence",
                   "incidence_angle", "track_angle", "los_east", "los_north", "los_up", "mean_velocity", "mean_velocity_std")
    
    for (a in 2:ncol(Table_PS_all)) {
      if (sum(colnames(Table_PS_all[a]) == column_ok, na.rm = TRUE) >= 1 | substr(colnames(Table_PS_all[a]), 0, 2) == "20") {
        Table_PS <- cbind(Table_PS, Table_PS_all[a])
      } # close if column
    } # close for column
    column <- c("pid", "eff_area", "latitude", "longitude", "easting", "northing", "height", "coherence",
                "inc_angle", "track_angle", "los_east", "los_north", "los_up", "vel", "vel_std")
    # modify date in Dddmmyyyy
    #Date_PS <- colnames(Table_PS[, grepl("20", names(Table_PS))])
    Date_PS <- colnames(Table_PS[1,16:ncol(Table_PS)])
    Date_PS <- Ddmy(Date_PS)
    colnames(Table_PS) <- c(column, Date_PS)
    # select PS into aoi
    sel1 <- Table_PS[which(select(Table_PS, longitude)>xmin(aoi), arr.ind = T)[,1],]
    sel2 <- sel1[which(select(sel1, longitude)<xmax(aoi), arr.ind = T)[,1],]
    sel3 <- sel2[which(select(sel2, latitude)>ymin(aoi), arr.ind = T)[,1],]
    sel_final <- sel3[which(select(sel3, latitude)<ymax(aoi), arr.ind = T)[,1],]
    assign(paste0("PS_sel", i), sel_final)
    # write PS_AoI
    if (i == 1) {
      PS_sel_aoi <- get("PS_sel1")
    } else {
      for (l in 1:i) {
        if (l == 1) {
          PS_sel_aoi <- get(paste0("PS_sel", l))
        } else {
          PS_sel_aoi <- rbind(PS_sel_aoi, get(paste0("PS_sel", l)))
        }
      }
    }
    colnames(PS_sel_aoi) <- c(column, Date_PS)
  }
  return(PS_sel_aoi)
}
  
  PS_aoiL2_ymd <- function(csv_list, path, aoi) {
    for (i in 1:length(csv_list)) {
      pathfile <- paste0(path, csv_list[i])
      Table_PS_all <- read_csv(pathfile)
      Table_PS <- Table_PS_all[1]
      column_ok <- c("pid", "mp_type", "latitude", "longitude", "easting", "northing", "height_wgs84", "temporal_coherence",
                     "incidence_angle", "track_angle", "los_east", "los_north", "los_up", "mean_velocity", "mean_velocity_std")
      
      for (a in 2:ncol(Table_PS_all)) {
        if (sum(colnames(Table_PS_all[a]) == column_ok, na.rm = TRUE) >= 1 | substr(colnames(Table_PS_all[a]), 0, 2) == "20") {
          Table_PS <- cbind(Table_PS, Table_PS_all[a])
        } # close if column
      } # close for column
      column <- c("pid", "eff_area", "latitude", "longitude", "easting", "northing", "height", "coherence",
                  "inc_angle", "track_angle", "los_east", "los_north", "los_up", "vel", "vel_std")
      # modify date in Dyyyymmdd
      #Date_PS <- colnames(Table_PS[, grepl("20", names(Table_PS))])
      Date_PS <- colnames(Table_PS[1,16:ncol(Table_PS)])
      Date_PS <- Dymd(Date_PS)
      colnames(Table_PS) <- c(column, Date_PS)
      # select PS into aoi
      sel1 <- Table_PS[which(select(Table_PS, longitude)>xmin(aoi), arr.ind = T)[,1],]
      sel2 <- sel1[which(select(sel1, longitude)<xmax(aoi), arr.ind = T)[,1],]
      sel3 <- sel2[which(select(sel2, latitude)>ymin(aoi), arr.ind = T)[,1],]
      sel_final <- sel3[which(select(sel3, latitude)<ymax(aoi), arr.ind = T)[,1],]
      assign(paste0("PS_sel", i), sel_final)
      
      # write PS_AoI
      if (i == 1) {
        PS_sel_aoi <- get("PS_sel1")
      } else {
        for (l in 1:i) {
          if (l == 1) {
            PS_sel_aoi <- get(paste0("PS_sel", l))
          } else {
            PS_sel_aoi <- rbind(PS_sel_aoi, get(paste0("PS_sel", l)))
          }
        }
      }
      colnames(PS_sel_aoi) <- colnames(Table_PS)
    }
    return(PS_sel_aoi)
  }
  
  PS_aoiL2_NTS <- function(csv_list, path, aoi) {
    for (i in 1:length(csv_list)) {
      pathfile <- paste0(path, csv_list[i])
      Table_PS_all <- read_csv(pathfile)
      Table_PS <- Table_PS_all[1]
      column_ok <- c("pid", "mp_type", "latitude", "longitude", "easting", "northing", "height_wgs84", "temporal_coherence",
                     "incidence_angle", "track_angle", "los_east", "los_north", "los_up", "mean_velocity", "mean_velocity_std")
      
      for (a in 2:ncol(Table_PS_all)) {
        if (sum(colnames(Table_PS_all[a]) == column_ok, na.rm = TRUE) >= 1) {
          Table_PS <- cbind(Table_PS, Table_PS_all[a])
        } # close if column
      } # close for column
      column <- c("pid", "eff_area", "latitude", "longitude", "easting", "northing", "height", "coherence",
                  "inc_angle", "track_angle", "los_east", "los_north", "los_up", "vel", "vel_std")
      colnames(Table_PS) <- column
      # select PS into aoi
      sel1 <- Table_PS[which(select(Table_PS, longitude)>xmin(aoi), arr.ind = T)[,1],]
      sel2 <- sel1[which(select(sel1, longitude)<xmax(aoi), arr.ind = T)[,1],]
      sel3 <- sel2[which(select(sel2, latitude)>ymin(aoi), arr.ind = T)[,1],]
      sel_final <- sel3[which(select(sel3, latitude)<ymax(aoi), arr.ind = T)[,1],]
      assign(paste0("PS_sel", i), sel_final)
      
      # write PS_AoI
      if (i == 1) {
        PS_sel_aoi <- get("PS_sel1")
      } else {
        for (l in 1:i) {
          if (l == 1) {
            PS_sel_aoi <- get(paste0("PS_sel", l))
          } else {
            PS_sel_aoi <- rbind(PS_sel_aoi, get(paste0("PS_sel", l)))
          }
        }
      }
      colnames(PS_sel_aoi) <- colnames(Table_PS)
    }
    return(PS_sel_aoi)
  }
  
  PS_L2_dmy_toGPKG <- function(csv_list, path, ext_path) {
    pathfile <- paste0(path, csv_list)
    Table_PS_all <- read_csv(pathfile)
    Table_PS <- Table_PS_all[1]
    column_ok <- c("pid", "mp_type", "latitude", "longitude", "easting", "northing", "height_wgs84", "temporal_coherence",
                   "incidence_angle", "track_angle", "los_east", "los_north", "los_up", "mean_velocity", "mean_velocity_std")
    
    for (a in 2:ncol(Table_PS_all)) {
      if (sum(colnames(Table_PS_all[a]) == column_ok, na.rm = TRUE) >= 1 | substr(colnames(Table_PS_all[a]), 0, 2) == "20") {
        Table_PS <- cbind(Table_PS, Table_PS_all[a])
      } # close if column
    } # close for column
    column <- c("pid", "eff_area", "latitude", "longitude", "easting", "northing", "height", "coherence",
                "inc_angle", "track_angle", "los_east", "los_north", "los_up", "vel", "vel_std")
    # modify date in Dddmmyyyy
    Date_PS <- colnames(Table_PS[1,16:ncol(Table_PS)])
    Date_PS <- Ddmy(Date_PS)
    colnames(Table_PS) <- c(column, Date_PS)
    #write_GPKG
    namefile <- paste0(substr(csv_list, 1, nchar(csv_list)-4))
    Write_GPKG(Table_PS, ext_path, namefile)
  }
  
  PS_L2_dmy_toSHP <- function(csv_list, path, ext_path) {
    pathfile <- paste0(path, csv_list)
    Table_PS_all <- read_csv(pathfile)
    Table_PS <- Table_PS_all[1]
    column_ok <- c("pid", "mp_type", "latitude", "longitude", "easting", "northing", "height_wgs84", "temporal_coherence",
                   "incidence_angle", "track_angle", "los_east", "los_north", "los_up", "mean_velocity", "mean_velocity_std")
    
    for (a in 2:ncol(Table_PS_all)) {
      if (sum(colnames(Table_PS_all[a]) == column_ok, na.rm = TRUE) >= 1 | substr(colnames(Table_PS_all[a]), 0, 2) == "20") {
        Table_PS <- cbind(Table_PS, Table_PS_all[a])
      } # close if column
    } # close for column
    column <- c("pid", "eff_area", "latitude", "longitude", "easting", "northing", "height", "coherence",
                "inc_angle", "track_angle", "los_east", "los_north", "los_up", "vel", "vel_std")
    # modify date in Dddmmyyyy
    Date_PS <- colnames(Table_PS[1,16:ncol(Table_PS)])
    Date_PS <- Ddmy(Date_PS)
    colnames(Table_PS) <- c(column, Date_PS)
    #write_SHP
    namefile <- paste0(substr(csv_list, 1, nchar(csv_list)-4))
    Write_SHP(Table_PS, ext_path, namefile)
  }
  
  PS_L2_ymd_toGPKG <- function(csv_list, path, ext_path) {
    pathfile <- paste0(path, csv_list)
    Table_PS_all <- read_csv(pathfile)
    Table_PS <- Table_PS_all[1]
    column_ok <- c("pid", "mp_type", "latitude", "longitude", "easting", "northing", "height_wgs84", "temporal_coherence",
                   "incidence_angle", "track_angle", "los_east", "los_north", "los_up", "mean_velocity", "mean_velocity_std")
    
    for (a in 2:ncol(Table_PS_all)) {
      if (sum(colnames(Table_PS_all[a]) == column_ok, na.rm = TRUE) >= 1 | substr(colnames(Table_PS_all[a]), 0, 2) == "20") {
        Table_PS <- cbind(Table_PS, Table_PS_all[a])
      } # close if column
    } # close for column
    column <- c("pid", "eff_area", "latitude", "longitude", "easting", "northing", "height", "coherence",
                "inc_angle", "track_angle", "los_east", "los_north", "los_up", "vel", "vel_std")
    # modify date in Dyyyymmdd
    Date_PS <- colnames(Table_PS[1,16:ncol(Table_PS)])
    Date_PS <- Dymd(Date_PS)
    colnames(Table_PS) <- c(column, Date_PS)
    #write_GPKG
    namefile <- paste0(substr(csv_list, 1, nchar(csv_list)-4))
    Write_GPKG(Table_PS, ext_path, namefile)
  }
  
  PS_L2_ymd_toSHP <- function(csv_list, path, ext_path) {
    pathfile <- paste0(path, csv_list)
    Table_PS_all <- read_csv(pathfile)
    Table_PS <- Table_PS_all[1]
    column_ok <- c("pid", "mp_type", "latitude", "longitude", "easting", "northing", "height_wgs84", "temporal_coherence",
                   "incidence_angle", "track_angle", "los_east", "los_north", "los_up", "mean_velocity", "mean_velocity_std")
    
    for (a in 2:ncol(Table_PS_all)) {
      if (sum(colnames(Table_PS_all[a]) == column_ok, na.rm = TRUE) >= 1 | substr(colnames(Table_PS_all[a]), 0, 2) == "20") {
        Table_PS <- cbind(Table_PS, Table_PS_all[a])
      } # close if column
    } # close for column
    column <- c("pid", "eff_area", "latitude", "longitude", "easting", "northing", "height", "coherence",
                "inc_angle", "track_angle", "los_east", "los_north", "los_up", "vel", "vel_std")
    # modify date in Dyyyymmdd
    Date_PS <- colnames(Table_PS[1,16:ncol(Table_PS)])
    Date_PS <- Dymd(Date_PS)
    colnames(Table_PS) <- c(column, Date_PS)
    #write_SHP
    namefile <- paste0(substr(csv_list, 1, nchar(csv_list)-4))
    Write_SHP(Table_PS, ext_path, namefile)
  }
  
  PS_L2_NTS_toGPKP <- function(csv_list, path, ext_path) {
    pathfile <- paste0(path, csv_list)
    Table_PS_all <- read_csv(pathfile)
    Table_PS <- Table_PS_all[1]
    column_ok <- c("pid", "mp_type", "latitude", "longitude", "easting", "northing", "height_wgs84", "temporal_coherence",
                   "incidence_angle", "track_angle", "los_east", "los_north", "los_up", "mean_velocity", "mean_velocity_std")
    
    for (a in 2:ncol(Table_PS_all)) {
      if (sum(colnames(Table_PS_all[a]) == column_ok, na.rm = TRUE) >= 1) {
        Table_PS <- cbind(Table_PS, Table_PS_all[a])
      } # close if column
    } # close for column
    column <- c("pid", "eff_area", "latitude", "longitude", "easting", "northing", "height", "coherence",
                "inc_angle", "track_angle", "los_east", "los_north", "los_up", "vel", "vel_std")
    colnames(Table_PS) <- column
    #write_GPKG
    namefile <- paste0(substr(csv_list, 1, nchar(csv_list)-4))
    Write_GPKG(Table_PS, ext_path, namefile)
  }
  
  PS_L2_NTS_toSHP <- function(csv_list, path, ext_path) {
    pathfile <- paste0(path, csv_list)
    Table_PS_all <- read_csv(pathfile)
    Table_PS <- Table_PS_all[1]
    column_ok <- c("pid", "mp_type", "latitude", "longitude", "easting", "northing", "height_wgs84", "temporal_coherence",
                   "incidence_angle", "track_angle", "los_east", "los_north", "los_up", "mean_velocity", "mean_velocity_std")
    
    for (a in 2:ncol(Table_PS_all)) {
      if (sum(colnames(Table_PS_all[a]) == column_ok, na.rm = TRUE) >= 1) {
        Table_PS <- cbind(Table_PS, Table_PS_all[a])
      } # close if column
    } # close for column
    column <- c("pid", "eff_area", "latitude", "longitude", "easting", "northing", "height", "coherence",
                "inc_angle", "track_angle", "los_east", "los_north", "los_up", "vel", "vel_std")
    colnames(Table_PS) <- column
    #write_SHP
    namefile <- paste0(substr(csv_list, 1, nchar(csv_list)-4))
    Write_SHP(Table_PS, ext_path, namefile)
  }
  
  PS_aoiL3_dmy <- function(csv_list, path, aoi_eea) {
    for (i in 1:length(csv_list)) {
      # progress$inc(1/length(csv_list), detail = paste("L3 data", i, "of", length(csv_list)))
      # Sys.sleep(0.25)
      # progress_list2 = c(progress_list2, paste("Converted", csv_list[i],'\n'))
      pathfile <- paste0(path, csv_list[i])
      Table_PS_all <- read_csv(pathfile)
      Table_PS <- Table_PS_all[1]
      column_ok <- c("pid", "easting", "northing", "height", "mean_velocity", "mean_velocity_std")
      
      for (a in 2:ncol(Table_PS_all)) {
        if (sum(colnames(Table_PS_all[a]) == column_ok, na.rm = TRUE) >= 1 | substr(colnames(Table_PS_all[a]), 0, 2) == "20") {
          Table_PS <- cbind(Table_PS, Table_PS_all[a])
        } # close if column
      } # close for column
      column <- c("pid", "easting", "northing", "height", "vel", "vel_std")
      # modify date in Dddmmyyyy
      Date_PS <- colnames(Table_PS[, grepl("20", names(Table_PS))])
      Date_PS <- Ddmy(Date_PS)
      colnames(Table_PS) <- c(column, Date_PS)
      # select PS into aoi_eea
      sel1 <- Table_PS[which(select(Table_PS, easting)>xmin(aoi_eea), arr.ind = T)[,1],]
      sel2 <- sel1[which(select(sel1, easting)<xmax(aoi_eea), arr.ind = T)[,1],]
      sel3 <- sel2[which(select(sel2, northing)>ymin(aoi_eea), arr.ind = T)[,1],]
      sel_final <- sel3[which(select(sel3, northing)<ymax(aoi_eea), arr.ind = T)[,1],]
      assign(paste0("PS_sel", i), sel_final)
      # write PS_AoI
      if (i == 1) {
        PS_sel_aoi <- get("PS_sel1")
      } else {
        for (l in 1:i) {
          if (l == 1) {
            PS_sel_aoi <- get(paste0("PS_sel", l))
          } else {
            PS_sel_aoi <- rbind(PS_sel_aoi, get(paste0("PS_sel", l)))
          }
        }
      }
      colnames(PS_sel_aoi) <- colnames(Table_PS)
    }
    return(PS_sel_aoi)
  }
  
  PS_aoiL3_ymd <- function(csv_list, path, aoi_eea) {
    for (i in 1:length(csv_list)) {
      pathfile <- paste0(path, csv_list[i])
      Table_PS_all <- read_csv(pathfile)
      Table_PS <- Table_PS_all[1]
      column_ok <- c("pid", "easting", "northing", "height", "mean_velocity", "mean_velocity_std")
      
      for (a in 2:ncol(Table_PS_all)) {
        if (sum(colnames(Table_PS_all[a]) == column_ok, na.rm = TRUE) >= 1 | substr(colnames(Table_PS_all[a]), 0, 2) == "20") {
          Table_PS <- cbind(Table_PS, Table_PS_all[a])
        } # close if column
      } # close for column
      column <- c("pid", "easting", "northing", "height", "vel", "vel_std")
      # modify date in Dyyyymmdd
      Date_PS <- colnames(Table_PS[, grepl("20", names(Table_PS))])
      Date_PS <- Dymd(Date_PS)
      colnames(Table_PS) <- c(column, Date_PS)
      # select PS into aoi_eea
      sel1 <- Table_PS[which(select(Table_PS, easting)>xmin(aoi_eea), arr.ind = T)[,1],]
      sel2 <- sel1[which(select(sel1, easting)<xmax(aoi_eea), arr.ind = T)[,1],]
      sel3 <- sel2[which(select(sel2, northing)>ymin(aoi_eea), arr.ind = T)[,1],]
      sel_final <- sel3[which(select(sel3, northing)<ymax(aoi_eea), arr.ind = T)[,1],]
      assign(paste0("PS_sel", i), sel_final)
      # write PS_AoI
      if (i == 1) {
        PS_sel_aoi <- get("PS_sel1")
      } else {
        for (l in 1:i) {
          if (l == 1) {
            PS_sel_aoi <- get(paste0("PS_sel", l))
          } else {
            PS_sel_aoi <- rbind(PS_sel_aoi, get(paste0("PS_sel", l)))
          }
        }
      }
      colnames(PS_sel_aoi) <- colnames(Table_PS)
    }
    return(PS_sel_aoi)
  }
  
  PS_aoiL3_NTS <- function(csv_list, path, aoi_eea) {
    for (i in 1:length(csv_list)) {
      pathfile <- paste0(path, csv_list[i])
      Table_PS_all <- read_csv(pathfile)
      Table_PS <- Table_PS_all[1]
      column_ok <- c("pid", "easting", "northing", "height", "mean_velocity", "mean_velocity_std")
      
      for (a in 2:ncol(Table_PS_all)) {
        if (sum(colnames(Table_PS_all[a]) == column_ok, na.rm = TRUE) >= 1) {
          Table_PS <- cbind(Table_PS, Table_PS_all[a])
        } # close if column
      } # close for column
      column <- c("pid", "easting", "northing", "height", "vel", "vel_std")
      colnames(Table_PS) <- column
      # select PS into aoi_eea
      sel1 <- Table_PS[which(select(Table_PS, easting)>xmin(aoi_eea), arr.ind = T)[,1],]
      sel2 <- sel1[which(select(sel1, easting)<xmax(aoi_eea), arr.ind = T)[,1],]
      sel3 <- sel2[which(select(sel2, northing)>ymin(aoi_eea), arr.ind = T)[,1],]
      sel_final <- sel3[which(select(sel3, northing)<ymax(aoi_eea), arr.ind = T)[,1],]
      assign(paste0("PS_sel", i), sel_final)
      # write PS_AoI
      if (i == 1) {
        PS_sel_aoi <- get("PS_sel1")
      } else {
        for (l in 1:i) {
          if (l == 1) {
            PS_sel_aoi <- get(paste0("PS_sel", l))
          } else {
            PS_sel_aoi <- rbind(PS_sel_aoi, get(paste0("PS_sel", l)))
          }
        }
      }
      colnames(PS_sel_aoi) <- colnames(Table_PS)
    }
    return(PS_sel_aoi)
  }
  
  PS_L3_dmy_toGPKG <- function(csv_list, path, ext_path) {
    pathfile <- paste0(path, csv_list)
    Table_PS_all <- read_csv(pathfile)
    Table_PS <- Table_PS_all[1]
    column_ok <- c("pid", "easting", "northing", "height", "mean_velocity", "mean_velocity_std")
    
    for (a in 2:ncol(Table_PS_all)) {
      if (sum(colnames(Table_PS_all[a]) == column_ok, na.rm = TRUE) >= 1 | substr(colnames(Table_PS_all[a]), 0, 2) == "20") {
        Table_PS <- cbind(Table_PS, Table_PS_all[a])
      } # close if column
    } # close for column
    column <- c("pid", "easting", "northing", "height", "vel", "vel_std")
    # modify date in Dddmmyyyy
    Date_PS <- colnames(Table_PS[, grepl("20", names(Table_PS))])
    Date_PS <- Ddmy(Date_PS)
    colnames(Table_PS) <- c(column, Date_PS)
    #write_GPKG
    namefile <- paste0(substr(csv_list, 1, nchar(csv_list)-4))
    Write_GPKG(Table_PS, ext_path, namefile)
  }
  
  PS_L3_dmy_toSHP <- function(csv_list, path, ext_path) {
    pathfile <- paste0(path, csv_list)
    Table_PS_all <- read_csv(pathfile)
    Table_PS <- Table_PS_all[1]
    column_ok <- c("pid", "easting", "northing", "height", "mean_velocity", "mean_velocity_std")
    
    for (a in 2:ncol(Table_PS_all)) {
      if (sum(colnames(Table_PS_all[a]) == column_ok, na.rm = TRUE) >= 1 | substr(colnames(Table_PS_all[a]), 0, 2) == "20") {
        Table_PS <- cbind(Table_PS, Table_PS_all[a])
      } # close if column
    } # close for column
    column <- c("pid", "easting", "northing", "height", "vel", "vel_std")
    # modify date in Dddmmyyyy
    Date_PS <- colnames(Table_PS[, grepl("20", names(Table_PS))])
    Date_PS <- Ddmy(Date_PS)
    colnames(Table_PS) <- c(column, Date_PS)
    #write_SHP
    namefile <- paste0(substr(csv_list, 1, nchar(csv_list)-4))
    Write_SHP(Table_PS, ext_path, namefile)
  }
  
  PS_L3_ymd_toGPKG <- function(csv_list, path, ext_path) {
    pathfile <- paste0(path, csv_list)
    Table_PS_all <- read_csv(pathfile)
    Table_PS <- Table_PS_all[1]
    column_ok <- c("pid", "easting", "northing", "height", "mean_velocity", "mean_velocity_std")
    
    for (a in 2:ncol(Table_PS_all)) {
      if (sum(colnames(Table_PS_all[a]) == column_ok, na.rm = TRUE) >= 1 | substr(colnames(Table_PS_all[a]), 0, 2) == "20") {
        Table_PS <- cbind(Table_PS, Table_PS_all[a])
      } # close if column
    } # close for column
    column <- c("pid", "easting", "northing", "height", "vel", "vel_std")
    # modify date in Dyyyymmdd
    Date_PS <- colnames(Table_PS[, grepl("20", names(Table_PS))])
    Date_PS <- Dymd(Date_PS)
    colnames(Table_PS) <- c(column, Date_PS)
    #write_GPKG
    namefile <- paste0(substr(csv_list, 1, nchar(csv_list)-4))
    Write_GPKG(Table_PS, ext_path, namefile)
  }
  
  PS_L3_ymd_toSHP <- function(csv_list, path, ext_path) {
    pathfile <- paste0(path, csv_list)
    Table_PS_all <- read_csv(pathfile)
    Table_PS <- Table_PS_all[1]
    column_ok <- c("pid", "easting", "northing", "height", "mean_velocity", "mean_velocity_std")
    
    for (a in 2:ncol(Table_PS_all)) {
      if (sum(colnames(Table_PS_all[a]) == column_ok, na.rm = TRUE) >= 1 | substr(colnames(Table_PS_all[a]), 0, 2) == "20") {
        Table_PS <- cbind(Table_PS, Table_PS_all[a])
      } # close if column
    } # close for column
    column <- c("pid", "easting", "northing", "height", "vel", "vel_std")
    # modify date in Dyyyymmdd
    Date_PS <- colnames(Table_PS[, grepl("20", names(Table_PS))])
    Date_PS <- Dymd(Date_PS)
    colnames(Table_PS) <- c(column, Date_PS)
    #write_SHP
    namefile <- paste0(substr(csv_list, 1, nchar(csv_list)-4))
    Write_SHP(Table_PS, ext_path, namefile)
  }
  
  PS_L3_NTS_toGPKG <- function(csv_list, path, ext_path) {
    pathfile <- paste0(path, csv_list)
    Table_PS_all <- read_csv(pathfile)
    Table_PS <- Table_PS_all[1]
    column_ok <- c("pid", "easting", "northing", "height", "mean_velocity", "mean_velocity_std")
    
    for (a in 2:ncol(Table_PS_all)) {
      if (sum(colnames(Table_PS_all[a]) == column_ok, na.rm = TRUE) >= 1) {
        Table_PS <- cbind(Table_PS, Table_PS_all[a])
      } # close if column
    } # close for column
    column <- c("pid", "easting", "northing", "height", "vel", "vel_std")
    colnames(Table_PS) <- column
    #write_GPKG
    namefile <- paste0(substr(csv_list, 1, nchar(csv_list)-4))
    Write_GPKG(Table_PS, ext_path, namefile)
  }
  
  PS_L3_NTS_toSHP <- function(csv_list, path, ext_path) {
    pathfile <- paste0(path, csv_list)
    Table_PS_all <- read_csv(pathfile)
    Table_PS <- Table_PS_all[1]
    column_ok <- c("pid", "easting", "northing", "height", "mean_velocity", "mean_velocity_std")
    
    for (a in 2:ncol(Table_PS_all)) {
      if (sum(colnames(Table_PS_all[a]) == column_ok, na.rm = TRUE) >= 1) {
        Table_PS <- cbind(Table_PS, Table_PS_all[a])
      } # close if column
    } # close for column
    column <- c("pid", "easting", "northing", "height", "vel", "vel_std")
    colnames(Table_PS) <- column
    #write_SHP
    namefile <- paste0(substr(csv_list, 1, nchar(csv_list)-4))
    Write_SHP(Table_PS, ext_path, namefile)
  }
  
  init <- function() {
    renderLeaflet({
      leaf <- 
        leaflet() %>%
        setView(lng=10.502930, lat=56.721087, zoom = 4) %>%
        addProviderTiles(providers$Esri.WorldImagery, group='Esri World Imagery') %>%
        addProviderTiles(providers$Esri.WorldStreetMap, group = "Esri World StreetMap") %>%
        addLayersControl(baseGroups = c("Esri World Imagery",'Esri World StreetMap'),
                         options = layersControlOptions(collapsed = FALSE)) %>% 
        addDrawToolbar(
          targetGroup='Draw AoI',
          polylineOptions=FALSE,
          markerOptions = FALSE,
          polygonOptions = FALSE,
          circleMarkerOptions = FALSE,
          circleOptions = FALSE,
          singleFeature = TRUE,
          # editOptions = editToolbarOptions(edit = TRUE, remove = TRUE, selectedPathOptions = NULL,
          #                                  allowIntersection = TRUE),
          rectangleOptions = filterNULL(list(shapeOptions = drawShapeOptions(color = "#03f", fill=T, weight=2, fillColor = "#03f", fillOpacity = 0.4),
                                             repeatMode = FALSE,
                                             showArea = TRUE, metric = TRUE))) # %>%
      # addControl(actionButton(inputId= "clearAoI",
      #                         label ="", 
      #                         icon=icon("trash")),position="topleft")
    })
  }
  
  init2 <- function() {
    renderLeaflet({
      leaf <- 
        leaflet() %>%
        setView(lng=10.502930, lat=56.721087, zoom = 4) %>%
        addProviderTiles(providers$Esri.WorldImagery, group='Esri World Imagery') %>%
        addProviderTiles(providers$Esri.WorldStreetMap, group = "Esri World StreetMap") %>%
        addLayersControl(baseGroups = c("Esri World Imagery",'Esri World StreetMap'),
                         options = layersControlOptions(collapsed = FALSE))
    })
  }
  #### END FUNCTION ####
  
  volumes = getVolumes() # this makes the directory at the base of your computer.
  observe({
    shinyDirChoose(
      input,
      'folder',
      roots = volumes(),
    )
  })
  
  global <- reactiveValues(datapath = getwd())
  dir <- reactive(input$folder)
  
  #providers <- c('Esri.WorldImagery', 'Esri.WorldStreetMap')
  output$Map <- renderLeaflet({
    
    #generate map
    leaflet() %>%
      setView(lng=10.502930, lat=56.721087, zoom = 4) %>%
      addProviderTiles(providers$Esri.WorldImagery, group='Esri World Imagery') %>%
      addProviderTiles(providers$Esri.WorldStreetMap, group = "Esri World StreetMap") %>%
      addLayersControl(baseGroups = c("Esri World Imagery",'Esri World StreetMap'),
                       options = layersControlOptions(collapsed = FALSE))
    
  })
  
  # output$txt2 <- renderText({
  #   paste0(input$txt1)
  # })
  
  observeEvent(req(isTRUE(input$DrawAoI)), {
    output$Map <- init()
  })
  
  observeEvent(req(isFALSE(input$DrawAoI)), {
    output$Map <- init2()
    shinyjs::runjs("$('.leaflet-draw').remove()")
    removeUI(selector = "div:has(> #clearAoI)")
    latlongs$df2 <- latlongs$df2[FALSE,]
  })
  
  latlongs<-reactiveValues()   #temporary to hold coords
  latlongs$df2 <- data.frame(Longitude = numeric(0), Latitude = numeric(0))
  
  
  observeEvent(input$Map_draw_new_feature, {
    #get the coordinates of the polygon
    coord <- unlist(input$Map_draw_new_feature$geometry$coordinates)
    Longitude <- coord [seq(1,length(coord), 2)]
    Latitude <- coord[seq(2,length(coord), 2)]
    isolate(latlongs$df2<-rbind(latlongs$df2, cbind(Longitude, Latitude)))
  })
  
  observeEvent(input$clearAoI,{
    latlongs$df2 <- latlongs$df2[FALSE,]
    output$Map <- init()
  })
  
  observeEvent(ignoreNULL = FALSE,
               eventExpr = {input$folder},
               handlerExpr = {
                 if (!"path" %in% names(dir())) return()
                 global$datapath <- parseDirPath(roots = volumes, input$folder)
               }
  )
  output$dir <- renderText({
    global$datapath
  })
  
  # output$txtout1 <- renderPrint({
  #   print(latlongs$df2)
  # })
  
  #### Function triggering EGMS process WITH Time Series ####
  
  observeEvent(input$RUN,{
    
    # required info
    req(input$LINKS)
    req(global$datapath)
    
    file_txt <- read.table(input$LINKS$datapath, quote="\"", comment.char="")
    
    getOption('timeout')
    options(timeout=3500)
    exdir_dw <- paste0(global$datapath, "/Downloaded/")
    dir.create(exdir_dw)
    
    #setting progress bar
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Downloading", value = 0)
    progress_list = c("Download history:\n")
    
    for (i in 1:nrow(file_txt)) {
      name <- substr(file_txt[i,], 60, which(strsplit(as.character(file_txt[i,]), "") [[1]] == "?")-1)
      myURL = paste(file_txt[i,],sep = "")
      dir = paste0(exdir_dw, name)
      download.file(myURL, dir, mode="wb", quiet = TRUE)
      progress$inc(1/length(file_txt), detail = paste(i, "of", nrow(file_txt)))
      Sys.sleep(0)
      progress_list = c(progress_list, paste(name, "downloaded", '\n'))
    } # close for download
    
    
    list_zip <- list.files(path = exdir_dw, all.files = FALSE, full.names = FALSE, recursive = FALSE,
                           ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
    
    file_zip <- array()
    for (i in 1:length(list_zip)) {
      dummy <- substr(list_zip[i], nchar(list_zip[i])-3, nchar(list_zip[i]))
      if (dummy == ".zip") {
        file_zip <- append(file_zip, list_zip[i])
      } # close if .zip
    } # close for .zip
    file_zip <- file_zip[2:length(file_zip)]
    
    exdir_unz <- paste0(global$datapath, "/Unzipped/")
    dir.create(exdir_unz)
    
    #setting progress bar
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Unzipping", value = 0)
    progress_list = c(progress_list, paste("\nUnzipping history:\n"))
    
    orbit_asc <- c("01", "02", "03", "04")
    orbit_desc <- c("06", "07", "08", "09")
    
    for (i in 1:length(file_zip)) {
      progress$inc(1/length(file_zip), detail = paste(i, "of", length(file_zip)))
      Sys.sleep(0.25)
      progress_list = c(progress_list, paste("Unzipped", " ", i, "of", " ", length(file_zip), '\n'))
      print(paste("Unzipping", i, "of", length(file_zip)))
      unzip(paste0(exdir_dw, file_zip[i]), exdir = exdir_unz)
      
      name_dummy <- substr(file_zip[i], 1, nchar(file_zip[i])-4)
      product <- substr(name_dummy, 6, 8)
      orbit <- substr(name_dummy, 14, 15)
      comp <- substr(name_dummy, nchar(name_dummy)-0, nchar(name_dummy)-0)
    } # close for unzip
    
    rm(name_dummy)
    rm(product)
    rm(orbit)
    rm(comp)
    unlink(exdir_dw, recursive = T, force = T)
    
    output$txtout1 <- renderPrint({
      cat(progress_list, "\nData download and unzipping completed")
    })
    
    #setting progress bar
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Converting", value = 0)
    progress_list2 = c("Conversion history:\n")
    
    # latlongs<-reactiveValues()   #temporary to hold coords
    # latlongs$df2 <- data.frame(Longitude = numeric(0), Latitude = numeric(0))
    # value<-reactiveValues()
    
    if (nrow(latlongs$df2)!=0) {
      
      # #get the coordinates of the polygon
      coord <- unlist(input$Map_draw_new_feature$geometry$coordinates)
      Longitude <- coord [seq(1,length(coord), 2)]
      Latitude <- coord[seq(2,length(coord), 2)]
      aoi <- isolate(latlongs$df2<-rbind(latlongs$df2, cbind(Longitude, Latitude)))
      aoi <- aoi[1:4,]
      coordinates(aoi) <- c("Longitude", "Latitude")
      proj4string(aoi) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")    # CRS("+init=epsg:4326") # WGS 84
      CRS_eea <- CRS("+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs")
      aoi_eea <- spTransform(aoi, CRS_eea)
      aoi_eea <- as.data.frame(aoi_eea)
      colnames(aoi_eea) <- c("easting", "northing")
      coordinates(aoi_eea) <- c("easting", "northing")
      proj4string(aoi_eea) <- CRS("+init=epsg:3035") # LAEA Europe
      
      
      if (input$TimeSeriesSelection == "YTS" && input$FormatSelection == "SHP") {
        if (input$DataFormat == "ymd") {
          list_file <- list.files(path = exdir_unz, all.files = FALSE, full.names = FALSE, recursive = FALSE,
                                  ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
          
          csv_L2a_asc <- matrix(nrow = 0, ncol = 0)
          csv_L2a_desc <- matrix(nrow = 0, ncol = 0)
          csv_L2b_asc <- matrix(nrow = 0, ncol = 0)
          csv_L2b_desc <- matrix(nrow = 0, ncol = 0)
          csv_L3_horiz <- matrix(nrow = 0, ncol = 0)
          csv_L3_vert <- matrix(nrow = 0, ncol = 0)
          orbit_asc <- c("01", "02", "03", "04")
          orbit_desc <- c("06", "07", "08", "09")
          
          for (i in 1:length(list_file)) {
            product <- substr(list_file[i], 6, 8)
            orbit <- substr(list_file[i], 14, 15)
            comp <- substr(list_file[i], nchar(list_file[i])-4, nchar(list_file[i])-4)
            extens <- substr(list_file[i], nchar(list_file[i])-3, nchar(list_file[i]))
            if (extens == ".csv" && orbit %in% orbit_asc && product == "L2a") {
              csv_L2a_asc <- append(csv_L2a_asc, list_file[i])
            } # close if L2a asc
            if (extens == ".csv" && orbit %in% orbit_asc && product == "L2b") {
              csv_L2b_asc <- append(csv_L2b_asc, list_file[i])
            } # close if L2b asc
            if (extens == ".csv" && orbit %in% orbit_desc && product == "L2a") {
              csv_L2a_desc <- append(csv_L2a_desc, list_file[i])
            } # close if L2a desc
            if (extens == ".csv" && orbit %in% orbit_desc && product == "L2b") {
              csv_L2b_desc <- append(csv_L2b_desc, list_file[i])
            } # close if L2b desc
            if (extens == ".csv" && comp == "E") {
              csv_L3_horiz <- append(csv_L3_horiz, list_file[i])
            } # close if L3 E
            if (extens == ".csv" && comp == "U") {
              csv_L3_vert <- append(csv_L3_vert, list_file[i])
            } # close if L3 U
          } # close for 
          
          rm(product)
          rm(orbit)
          rm(comp)
          rm(extens)
          
          # create folder AoI
          dir.create(paste0(global$datapath, "/PS_AoI_SHP_TS/"))
          exdir <- paste0(global$datapath, "/PS_AoI_SHP_TS/")
          
          # LOS
          if (sum(csv_L2a_asc != 0) != 0) {
            tracks_csv <- array(dim = length(csv_L2a_asc))
            for (i in 1:length(csv_L2a_asc)) {
              tracks_csv[i] <- substring(csv_L2a_asc[i], 10, 12)
            }
            tracks <- unique(tracks_csv)
            for (i in 1:length(tracks)) {
              dummy <- csv_L2a_asc[which(tracks_csv == tracks[i], arr.ind = TRUE)]
              assign(tracks[i], dummy)
              rm(dummy)
            }
            for (t in 1:length(tracks)) {
              for (i in length(get(tracks[t]))) {
                PS_sel_aoi <- PS_aoiL2_ymd(get(tracks[t])[i], exdir_unz, aoi)
                if (nrow(PS_sel_aoi) !=0) {
                  Write_track_SHP(PS_sel_aoi, exdir, "L2a_asc_AoI", tracks[t])
                } else {
                  showNotification("AoI exceeding the extent of downloaded data!\n Please reshape AoI!", type = c("error"), duration=NULL)
                } 
              } # close for i get(tracks)
            } # close for t tracks 
          } else {
            rm(csv_L2a_asc)
          }
          
          if (sum(csv_L2a_desc != 0) != 0) {
            tracks_csv <- array(dim = length(csv_L2a_desc))
            for (i in 1:length(csv_L2a_desc)) {
              tracks_csv[i] <- substring(csv_L2a_desc[i], 10, 12)
            }
            tracks <- unique(tracks_csv)
            for (i in 1:length(tracks)) {
              dummy <- csv_L2a_desc[which(tracks_csv == tracks[i], arr.ind = TRUE)]
              assign(tracks[i], dummy)
              rm(dummy)
            }
            for (t in 1:length(tracks)) {
              for (i in length(get(tracks[t]))) {
                PS_sel_aoi <- PS_aoiL2_ymd(get(tracks[t])[i], exdir_unz, aoi)
                if (nrow(PS_sel_aoi) !=0) {
                  Write_track_SHP(PS_sel_aoi, exdir, "L2a_desc_AoI", tracks[t])
                } else {
                  showNotification("AoI exceeding the extent of downloaded data!\n Please reshape AoI!", type = c("error"), duration=NULL)
                } 
              } # close for i get(tracks)
            } # close for t tracks 
          } else {
            rm(csv_L2a_desc)
          }
          
          if (sum(csv_L2b_asc != 0) != 0) {
            tracks_csv <- array(dim = length(csv_L2b_asc))
            for (i in 1:length(csv_L2b_asc)) {
              tracks_csv[i] <- substring(csv_L2b_asc[i], 10, 12)
            }
            tracks <- unique(tracks_csv)
            for (i in 1:length(tracks)) {
              dummy <- csv_L2b_asc[which(tracks_csv == tracks[i], arr.ind = TRUE)]
              assign(tracks[i], dummy)
              rm(dummy)
            }
            for (t in 1:length(tracks)) {
              for (i in length(get(tracks[t]))) {
                PS_sel_aoi <- PS_aoiL2_ymd(get(tracks[t])[i], exdir_unz, aoi)
                if (nrow(PS_sel_aoi) !=0) {
                  Write_track_SHP(PS_sel_aoi, exdir, "L2b_asc_AoI", tracks[t])
                } else {
                  showNotification("AoI exceeding the extent of downloaded data!\n Please reshape AoI!", type = c("error"), duration=NULL)
                } 
              } # close for i get(tracks)
            } # close for t tracks 
          } else {
            rm(csv_L2b_asc)
          }
          
          if (sum(csv_L2b_desc != 0) != 0) {
            tracks_csv <- array(dim = length(csv_L2b_desc))
            for (i in 1:length(csv_L2b_desc)) {
              tracks_csv[i] <- substring(csv_L2b_desc[i], 10, 12)
            }
            tracks <- unique(tracks_csv)
            for (i in 1:length(tracks)) {
              dummy <- csv_L2b_desc[which(tracks_csv == tracks[i], arr.ind = TRUE)]
              assign(tracks[i], dummy)
              rm(dummy)
            }
            for (t in 1:length(tracks)) {
              for (i in length(get(tracks[t]))) {
                PS_sel_aoi <- PS_aoiL2_ymd(get(tracks[t])[i], exdir_unz, aoi)
                if (nrow(PS_sel_aoi) !=0) {
                  Write_track_SHP(PS_sel_aoi, exdir, "L2b_desc_AoI", tracks[t])
                } else {
                  showNotification("AoI exceeding the extent of downloaded data!\n Please reshape AoI!", type = c("error"), duration=NULL)
                } 
              } # close for i get(tracks)
            } # close for t tracks 
          } else {
            rm(csv_L2b_desc)
          }
          
          # COMPONENTS
          if (sum(csv_L3_horiz != 0)!= 0) {
            PS_sel_aoi <- PS_aoiL3_ymd(csv_L3_horiz, exdir_unz, aoi_eea)
            if (nrow(PS_sel_aoi) !=0) {
              Write_SHP(PS_sel_aoi, exdir, "L3_horiz_AoI")
            } else {
              showNotification("AoI exceeding the extent of downloaded data!\n Please reshape AoI!", type = c("error"), duration=NULL)
            }
          } else {
            rm(csv_L3_horiz)
          }
          
          if (sum(csv_L3_vert != 0)!= 0) {
            PS_sel_aoi <- PS_aoiL3_ymd(csv_L3_vert, exdir_unz, aoi_eea)
            if (nrow(PS_sel_aoi) !=0) {
              Write_SHP(PS_sel_aoi, exdir, "L3_vert_AoI")
            } else {
              showNotification("AoI exceeding the extent of downloaded data!\n Please reshape AoI!", type = c("error"), duration=NULL)
            }
          } else {
            rm(csv_L3_vert)
          } # close else
          
        } else if (input$DataFormat == "dmy") {
          list_file <- list.files(path = exdir_unz, all.files = FALSE, full.names = FALSE, recursive = FALSE,
                                  ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
          
          csv_L2a_asc <- matrix(nrow = 0, ncol = 0)
          csv_L2a_desc <- matrix(nrow = 0, ncol = 0)
          csv_L2b_asc <- matrix(nrow = 0, ncol = 0)
          csv_L2b_desc <- matrix(nrow = 0, ncol = 0)
          csv_L3_horiz <- matrix(nrow = 0, ncol = 0)
          csv_L3_vert <- matrix(nrow = 0, ncol = 0)
          orbit_asc <- c("01", "02", "03", "04")
          orbit_desc <- c("06", "07", "08", "09")
          
          for (i in 1:length(list_file)) {
            product <- substr(list_file[i], 6, 8)
            orbit <- substr(list_file[i], 14, 15)
            comp <- substr(list_file[i], nchar(list_file[i])-4, nchar(list_file[i])-4)
            extens <- substr(list_file[i], nchar(list_file[i])-3, nchar(list_file[i]))
            if (extens == ".csv" && orbit %in% orbit_asc && product == "L2a") {
              csv_L2a_asc <- append(csv_L2a_asc, list_file[i])
            } # close if L2a asc
            if (extens == ".csv" && orbit %in% orbit_asc && product == "L2b") {
              csv_L2b_asc <- append(csv_L2b_asc, list_file[i])
            } # close if L2b asc
            if (extens == ".csv" && orbit %in% orbit_desc && product == "L2a") {
              csv_L2a_desc <- append(csv_L2a_desc, list_file[i])
            } # close if L2a desc
            if (extens == ".csv" && orbit %in% orbit_desc && product == "L2b") {
              csv_L2b_desc <- append(csv_L2b_desc, list_file[i])
            } # close if L2b desc
            if (extens == ".csv" && comp == "E") {
              csv_L3_horiz <- append(csv_L3_horiz, list_file[i])
            } # close if L3 E
            if (extens == ".csv" && comp == "U") {
              csv_L3_vert <- append(csv_L3_vert, list_file[i])
            } # close if L3 U
          } # close for 
          
          rm(product)
          rm(orbit)
          rm(comp)
          rm(extens)
          
          # create folder AoI
          dir.create(paste0(global$datapath, "/PS_AoI_SHP_TS/"))
          exdir <- paste0(global$datapath, "/PS_AoI_SHP_TS/")
          
          #LOS
          if (sum(csv_L2a_asc != 0) != 0) {
            tracks_csv <- array(dim = length(csv_L2a_asc))
            for (i in 1:length(csv_L2a_asc)) {
              tracks_csv[i] <- substring(csv_L2a_asc[i], 10, 12)
            }
            tracks <- unique(tracks_csv)
            for (i in 1:length(tracks)) {
              dummy <- csv_L2a_asc[which(tracks_csv == tracks[i], arr.ind = TRUE)]
              assign(tracks[i], dummy)
              rm(dummy)
            }
            for (t in 1:length(tracks)) {
              for (i in length(get(tracks[t]))) {
                PS_sel_aoi <- PS_aoiL2_dmy(get(tracks[t])[i], exdir_unz, aoi)
                if (nrow(PS_sel_aoi) !=0) {
                  Write_track_SHP(PS_sel_aoi, exdir, "L2a_asc_AoI", tracks[t])
                } else {
                  showNotification("AoI exceeding the extent of downloaded data!\n Please reshape AoI!", type = c("error"), duration=NULL)
                } 
              } # close for i get(tracks)
            } # close for t tracks 
          } else {
            rm(csv_L2a_asc)
          }
          
          if (sum(csv_L2a_desc != 0) != 0) {
            tracks_csv <- array(dim = length(csv_L2a_desc))
            for (i in 1:length(csv_L2a_desc)) {
              tracks_csv[i] <- substring(csv_L2a_desc[i], 10, 12)
            }
            tracks <- unique(tracks_csv)
            for (i in 1:length(tracks)) {
              dummy <- csv_L2a_desc[which(tracks_csv == tracks[i], arr.ind = TRUE)]
              assign(tracks[i], dummy)
              rm(dummy)
            }
            for (t in 1:length(tracks)) {
              for (i in length(get(tracks[t]))) {
                PS_sel_aoi <- PS_aoiL2_dmy(get(tracks[t])[i], exdir_unz, aoi)
                if (nrow(PS_sel_aoi) !=0) {
                  Write_track_SHP(PS_sel_aoi, exdir, "L2a_desc_AoI", tracks[t])
                } else {
                  showNotification("AoI exceeding the extent of downloaded data!\n Please reshape AoI!", type = c("error"), duration=NULL)
                } 
              } # close for i get(tracks)
            } # close for t tracks 
          } else {
            rm(csv_L2a_desc)
          }
          
          if (sum(csv_L2b_asc != 0) != 0) {
            tracks_csv <- array(dim = length(csv_L2b_asc))
            for (i in 1:length(csv_L2b_asc)) {
              tracks_csv[i] <- substring(csv_L2b_asc[i], 10, 12)
            }
            tracks <- unique(tracks_csv)
            for (i in 1:length(tracks)) {
              dummy <- csv_L2b_asc[which(tracks_csv == tracks[i], arr.ind = TRUE)]
              assign(tracks[i], dummy)
              rm(dummy)
            }
            for (t in 1:length(tracks)) {
              for (i in length(get(tracks[t]))) {
                PS_sel_aoi <- PS_aoiL2_dmy(get(tracks[t])[i], exdir_unz, aoi)
                if (nrow(PS_sel_aoi) !=0) {
                  Write_track_SHP(PS_sel_aoi, exdir, "L2b_asc_AoI", tracks[t])
                } else {
                  showNotification("AoI exceeding the extent of downloaded data!\n Please reshape AoI!", type = c("error"), duration=NULL)
                } 
              } # close for i get(tracks)
            } # close for t tracks 
          } else {
            rm(csv_L2b_asc)
          }
          
          if (sum(csv_L2b_desc != 0) != 0) {
            tracks_csv <- array(dim = length(csv_L2b_desc))
            for (i in 1:length(csv_L2b_desc)) {
              tracks_csv[i] <- substring(csv_L2b_desc[i], 10, 12)
            }
            tracks <- unique(tracks_csv)
            for (i in 1:length(tracks)) {
              dummy <- csv_L2b_desc[which(tracks_csv == tracks[i], arr.ind = TRUE)]
              assign(tracks[i], dummy)
              rm(dummy)
            }
            for (t in 1:length(tracks)) {
              for (i in length(get(tracks[t]))) {
                PS_sel_aoi <- PS_aoiL2_dmy(get(tracks[t])[i], exdir_unz, aoi)
                if (nrow(PS_sel_aoi) !=0) {
                  Write_track_SHP(PS_sel_aoi, exdir, "L2b_desc_AoI", tracks[t])
                } else {
                  showNotification("AoI exceeding the extent of downloaded data!\n Please reshape AoI!", type = c("error"), duration=NULL)
                } 
              } # close for i get(tracks)
            } # close for t tracks 
          } else {
            rm(csv_L2b_desc)
          }
          
          # COMPONENTS
          if (sum(csv_L3_horiz != 0)!= 0) {
            PS_sel_aoi <- PS_aoiL3_dmy(csv_L3_horiz, exdir_unz, aoi_eea)
            if (nrow(PS_sel_aoi) !=0) {
              Write_SHP(PS_sel_aoi, exdir, "L3_horiz_AoI")
            } else {
              showNotification("AoI exceeding the extent of downloaded data!\n Please reshape AoI!", type = c("error"), duration=NULL)
            }
          } else {
            rm(csv_L3_horiz)
          }
          
          if (sum(csv_L3_vert != 0)!= 0) {
            PS_sel_aoi <- PS_aoiL3_dmy(csv_L3_vert, exdir_unz, aoi_eea)
            if (nrow(PS_sel_aoi) !=0) {
              Write_SHP(PS_sel_aoi, exdir, "L3_vert_AoI")
            } else {
              showNotification("AoI exceeding the extent of downloaded data!\n Please reshape AoI!", type = c("error"), duration=NULL)
            }
          } else {
            rm(csv_L3_vert)
          } # close else
        } # close YTS SHP dmy
        
      } else if (input$TimeSeriesSelection == "NTS" && input$FormatSelection == "SHP") {
        list_file <- list.files(path = exdir_unz, all.files = FALSE, full.names = FALSE, recursive = FALSE,
                                ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
        
        csv_L2a_asc <- matrix(nrow = 0, ncol = 0)
        csv_L2a_desc <- matrix(nrow = 0, ncol = 0)
        csv_L2b_asc <- matrix(nrow = 0, ncol = 0)
        csv_L2b_desc <- matrix(nrow = 0, ncol = 0)
        csv_L3_horiz <- matrix(nrow = 0, ncol = 0)
        csv_L3_vert <- matrix(nrow = 0, ncol = 0)
        orbit_asc <- c("01", "02", "03", "04")
        orbit_desc <- c("06", "07", "08", "09")
        
        for (i in 1:length(list_file)) {
          product <- substr(list_file[i], 6, 8)
          orbit <- substr(list_file[i], 14, 15)
          comp <- substr(list_file[i], nchar(list_file[i])-4, nchar(list_file[i])-4)
          extens <- substr(list_file[i], nchar(list_file[i])-3, nchar(list_file[i]))
          if (extens == ".csv" && orbit %in% orbit_asc && product == "L2a") {
            csv_L2a_asc <- append(csv_L2a_asc, list_file[i])
          } # close if L2a asc
          if (extens == ".csv" && orbit %in% orbit_asc && product == "L2b") {
            csv_L2b_asc <- append(csv_L2b_asc, list_file[i])
          } # close if L2b asc
          if (extens == ".csv" && orbit %in% orbit_desc && product == "L2a") {
            csv_L2a_desc <- append(csv_L2a_desc, list_file[i])
          } # close if L2a desc
          if (extens == ".csv" && orbit %in% orbit_desc && product == "L2b") {
            csv_L2b_desc <- append(csv_L2b_desc, list_file[i])
          } # close if L2b desc
          if (extens == ".csv" && comp == "E") {
            csv_L3_horiz <- append(csv_L3_horiz, list_file[i])
          } # close if L3 E
          if (extens == ".csv" && comp == "U") {
            csv_L3_vert <- append(csv_L3_vert, list_file[i])
          } # close if L3 U
        } # close for 
        
        rm(product)
        rm(orbit)
        rm(comp)
        rm(extens)
        
        # create folder AoI
        dir.create(paste0(global$datapath, "/PS_AoI_SHP_nTS/"))
        exdir <- paste0(global$datapath, "/PS_AoI_SHP_nTS/")
        
        # LOS
        if (sum(csv_L2a_asc != 0) != 0) {
          PS_sel_aoi <- PS_aoiL2_NTS(csv_L2a_asc, exdir_unz, aoi)
          if (nrow(PS_sel_aoi) !=0) {
            Write_SHP(PS_sel_aoi, exdir, "L2a_asc_AoI")
          } else {
            showNotification("AoI exceeding the extent of downloaded data!\n Please reshape AoI!", type = c("error"), duration=NULL)
          }
        } else {
          rm(csv_L2a_asc)
        }
        if (sum(csv_L2a_desc != 0) != 0) {
          PS_sel_aoi <- PS_aoiL2_NTS(csv_L2a_desc, exdir_unz, aoi)
          if (nrow(PS_sel_aoi) !=0) {
            Write_SHP(PS_sel_aoi, exdir, "L2a_desc_AoI")
          } else {
            showNotification("AoI exceeding the extent of downloaded data!\n Please reshape AoI!", type = c("error"), duration=NULL)
          }
        } else {
          rm(csv_L2a_desc)
        }
        if (sum(csv_L2b_asc != 0) != 0) {
          PS_sel_aoi <- PS_aoiL2_NTS(csv_L2b_asc, exdir_unz, aoi)
          if (nrow(PS_sel_aoi) !=0) {
            Write_SHP(PS_sel_aoi,exdir, "L2b_asc_AoI")
          } else {
            showNotification("AoI exceeding the extent of downloaded data!\n Please reshape AoI!", type = c("error"), duration=NULL)
          }
        } else {
          rm(csv_L2b_asc)
        }
        if (sum(csv_L2b_desc != 0) != 0) {
          PS_sel_aoi <- PS_aoiL2_NTS(csv_L2b_desc, exdir_unz, aoi)
          if (nrow(PS_sel_aoi) !=0) {
            Write_SHP(PS_sel_aoi, exdir, "L2b_desc_AoI")
          } else {
            showNotification("AoI exceeding the extent of downloaded data!\n Please reshape AoI!", type = c("error"), duration=NULL)
          }
        } else {
          rm(csv_L2b_desc)
        }
        
        # COMPONENTS
        if (sum(csv_L3_horiz != 0)!= 0) {
          PS_sel_aoi <- PS_aoiL3_NTS(csv_L3_horiz, exdir_unz, aoi_eea)
          if (nrow(PS_sel_aoi) !=0) {
            Write_SHP(PS_sel_aoi, exdir, "L3_horiz_AoI")
          } else {
            showNotification("AoI exceeding the extent of downloaded data!\n Please reshape AoI!", type = c("error"), duration=NULL)
          }
        } else {
          rm(csv_L3_horiz)
        }
        if (sum(csv_L3_vert != 0)!= 0) {
          PS_sel_aoi <- PS_aoiL3_NTS(csv_L3_vert, exdir_unz, aoi_eea)
          if (nrow(PS_sel_aoi) !=0) {
            Write_SHP(PS_sel_aoi, exdir, "L3_vert_AoI")
          } else {
            showNotification("AoI exceeding the extent of downloaded data!\n Please reshape AoI!", type = c("error"), duration=NULL)
          }
        } else {
          rm(csv_L3_vert)
        } # close else
        
      } else if (input$TimeSeriesSelection == "YTS" && input$FormatSelection == "GPKG") {
        if (input$DataFormat == "ymd") {
          list_file <- list.files(path = exdir_unz, all.files = FALSE, full.names = FALSE, recursive = FALSE,
                                  ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
          
          csv_L2a_asc <- matrix(nrow = 0, ncol = 0)
          csv_L2a_desc <- matrix(nrow = 0, ncol = 0)
          csv_L2b_asc <- matrix(nrow = 0, ncol = 0)
          csv_L2b_desc <- matrix(nrow = 0, ncol = 0)
          csv_L3_horiz <- matrix(nrow = 0, ncol = 0)
          csv_L3_vert <- matrix(nrow = 0, ncol = 0)
          orbit_asc <- c("01", "02", "03", "04")
          orbit_desc <- c("06", "07", "08", "09")
          
          for (i in 1:length(list_file)) {
            product <- substr(list_file[i], 6, 8)
            orbit <- substr(list_file[i], 14, 15)
            comp <- substr(list_file[i], nchar(list_file[i])-4, nchar(list_file[i])-4)
            extens <- substr(list_file[i], nchar(list_file[i])-3, nchar(list_file[i]))
            if (extens == ".csv" && orbit %in% orbit_asc && product == "L2a") {
              csv_L2a_asc <- append(csv_L2a_asc, list_file[i])
            } # close if L2a asc
            if (extens == ".csv" && orbit %in% orbit_asc && product == "L2b") {
              csv_L2b_asc <- append(csv_L2b_asc, list_file[i])
            } # close if L2b asc
            if (extens == ".csv" && orbit %in% orbit_desc && product == "L2a") {
              csv_L2a_desc <- append(csv_L2a_desc, list_file[i])
            } # close if L2a desc
            if (extens == ".csv" && orbit %in% orbit_desc && product == "L2b") {
              csv_L2b_desc <- append(csv_L2b_desc, list_file[i])
            } # close if L2b desc
            if (extens == ".csv" && comp == "E") {
              csv_L3_horiz <- append(csv_L3_horiz, list_file[i])
            } # close if L3 E
            if (extens == ".csv" && comp == "U") {
              csv_L3_vert <- append(csv_L3_vert, list_file[i])
            } # close if L3 U
          } # close for
          
          file_csv_los <- c(csv_L2a_asc, csv_L2a_desc, csv_L2b_asc, csv_L2b_desc)
          file_csv_comp <- c(csv_L3_horiz, csv_L3_vert)
          rm(product)
          rm(orbit)
          rm(comp)
          rm(extens)
          
          # create folder AoI
          dir.create(paste0(global$datapath, "/PS_AoI_GPKG_TS/"))
          exdir <- paste0(global$datapath, "/PS_AoI_GPKG_TS/")
          
          # LOS
          if (sum(csv_L2a_asc != 0) != 0) {
            tracks_csv <- array(dim = length(csv_L2a_asc))
            for (i in 1:length(csv_L2a_asc)) {
              tracks_csv[i] <- substring(csv_L2a_asc[i], 10, 12)
            }
            tracks <- unique(tracks_csv)
            for (i in 1:length(tracks)) {
              dummy <- csv_L2a_asc[which(tracks_csv == tracks[i], arr.ind = TRUE)]
              assign(tracks[i], dummy)
              rm(dummy)
            }
            for (t in 1:length(tracks)) {
              for (i in length(get(tracks[t]))) {
                PS_sel_aoi <- PS_aoiL2_ymd(get(tracks[t])[i], exdir_unz, aoi)
                if (nrow(PS_sel_aoi) !=0) {
                  Write_track_GPKG(PS_sel_aoi, exdir, "L2a_asc_AoI", tracks[t])
                } else {
                  showNotification("AoI exceeding the extent of downloaded data!\n Please reshape AoI!", type = c("error"), duration=NULL)
                } 
              } # close for i get(tracks)
            } # close for t tracks 
          } else {
            rm(csv_L2a_asc)
          }
          
          if (sum(csv_L2a_desc != 0) != 0) {
            tracks_csv <- array(dim = length(csv_L2a_desc))
            for (i in 1:length(csv_L2a_desc)) {
              tracks_csv[i] <- substring(csv_L2a_desc[i], 10, 12)
            }
            tracks <- unique(tracks_csv)
            for (i in 1:length(tracks)) {
              dummy <- csv_L2a_desc[which(tracks_csv == tracks[i], arr.ind = TRUE)]
              assign(tracks[i], dummy)
              rm(dummy)
            }
            for (t in 1:length(tracks)) {
              for (i in length(get(tracks[t]))) {
                PS_sel_aoi <- PS_aoiL2_ymd(get(tracks[t])[i], exdir_unz, aoi)
                if (nrow(PS_sel_aoi) !=0) {
                  Write_track_GPKG(PS_sel_aoi, exdir, "L2a_desc_AoI", tracks[t])
                } else {
                  showNotification("AoI exceeding the extent of downloaded data!\n Please reshape AoI!", type = c("error"), duration=NULL)
                } 
              } # close for i get(tracks)
            } # close for t tracks 
          } else {
            rm(csv_L2a_desc)
          }
          
          if (sum(csv_L2b_asc != 0) != 0) {
            tracks_csv <- array(dim = length(csv_L2b_asc))
            for (i in 1:length(csv_L2b_asc)) {
              tracks_csv[i] <- substring(csv_L2b_asc[i], 10, 12)
            }
            tracks <- unique(tracks_csv)
            for (i in 1:length(tracks)) {
              dummy <- csv_L2b_asc[which(tracks_csv == tracks[i], arr.ind = TRUE)]
              assign(tracks[i], dummy)
              rm(dummy)
            }
            for (t in 1:length(tracks)) {
              for (i in length(get(tracks[t]))) {
                PS_sel_aoi <- PS_aoiL2_ymd(get(tracks[t])[i], exdir_unz, aoi)
                if (nrow(PS_sel_aoi) !=0) {
                  Write_track_GPKG(PS_sel_aoi, exdir, "L2b_asc_AoI", tracks[t])
                } else {
                  showNotification("AoI exceeding the extent of downloaded data!\n Please reshape AoI!", type = c("error"), duration=NULL)
                } 
              } # close for i get(tracks)
            } # close for t tracks 
          } else {
            rm(csv_L2b_asc)
          }
          
          if (sum(csv_L2b_desc != 0) != 0) {
            tracks_csv <- array(dim = length(csv_L2b_desc))
            for (i in 1:length(csv_L2b_desc)) {
              tracks_csv[i] <- substring(csv_L2b_desc[i], 10, 12)
            }
            tracks <- unique(tracks_csv)
            for (i in 1:length(tracks)) {
              dummy <- csv_L2b_desc[which(tracks_csv == tracks[i], arr.ind = TRUE)]
              assign(tracks[i], dummy)
              rm(dummy)
            }
            for (t in 1:length(tracks)) {
              for (i in length(get(tracks[t]))) {
                PS_sel_aoi <- PS_aoiL2_ymd(get(tracks[t])[i], exdir_unz, aoi)
                if (nrow(PS_sel_aoi) !=0) {
                  Write_track_GPKG(PS_sel_aoi, exdir, "L2b_desc_AoI", tracks[t])
                } else {
                  showNotification("AoI exceeding the extent of downloaded data!\n Please reshape AoI!", type = c("error"), duration=NULL)
                } 
              } # close for i get(tracks)
            } # close for t tracks 
          } else {
            rm(csv_L2b_desc)
          }
          
          # COMPONENTS
          if (sum(csv_L3_horiz != 0)!= 0) {
            PS_sel_aoi <- PS_aoiL3_ymd(csv_L3_horiz, exdir_unz, aoi_eea)
            if (nrow(PS_sel_aoi) !=0) {
              Write_GPKG(PS_sel_aoi, exdir, "L3_horiz_AoI")
            } else {
              showNotification("AoI exceeding the extent of downloaded data!\n Please reshape AoI!", type = c("error"), duration=NULL)
            }
          } else {
            rm(csv_L3_horiz)
          }
          
          if (sum(csv_L3_vert != 0)!= 0) {
            PS_sel_aoi <- PS_aoiL3_ymd(csv_L3_vert, exdir_unz, aoi_eea)
            if (nrow(PS_sel_aoi) !=0) {
              Write_GPKG(PS_sel_aoi, exdir, "L3_vert_AoI")
            } else {
              showNotification("AoI exceeding the extent of downloaded data!\n Please reshape AoI!", type = c("error"), duration=NULL)
            }
          } else {
            rm(csv_L3_vert)
          } # close else
          
        } else if (input$DataFormat == "dmy") {
          list_file <- list.files(path = exdir_unz, all.files = FALSE, full.names = FALSE, recursive = FALSE,
                                  ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
          
          csv_L2a_asc <- matrix(nrow = 0, ncol = 0)
          csv_L2a_desc <- matrix(nrow = 0, ncol = 0)
          csv_L2b_asc <- matrix(nrow = 0, ncol = 0)
          csv_L2b_desc <- matrix(nrow = 0, ncol = 0)
          csv_L3_horiz <- matrix(nrow = 0, ncol = 0)
          csv_L3_vert <- matrix(nrow = 0, ncol = 0)
          orbit_asc <- c("01", "02", "03", "04")
          orbit_desc <- c("06", "07", "08", "09")
          
          for (i in 1:length(list_file)) {
            product <- substr(list_file[i], 6, 8)
            orbit <- substr(list_file[i], 14, 15)
            comp <- substr(list_file[i], nchar(list_file[i])-4, nchar(list_file[i])-4)
            extens <- substr(list_file[i], nchar(list_file[i])-3, nchar(list_file[i]))
            if (extens == ".csv" && orbit %in% orbit_asc && product == "L2a") {
              csv_L2a_asc <- append(csv_L2a_asc, list_file[i])
            } # close if L2a asc
            if (extens == ".csv" && orbit %in% orbit_asc && product == "L2b") {
              csv_L2b_asc <- append(csv_L2b_asc, list_file[i])
            } # close if L2b asc
            if (extens == ".csv" && orbit %in% orbit_desc && product == "L2a") {
              csv_L2a_desc <- append(csv_L2a_desc, list_file[i])
            } # close if L2a desc
            if (extens == ".csv" && orbit %in% orbit_desc && product == "L2b") {
              csv_L2b_desc <- append(csv_L2b_desc, list_file[i])
            } # close if L2b desc
            if (extens == ".csv" && comp == "E") {
              csv_L3_horiz <- append(csv_L3_horiz, list_file[i])
            } # close if L3 E
            if (extens == ".csv" && comp == "U") {
              csv_L3_vert <- append(csv_L3_vert, list_file[i])
            } # close if L3 U
          } # close for 
          
          file_csv_los <- c(csv_L2a_asc, csv_L2a_desc, csv_L2b_asc, csv_L2b_desc)
          file_csv_comp <- c(csv_L3_horiz, csv_L3_vert)
          rm(product)
          rm(orbit)
          rm(comp)
          rm(extens)
          
          # create folder AoI
          dir.create(paste0(global$datapath, "/PS_AoI_GPKG_TS/"))
          exdir <- paste0(global$datapath, "/PS_AoI_GPKG_TS/")
          
          #LOS
          if (sum(csv_L2a_asc != 0) != 0) {
            tracks_csv <- array(dim = length(csv_L2a_asc))
            for (i in 1:length(csv_L2a_asc)) {
              tracks_csv[i] <- substring(csv_L2a_asc[i], 10, 12)
            }
            tracks <- unique(tracks_csv)
            for (i in 1:length(tracks)) {
              dummy <- csv_L2a_asc[which(tracks_csv == tracks[i], arr.ind = TRUE)]
              assign(tracks[i], dummy)
              rm(dummy)
            }
            for (t in 1:length(tracks)) {
              for (i in length(get(tracks[t]))) {
                PS_sel_aoi <- PS_aoiL2_dmy(get(tracks[t])[i], exdir_unz, aoi)
                if (nrow(PS_sel_aoi) !=0) {
                  Write_track_SHP(PS_sel_aoi, exdir, "L2a_asc_AoI", tracks[t])
                } else {
                  showNotification("AoI exceeding the extent of downloaded data!\n Please reshape AoI!", type = c("error"), duration=NULL)
                } 
              } # close for i get(tracks)
            } # close for t tracks 
          } else {
            rm(csv_L2a_asc)
          }
          
          if (sum(csv_L2a_desc != 0) != 0) {
            tracks_csv <- array(dim = length(csv_L2a_desc))
            for (i in 1:length(csv_L2a_desc)) {
              tracks_csv[i] <- substring(csv_L2a_desc[i], 10, 12)
            }
            tracks <- unique(tracks_csv)
            for (i in 1:length(tracks)) {
              dummy <- csv_L2a_desc[which(tracks_csv == tracks[i], arr.ind = TRUE)]
              assign(tracks[i], dummy)
              rm(dummy)
            }
            for (t in 1:length(tracks)) {
              for (i in length(get(tracks[t]))) {
                PS_sel_aoi <- PS_aoiL2_dmy(get(tracks[t])[i], exdir_unz, aoi)
                if (nrow(PS_sel_aoi) !=0) {
                  Write_track_SHP(PS_sel_aoi, exdir, "L2a_desc_AoI", tracks[t])
                } else {
                  showNotification("AoI exceeding the extent of downloaded data!\n Please reshape AoI!", type = c("error"), duration=NULL)
                } 
              } # close for i get(tracks)
            } # close for t tracks 
          } else {
            rm(csv_L2a_desc)
          }
          
          if (sum(csv_L2b_asc != 0) != 0) {
            tracks_csv <- array(dim = length(csv_L2b_asc))
            for (i in 1:length(csv_L2b_asc)) {
              tracks_csv[i] <- substring(csv_L2b_asc[i], 10, 12)
            }
            tracks <- unique(tracks_csv)
            for (i in 1:length(tracks)) {
              dummy <- csv_L2b_asc[which(tracks_csv == tracks[i], arr.ind = TRUE)]
              assign(tracks[i], dummy)
              rm(dummy)
            }
            for (t in 1:length(tracks)) {
              for (i in length(get(tracks[t]))) {
                PS_sel_aoi <- PS_aoiL2_dmy(get(tracks[t])[i], exdir_unz, aoi)
                if (nrow(PS_sel_aoi) !=0) {
                  Write_track_SHP(PS_sel_aoi, exdir, "L2b_asc_AoI", tracks[t])
                } else {
                  showNotification("AoI exceeding the extent of downloaded data!\n Please reshape AoI!", type = c("error"), duration=NULL)
                } 
              } # close for i get(tracks)
            } # close for t tracks 
          } else {
            rm(csv_L2b_asc)
          }
          
          if (sum(csv_L2b_desc != 0) != 0) {
            tracks_csv <- array(dim = length(csv_L2b_desc))
            for (i in 1:length(csv_L2b_desc)) {
              tracks_csv[i] <- substring(csv_L2b_desc[i], 10, 12)
            }
            tracks <- unique(tracks_csv)
            for (i in 1:length(tracks)) {
              dummy <- csv_L2b_desc[which(tracks_csv == tracks[i], arr.ind = TRUE)]
              assign(tracks[i], dummy)
              rm(dummy)
            }
            for (t in 1:length(tracks)) {
              for (i in length(get(tracks[t]))) {
                PS_sel_aoi <- PS_aoiL2_dmy(get(tracks[t])[i], exdir_unz, aoi)
                if (nrow(PS_sel_aoi) !=0) {
                  Write_track_SHP(PS_sel_aoi, exdir, "L2b_desc_AoI", tracks[t])
                } else {
                  showNotification("AoI exceeding the extent of downloaded data!\n Please reshape AoI!", type = c("error"), duration=NULL)
                } 
              } # close for i get(tracks)
            } # close for t tracks 
          } else {
            rm(csv_L2b_desc)
          }
          
          # COMPONENTS
          if (sum(csv_L3_horiz != 0)!= 0) {
            PS_sel_aoi <- PS_aoiL3_dmy(csv_L3_horiz, exdir_unz, aoi_eea)
            if (nrow(PS_sel_aoi) !=0) {
              Write_GPKG(PS_sel_aoi, exdir, "L3_horiz_AoI")
            } else {
              showNotification("AoI exceeding the extent of downloaded data!\n Please reshape AoI!", type = c("error"), duration=NULL)
            }
          } else {
            rm(csv_L3_horiz)
          }
          
          if (sum(csv_L3_vert != 0)!= 0) {
            PS_sel_aoi <- PS_aoiL3_dmy(csv_L3_vert, exdir_unz, aoi_eea)
            if (nrow(PS_sel_aoi) !=0) {
              Write_GPKG(PS_sel_aoi, exdir, "L3_vert_AoI")
            } else {
              showNotification("AoI exceeding the extent of downloaded data!\n Please reshape AoI!", type = c("error"), duration=NULL)
            }
          } else {
            rm(csv_L3_vert)
          } # close else
          
        } # close YTS GPKG dmy
        
      } else { # input$TimeSeriesSelection == "NTS" && input$FormatSelection == "GPKG"
        list_file <- list.files(path = exdir_unz, all.files = FALSE, full.names = FALSE, recursive = FALSE,
                                ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
        
        csv_L2a_asc <- matrix(nrow = 0, ncol = 0)
        csv_L2a_desc <- matrix(nrow = 0, ncol = 0)
        csv_L2b_asc <- matrix(nrow = 0, ncol = 0)
        csv_L2b_desc <- matrix(nrow = 0, ncol = 0)
        csv_L3_horiz <- matrix(nrow = 0, ncol = 0)
        csv_L3_vert <- matrix(nrow = 0, ncol = 0)
        orbit_asc <- c("01", "02", "03", "04")
        orbit_desc <- c("06", "07", "08", "09")
        
        for (i in 1:length(list_file)) {
          product <- substr(list_file[i], 6, 8)
          orbit <- substr(list_file[i], 14, 15)
          comp <- substr(list_file[i], nchar(list_file[i])-4, nchar(list_file[i])-4)
          extens <- substr(list_file[i], nchar(list_file[i])-3, nchar(list_file[i]))
          if (extens == ".csv" && orbit %in% orbit_asc && product == "L2a") {
            csv_L2a_asc <- append(csv_L2a_asc, list_file[i])
          } # close if L2a asc
          if (extens == ".csv" && orbit %in% orbit_asc && product == "L2b") {
            csv_L2b_asc <- append(csv_L2b_asc, list_file[i])
          } # close if L2b asc
          if (extens == ".csv" && orbit %in% orbit_desc && product == "L2a") {
            csv_L2a_desc <- append(csv_L2a_desc, list_file[i])
          } # close if L2a desc
          if (extens == ".csv" && orbit %in% orbit_desc && product == "L2b") {
            csv_L2b_desc <- append(csv_L2b_desc, list_file[i])
          } # close if L2b desc
          if (extens == ".csv" && comp == "E") {
            csv_L3_horiz <- append(csv_L3_horiz, list_file[i])
          } # close if L3 E
          if (extens == ".csv" && comp == "U") {
            csv_L3_vert <- append(csv_L3_vert, list_file[i])
          } # close if L3 U
        } # close for 
        
        file_csv_los <- c(csv_L2a_asc, csv_L2a_desc, csv_L2b_asc, csv_L2b_desc)
        file_csv_comp <- c(csv_L3_horiz, csv_L3_vert)
        rm(product)
        rm(orbit)
        rm(comp)
        rm(extens)
        
        # create folder AoI
        dir.create(paste0(global$datapath, "/PS_AoI_GPKG_nTS/"))
        exdir <- paste0(global$datapath, "/PS_AoI_GPKG_nTS/")
        
        # LOS        
        if (sum(csv_L2a_asc != 0) != 0) {
          PS_sel_aoi <- PS_aoiL2_NTS(csv_L2a_asc, exdir_unz, aoi)
          if (nrow(PS_sel_aoi) !=0) {
            Write_GPKG(PS_sel_aoi, exdir, "L2a_asc_AoI")
          } else {
            showNotification("AoI exceeding the extent of downloaded data!\n Please reshape AoI!", type = c("error"), duration=NULL)
          }
        } else {
          rm(csv_L2a_asc)
        }
        
        if (sum(csv_L2a_desc != 0) != 0) {
          PS_sel_aoi <- PS_aoiL2_NTS(csv_L2a_desc, exdir_unz, aoi)
          if (nrow(PS_sel_aoi) !=0) {
            Write_GPKG(PS_sel_aoi, exdir, "L2a_desc_AoI")
          } else {
            showNotification("AoI exceeding the extent of downloaded data!\n Please reshape AoI!", type = c("error"), duration=NULL)
          }
        } else {
          rm(csv_L2a_desc)
        }
        if (sum(csv_L2b_asc != 0) != 0) {
          PS_sel_aoi <- PS_aoiL2_NTS(csv_L2b_asc, exdir_unz, aoi)
          if (nrow(PS_sel_aoi) !=0) {
            Write_GPKG(PS_sel_aoi, exdir, "L2b_asc_AoI")
          } else {
            showNotification("AoI exceeding the extent of downloaded data!\n Please reshape AoI!", type = c("error"), duration=NULL)
          }
        } else {
          rm(csv_L2b_asc)
        }
        if (sum(csv_L2b_desc != 0) != 0) {
          PS_sel_aoi <- PS_aoiL2_NTS(csv_L2b_desc, exdir_unz, aoi)
          if (nrow(PS_sel_aoi) !=0) {
            Write_GPKG(PS_sel_aoi, exdir, "L2b_desc_AoI")
          } else {
            showNotification("AoI exceeding the extent of downloaded data!\n Please reshape AoI!", type = c("error"), duration=NULL)
          }
        } else {
          rm(csv_L2b_desc)
        }
        
        # COMPONENTS
        if (sum(csv_L3_horiz != 0)!= 0) {
          PS_sel_aoi <- PS_aoiL3_NTS(csv_L3_horiz, exdir_unz, aoi_eea)
          if (nrow(PS_sel_aoi) !=0) {
            Write_GPKG(PS_sel_aoi, exdir, "L3_horiz_AoI")
          } else {
            showNotification("AoI exceeding the extent of downloaded data!\n Please reshape AoI!", type = c("error"), duration=NULL)
          }
        } else {
          rm(csv_L3_horiz)
        }
        if (sum(csv_L3_vert != 0)!= 0) {
          PS_sel_aoi <- PS_aoiL3_NTS(csv_L3_vert, exdir_unz, aoi_eea)
          if (nrow(PS_sel_aoi) !=0) {
            Write_GPKG(PS_sel_aoi, exdir, "L3_vert_AoI")
          } else {
            showNotification("AoI exceeding the extent of downloaded data!\n Please reshape AoI!", type = c("error"), duration=NULL)
          }
        } else {
          rm(csv_L3_vert)
        } # close else
      } # close TimeSeriesSelection 
      
    } else { # if NO aoi
      
      if (input$TimeSeriesSelection == "YTS" && input$FormatSelection == "SHP") {
        if (input$DataFormat == "ymd") {
          list_file <- list.files(path = exdir_unz, all.files = FALSE, full.names = FALSE, recursive = FALSE,
                                  ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
          
          csv_L2a_asc <- matrix(nrow = 0, ncol = 0)
          csv_L2a_desc <- matrix(nrow = 0, ncol = 0)
          csv_L2b_asc <- matrix(nrow = 0, ncol = 0)
          csv_L2b_desc <- matrix(nrow = 0, ncol = 0)
          csv_L3_horiz <- matrix(nrow = 0, ncol = 0)
          csv_L3_vert <- matrix(nrow = 0, ncol = 0)
          orbit_asc <- c("01", "02", "03", "04")
          orbit_desc <- c("06", "07", "08", "09")
          
          for (i in 1:length(list_file)) {
            product <- substr(list_file[i], 6, 8)
            orbit <- substr(list_file[i], 14, 15)
            comp <- substr(list_file[i], nchar(list_file[i])-4, nchar(list_file[i])-4)
            extens <- substr(list_file[i], nchar(list_file[i])-3, nchar(list_file[i]))
            if (extens == ".csv" && orbit %in% orbit_asc && product == "L2a") {
              csv_L2a_asc <- append(csv_L2a_asc, list_file[i])
            } # close if L2a asc
            if (extens == ".csv" && orbit %in% orbit_asc && product == "L2b") {
              csv_L2b_asc <- append(csv_L2b_asc, list_file[i])
            } # close if L2b asc
            if (extens == ".csv" && orbit %in% orbit_desc && product == "L2a") {
              csv_L2a_desc <- append(csv_L2a_desc, list_file[i])
            } # close if L2a desc
            if (extens == ".csv" && orbit %in% orbit_desc && product == "L2b") {
              csv_L2b_desc <- append(csv_L2b_desc, list_file[i])
            } # close if L2b desc
            if (extens == ".csv" && comp == "E") {
              csv_L3_horiz <- append(csv_L3_horiz, list_file[i])
            } # close if L3 E
            if (extens == ".csv" && comp == "U") {
              csv_L3_vert <- append(csv_L3_vert, list_file[i])
            } # close if L3 U
          } # close for 
          
          rm(product)
          rm(orbit)
          rm(comp)
          rm(extens)
          
          # LOS
          if (sum(csv_L2a_asc != 0) != 0) {
            dir.create(paste0(global$datapath, "/PS_L2a_asc/"))
            exdir <- paste0(global$datapath, "/PS_L2a_asc/")
            for (i in 1:length(csv_L2a_asc)) {
              progress$inc(1/length(csv_L2a_asc), detail = paste("L2 data", i, "of", length(csv_L2a_asc)))
              Sys.sleep(0.25)
              progress_list2 = c(progress_list2, paste("Converted", csv_L2a_asc[i],'\n'))
              PS_L2_ymd_toSHP(csv_L2a_asc[i], exdir_unz, exdir)
            }
          } else {
            rm(csv_L2a_asc)
          }
          if (sum(csv_L2a_desc != 0) != 0) {
            dir.create(paste0(global$datapath, "/PS_L2a_desc/"))
            exdir <- paste0(global$datapath, "/PS_L2a_desc/")
            for (i in 1:length(csv_L2a_desc)) {
              progress$inc(1/length(csv_L2a_desc), detail = paste("L2 data", i, "of", length(csv_L2a_desc)))
              Sys.sleep(0.25)
              progress_list2 = c(progress_list2, paste("Converted", csv_L2a_desc[i],'\n'))
              PS_L2_ymd_toSHP(csv_L2a_desc[i], exdir_unz, exdir)
            }
          } else {
            rm(csv_L2a_desc)
          }
          if (sum(csv_L2b_asc != 0) != 0) {
            dir.create(paste0(global$datapath, "/PS_L2b_asc/"))
            exdir <- paste0(global$datapath, "/PS_L2b_asc/")
            for (i in 1:length(csv_L2b_asc)) {
              progress$inc(1/length(csv_L2b_asc), detail = paste("L2 data", i, "of", length(csv_L2b_asc)))
              Sys.sleep(0.25)
              progress_list2 = c(progress_list2, paste("Converted", csv_L2b_asc[i],'\n'))
              PS_L2_ymd_toSHP(csv_L2b_asc[i], exdir_unz, exdir)
            }
          } else {
            rm(csv_L2b_asc)
          }
          if (sum(csv_L2b_desc != 0) != 0) {
            dir.create(paste0(global$datapath, "/PS_L2b_desc/"))
            exdir <- paste0(global$datapath, "/PS_L2b_desc/")
            for (i in 1:length(csv_L2b_desc)) {
              progress$inc(1/length(csv_L2b_desc), detail = paste("L2 data", i, "of", length(csv_L2b_desc)))
              Sys.sleep(0.25)
              progress_list2 = c(progress_list2, paste("Converted", csv_L2b_desc[i],'\n'))
              PS_L2_ymd_toSHP(csv_L2b_desc[i], exdir_unz, exdir)
            }
          } else {
            rm(csv_L2b_desc)
          }
          
          # COMPONENTS
          if (sum(csv_L3_horiz != 0)!= 0) {
            dir.create(paste0(global$datapath, "/PS_L3_E/"))
            exdir <- paste0(global$datapath, "/PS_L3_E/")
            for (i in 1:length(csv_L3_horiz_asc)) {
              progress$inc(1/length(csv_L3_horiz_asc), detail = paste("L3 data", i, "of", length(csv_L3_horiz_asc)))
              Sys.sleep(0.25)
              progress_list2 = c(progress_list2, paste("Converted", csv_L3_horiz_asc[i],'\n'))
              PS_L3_ymd_toSHP(csv_L3_horiz, exdir_unz, exdir)
            }
          } else {
            rm(csv_L3_horiz)
          }
          if (sum(csv_L3_vert != 0)!= 0) {
            dir.create(paste0(global$datapath, "/PS_L3_U/"))
            exdir <- paste0(global$datapath, "/PS_L3_U/")
            for (i in 1:length(csv_L3_vert_asc)) {
              progress$inc(1/length(csv_L3_vert_asc), detail = paste("L3 data", i, "of", length(csv_L3_vert_asc)))
              Sys.sleep(0.25)
              progress_list2 = c(progress_list2, paste("Converted", csv_L3_vert_asc[i],'\n'))
              PS_L3_ymd_toSHP(csv_L3_vert, exdir_unz, exdir)
            }
          } else {
            rm(csv_L3_vert)
          } # close else
          
        } else if (input$DataFormat == "dmy") {
          list_file <- list.files(path = exdir_unz, all.files = FALSE, full.names = FALSE, recursive = FALSE,
                                  ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
          
          csv_L2a_asc <- matrix(nrow = 0, ncol = 0)
          csv_L2a_desc <- matrix(nrow = 0, ncol = 0)
          csv_L2b_asc <- matrix(nrow = 0, ncol = 0)
          csv_L2b_desc <- matrix(nrow = 0, ncol = 0)
          csv_L3_horiz <- matrix(nrow = 0, ncol = 0)
          csv_L3_vert <- matrix(nrow = 0, ncol = 0)
          orbit_asc <- c("01", "02", "03", "04")
          orbit_desc <- c("06", "07", "08", "09")
          
          for (i in 1:length(list_file)) {
            product <- substr(list_file[i], 6, 8)
            orbit <- substr(list_file[i], 14, 15)
            comp <- substr(list_file[i], nchar(list_file[i])-4, nchar(list_file[i])-4)
            extens <- substr(list_file[i], nchar(list_file[i])-3, nchar(list_file[i]))
            if (extens == ".csv" && orbit %in% orbit_asc && product == "L2a") {
              csv_L2a_asc <- append(csv_L2a_asc, list_file[i])
            } # close if L2a asc
            if (extens == ".csv" && orbit %in% orbit_asc && product == "L2b") {
              csv_L2b_asc <- append(csv_L2b_asc, list_file[i])
            } # close if L2b asc
            if (extens == ".csv" && orbit %in% orbit_desc && product == "L2a") {
              csv_L2a_desc <- append(csv_L2a_desc, list_file[i])
            } # close if L2a desc
            if (extens == ".csv" && orbit %in% orbit_desc && product == "L2b") {
              csv_L2b_desc <- append(csv_L2b_desc, list_file[i])
            } # close if L2b desc
            if (extens == ".csv" && comp == "E") {
              csv_L3_horiz <- append(csv_L3_horiz, list_file[i])
            } # close if L3 E
            if (extens == ".csv" && comp == "U") {
              csv_L3_vert <- append(csv_L3_vert, list_file[i])
            } # close if L3 U
          } # close for 
          
          rm(product)
          rm(orbit)
          rm(comp)
          rm(extens)
          
          # LOS
          if (sum(csv_L2a_asc != 0) != 0) {
            dir.create(paste0(global$datapath, "/PS_L2a_asc/"))
            exdir <- paste0(global$datapath, "/PS_L2a_asc/")
            for (i in 1:length(csv_L2a_asc)) {
              progress$inc(1/length(csv_L2a_asc), detail = paste("L2 data", i, "of", length(csv_L2a_asc)))
              Sys.sleep(0.25)
              progress_list2 = c(progress_list2, paste("Converted", csv_L2a_asc[i],'\n'))
              PS_L2_dmy_toSHP(csv_L2a_asc[i], exdir_unz, exdir)
            }
          } else {
            rm(csv_L2a_asc)
          }
          if (sum(csv_L2a_desc != 0) != 0) {
            dir.create(paste0(global$datapath, "/PS_L2a_desc/"))
            exdir <- paste0(global$datapath, "/PS_L2a_desc/")
            for (i in 1:length(csv_L2a_desc)) {
              progress$inc(1/length(csv_L2a_desc), detail = paste("L2 data", i, "of", length(csv_L2a_desc)))
              Sys.sleep(0.25)
              progress_list2 = c(progress_list2, paste("Converted", csv_L2a_desc[i],'\n'))
              PS_L2_dmy_toSHP(csv_L2a_desc[i], exdir_unz, exdir)
            }
          } else {
            rm(csv_L2a_desc)
          }
          if (sum(csv_L2b_asc != 0) != 0) {
            dir.create(paste0(global$datapath, "/PS_L2b_asc/"))
            exdir <- paste0(global$datapath, "/PS_L2b_asc/")
            for (i in 1:length(csv_L2b_asc)) {
              progress$inc(1/length(csv_L2b_asc), detail = paste("L2 data", i, "of", length(csv_L2b_asc)))
              Sys.sleep(0.25)
              progress_list2 = c(progress_list2, paste("Converted", csv_L2b_asc[i],'\n'))
              PS_L2_dmy_toSHP(csv_L2b_asc[i], exdir_unz, exdir)
            }
          } else {
            rm(csv_L2b_asc)
          }
          if (sum(csv_L2b_desc != 0) != 0) {
            dir.create(paste0(global$datapath, "/PS_L2b_desc/"))
            exdir <- paste0(global$datapath, "/PS_L2b_desc/")
            for (i in 1:length(csv_L2b_desc)) {
              progress$inc(1/length(csv_L2b_desc), detail = paste("L2 data", i, "of", length(csv_L2b_desc)))
              Sys.sleep(0.25)
              progress_list2 = c(progress_list2, paste("Converted", csv_L2b_desc[i],'\n'))
              PS_L2_dmy_toSHP(csv_L2b_desc[i], exdir_unz, exdir)
            }
          } else {
            rm(csv_L2b_desc)
          }
          
          # COMPONENTS
          if (sum(csv_L3_horiz != 0)!= 0) {
            dir.create(paste0(global$datapath, "/PS_L3_E/"))
            exdir <- paste0(global$datapath, "/PS_L3_E/")
            for (i in 1:length(csv_L3_horiz)) {
              progress$inc(1/length(csv_L3_horiz), detail = paste("L3 data", i, "of", length(csv_L3_horiz)))
              Sys.sleep(0.25)
              progress_list2 = c(progress_list2, paste("Converted", csv_L3_horiz[i],'\n'))
              PS_L3_dmy_toSHP(csv_L3_horiz[i], exdir_unz, exdir)
            }
          } else {
            rm(csv_L3_horiz)
          }
          if (sum(csv_L3_vert != 0)!= 0) {
            dir.create(paste0(global$datapath, "/PS_L3_U/"))
            exdir <- paste0(global$datapath, "/PS_L3_U/")
            for (i in 1:length(csv_L3_vert)) {
              progress$inc(1/length(csv_L3_vert), detail = paste("L3 data", i, "of", length(csv_L3_vert)))
              Sys.sleep(0.25)
              progress_list2 = c(progress_list2, paste("Converted", csv_L3_vert[i],'\n'))
              PS_L3_dmy_toSHP(csv_L3_vert[i], exdir_unz, exdir)
            }
          } else {
            rm(csv_L3_vert)
          } # close else
        } # close YTS SHP dmy
        
      } else if (input$TimeSeriesSelection == "NTS" && input$FormatSelection == "SHP") {
        list_file <- list.files(path = exdir_unz, all.files = FALSE, full.names = FALSE, recursive = FALSE,
                                ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
        
        csv_L2a_asc <- matrix(nrow = 0, ncol = 0)
        csv_L2a_desc <- matrix(nrow = 0, ncol = 0)
        csv_L2b_asc <- matrix(nrow = 0, ncol = 0)
        csv_L2b_desc <- matrix(nrow = 0, ncol = 0)
        csv_L3_horiz <- matrix(nrow = 0, ncol = 0)
        csv_L3_vert <- matrix(nrow = 0, ncol = 0)
        orbit_asc <- c("01", "02", "03", "04")
        orbit_desc <- c("06", "07", "08", "09")
        
        for (i in 1:length(list_file)) {
          product <- substr(list_file[i], 6, 8)
          orbit <- substr(list_file[i], 14, 15)
          comp <- substr(list_file[i], nchar(list_file[i])-4, nchar(list_file[i])-4)
          extens <- substr(list_file[i], nchar(list_file[i])-3, nchar(list_file[i]))
          if (extens == ".csv" && orbit %in% orbit_asc && product == "L2a") {
            csv_L2a_asc <- append(csv_L2a_asc, list_file[i])
          } # close if L2a asc
          if (extens == ".csv" && orbit %in% orbit_asc && product == "L2b") {
            csv_L2b_asc <- append(csv_L2b_asc, list_file[i])
          } # close if L2b asc
          if (extens == ".csv" && orbit %in% orbit_desc && product == "L2a") {
            csv_L2a_desc <- append(csv_L2a_desc, list_file[i])
          } # close if L2a desc
          if (extens == ".csv" && orbit %in% orbit_desc && product == "L2b") {
            csv_L2b_desc <- append(csv_L2b_desc, list_file[i])
          } # close if L2b desc
          if (extens == ".csv" && comp == "E") {
            csv_L3_horiz <- append(csv_L3_horiz, list_file[i])
          } # close if L3 E
          if (extens == ".csv" && comp == "U") {
            csv_L3_vert <- append(csv_L3_vert, list_file[i])
          } # close if L3 U
        } # close for 
        
        rm(product)
        rm(orbit)
        rm(comp)
        rm(extens)
        
        # LOS
        if (sum(csv_L2a_asc != 0) != 0) {
          dir.create(paste0(global$datapath, "/PS_L2a_asc/"))
          exdir <- paste0(global$datapath, "/PS_L2a_asc/")
          for (i in 1:length(csv_L2a_asc)) {
            progress$inc(1/length(csv_L2a_asc), detail = paste("L2 data", i, "of", length(csv_L2a_asc)))
            Sys.sleep(0.25)
            progress_list2 = c(progress_list2, paste("Converted", csv_L2a_asc[i],'\n'))
            PS_L2_NTS_toSHP(csv_L2a_asc[i], exdir_unz, exdir)
          }
        } else {
          rm(csv_L2a_asc)
        }
        if (sum(csv_L2a_desc != 0) != 0) {
          dir.create(paste0(global$datapath, "/PS_L2a_desc/"))
          exdir <- paste0(global$datapath, "/PS_L2a_desc/")
          for (i in 1:length(csv_L2a_desc)) {
            progress$inc(1/length(csv_L2a_desc), detail = paste("L2 data", i, "of", length(csv_L2a_desc)))
            Sys.sleep(0.25)
            progress_list2 = c(progress_list2, paste("Converted", csv_L2a_desc[i],'\n'))
            PS_L2_NTS_toSHP(csv_L2a_desc[i], exdir_unz, exdir)
          }
        } else {
          rm(csv_L2a_desc)
        }
        if (sum(csv_L2b_asc != 0) != 0) {
          dir.create(paste0(global$datapath, "/PS_L2b_asc/"))
          exdir <- paste0(global$datapath, "/PS_L2b_asc/")
          for (i in 1:length(csv_L2b_asc)) {
            progress$inc(1/length(csv_L2b_asc), detail = paste("L2 data", i, "of", length(csv_L2b_asc)))
            Sys.sleep(0.25)
            progress_list2 = c(progress_list2, paste("Converted", csv_L2b_asc[i],'\n'))
            PS_L2_NTS_toSHP(csv_L2b_asc[i], exdir_unz, exdir)
          }
        } else {
          rm(csv_L2b_asc)
        }
        if (sum(csv_L2b_desc != 0) != 0) {
          dir.create(paste0(global$datapath, "/PS_L2b_desc/"))
          exdir <- paste0(global$datapath, "/PS_L2b_desc/")
          for (i in 1:length(csv_L2b_desc)) {
            progress$inc(1/length(csv_L2b_desc), detail = paste("L2 data", i, "of", length(csv_L2b_desc)))
            Sys.sleep(0.25)
            progress_list2 = c(progress_list2, paste("Converted", csv_L2b_desc[i],'\n'))
            PS_L2_NTS_toSHP(csv_L2b_desc[i], exdir_unz, exdir)
          }
        } else {
          rm(csv_L2b_desc)
        }
        
        # COMPONENTS
        if (sum(csv_L3_horiz != 0)!= 0) {
          dir.create(paste0(global$datapath, "/PS_L3_E/"))
          exdir <- paste0(global$datapath, "/PS_L3_E/")
          for (i in 1:length(csv_L3_horiz)) {
            progress$inc(1/length(csv_L3_horiz), detail = paste("L3 data", i, "of", length(csv_L3_horiz)))
            Sys.sleep(0.25)
            progress_list2 = c(progress_list2, paste("Converted", csv_L3_horiz[i],'\n'))
            PS_L3_NTS_toSHP(csv_L3_horiz[i], exdir_unz, exdir)
          }
        } else {
          rm(csv_L3_horiz)
        }
        if (sum(csv_L3_vert != 0)!= 0) {
          dir.create(paste0(global$datapath, "/PS_L3_U/"))
          exdir <- paste0(global$datapath, "/PS_L3_U/")
          for (i in 1:length(csv_L3_vert)) {
            progress$inc(1/length(csv_L3_vert), detail = paste("L3 data", i, "of", length(csv_L3_vert)))
            Sys.sleep(0.25)
            progress_list2 = c(progress_list2, paste("Converted", csv_L3_vert[i],'\n'))
            PS_L3_NTS_toSHP(csv_L3_vert[i], exdir_unz, exdir)
          }
        } else {
          rm(csv_L3_vert)
        } # close else
        
      } else if (input$TimeSeriesSelection == "YTS" && input$FormatSelection == "GPKG") {
        if (input$DataFormat == "ymd") {
          list_file <- list.files(path = exdir_unz, all.files = FALSE, full.names = FALSE, recursive = FALSE,
                                  ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
          
          csv_L2a_asc <- matrix(nrow = 0, ncol = 0)
          csv_L2a_desc <- matrix(nrow = 0, ncol = 0)
          csv_L2b_asc <- matrix(nrow = 0, ncol = 0)
          csv_L2b_desc <- matrix(nrow = 0, ncol = 0)
          csv_L3_horiz <- matrix(nrow = 0, ncol = 0)
          csv_L3_vert <- matrix(nrow = 0, ncol = 0)
          orbit_asc <- c("01", "02", "03", "04")
          orbit_desc <- c("06", "07", "08", "09")
          
          for (i in 1:length(list_file)) {
            product <- substr(list_file[i], 6, 8)
            orbit <- substr(list_file[i], 14, 15)
            comp <- substr(list_file[i], nchar(list_file[i])-4, nchar(list_file[i])-4)
            extens <- substr(list_file[i], nchar(list_file[i])-3, nchar(list_file[i]))
            if (extens == ".csv" && orbit %in% orbit_asc && product == "L2a") {
              csv_L2a_asc <- append(csv_L2a_asc, list_file[i])
            } # close if L2a asc
            if (extens == ".csv" && orbit %in% orbit_asc && product == "L2b") {
              csv_L2b_asc <- append(csv_L2b_asc, list_file[i])
            } # close if L2b asc
            if (extens == ".csv" && orbit %in% orbit_desc && product == "L2a") {
              csv_L2a_desc <- append(csv_L2a_desc, list_file[i])
            } # close if L2a desc
            if (extens == ".csv" && orbit %in% orbit_desc && product == "L2b") {
              csv_L2b_desc <- append(csv_L2b_desc, list_file[i])
            } # close if L2b desc
            if (extens == ".csv" && comp == "E") {
              csv_L3_horiz <- append(csv_L3_horiz, list_file[i])
            } # close if L3 E
            if (extens == ".csv" && comp == "U") {
              csv_L3_vert <- append(csv_L3_vert, list_file[i])
            } # close if L3 U
          } # close for 
          
          rm(product)
          rm(orbit)
          rm(comp)
          rm(extens)
          
          # LOS
          if (sum(csv_L2a_asc != 0) != 0) {
            dir.create(paste0(global$datapath, "/PS_L2a_asc/"))
            exdir <- paste0(global$datapath, "/PS_L2a_asc/")
            for (i in 1:length(csv_L2a_asc)) {
              progress$inc(1/length(csv_L2a_asc), detail = paste("L2 data", i, "of", length(csv_L2a_asc)))
              Sys.sleep(0.25)
              progress_list2 = c(progress_list2, paste("Converted", csv_L2a_asc[i],'\n'))
              PS_L2_ymd_toGPKG(csv_L2a_asc[i], exdir_unz, exdir)
            }
          } else {
            rm(csv_L2a_asc)
          }
          if (sum(csv_L2a_desc != 0) != 0) {
            dir.create(paste0(global$datapath, "/PS_L2a_desc/"))
            exdir <- paste0(global$datapath, "/PS_L2a_desc/")
            for (i in 1:length(csv_L2a_desc)) {
              progress$inc(1/length(csv_L2a_desc), detail = paste("L2 data", i, "of", length(csv_L2a_desc)))
              Sys.sleep(0.25)
              progress_list2 = c(progress_list2, paste("Converted", csv_L2a_desc[i],'\n'))
              PS_L2_ymd_toGPKG(csv_L2a_desc[i], exdir_unz, exdir)
            }
          } else {
            rm(csv_L2a_desc)
          }
          if (sum(csv_L2b_asc != 0) != 0) {
            dir.create(paste0(global$datapath, "/PS_L2b_asc/"))
            exdir <- paste0(global$datapath, "/PS_L2b_asc/")
            for (i in 1:length(csv_L2b_asc)) {
              progress$inc(1/length(csv_L2b_asc), detail = paste("L2 data", i, "of", length(csv_L2b_asc)))
              Sys.sleep(0.25)
              progress_list2 = c(progress_list2, paste("Converted", csv_L2b_asc[i],'\n'))
              PS_L2_ymd_toGPKG(csv_L2b_asc[i], exdir_unz, exdir)
            }
          } else {
            rm(csv_L2b_asc)
          }
          if (sum(csv_L2b_desc != 0) != 0) {
            dir.create(paste0(global$datapath, "/PS_L2b_desc/"))
            exdir <- paste0(global$datapath, "/PS_L2b_desc/")
            for (i in 1:length(csv_L2b_desc)) {
              progress$inc(1/length(csv_L2b_desc), detail = paste("L2 data", i, "of", length(csv_L2b_desc)))
              Sys.sleep(0.25)
              progress_list2 = c(progress_list2, paste("Converted", csv_L2b_desc[i],'\n'))
              PS_L2_ymd_toGPKG(csv_L2b_desc[i], exdir_unz, exdir)
            }
          } else {
            rm(csv_L2b_desc)
          }
          
          # COMPONENTS
          if (sum(csv_L3_horiz != 0)!= 0) {
            dir.create(paste0(global$datapath, "/PS_L3_E/"))
            exdir <- paste0(global$datapath, "/PS_L3_E/")
            for (i in 1:length(csv_L3_horiz)) {
              progress$inc(1/length(csv_L3_horiz), detail = paste("L3 data", i, "of", length(csv_L3_horiz)))
              Sys.sleep(0.25)
              progress_list2 = c(progress_list2, paste("Converted", csv_L3_horiz[i],'\n'))
              PS_L3_ymd_toGPKG(csv_L3_horiz[i], exdir_unz, exdir)
            }
          } else {
            rm(csv_L3_horiz)
          }
          if (sum(csv_L3_vert != 0)!= 0) {
            dir.create(paste0(global$datapath, "/PS_L3_U/"))
            exdir <- paste0(global$datapath, "/PS_L3_U/")
            for (i in 1:length(csv_L3_vert)) {
              progress$inc(1/length(csv_L3_vert), detail = paste("L3 data", i, "of", length(csv_L3_vert)))
              Sys.sleep(0.25)
              progress_list2 = c(progress_list2, paste("Converted", csv_L3_vert[i],'\n'))
              PS_L3_ymd_toGPKG(csv_L3_vert[i], exdir_unz, exdir)
            }
          } else {
            rm(csv_L3_vert)
          } # close else
          
        } else if (input$DataFormat == "dmy") {
          list_file <- list.files(path = exdir_unz, all.files = FALSE, full.names = FALSE, recursive = FALSE,
                                  ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
          
          csv_L2a_asc <- matrix(nrow = 0, ncol = 0)
          csv_L2a_desc <- matrix(nrow = 0, ncol = 0)
          csv_L2b_asc <- matrix(nrow = 0, ncol = 0)
          csv_L2b_desc <- matrix(nrow = 0, ncol = 0)
          csv_L3_horiz <- matrix(nrow = 0, ncol = 0)
          csv_L3_vert <- matrix(nrow = 0, ncol = 0)
          orbit_asc <- c("01", "02", "03", "04")
          orbit_desc <- c("06", "07", "08", "09")
          
          for (i in 1:length(list_file)) {
            product <- substr(list_file[i], 6, 8)
            orbit <- substr(list_file[i], 14, 15)
            comp <- substr(list_file[i], nchar(list_file[i])-4, nchar(list_file[i])-4)
            extens <- substr(list_file[i], nchar(list_file[i])-3, nchar(list_file[i]))
            if (extens == ".csv" && orbit %in% orbit_asc && product == "L2a") {
              csv_L2a_asc <- append(csv_L2a_asc, list_file[i])
            } # close if L2a asc
            if (extens == ".csv" && orbit %in% orbit_asc && product == "L2b") {
              csv_L2b_asc <- append(csv_L2b_asc, list_file[i])
            } # close if L2b asc
            if (extens == ".csv" && orbit %in% orbit_desc && product == "L2a") {
              csv_L2a_desc <- append(csv_L2a_desc, list_file[i])
            } # close if L2a desc
            if (extens == ".csv" && orbit %in% orbit_desc && product == "L2b") {
              csv_L2b_desc <- append(csv_L2b_desc, list_file[i])
            } # close if L2b desc
            if (extens == ".csv" && comp == "E") {
              csv_L3_horiz <- append(csv_L3_horiz, list_file[i])
            } # close if L3 E
            if (extens == ".csv" && comp == "U") {
              csv_L3_vert <- append(csv_L3_vert, list_file[i])
            } # close if L3 U
          } # close for 
          
          rm(product)
          rm(orbit)
          rm(comp)
          rm(extens)
          
          # LOS
          if (sum(csv_L2a_asc != 0) != 0) {
            dir.create(paste0(global$datapath, "/PS_L2a_asc/"))
            exdir <- paste0(global$datapath, "/PS_L2a_asc/")
            for (i in 1:length(csv_L2a_asc)) {
              progress$inc(1/length(csv_L2a_asc), detail = paste("L2 data", i, "of", length(csv_L2a_asc)))
              Sys.sleep(0.25)
              progress_list2 = c(progress_list2, paste("Converted", csv_L2a_asc[i],'\n'))
              PS_L2_dmy_toGPKG(csv_L2a_asc[i], exdir_unz, exdir)
            }
          } else {
            rm(csv_L2a_asc)
          }
          if (sum(csv_L2a_desc != 0) != 0) {
            dir.create(paste0(global$datapath, "/PS_L2a_desc/"))
            exdir <- paste0(global$datapath, "/PS_L2a_desc/")
            for (i in 1:length(csv_L2a_desc)) {
              progress$inc(1/length(csv_L2a_desc), detail = paste("L2 data", i, "of", length(csv_L2a_desc)))
              Sys.sleep(0.25)
              progress_list2 = c(progress_list2, paste("Converted", csv_L2a_desc[i],'\n'))
              PS_L2_dmy_toGPKG(csv_L2a_desc[i], exdir_unz, exdir)
            }
          } else {
            rm(csv_L2a_desc)
          }
          if (sum(csv_L2b_asc != 0) != 0) {
            dir.create(paste0(global$datapath, "/PS_L2b_asc/"))
            exdir <- paste0(global$datapath, "/PS_L2b_asc/")
            for (i in 1:length(csv_L2b_asc)) {
              progress$inc(1/length(csv_L2b_asc), detail = paste("L2 data", i, "of", length(csv_L2b_asc)))
              Sys.sleep(0.25)
              progress_list2 = c(progress_list2, paste("Converted", csv_L2b_asc[i],'\n'))
              PS_L2_dmy_toGPKG(csv_L2b_asc[i], exdir_unz, exdir)
            }
          } else {
            rm(csv_L2b_asc)
          }
          if (sum(csv_L2b_desc != 0) != 0) {
            dir.create(paste0(global$datapath, "/PS_L2b_desc/"))
            exdir <- paste0(global$datapath, "/PS_L2b_desc/")
            for (i in 1:length(csv_L2b_desc)) {
              progress$inc(1/length(csv_L2b_desc), detail = paste("L2 data", i, "of", length(csv_L2b_desc)))
              Sys.sleep(0.25)
              progress_list2 = c(progress_list2, paste("Converted", csv_L2b_desc[i],'\n'))
              PS_L2_dmy_toGPKG(csv_L2b_desc[i], exdir_unz, exdir)
            }
          } else {
            rm(csv_L2b_desc)
          }
          
          # COMPONENTS
          if (sum(csv_L3_horiz != 0)!= 0) {
            dir.create(paste0(global$datapath, "/PS_L3_E/"))
            exdir <- paste0(global$datapath, "/PS_L3_E/")
            for (i in 1:length(csv_L3_horiz)) {
              progress$inc(1/length(csv_L3_horiz), detail = paste("L3 data", i, "of", length(csv_L3_horiz)))
              Sys.sleep(0.25)
              progress_list2 = c(progress_list2, paste("Converted", csv_L3_horiz[i],'\n'))
              PS_L3_dmy_toGPKG(csv_L3_horiz[i], exdir_unz, exdir)
            }
          } else {
            rm(csv_L3_horiz)
          }
          if (sum(csv_L3_vert != 0)!= 0) {
            dir.create(paste0(global$datapath, "/PS_L3_U/"))
            exdir <- paste0(global$datapath, "/PS_L3_U/")
            for (i in 1:length(csv_L3_vert)) {
              progress$inc(1/length(csv_L3_vert), detail = paste("L3 data", i, "of", length(csv_L3_vert)))
              Sys.sleep(0.25)
              progress_list2 = c(progress_list2, paste("Converted", csv_L3_vert[i],'\n'))
              PS_L3_dmy_toGPKG(csv_L3_vert[i], exdir_unz, exdir)
            }
          } else {
            rm(csv_L3_vert)
          } # close else
        } # close YTS GPKG dmy
        
      } else if (input$TimeSeriesSelection == "NTS" && input$FormatSelection == "GPKG") {
        list_file <- list.files(path = exdir_unz, all.files = FALSE, full.names = FALSE, recursive = FALSE,
                                ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
        
        csv_L2a_asc <- matrix(nrow = 0, ncol = 0)
        csv_L2a_desc <- matrix(nrow = 0, ncol = 0)
        csv_L2b_asc <- matrix(nrow = 0, ncol = 0)
        csv_L2b_desc <- matrix(nrow = 0, ncol = 0)
        csv_L3_horiz <- matrix(nrow = 0, ncol = 0)
        csv_L3_vert <- matrix(nrow = 0, ncol = 0)
        orbit_asc <- c("01", "02", "03", "04")
        orbit_desc <- c("06", "07", "08", "09")
        
        for (i in 1:length(list_file)) {
          product <- substr(list_file[i], 6, 8)
          orbit <- substr(list_file[i], 14, 15)
          comp <- substr(list_file[i], nchar(list_file[i])-4, nchar(list_file[i])-4)
          extens <- substr(list_file[i], nchar(list_file[i])-3, nchar(list_file[i]))
          if (extens == ".csv" && orbit %in% orbit_asc && product == "L2a") {
            csv_L2a_asc <- append(csv_L2a_asc, list_file[i])
          } # close if L2a asc
          if (extens == ".csv" && orbit %in% orbit_asc && product == "L2b") {
            csv_L2b_asc <- append(csv_L2b_asc, list_file[i])
          } # close if L2b asc
          if (extens == ".csv" && orbit %in% orbit_desc && product == "L2a") {
            csv_L2a_desc <- append(csv_L2a_desc, list_file[i])
          } # close if L2a desc
          if (extens == ".csv" && orbit %in% orbit_desc && product == "L2b") {
            csv_L2b_desc <- append(csv_L2b_desc, list_file[i])
          } # close if L2b desc
          if (extens == ".csv" && comp == "E") {
            csv_L3_horiz <- append(csv_L3_horiz, list_file[i])
          } # close if L3 E
          if (extens == ".csv" && comp == "U") {
            csv_L3_vert <- append(csv_L3_vert, list_file[i])
          } # close if L3 U
        } # close for 
        
        rm(product)
        rm(orbit)
        rm(comp)
        rm(extens)
        
        # LOS
        if (sum(csv_L2a_asc != 0) != 0) {
          dir.create(paste0(global$datapath, "/PS_L2a_asc/"))
          exdir <- paste0(global$datapath, "/PS_L2a_asc/")
          for (i in 1:length(csv_L2a_asc)) {
            progress$inc(1/length(csv_L2a_asc), detail = paste("L2 data", i, "of", length(csv_L2a_asc)))
            Sys.sleep(0.25)
            progress_list2 = c(progress_list2, paste("Converted", csv_L2a_asc[i],'\n'))
            PS_L2_NTS_toGPKP(csv_L2a_asc[i], exdir_unz, exdir)
          }
        } else {
          rm(csv_L2a_asc)
        }
        if (sum(csv_L2a_desc != 0) != 0) {
          dir.create(paste0(global$datapath, "/PS_L2a_desc/"))
          exdir <- paste0(global$datapath, "/PS_L2a_desc/")
          for (i in 1:length(csv_L2a_desc)) {
            progress$inc(1/length(csv_L2a_desc), detail = paste("L2 data", i, "of", length(csv_L2a_desc)))
            Sys.sleep(0.25)
            progress_list2 = c(progress_list2, paste("Converted", csv_L2a_desc[i],'\n'))
            PS_L2_NTS_toGPKP(csv_L2a_desc[i], exdir_unz, exdir)
          }
        } else {
          rm(csv_L2a_desc)
        }
        if (sum(csv_L2b_asc != 0) != 0) {
          dir.create(paste0(global$datapath, "/PS_L2b_asc/"))
          exdir <- paste0(global$datapath, "/PS_L2b_asc/")
          for (i in 1:length(csv_L2b_asc)) {
            progress$inc(1/length(csv_L2b_asc), detail = paste("L2 data", i, "of", length(csv_L2b_asc)))
            Sys.sleep(0.25)
            progress_list2 = c(progress_list2, paste("Converted", csv_L2b_asc[i],'\n'))
            PS_L2_NTS_toGPKP(csv_L2b_asc[i], exdir_unz, exdir)
          }
        } else {
          rm(csv_L2b_asc)
        }
        if (sum(csv_L2b_desc != 0) != 0) {
          dir.create(paste0(global$datapath, "/PS_L2b_desc/"))
          exdir <- paste0(global$datapath, "/PS_L2b_desc/")
          for (i in 1:length(csv_L2b_desc)) {
            progress$inc(1/length(csv_L2b_desc), detail = paste("L2 data", i, "of", length(csv_L2b_desc)))
            Sys.sleep(0.25)
            progress_list2 = c(progress_list2, paste("Converted", csv_L2b_desc[i],'\n'))
            PS_L2_NTS_toGPKP(csv_L2b_desc[i], exdir_unz, exdir)
          }
        } else {
          rm(csv_L2b_desc)
        }
        
        # COMPONENTS
        if (sum(csv_L3_horiz != 0)!= 0) {
          dir.create(paste0(global$datapath, "/PS_L3_E/"))
          exdir <- paste0(global$datapath, "/PS_L3_E/")
          for (i in 1:length(csv_L3_horiz)) {
            progress$inc(1/length(csv_L3_horiz), detail = paste("L3 data", i, "of", length(csv_L3_horiz)))
            Sys.sleep(0.25)
            progress_list2 = c(progress_list2, paste("Converted", csv_L3_horiz[i],'\n'))
            PS_L3_NTS_toGPKG(csv_L3_horiz[i], exdir_unz, exdir)
          }
        } else {
          rm(csv_L3_horiz)
        }
        if (sum(csv_L3_vert != 0)!= 0) {
          dir.create(paste0(global$datapath, "/PS_L3_U/"))
          exdir <- paste0(global$datapath, "/PS_L3_U/")
          for (i in 1:length(csv_L3_vert)) {
            progress$inc(1/length(csv_L3_vert), detail = paste("L3 data", i, "of", length(csv_L3_vert)))
            Sys.sleep(0.25)
            progress_list2 = c(progress_list2, paste("Converted", csv_L3_vert[i],'\n'))
            PS_L3_NTS_toGPKG(csv_L3_vert[i], exdir_unz, exdir)
          }
        } else {
          rm(csv_L3_vert)
        } # close else
      } # close TimeSeriesSelection 
    }
    
    showNotification("Data conversion successful!", type = c("message"), duration=NULL)
    
    output$txtout2 <- renderPrint({
      cat(progress_list2, "\nData conversion completed")
    })
    
    unlink(exdir_unz, recursive = T, force = T)
    
  }) # close observeEvent input$RUN
  
  # close the R session when Chrome closes
  session$onSessionEnded(function() { 
    stopApp()
    q("no") 
  }) # close session
  
} # server

# Create Shiny object
shinyApp(ui = ui, server = server)
