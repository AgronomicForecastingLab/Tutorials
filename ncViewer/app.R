library(shiny)
library(mapview)
library(leaflet)
library(leaflet.extras)
library(tidyverse)
library(DT)
library(shinytoastr)
library(raster)
library(shinyjs)
library(listviewer)
library(shinyWidgets)
library(leaflet.extras)
library(shinyFiles)
library(excelR)
library(shinyglide)
library(leafem)

counties_poly <- sf::read_sf('counties.geojson') %>% sf::as_Spatial()
#helper functions
source(file.path('Modules','Helper_functions.R'), local = TRUE)$value

#Ideas---
#- When click on edit . turn raster to geojson, let them to edit values and then take it and use CDO commandline
#-----

home.path <- "/mnt"
options(shiny.trace=FALSE)
#a
shinyApp(
    ui = navbarPage("pSIMS Toolkit",
                    theme = shinythemes::shinytheme("paper"),  # <--- Specify theme here
        source(file.path("Ui","Home.R"), local = TRUE)$value,
#Inputs-----------------------------------------------------------------------------------------------                  
        source(file.path("Ui","Inputs.R"), local = TRUE)$value,
#Output-----------------------------------------------------------------------------------------------
        source(file.path("Ui","Outputs.R"), local = TRUE)$value,
#Utilities--------------------------------------------------------------------------------------------                               
        source(file.path("Ui","Utilities.R"), local = TRUE)$value,
#Loading panel----------------------------------------------------------------------------------------
        conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                         tags$div(id="loadmessage",
                                  HTML(paste0("<div class=\"span4\"><img src=\"loader.gif\" height=\"60\" width=\"111\"></div>
                                     "))
                         ))
                    

    ),
#Server------------------------------------------------------------------------------------------------
    server = function(session, input, output) {

    # dir --------------------------------------
    shinyDirChoose(input, 'dir', roots = c(home = home.path), filetypes = c('', 'txt'))
    shinyDirChoose(input, 'diroutput', roots = c(home = home.path), filetypes = c('', 'txt'))
    shinyDirChoose(input, 'dir_camp_json', roots = c(home = home.path), filetypes = c('', 'json'))
    shinyDirChoose(input, 'dir_param_yaml', roots = c(home = home.path), filetypes = c('', 'json'))
    shinyFileChoose(input, 'files', root=c(root='.'), filetypes=c('', 'nc','nc4'))
    
    #Input------------------------------------------------------
    source(file.path("Modules","Inputs.R"), local = TRUE)$value
    
    # Home------------------------------------
    source(file.path("Modules","Home.R"), local = TRUE)$value
    
    #Output-------------------------------------------------------------
    source(file.path("Modules","Outputs.R"), local = TRUE)$value

    #Utilities --------------
    source(file.path("Modules","Utilities.R"), local = TRUE)$value
  
    
    }# end server
)