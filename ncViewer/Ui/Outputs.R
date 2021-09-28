navbarMenu("Outputs", icon = icon("cloud-download"),
           
           tabPanel("Visualizer",icon = icon("map"),
                    useToastr(),
                    shinyjs::useShinyjs(),
                    tags$head(
                      tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
                    ),
                    fluidRow(
                      column(6,
                             actionGroupButtons(
                               inputIds = c("meta_btn", "download_output_btn","clear_map_btn"),
                               labels = list(tags$span(icon("align-justify"), "Meta-data"),
                                             tags$span(icon("download"), "Download !"),
                                             tags$span(icon("broom"), "Clear !")),
                               status = "primary"
                             )
                      )
                    ),hr(), 
                    fluidRow(
                      column(2,
                             fluidRow(
                               column(12,
                                      shinyDirButton("dir", "Chose directory", "Upload"),
                                      br(),br(),
                                      DT::DTOutput("files")
                                      
                                      
                               )
                             )
                      ),
                      column(10, leafletOutput("mymap", height = "800px"),
                             absolutePanel(id = "controls", class = "info legend leaflet-control", fixed = FALSE,
                                           draggable = TRUE, top = 1, left = "auto", right = 15, bottom = "auto",
                                           #style="z-index:15000;",
                                           width = 200, height = "auto",
                                           column(12,
                                                  fluidRow(
                                                    column(12,  
                                                           
                                                           pickerInput(
                                                             inputId = "var.inp",
                                                             label = "Variables:", 
                                                             choices = c(),
                                                             options = list(
                                                               `live-search` = TRUE,
                                                               style = "btn-primary"
                                                             )
                                                           ),
                                                           sliderInput("opacity", "Opacity :", min = 10, max=100, value = 75),
                                                           sliderInput("time", "Time :", min = 1, max=3, value = 1),
                                                           sliderInput("scenario_output_slider", "Scenario :", min = 1, max=3, value = 1),
                                                           awesomeCheckboxGroup(
                                                             inputId = "out_options",
                                                             label = "Options:", 
                                                             choices = c("Interpolate"),
                                                             inline = TRUE,
                                                             selected = "Interpolate",
                                                             status = "primary"
                                                           )
                                                    )
                                                  ),
                                                  br()
                                                  
                                                  
                                                  
                                           )
                             )
                      ) # end e leaflet column
                      
                    )
           ), # end of the output tab panel
           tabPanel("Aggregate", icon = icon("calculator"))
)