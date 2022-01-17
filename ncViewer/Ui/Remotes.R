navbarMenu("Remote", icon = icon("server"),
           
           tabPanel("Pivot Table",icon = icon("map"),
                    useToastr(),
                    shinyjs::useShinyjs(),
                    tags$head(
                      tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
                    ),
                    fluidRow(
                      column(6,actionGroupButtons(
                        inputIds = c(
                          "tunnel_setup_btn"
                        ),
                        labels = list(
                          tags$span(icon("copy"), "Tunnel setup")
                        ),
                        status = "primary"
                      )
                      )
                    ),hr(), 
              fluidRow(
                column(12, 
                       id="remote_main",
                       fluidRow(
                         column(2,

                                fluidRow(
                                  column(12,
                                         shinyTree("tree123", stripes = TRUE, multiple = FALSE, animation = TRUE, 
                                                   search = FALSE)
                                         
                                  )
                                )
                         ),
                         column(10,
                                fluidRow(
                                  column(12,   uiOutput("remote_moreControls"))
                                ),
                              
                                rpivotTableOutput("psims_pivot", height = "800px", width = "100%")
                                
                         ) # end e leaflet column
                         
                       ))
              )
           )
)