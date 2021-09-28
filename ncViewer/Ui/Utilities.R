navbarMenu("Utilities", icon = icon("cogs"),
           tabPanel("Grid System",icon = icon("globe-americas"),
                    leafletOutput("util.map", height = "800px")),
           tabPanel("File Transfer",icon = icon("sort"),
                    br(),
                    fluidRow(
                      column(6, line_breaker("Local ↔ Remote"),
                             br(),br(),
                             selectInput("LR_side", NULL,
                                         c(`Upload to Remote` = "TR",
                                           `Download to Local` = "TL"),
                                         width = "100%"
                             ),
                             fluidRow(
                               column(12,
                                      conditionalPanel(
                                        condition = "input.LR_side == 'TR'",
                                        selectizeInput(
                                          "breaks", "Breaks",
                                          c("Sturges", "Scott", "Freedman-Diaconis", "[Custom]" = "custom")
                                        )),
                                        # Only show this panel if Custom is selected
                                        conditionalPanel(
                                          condition = "input.LR_side == 'TL'",
                                          sliderInput("breakCount", "Break Count", min = 1, max = 50, value = 10)
                                        )
                                      
                               )
                             )
                             
                             )
                      ,
                      column(6, line_breaker("Remote ↔ Remote"))
                      
                    ))
)
