navbarMenu("Inputs", icon = icon("cloud-upload"),
           tabPanel("Campaign",icon = icon("layer-group"), 
                    fluidRow(
                      column(12,
                             actionGroupButtons(
                               inputIds = c(
                                           "create_new_camp_diag_btn",
                                           "new_var_camp_diag_btn",
                                            "save_campnc_diag_btn",
                                            'clip_diag_btn',
                                            'refresh_btn',
                                            "meta_input_btn"
                                            ),
                               labels = list(
                                             tags$span(icon("copy"), "Create New Campaign"),
                                             tags$span(icon("plus"), "Add a new variable"),
                                             tags$span(icon("save"), "Save edits"),
                                             tags$span(icon("adjust"), "Clip Campaign"),
                                             tags$span(icon("refresh"), "Refresh"),
                                             tags$span(icon("align-justify"), "Meta-data")
                                             ),
                               status = "primary"
                             )
                      )
                    ),hr(),
                    fluidRow(
                      column(2,
                             fluidRow(
                               column(12,
                                      shinyDirButton("diroutput", "Chose directory", "Upload")
                                      ),br(),
                                      hr(),
                               column(12,
                                      pickerInput(
                                        inputId = "var.inp.out",
                                        label = "Variables:", 
                                        choices = c(),
                                        options = list(
                                          `live-search` = TRUE,
                                          style = "btn-primary"
                                        )
                                      )),
                               column(12,  
                                      sliderInput("scen", "Scenario :", min = 1, max=3, value = 1)
                               ),
                               column(12,
                                      br(),
                                      DT::DTOutput("filesoutput")
   
                               )
                             )
                      ),
                      column(10,
                             tabsetPanel(
                               tabPanel("Visualize", icon = icon("map"),
                                        br(),
                                        leafletOutput("mymapoutput", height = "800px"),
                                        absolutePanel(id = "controlsoutput", class = "panel panel-default", fixed = FALSE,
                                                      draggable = TRUE, top = 65, left = "auto", right = 20, bottom = "auto",
                                                      #style="z-index:15000;",
                                                      width = 220, height = "auto",
                                                      column(12,
                                                             fluidRow(
    
                                                             )
  
                                                      )
                                        )
                               ),
                               tabPanel("Edit",icon = icon("gear"),
                                        br(),
                                        fluidRow(
                                          column(12,
                                                 tags$div(style="overflow: scroll;",
                                                          excelOutput("table", height = "800px"))
                                          )
                                        )
                               )
                             )
                             
                      ) # end e leaflet column
                      
                    )),
           tabPanel("Param file", icon = icon("newspaper"),
                    fluidRow(
                      column(2,shinyDirButton("dir_param_yaml", "Chose directory", "Upload")),
                      column(4)
                    ), hr(),
                    fluidRow(
                      
                      column(2,
                             fluidRow(
                               column(12,
                                      actionGroupButtons(
                                        inputIds = c("save_yaml_btn"),
                                        labels = list(tags$span(icon("save"), " Save !")),
                                        status = "primary",
                                        fullwidth =TRUE
                                      )
                                      )
                             ),
                             fluidRow(
                               column(12,br(), br()),
                               column(12, DT::DTOutput("files_param_yaml_tbl"))
                             )
                      ),
                      column(6, reactjsonOutput("param_yaml_list", height = "600px")),
                      column(4,
                             alert(type='primary',
                                   title='How to use this ?',
                                   msg='msg'),
                             alert(type='info',
                                   title='Oh snap!',
                                   msg='msg'),
                             alert(type='danger',
                                   title='Oh snap!',
                                   msg='msg'),
                             alert(type='warning',
                                   title='Oh snap!',
                                   msg='msg'),
                             alert(type='dark',
                                   title='Oh snap!',
                                   msg='msg'),
                             alert(type='light',
                                   title='Oh snap!',
                                   msg='msg')
                             )
                    )
           ),
           tabPanel("Campaign json", icon = icon("tag"),
                    fluidRow(
                      column(2,shinyDirButton("dir_camp_json", "Chose directory", "Upload")),
                      column(4)
                    ), hr(),
                    fluidRow(
                      
                      column(2,
                             fluidRow(
                               column(12,br(), br()),
                               column(12, DT::DTOutput("files_camp_json"))
                             ),
                             fluidRow(
                               column(12,
                                      actionGroupButtons(
                                        inputIds = c("save_json_btn"),
                                        labels = list(tags$span(icon("save"), " Save !")),
                                        status = "primary",
                                        fullwidth=TRUE
                                        
                                      )
                               )
                             )
                      ),
                      column(5, reactjsonOutput("json_camp_list", height = "600px")),
                      column(5,
                             alert(type='primary',
                                      title='How to use this ?',
                                      msg='msg'),
                             alert(type='info',
                                   title='Oh snap!',
                                   msg='msg'),
                             alert(type='danger',
                                   title='Oh snap!',
                                   msg='msg'),
                             alert(type='warning',
                                   title='Oh snap!',
                                   msg='msg'),
                             alert(type='dark',
                                   title='Oh snap!',
                                   msg='msg'),
                             alert(type='light',
                                   title='Oh snap!',
                                   msg='msg')
                             )
                    )
                    
           ),
           tabPanel("Soil ", icon = icon("layer-group")),
           tabPanel("Weather ", icon = icon("cloud"))
)