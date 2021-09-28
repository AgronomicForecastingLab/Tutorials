observeEvent(input$new_run_btn,{
  modal_controls <- glideControls(
    list(
      prevButton(),
      firstButton(
        class = "btn btn-danger",
        `data-dismiss`="modal",
        "No, thanks !"
      )
    ),
    list(
      nextButton(),
      lastButton(
        class = "btn btn-success",
        `data-dismiss`="modal",
        "Done"
      )
    )
  )
  
  glide_modal <- modalDialog(
    title = "New Run !",
    easyClose = FALSE,
    footer = NULL,
    size='m',
    glide(
      custom_controls = modal_controls,
      screen(
        next_label = 'Yes, please ! <span class="glyphicon glyphicon-chevron-right" aria-hidden="true"></span>',
        p("Let's initialize some values, would you ?")
      ),
      screen(
        fluidRow(
          column(6, pickerInput("models_sel","Models:",
                                choices=list("APSIM","DSSAT","BioCro"),
                                multiple = TRUE)),
          column(6,p(tags$b("Campign File: ")), 
                 shinyFilesButton('files',"dd" ,label='File select',
                                  title='Please select a file', multiple=FALSE)
          )
        ),hr(),
        fluidRow(
          column(6,p(tags$b("Campign json: ")), 
                 shinyFilesButton('files',"dd" ,label='File select',
                                  title='Please select a file', multiple=FALSE)),
          column(6,p(tags$b("Param File: ")), 
                 shinyFilesButton('files',"dd" ,label='File select',
                                  title='Please select a file', multiple=FALSE)
          )
        ),hr()
      ),
      screen(
        p("Next, please select a standard deviation value"),
        numericInput("sd_modal", "Standard deviation", value = 1, min = 0)
      ),
      screen(
        p("Thanks, we're all set !")
      )
    )
  )
  
  showModal(glide_modal)
  
  
})