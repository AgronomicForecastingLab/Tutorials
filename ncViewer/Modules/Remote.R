observeEvent(input$tunnel_setup_btn,{
  showModal(modalDialog(
    title = "Tunnel setup",
    textInput('tunnel_user', 'Username:', value = "hamzed", width = "100%"),
    textInput('tunnel_path', 'Path to tunnel:', value = "~/tunnel/tunnel", width = "100%"),
    selectizeInput('output_type_sims', label="Output Type", choices= c("csv", "out"), selected = "csv", multiple = FALSE),
    textInput('SimPath_path', 'Path to your simulations:', value = "/home/hamzed/project-aces/psims/Data/sims", width = "100%"),
    fluidRow(
      column(3),
      column(6,actionGroupButtons(
        inputIds = c("tunnel_save_btn"),
        labels = list(tags$span(icon("copy"), "Save !")),
        status = "primary", fullwidth = TRUE
      )),
      column(3)
    ),
    easyClose = TRUE,
    footer = NULL
  ))
})


observeEvent(input$tunnel_save_btn, {
plan(multisession)
  
  simType <- input$output_type_sims
  host <-
    list(name = 'cc-login.campuscluster.illinois.edu',
         user = input$tunnel_user,
         tunnel = input$tunnel_path,
         from=paste0('/home/hamzed/pSIMS/'),
         to=input$SimPath_path)
  
  
  tunnel_test <- pSIMSSiteMaker::remote.execute.cmd(host,'echo Ok')
  
  if (tunnel_test=="Ok"){

    toastr_success(paste0("Connection was successfuly established !")) 
    removeModal()


    # Find all the Sims dirs
    sims <- pSIMSSiteMaker::remote.execute.cmd(host,
                                                   paste0("ls -R -d ",host$to,"/*")
                                                   ) %>%
      discard(grepl("*job*|*.sh$",.)) %>%
      basename()
    

    # For each sim find the out files
    All.sims <- sims[1:50] %>%
      future_map(function(eachsim){
        All.sims <- pSIMSSiteMaker::remote.execute.cmd(host,
                                                       paste0("find ",file.path(host$to, eachsim)," -name '*.",simType,"'")                                     
        ) %>%
          discard(grepl("*job*|*.sh$",.)) %>%
          basename() 
      }) %>%
      setNames(sims[1:50])


All.sims <-All.sims %>%
    map(function(ss){
      if(length(ss)>0){
        as.list(ss) %>%
          setNames(ss)
      }
    })
    

    output$tree123 <- renderTree({
      All.sims
    })

    
    
  }else{
    toastr_error(title = "Error", "Something is wrong with the tunnel !")
  }
})


observeEvent(input$tree123, {

  # req(tree)
  # get_selected(tree123)

  w$show()
  sim_dir <- file.path(input$SimPath_path,get_selected(input$tree123))

  host <-
    list(name = 'cc-login.campuscluster.illinois.edu',
         user = input$tunnel_user,
         tunnel = input$tunnel_path,
         from=paste0('/home/hamzed/pSIMS/'),
         to=input$SimPath_path)
  
  

  All.sims <- pSIMSSiteMaker::remote.execute.cmd(host,
                                                 paste0("find ",sim_dir," -name '*.",input$output_type_sims,"'")  
  )
  w$update(html = "Transfering files from the cluster ... This may take a minute !")
  # I'm going to create a tmp dir and move files back
  tmpdir <- tempdir()
  newdir <-  paste(sample(letters, 5), collapse = "")
  dir.create(file.path(tmpdir, newdir))
  
  message(All.sims)
  if(length(All.sims)>0 ) {
    # bring the file back
    All.sims %>%
      future_map(function(.x){
        remote.copy.from(host=host,
                         src=.x,
                         dst=file.path(tmpdir, newdir),
                         delete = TRUE) 
        
      }
      )
    
    w$update(html = "Reading them in ... This may take a minute !")
    # Read and reformat the outputs  
    apsim.out <- list.files(file.path(tmpdir, newdir), full.names = TRUE) %>%
      future_map_dfr(~ suppressMessages(readr::read_csv(.x)))
  
    Remotevalues$Outputs <- apsim.out

    w$hide()
    }else{
    toastr_error(title = "Error", "Something is wrong with the tunnel !")
  }

})


observeEvent(Remotevalues$Outputs, {
  
  
  output$remote_moreControls <- renderUI({
    tagList(
      fluidRow(
        column(8, 
               selectizeInput('remote_load_var', label="Select: ", choices= colnames(Remotevalues$Outputs)%>% as.list(), selected = colnames(Remotevalues$Outputs)[1:10], multiple = TRUE),
               ), 
        column(4)
      ),hr()
    )
  })
  
})

observeEvent(input$remote_load_var, {

  output$psims_pivot <- renderRpivotTable({
           rpivotTable(data =   Remotevalues$Outputs[,input$remote_load_var], width = "100px")
  })
  
})