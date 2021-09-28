output$mymapoutput <- renderLeaflet({
  leaflet() %>%
    setView(lng = -91.0589, lat = 42.3601, zoom = 5)%>%
    addProviderTiles(providers$Hydda.Base, group = "Default")%>% 
    addProviderTiles(providers$Esri.WorldImagery, group = "WorldImagery") %>%
    addProviderTiles(providers$Stamen.TonerLines,
                     options = providerTileOptions(opacity = 0.25),
                     group = "Borders") %>%
    addProviderTiles(providers$Stamen.TonerLabels,
                     options = providerTileOptions(opacity = 0.25),
                     group = "Labels") %>%
    addPolygons(data=counties_poly, stroke = TRUE, fillOpacity = 0.01,
                weight = 0.5, color = "#111822",
                label = ~paste0(NAME), group = "Counties"
    ) %>%
    addScaleBar(position = "bottomleft") %>%
    addMeasure(position = "bottomleft")%>% 
    addFullscreenControl() %>%
    addMouseCoordinates() %>%
    addLayersControl(overlayGroups = c('Labels','Borders','Counties'),
                     baseGroups = c('Default','WorldImagery'),
                     options = layersControlOptions(collapsed=TRUE),
                     position = "bottomright") %>%
    hideGroup("Counties")
})
#Param-------------------------------------------------------
# list file yaml param
observe({
  
  folder.path <-input$dir_param_yaml
  if (class(folder.path) == "list") {
    #browser()
    folder.path <- file.path(home.path, input$dir_param_yaml$path %>% paste(collapse = "/"))
    
    output$files_param_yaml_tbl <- DT::renderDT(
      DT::datatable(
        
        list.files(folder.path, "") %>%
          as.data.frame() %>%
          `colnames<-`(c("File name")),
        
        escape = F,
        selection = "single",
        style = 'bootstrap',
        rownames = FALSE,
        options = list(
          dom = 'ftp',
          pageLength = 10,
          scrollX = TRUE,
          scrollCollapse = TRUE,
          initComplete = DT::JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
            "}"
          )
        )
      )
      
    )
  }
  
})

#preview param yaml
observe({
  req(input$files_param_yaml_tbl_cell_clicked)
  
  #browser()
  if(class(input$dir_param_yaml$path)!="list") return(NULL)
  
  folder.path <- file.path(home.path, input$dir_param_yaml$path %>% paste(collapse = "/"))
  # browser()
  if(length(input$files_param_yaml_tbl_cell_clicked$value)>0){
    # browser()
    file.full.path <- file.path(folder.path, input$files_param_yaml_tbl_cell_clicked$value)
    
    
    # reading it as a raster
    tryCatch({
      
      
      output$param_yaml_list<-renderReactjson({
        reactjson(
          yaml::read_yaml(file.full.path)
        )
      })
      
      
      # message
      toastr_success(paste0("Param file is loaded !")) 
      
    },error = function(e) {
      toastr_error(title = "Error mapping", conditionMessage(e))
    }
    )
    
    
  }
  
})

observeEvent(input$save_yaml_btn,{
  #browser()
  

  tryCatch({
    folder.path <- file.path(home.path, input$dir_param_yaml$path %>% paste(collapse = "/"))
    # browser()
    if(length(input$files_param_yaml_tbl_cell_clicked$value)>0){
      # browser()
      file.full.path <- file.path(folder.path, input$files_param_yaml_tbl_cell_clicked$value)
    }
    
    if(!is.null(input$param_yaml_list_change)){
      yaml::write_yaml(input$param_yaml_list_change$value$updated_src ,file = file.full.path)
    }
    
    toastr_success("File was successfully saved !")
  },error = function(e) {
    toastr_error(title = "Error saving param file", conditionMessage(e))
  })
})


#json-------------------------------------------------------------
#preview camp json
observe({
  req(input$files_camp_json_cell_clicked)
  
  #browser()
  if(class(input$dir_camp_json$path)!="list") return(NULL)
  
  folder.path <- file.path(home.path, input$dir_camp_json$path %>% paste(collapse = "/"))
  # browser()
  if(length(input$files_camp_json_cell_clicked$value)>0){
    # browser()
    file.full.path <- file.path(folder.path, input$files_camp_json_cell_clicked$value)
    
    
    # reading it as a raster
    tryCatch({
      
      
      output$json_camp_list<-renderJsonedit({
        jsonedit(
          jsonlite::fromJSON(file.full.path),
          mode='tree'
        )
      })

      
      # message
      toastr_success(paste0(input$var.inp.out, " is maped !")) 
      
    },error = function(e) {
      toastr_error(title = "Error mapping", conditionMessage(e))
    }
    )
    
    
  }
  
})

# list file campjson
observe({
  
  folder.path <-input$dir_camp_json
  if (class(folder.path) == "list") {
    #browser()
    folder.path <- file.path(home.path, input$dir_camp_json$path %>% paste(collapse = "/"))
    output$files_camp_json <- DT::renderDT(
      DT::datatable(
        list.files(folder.path, "*.json") %>%
          as.data.frame() %>%
          `colnames<-`(c("File name")),
        escape = F,
        selection = "single",
        style = 'bootstrap',
        rownames = FALSE,
        options = list(
          dom = 'ftp',
          pageLength = 10,
          scrollX = TRUE,
          scrollCollapse = TRUE,
          initComplete = DT::JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
            "}"
          )
        )
      )
      
    )
  }
  
})

#nc file --------------------------------------------------------
#list of files Camp
dir_info<-reactive({
  
  input$refresh_btn
  folder.path <-input$diroutput
  if (class(folder.path) == "list") {
    #browser()
    folder.path <- file.path(home.path, input$diroutput$path %>% paste(collapse = "/"))
  }
  
})


observe({
  input$refresh_btn
  folder.path <-dir_info()
   req(input$diroutput)
   if (!is.null(folder.path))
    output$filesoutput <- DT::renderDT(
      DT::datatable(
        list.files(folder.path, "*.nc") %>%
          as.data.frame() %>%
          `colnames<-`(c("File name")),
        escape = F,
        selection = "single",
        style = 'bootstrap',
        rownames = FALSE,
        options = list(
          dom = 'ftp',
          pageLength = 10,
          scrollX = TRUE,
          scrollCollapse = TRUE,
          initComplete = DT::JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
            "}"
          )
        )
      )
      
    )
  
  
})
# camp nc file preview
observe({
  req(input$filesoutput_cell_clicked)

  folder.path <- dir_info()
  # browser()
  if(length(input$filesoutput_cell_clicked$value)>0){
    # browser()
    file.full.path <- file.path(folder.path, input$filesoutput_cell_clicked$value)
    
    tryCatch({
      nc.vars <- ncdf4::nc_open(file.full.path)$var %>% names
    },error = function(e) {
      toastr_error(title = "Error reading var names", conditionMessage(e))
    })
    
    #browser()
    
    if(exists("nc.vars")) {
      if(is.null(input$var.inp.out)){
        tryCatch({
          updatePickerInput(session, 'var.inp.out', choices = nc.vars)
          #browser()
          updateSliderInput(session, 'scen',max=ncdf4::nc_open(file.full.path)$dim$scen$len)
          #
        },error = function(e) {
          toastr_error(title = "Error updaing controls", conditionMessage(e))
        })
      }else if (!(input$var.inp.out %in% nc.vars)) {
        
        tryCatch({
          updatePickerInput(session, 'var.inp.out', choices = nc.vars)
          updateSliderInput(session, 'scen',max=ncdf4::nc_open(file.full.path)$dim$len)
        },error = function(e) {
          toastr_error(title = "Error updaing controls", conditionMessage(e))
        })
      }
    }
    

    #Map the map
    if(!is.null(input$var.inp.out)){
      # reading it as a raster
      tryCatch({
        
        matrix.data <- easyNCDF::NcToArray(file_to_read=file.full.path,
                                           dim_indices =NULL ,
                                           vars_to_read = input$var.inp.out,
                                           drop_var_dim = FALSE,
                                           unlist = TRUE,
                                           expect_all_indices = FALSE,
                                           allow_out_of_range = TRUE)
        
  
        lats <-(easyNCDF::NcToArray(file.full.path, vars_to_read = "lat")) %>% as.numeric()
        lons <-easyNCDF::NcToArray(file.full.path, vars_to_read = "lon") %>% as.numeric()
        
        
        if (length(dim(matrix.data))>3) {
          matrix.data <- matrix.data[, , , input$scen]
        } 
        
        
        output$table <-
          renderExcel({
            excelTable(data =mirror.matrix(mirror.matrix(t(matrix.data))),
                       #colHeaders = as.character(lonv),
                       loadingSpin = TRUE,
                       lazyLoading = TRUE,
                       search = FALSE)
          })
       # browser()
        prec <- stack(file.full.path, varname=input$var.inp.out)
        prec <- prec [[input$scen]]
       
        # setting the extent
        extent(prec)[1] <- min(lons)
        extent(prec)[2] <-max(lons)
        extent(prec)[3] <-min(lats)
        extent(prec)[4] <-max(lats)
        
        #
       # browser()
        #prec.poly <- raster::rasterToPolygons(prec)
        # make the legend
        pal <- colorNumeric(c("#a50026", "#d73027", "#f46d43",
                              "#fdae61", "#fee08b", "#ffffbf",
                              "#d9ef8b", "#a6d96a","#66bd63",
                              "#1a9850", "#006837"), raster::values(prec),
                            na.color = "transparent", reverse = FALSE)
        # plotting the file
        leafletProxy("mymapoutput") %>%
          clearImages()%>%
          clearControls()%>%
          addRasterImage(prec, colors = pal, opacity = 50/100, layerId = "values") %>%
          addLegend(pal = pal, values = raster::values(prec),
                    title = input$var.inp.out, position = "bottomleft")
        
        
        # message
        toastr_success(paste0(input$var.inp.out, " is maped !")) 
        
      },error = function(e) {
        toastr_error(title = "Error mapping", conditionMessage(e))
        
      }
      )
      
      
      
    }# end if there was input selected
    
    
    
    
    
    
  }
  
  
  
  
})

#Toolbar -------------------------------------------------------
# edit scenario  ncatted -O -a scen,scen,o,s,"010,101,111,121,11,12,13" out.nc4
#
# ncap2 -s 'scen=int(6)' 2545_Camp.nc4 out2.nc4
# ncap2 -s 'z_3D[scen, lat,lon]=1' out2.nc4 out3.nc4

#new Campaign ------------------------------------
observeEvent(input$create_newcamp_btn,{
  
  req(input$diroutput)
  
  folder.path <-dir_info()
  
  New_Camp_Maker(filename=file.path(folder.path, input$out_newcamp_nc_name),
                 lenght.x.y=input$pix_len_new_nc_txt,
                 box.camp=c(as.numeric(input$wclip_nc_txt),
                            as.numeric(input$eclip_nc_txt),
                            as.numeric(input$sclip_nc_txt),
                            as.numeric(input$nclip_nc_txt)),
                 scen.len=input$Scen_len_new_nc_txt
                 )
  
  removeModal()
  toastr_info("Done !", title = "Info")
  
  
})


observeEvent(input$create_new_camp_diag_btn,{
  req(input$filesoutput_cell_clicked)
  
  showModal(modalDialog(
    title = "Create a new Campaign",
    fluidRow(
      column(12,
             fluidRow(
               column(3,textInput("wclip_nc_txt","West:","-92")),
               column(3,textInput("eclip_nc_txt","East:","-87")),
               column(3,textInput("sclip_nc_txt","South:","36")),
               column(3,textInput("nclip_nc_txt","North:","42.5"))
             ),br(),
             fluidRow(
               column(6, numericInput("pix_len_new_nc_txt","Pixels in each direction:", 5, min=1, max=100, step = 1)),
               column(6, numericInput("Scen_len_new_nc_txt","Scenario lenght:", 1, min=1, max=100, step = 1))
             ),
             fluidRow(
               column(12, textInput("out_newcamp_nc_name","Output file name:","outfile_new.nc4"))
             ),hr(),
             fluidRow(
               column(3 ),
               column(6, actionButton("create_newcamp_btn", "Create !", class="btn-primary btn-block") ),
               column(3 )
             )
      )
    ),
    easyClose = TRUE,
    footer = NULL
  ))
  
})
#new Variable ---------------------------------
observeEvent(input$add_newvar_campnc_btn,{
  
  req(input$diroutput)
  
  folder.path <-dir_info()
  
    file.full.path <- file.path(folder.path, input$filesoutput_cell_clicked$value)
    #browser()
    clip_opt <- system(paste0("ncap2 -s '", input$out.newvar.camp.var,"[scen, lat,lon]=1' ",
                              file.full.path, " ",
                              file.path(folder.path, input$out.newvar.camp)
    ), intern = TRUE)
    
    removeModal()
    toastr_info("Done !", title = "Info")
 
  
})

observeEvent(input$new_var_camp_diag_btn,{
  req(input$filesoutput_cell_clicked)
  #new variable  ncap2 -s 'var3=(pdate)' Playground.nc Playground4.nc
  
  showModal(modalDialog(
    title = "Make a new variable",
    fluidRow(
      column(12,br(),
             fluidRow(
               column(6, selectizeInput("based_newvar_camp_sel",
                                        tags$b("Based variable:"), 
                                        choices=input$var.inp.out)
                      ),
               column(6, textInput("out.newvar.camp.var",tags$b("Variable name:"),"newvar", width = "100%"))
             ),
             fluidRow(
               column(12,  textInput("out.newvar.camp",tags$b("Output file name:"),"outfile_new.nc", width = "100%"))
             ),
             hr(),
             fluidRow(
               column(3 ),
               column(6, actionButton("add_newvar_campnc_btn", "Save !", class="btn-primary btn-block") ),
               column(3 )
             )
      )
    ),
    easyClose = TRUE,
    footer = NULL
  ))
  
})
#
#Clip------------------------------------------
observeEvent(input$clip_diag_btn,{
 
  showModal(modalDialog(
    title = "Clip my Campign",
    fluidRow(
      column(12,
             fluidRow(
               column(3,textInput("wclip_txt","West:","-92")),
               column(3,textInput("eclip_txt","East:","-87")),
               column(3,textInput("sclip_txt","South:","36")),
               column(3,textInput("nclip_txt","North:","42.5"))
             ),br(),
             fluidRow(
               column(12, textInput("out.f.name","Output file name:","outfile_new.nc"))
             ),hr(),
             fluidRow(
               column(3 ),
               column(6, actionButton("clip_btn", "Clip it !", class="btn-primary btn-block") ),
               column(3 )
             )
      )
    ),
    easyClose = TRUE,
    footer = NULL
  ))
  
})

observeEvent(input$clip_btn,{
  req(input$diroutput)
  
  folder.path <-input$diroutput
  if (class(folder.path) == "list") {
    #browser()
    folder.path <- file.path(home.path, input$diroutput$path %>% paste(collapse = "/"))
    file.full.path <- file.path(folder.path, input$filesoutput_cell_clicked$value)
    #browser()
    clip_opt <- system(paste0('cdo sellonlatbox,',
                      input$wclip_txt,',',
                      input$eclip_txt,',',
                      input$sclip_txt,',',
                      input$nclip_txt,' ',
                      file.full.path, ' ',
                      file.path(folder.path, input$out.f.name)
                  ), intern = TRUE)
    
    removeModal()
    toastr_info("Done !", title = "Info")
  }
  
  #
  
  
})

#save------------------------------------------
observeEvent(input$save_campnc_diag_btn,{
  showModal(modalDialog(
    title = "Edit my Campign",
    fluidRow(
      column(12,br(),
             fluidRow(
               column(12, textInput("out.edit.camp","Output file name:","outfile_new.nc", width = "100%"))
             ),hr(),
             fluidRow(
               column(3 ),
               column(6, actionButton("save_campnc_btn", "Save !", class="btn-primary btn-block") ),
               column(3 )
             )
      )
    ),
    easyClose = TRUE,
    footer = NULL
  ))
})

observeEvent(input$save_campnc_btn,{
  req(input$filesoutput_cell_clicked$value)
  
  folder.path <- file.path(home.path, input$diroutput$path %>% paste(collapse = "/"))
  # browser()
  if(length(input$filesoutput_cell_clicked$value)>0){
    # browser()
    file.full.path <- file.path(folder.path, input$filesoutput_cell_clicked$value)
    # read the edits
    new.mat <- input$table
    new.values <-do.call("c", new.mat$data) %>% 
      unlist()
    #read the file
    file_nc <-easyNCDF::NcReadDims(file.full.path)
    lats <-easyNCDF::nc2a(file.full.path, vars_to_read = "lat") %>% as.numeric()
    lons <-easyNCDF::nc2a(file.full.path, vars_to_read = "lon") %>% as.numeric()
    
   
    grid <-expand.grid(lats,lons)
    # do the real editing using hdfr5 package
   Edit_Campaign(CampaignFile=file.full.path,
                 lat=grid$Var1,
                 lon=grid$Var2,
                 scen.id=rep(input$scen, length(new.values)),
                 var=rep(input$var.inp.out, length(new.values)),
                 val=new.values,
                 OutFile=input$out.edit.camp
                 )
    
    toastr_info("Finished editing the camp file !")
    removeModal()
    }

  
  
  
})
