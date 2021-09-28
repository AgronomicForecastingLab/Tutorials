#rerendering maps
output$mymap <- renderLeaflet({
  leaflet() %>%
    setView(lng = -91.0589, lat = 42.3601, zoom = 7)%>%
    addProviderTiles(providers$Hydda.Base, group = "Default")%>% 
    addProviderTiles(providers$Esri.WorldImagery, group = "WorldImagery") %>%
    addProviderTiles(providers$Stamen.TonerLines,
                     options = providerTileOptions(opacity = 0.45),
                     group = "Borders") %>%
    addProviderTiles(providers$Stamen.TonerLabels,
                     options = providerTileOptions(opacity = 0.5),
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
                     position = "bottomright")%>%
    hideGroup("Counties")
})

#list of files
observe({
  
  folder.path <-input$dir
  if (class(folder.path) == "list") {
    #browser()
    folder.path <- file.path(home.path, input$dir$path %>% paste(collapse = "/"))
    
    suppressWarnings({

      output$files <- DT::renderDT(
       
        DT::datatable(
          list.files(folder.path, "*.nc", recursive = TRUE) %>%
            as.data.frame() %>%
            `colnames<-`(c("File name")),
          escape = F,
          selection = "single",
          style = 'bootstrap',
          rownames = FALSE,
          options = list(
            search = list(regex = TRUE, caseInsensitive = FALSE, search = 'output'),
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
      
    }) # end suppresss warning

  }
  
})

observe({
  req(input$files_cell_clicked)
  folder.path <-file.path(home.path, input$dir$path %>% paste(collapse = "/"))
  # browser()
  if(length(input$files_cell_clicked$value)>0){
    # browser()
    file.full.path <- file.path(folder.path, input$files_cell_clicked$value)
    
    tryCatch({
      nc.vars <- ncdf4::nc_open(file.full.path)$var %>% names
    },error = function(e) {
      toastr_error(title = "Error", conditionMessage(e))
    })
    
    #browser()
    
    if(exists("nc.vars")) {
      if(is.null(input$var.inp)){
        tryCatch({
          updatePickerInput(session, 'var.inp', choices = nc.vars)
          
          #
        },error = function(e) {
          toastr_error(title = "Error", conditionMessage(e))
        })
      }else if (!(input$var.inp %in% nc.vars)) {
        updatePickerInput(session, 'var.inp', choices = nc.vars)
      }
    }
    
    
    
    #browser()
    #Map the map
    if(!is.null(input$var.inp)){
      # reading it as a raster
      tryCatch({
        #browser()
        #find the start year from unit
        starty <- as.Date(stringr::str_extract(ncdf4::nc_open(file.full.path)$dim$time$units,
                                               "[0-9]{4}-[0-9]{2}-[0-9]{2}"), format="%Y-%m-%d") %>%
          lubridate::year()
        
        # all the years
        all.years <- seq(starty, starty + ncdf4::nc_open(file.full.path)$dim$time$len - 1)
        
        #update slider
        updateSliderInput(session,
                          'time',
                          min=min(all.years),
                          max=max(all.years))
        
        updateSliderInput(session,
                          'scenario_output_slider',
                          min=1,
                          max=ncdf4::nc_open(file.full.path)$dim$scen$len)
        
       #  
        prec <- brick(file.full.path, varname=input$var.inp, level=input$scenario_output_slider)
        
        prec <- prec[[which(all.years==as.numeric(input$time))]]
        
       # browser()
        #out_options
       if("Interpolate" %in% input$out_options) prec <- my_interpolate(prec)
        
        #browser()
        # make the legend
        pal <- colorNumeric(c("#a50026", "#d73027", "#f46d43",
                              "#fdae61", "#fee08b", "#ffffbf",
                              "#d9ef8b", "#a6d96a","#66bd63",
                              "#1a9850", "#006837"), raster::values(prec),
                            na.color = "transparent", reverse = FALSE)
        # plotting the file
        leafletProxy("mymap") %>%
          #clearImages()%>%
          removeControl(layerId = input$files_cell_clicked$value) %>%
          addRasterImage(prec, colors = pal, opacity = as.numeric(input$opacity)/100,
                         layerId = paste0(input$files_cell_clicked$value,"raster")) %>%
          addLegend(pal = pal, values = raster::values(prec),
                    title = input$files_cell_clicked$value, position = "bottomleft",
                    group = input$files_cell_clicked$value)%>%
          addLayersControl(
            #baseGroups = c('Default','WorldImagery'),
            overlayGroups = c(isolate({input$mymap_groups}),input$files_cell_clicked$value),
            options = layersControlOptions(collapsed=TRUE),
            position = "bottomright"
          ) 
        
        #meta data
        output$nc_meta <- renderJsonedit({
          jsonedit(prec[[1]], mode='view')
        })
        # message
        toastr_success(paste0(input$var.inp, " is maped !")) 
        
      },error = function(e) {
        toastr_error(title = "Error", conditionMessage(e))
      }
      )
      
      
      
    }# end if there was input selected
    
    
    
    
    
    
  }
  
  
  
  
})

observeEvent(input$meta_btn,{
  req(input$files_cell_clicked)
  showModal(modalDialog(
    title = "Meta-data",
    jsoneditOutput("nc_meta", height = "600px"),
    easyClose = TRUE,
    footer = NULL
  ))
})


observeEvent(input$clear_map_btn,{
  leafletProxy("mymap") %>%
    clearImages()%>%
    clearControls()
})