library(shiny)
library(leaflet)
library(leaflet.extras)
library(DT)
library(shinytoastr)
options(shiny.trace = FALSE)
# Define UI for application that draws a histogram
ui <- fluidPage(
  useToastr(),
    # Application title
    titlePanel("Create Polygons for Farms based on Lat/Lon"),
    # Sidebar with a slider input for number of bins 
    # Show a plot of the generated distribution
    mainPanel(width = 9,
              leafletOutput('mymap', height = "800px")
    ),
    sidebarPanel(width = 3,
                 fluidRow(
                   column(12, textInput('latlon','Lat/Lon','-88,41')),
                   column(12, textInput('idtxt','SiteID','')),
                   column(12,br(),actionButton("addbtn","Add", width = "100%")),
                   column(12,br(),actionButton("viewbtn","View", width = "100%")),
                   column(12, br(), downloadButton("savebtn", "Download !", style = "width:100%;"))
                 ),br(),
                 fluidRow(
                   column(12, DT::dataTableOutput("saveddata_tbl"))
                 ))
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
  savedpol <- reactiveValues(data=NULL)
  
  observe({
    savedpol
    output$saveddata_tbl <- renderDT({
      savedpol$data[,c(1,2)]
    })
    
  })

  
  
  output$savebtn <- downloadHandler(
    filename = function() {
      paste0(gsub(':','_',gsub(' ','_',as.character(Sys.time()))),".RDS")
    },
    content = function(file) {
      dat <- isolate(savedpol$data)
      saveRDS( dat, file = file)
      toastr_success("Saved to the disk !")
    }
  )

  output$mymap <- renderLeaflet({
                  leaflet() %>%
              addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
              addProviderTiles(group = "USGS.USImageryTopo", providers$Esri.WorldImagery,
                               options = providerTileOptions(noWrap = TRUE)
              ) %>%
              addResetMapButton() %>%
              addDrawToolbar(singleFeature=TRUE) %>%
              addFullscreenControl() %>%
              addMeasure(primaryLengthUnit="meters", position = "bottomleft")%>%
              leafem::addMouseCoordinates	() %>%
              addLayersControl(
                baseGroups = c("USGS.USImageryTopo", "Toner Lite"),
                options = layersControlOptions(collapsed = FALSE)
              )
          
  })
    

  observe({
        req(input$latlon)
        latlonvec <- strsplit(input$latlon,",")[[1]]
        if(length(latlonvec)!=2) return(NULL)
        
        points <- cbind(as.numeric(latlonvec[1]),as.numeric(latlonvec[2]))
        
 
        proxy <- leafletProxy("mymap")
        proxy %>% 
            clearMarkers() %>%
            setView(as.numeric(latlonvec[1])+0.005, as.numeric(latlonvec[2])-0.005,16)%>%
            addMarkers(data=points)
        
    })
    
  observeEvent(input$viewbtn,{
    
    
    
    showModal(modalDialog(
      title = "Polygons",
      DT::DTOutput('siteinfo'),
      size='l'
    ))
    
    output$siteinfo <- renderDT({
      savedpol$data[,c(1,2)] %>%
        dplyr::mutate(GEE=paste0('<textarea id="w3review" name="w3review" rows="4" cols="100%">ee.Geometry.Polygon(',GEE,')
</textarea>'))
    }, escape = FALSE)
    
  })
    
  observeEvent(input$addbtn,{
      
      olddata <- isolate(savedpol$data)
      if(input$idtxt==""){
        toastr_warning("Please add the ID and try again !", 
                       position = "top-center")
        return(NULL)
      }
        
      val <- input$mymap_draw_new_feature$geometry$coordinates[[1]] %>% 
          purrr::map_chr( ~.x %>% unlist %>% paste0(collapse = " , ") %>%
                              {paste0("[",.,"]")}
          ) %>%
          paste(collapse = ",") %>%
        {paste0("[[",.,"]]")}

   
        savedpol$data <- rbind(data.frame(ID=input$idtxt, 
                                          GEE=val,
                                          json=as.character(geojsonio::as.json(input$mymap_draw_new_feature))),
                               olddata)
        
    })

}

# Run the application 
shinyApp(ui = ui, server = server)
