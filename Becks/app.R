

library(shiny)
library(dplyr)
library(DT)
require(leaflet)
library(leaflet.extras)
library(bslib)
library(shinytoastr)
#%>%
  #dplyr::select(ID=Id, Year=pYear, Crop=Family, lat=Latitude, long=Longitude, YieldM) 

# api_sites <- c(1:3) %>%
#     purrr::map_dfr(function(.y){
#         c(2013:2021) %>%
#     purrr::map_dfr(function(.x){
# 
#         jsonlite::fromJSON(paste0("https://www.beckshybrids.com/YieldDataHandler16.ashx?zipcode=62223&mileRadius=75000&family=",.y,"&product=&year=",
#                                   .x,"&challenge=0&method=getmapmarkers"))
#     })
#     })
# 
# #
# dd <- dd %>%
#     left_join(
#     api_sites %>% mutate(TestPlotId=as.numeric(TestPlotId)),
#     by=c('ID'='TestPlotId')
# ) %>%
#     dplyr::select(-Year, -Address) %>%
#   mutate(Year=as.numeric(YearCode)) %>%
#   select(-YearCode, -Challenge, -Name, -Latitude, -Longitude, -PdfFile)

#
#write.csv(dd, "dd.csv", row.names = FALSE)

# Define UI for application that draws a histogram
ui <- fluidPage(
  useToastr(),
  theme = bs_theme(version = 4, bootswatch = "litera"),

    # Application title
    titlePanel("Beck's Yield Dataset"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(width = 4,
                     fluidRow(
                         column(4,actionButton(
                             "select_all_rows_button",
                             "Select All Table Rows", 
                             width = "100%", 
                             class="btn-primary"
                         )),
                         column(4,actionButton(
                             "clear_rows_button",
                             "Clear Table Selections", 
                             width = "100%", 
                             class="btn-primary"
                         )),
                         column(4,actionButton(
                           "save_button",
                           "Save changes !", 
                           width = "100%", 
                           class="btn-warning"
                         ))
                     ),
                     hr(),
                     DTOutput('my_datatable', width = "100%")

        ),

        # Show a plot of the generated distribution
        mainPanel(
            fluidRow(
                column(
                    width = 12,
                    solidHeader = TRUE,
                    leafletOutput(
                        "my_leaflet",
                        height = "550px",
                        width = "100%"
                    )
                )

            ), uiOutput('slide'),
            fluidRow(
              column(6,
                     fluidRow(
                       column(12, h4("Google map : "),br(), imageOutput("GoogleImg"))
                     )),
              column(
                6,
                fluidRow(
                  column(12,h4("Street View : "), br(), imageOutput("GoogleImg2"))
                )
              )
            ), br()
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  if(file.exists("dd2.csv")){
    unlink('dd.csv')
    file.rename('dd2.csv', 'dd.csv')
    unlink('dd2.csv')
  }
  dd <- readr::read_csv('dd.csv')

    x <- as_tibble(dd)
    quakes_r <- reactive({ as_tibble(dd) })


    output$my_datatable <-  renderDT({

        datatable(quakes_r(), filter = 'top', 
                  options = list(pageLength = 15,
                                 autoWidth = TRUE ),
                  editable = TRUE, rownames = FALSE)

    })


    # create a proxy to modify datatable without recreating it completely
    DT_proxy <- dataTableProxy("my_datatable")


    observeEvent(input$my_datatable_cell_edit, {
      info = input$my_datatable_cell_edit
      str(info)
      i = info$row
      j = info$col +1 
      v = as.numeric(info$value)

      x[i, j] <<- DT::coerceValue(v, x[i, j])
      #replaceData(DT_proxy, x, resetPaging = FALSE, clearSelection="none")  # important
      
  
    })


    # clear row selections when clear_rows_button is clicked
    observeEvent(input$save_button, {

      write.table(x, "dd2.csv", row.names = FALSE, sep = ",")
      toastr_success("Your changes were successfully save !", position = "top-full-width")
    })


    # clear markers from leaflet when clear_rows_button is clicked
    observeEvent(input$clear_rows_button, {
      selectRows(DT_proxy, NULL)
      clearMarkers(leafletProxy("my_leaflet", session))
    })

    # select all rows when select_all_rows_button is clicked
    observeEvent(input$select_all_rows_button, {
        selectRows(DT_proxy, input$my_datatable_rows_all)
    })

    output$my_leaflet <- renderLeaflet({

        leaflet() %>%
            addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
            addProviderTiles(group = "USGS.USImageryTopo", providers$Esri.WorldImagery,
                             options = providerTileOptions(noWrap = TRUE,opacity = 0.95)
            ) %>%
            addProviderTiles(providers$Stamen.TonerLines,
                             options = providerTileOptions(opacity = 0.95), group = "USGS.USImageryTopo") %>%
            addProviderTiles(providers$Stamen.TonerLabels,group = "USGS.USImageryTopo",
                             options = providerTileOptions(opacity = 0.25))%>%
            setView(
                lat = 40,
                lng = -90,
                zoom = 6
            )%>%
            addResetMapButton() %>%
            addFullscreenControl() %>%
            addMeasure(primaryLengthUnit="meters", position = "bottomleft")%>%
            leafem::addMouseCoordinates	() %>%
            addLayersControl(
                baseGroups = c("USGS.USImageryTopo", "Toner Lite"),
                options = layersControlOptions(collapsed = FALSE)
            )

    })

    observeEvent(input$my_datatable_rows_selected, {

      output$slide <- renderUI({

        sliderInput("headslid",min = 0, max = 365, step = 1, value = 50, label = "Heading:", width = "100%")
      })

        selected_lats <- eventReactive(input$my_datatable_rows_selected, {
            as.list(quakes_r()$lat[c(unique(input$my_datatable_rows_selected))])
        })

        selected_longs <- eventReactive(input$my_datatable_rows_selected, {
            as.list(quakes_r()$long[c(unique(input$my_datatable_rows_selected))])
        })

        selected_crop <- eventReactive(input$my_datatable_rows_selected, {
            as.list(quakes_r()$Crop[c(unique(input$my_datatable_rows_selected))])
        })

        selected_yield <- eventReactive(input$my_datatable_rows_selected, {
            as.list(quakes_r()$YieldM[c(unique(input$my_datatable_rows_selected))])
        })

        selected_ID <- eventReactive(input$my_datatable_rows_selected, {
            as.list(quakes_r()$ID[c(unique(input$my_datatable_rows_selected))])
        })
        selected_year <- eventReactive(input$my_datatable_rows_selected, {
            as.list(quakes_r()$Year[c(unique(input$my_datatable_rows_selected))])
        })
        # this is the data that will be passed to the leaflet in the addCircleMarkers argument,
        # as well as the popups when the points are hovered over
        map_df <- reactive({
            tibble(lat = unlist(selected_lats()),
                   lng = unlist(selected_longs()),
                   crop=unlist(selected_crop()),
                   yield=unlist(selected_yield()),
                   ID=unlist(selected_ID()),
                   Year=unlist(selected_year())
                   )
        })
        tmpd<-tempdir()

        width  <- session$clientData$output_GoogleImg_width
        height <- session$clientData$output_GoogleImg_height

        #--------------------------------------------------------------------------------------------------------------------
        #------------------------------------------------------- Google ------------------------------------------------------
        #--------------------------------------------------------------------------------------------------------------------
        output$GoogleImg <- renderImage({
          # Making google static image plots
              unlink(paste0(tmpd,"/",map_df()$ID[nrow(map_df())],"1.png"))
              download.file(paste0("https://maps.googleapis.com/maps/api/staticmap?sensor=false&size=768x1024&maptype=hybrid&zoom=17&center=",
                                   map_df()$lat[nrow(map_df())],",",map_df()$lng[nrow(map_df())],"&visible=",
                                   map_df()$lat[nrow(map_df())],",",map_df()$lng[nrow(map_df())],"&markers=color:red%7Clabel:S%7C",
                                   map_df()$lat[nrow(map_df())],",",map_df()$lng[nrow(map_df())],"&key=key"),
                            paste0(tmpd,"/",map_df()$ID[nrow(map_df())],"1.png"))


              # Return a list containing the filename
              list(src = paste0(tmpd,"/",map_df()$ID[nrow(map_df())],"1.png"),
                   contentType = 'image/png',
                   width = width,
                   height = height,
                   alt = "This is alternate text")

        }, deleteFile = F)


        output$GoogleImg2 <- renderImage({
         
          # Making google static image plots
          unlink(paste0(tmpd,"/",map_df()$ID[nrow(map_df())],"2.png"))

          download.file(paste0("https://maps.googleapis.com/maps/api/streetview?size=600x300&location=",
                               map_df()$lat[nrow(map_df())],",",map_df()$lng[nrow(map_df())],"&heading=",input$headslid,"&pitch=-0.76&radius=200&key=key"),
                        paste0(tmpd,"/",map_df()$ID[nrow(map_df())],"2.png"))


          # Return a list containing the filename
          list(src = paste0(tmpd,"/",map_df()$ID[nrow(map_df())],"2.png"),
               contentType = 'image/png',
               width = width,
               height = height,
               alt = "This is alternate text")

        }, deleteFile = F)

        #--------------------------------------------------------------------------------------------------------------------
        #------------------------------------------------------- Leaflet proxy ----------------------------------------------
        #--------------------------------------------------------------------------------------------------------------------
        factpal <- colorFactor(RColorBrewer::brewer.pal(5,"PiYG"), as.factor(map_df()$crop))

        leafletProxy("my_leaflet", session) %>%
            clearMarkers() %>%
            clearControls() %>%
            addCircleMarkers(
                data = map_df(),
                lng = ~lng,
                lat = ~lat,
                fillColor = ~factpal(map_df()$crop),
                stroke = TRUE,
                color = ~factpal(map_df()$crop),
                radius = 5,
                weight = 1,
                fillOpacity = 0.74,
                popup = paste0("<b>",map_df()$ID,"<b> <br>",
                                "lat: ", map_df()$lat, "<br>",
                               "lng: ", map_df()$lng, "<br>",
                               "Crop: ", map_df()$crop, "<br>",
                               "Year: ", map_df()$Year, "<br>",
                               "Yield (kg/ha): ",map_df()$yield * 62.77,"<br>",
                               "<a href=https://www.beckshybrids.com/Portals/0/SiteContent/YieldData/",map_df()$Year,"/",map_df()$ID,".pdf  target='_blank'> PDF file </a>"
                               )
            ) %>%
            addLegend(pal = factpal, values = as.factor(map_df()$crop), opacity = 1)

    })


}

# Run the application
shinyApp(ui = ui, server = server)
