output$util.map <- renderLeaflet({
  
  
  nycounties <- sf::read_sf("psims-grid.geojson") %>% sf::as_Spatial()
  
  
  leaflet(nycounties) %>%
    setView(lng = -91.0589, lat = 42.3601, zoom = 4)%>%
    addProviderTiles(providers$Esri.WorldImagery, group = "Toner") %>%
    addProviderTiles(providers$Stamen.TonerLines,
                     options = providerTileOptions(opacity = 0.65)) %>%
    addProviderTiles(providers$Stamen.TonerLabels,
                     options = providerTileOptions(opacity = 0.65)) %>%
    addScaleBar(position = "bottomleft") %>%
    addMeasure(position = "bottomleft")%>% 
    addFullscreenControl() %>%
    addMouseCoordinates() %>%
    addPolygons(stroke = TRUE, fillOpacity = 0.05,
                weight = 1.5, color = "#e1e7f0",label =~name)
  
  
})