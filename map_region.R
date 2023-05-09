create_psrc_map <- function(lyr,
                            lyr_data_field,
                            legend_title, 
                            legend_subtitle,
                            psrc_col_pal,
                            map_lat=47.615, map_lon=-122.257, 
                            map_zoom=8.5, wgs84=4326){
  
  # psrc colors need more contrast to work
  pal <- leaflet::colorNumeric(palette=psrc_col_pal, domain = lyr_datafield)
  
  #pal<-leaflet::colorNumeric(palette="Blues", domain = lyr_datafield)
  
  labels <- paste0('Estimate: ', prettyNum(round(lyr_datafield, -1), big.mark = ",")) %>%
    lapply(htmltools::HTML)
  
  m <- leaflet::leaflet() %>%
    leaflet::addMapPane(name = "polygons", zIndex = 410) %>%
    leaflet::addMapPane(name = "maplabels", zIndex = 500) %>% # higher zIndex rendered on top
    
    leaflet::addProviderTiles("CartoDB.VoyagerNoLabels") %>%
    leaflet::addProviderTiles("CartoDB.VoyagerOnlyLabels",
                              options = leaflet::leafletOptions(pane = "maplabels"),
                              group = "Labels") %>%
    
    leaflet::addEasyButton(leaflet::easyButton(icon="fa-globe",
                                               title="Region",
                                               onClick=leaflet::JS("function(btn, map){map.setView([47.615,-122.257],8.5); }"))) %>%
    leaflet::addPolygons(data=lyr,
                         fillOpacity = 0.7,
                         fillColor = pal(lyr_datafield),
                         weight = 0.7,
                         color = "#BCBEC0",
                         group="estimate",
                         opacity = 0,
                         stroke=FALSE,
                         options = leaflet::leafletOptions(pane = "polygons"),
                         dashArray = "",
                         highlight = leaflet::highlightOptions(
                           weight =5,
                           color = "76787A",
                           dashArray ="",
                           fillOpacity = 0.7,
                           bringToFront = TRUE),
                         label = labels,
                         labelOptions = leaflet::labelOptions(
                           style = list("font-weight" = "normal", padding = "3px 8px", "font-family" ="Poppins"))) %>%
    
    leaflet::addLegend(pal = pal,
                       values = lyr_datafield,
                       position = "bottomright",
                       title = paste(legend_title, '<br>', legend_subtitle)) %>%
    
    leaflet::addLayersControl(baseGroups = "CartoDB.VoyagerNoLabels",
                              overlayGroups = c("Labels", "estimate")) %>%
    
    leaflet::setView(lng=map_lon, lat=map_lat, zoom=map_zoom)
  
  return(m)
  
}