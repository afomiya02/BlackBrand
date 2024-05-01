source("code/sodem.R")

#Family Dynamic ---------------------------------------------------------
generateMap <- function(data, title, labelSuffix = "%") {
    pal <- colorBin(palette = continuous_pal, domain = data$Percent)
    data %>% 
        leaflet(options = leafletOptions(minZoom = 5, maxZoom = 15, drag = FALSE)) %>%
        addTiles() %>%
        addPolygons(
            fillColor = ~ pal(Percent), 
            color = "black",
            weight = 1,
            fillOpacity = 0.75,
            smoothFactor = 0.5,
            opacity = 1.0,
            highlightOptions = highlightOptions(
                bringToFront = TRUE, 
                color = "white",
                weight = 2),
            label = ~ paste0(NAME, " - ", title, ": ", Percent, labelSuffix)) %>%
        addLegend(
            "bottomright",
            pal = pal,
            values = ~ Percent,
            title = title,
            labFormat = labelFormat(suffix = labelSuffix),
            opacity = 1
        )
}