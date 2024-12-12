# Parameters
siteNumber <- "11303500" #"11447650"  # Sacramento River
parameterCd <- "00010"  # Tidally Filtered Discharge
start_date <- "2023-12-07"
end_date <- "2024-12-06"

# Fetch data
data <- readNWISdata(
  sites = siteNumber,
  startDate = start_date,
  endDate = end_date,
  parameterCd = parameterCd,
  service = "uv"
)

# Inspect data structure
str(data)
head(data)

# Identify the correct column
temp_col <- grep("_00010_00000", colnames(data), value = TRUE)
print(temp_col)



# Render Interactive Map
output$map <- renderLeaflet({
  selected_site <- site_coords[site_coords$site_no == input$waterway, ]
  
  if (nrow(selected_site) == 0) {
    showNotification("Site coordinates not available for this site.", type = "error")
    return(NULL)
  }
  
  leaflet() %>%
    addTiles() %>%
    setView(lng = selected_site$lon, lat = selected_site$lat, zoom = 10) %>%
    addMarkers(lng = selected_site$lon, lat = selected_site$lat, popup = selected_site$site_name)
})
}
