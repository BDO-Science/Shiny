library(shiny)
library(shinydashboard)
library(dataRetrieval)
library(tidyverse)
library(leaflet)

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Central Valley Waterways Data"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Water Quality", tabName = "water_quality", icon = icon("tint"))
    ),
    selectInput("waterway", "Select Waterway:", 
                choices = list(
                  "Sacramento River" = list(
                    "Sacramento River at Keswick" = "11370500",
                    "Sacramento River above Bend" = "11377100",
                    "Sacramento River at Colusa" = "11389500",
                    "Sacramento River below Wilkins" = "11390500",
                    "Sacramento River at Verona" = "11425500",
                    "Sacramento River at Freeport" = "11447650",
                    "Sacramento River at Freeport (Delta Start)" = "11447890",
                    "Sacramento River above Delta Cross Channel" = "11447905",
                    "Sacramento River above Walnut Grove Bridge" = "381436121304901",
                    "Sacramento River below Walnut Grove Bridge" = "381427121305401",
                    "Sacramento River below Georgiana Slough" = "11447905",
                    "Sacramento River at Rio Vista" = "11455420",
                    "Sacramento River below Toland Landing" = "11455485",
                    "Sacramento River at Channel Marker 10" = "11455495",
                    "Sacramento River at Channel Marker 5" = "11455498"
                  ),
                  "East Side Sacramento Tributaries" = list(
                    # East Side Sacramento Tributaries from North to South
                    "Cow C near Millville CA" = "11374000",
                    "NF Battle C below N Battle C Dam near Manzanita Lake CA" = "11376015",
                    "NF Battle C below Div to Al Smith CN near Manton CA" = "11376040",
                    "NF Battle C below Div to XCountry CN near Manton CA" = "11376140",
                    "SF Battle C below Div to Coleman CN near Manton CA" = "11376460",
                    "Battle C below Coleman Fish Hatchery near Cottonwood CA" = "11376550",
                    "Mill C near Los Molinos CA" = "11381500",
                    "Deer C near Vina CA" = "11383500",
                    "Butte C below Div Dam near Stirling City CA" = "11389720",
                    "Butte C below Fks of Butte Div Dam near De Sabla CA" = "11389740",
                    "Toadtown Canal above Butte Canal near Stirling City CA" = "11389800",
                    "Butte C below Centerville Div Dam near Paradise CA" = "11389780",
                    "Butte C near Chico CA" = "11390000",
                    "Palermo CN at Oroville Dam CA" = "11406810",
                    "Feather R at Oroville CA" = "11407000",
                    "Sutter Butte CN at Intake near Oroville CA" = "11406910",
                    "Thermalito Afterbay Rel to Feather R near Oroville CA" = "11406920",
                    "Yuba R below Englebright Dam near Smartsville CA" = "11418000",
                    "Yuba R near Marysville CA" = "11421000",
                    "Bear R near Wheatland CA" = "11424000",
                    "American R below Folsom Dam near Folsom CA" = "11446220",
                    "American R at Fair Oaks CA" = "11446500",
                    "American R at William B Pond Park at Carmichael CA" = "11446700",
                    "American R below Watt Ave Brdg near Carmichael CA" = "11446980"
                  ),
                  
                  "West Side Sacramento Tributaries" = list(
                    "Clear C near Igo CA" = "11372000",
                    "Cottonwood C near Cottonwood CA" = "11376000",
                    "Elder C near Paskenta CA" = "11379500",
                    "Cache C at Rumsey CA" = "11451800",
                    "Cache C at Yolo CA" = "11452500",
                    "Cache C Total Flow from Settling Bas near Woodland CA" = "11452901",
                    "Cache C Outflow from Settling Basin near Woodland CA" = "11452900",
                    "Yolo Bypass near Woodland CA" = "11453000",
                    "Putah C near Winters CA" = "11454000",
                    "Putah South CN near Winters CA" = "11454210",
                    "Willow Slough Bypass near Davis CA" = "383525121434601"
                  ),
                  "San Joaquin River" = list(
                    # Added New Sites Upstream to Downstream
                    "San Joaquin R Release at Friant Dam CA" = "11250110",
                    "San Joaquin R below Friant CA" = "11251000",
                    "San Joaquin R at Gravelly Ford CN near Kerman CA" = "11253058",
                    "San Joaquin R below Chowchilla CN Intake near Mendota" = "11253115",
                    "San Joaquin R near Mendota CA" = "11254000",
                    "San Joaquin R at Fremont Ford Bridge CA" = "11261500",
                    "San Joaquin R above Merced River near Newman CA" = "11273400",
                    "San Joaquin R near Newman CA" = "11274000",
                    "San Joaquin R near Crows Landing CA" = "11274550",
                    "San Joaquin R near Vernalis CA" = "11303500",
                    "San Joaquin R below Garwood Bridge near Stockton CA" = "11304810",
                    "San Joaquin R at Buckley Cove near Stockton CA" = "375831121223701",
                    "San Joaquin R above Buckley Cove near Stockton CA" = "375841121225601",
                    "San Joaquin R at Prisoners Point near Terminous CA" = "11313460",
                    "San Joaquin R at Channel Marker 42 near Isleton CA" = "11336955",
                    "Twitchell Island DR at Pump Station near Rio Vista CA" = "380548121390501",
                    "San Joaquin R at Jersey Point CA" = "11337190",
                    "San Joaquin R at Channel Marker 18 near Oakley CA" = "380138121441401"
                  ),
                  "San Joaquin Tributaries" = list(
                    # San Joaquin Tributaries from South to North
                    "Merced R below Merced Falls Dam near Snelling CA" = "11270900",
                    "Merced R at Shaffer Bridge near Cressey CA" = "11271290",
                    "Combined Flow Tuolumne R + Modesto CN + Turlock CN" = "11289651",
                    "Tuolumne R below La Grange Dam near La Grange CA" = "11289650",
                    "Tuolumne R at Modesto CA" = "11290000",
                    "Stanislaus R below Tulloch PP near Knights Ferry CA" = "11299997",
                    "Stanislaus R below Goodwin Dam near Knights Ferry CA" = "11302000",
                    "Stanislaus R at Oakdale CA" = "11302500",
                    "Stanislaus R at Ripon CA" = "11303000"
                  ),
                  
                  "Delta Tributaries" = list(
                    "Cosumnes R at Michigan Bar CA" = "11335000",
                    "Mokelumne R at Woodbridge CA" = "11325500"
                  ),
                  
                  "North Delta" = list(
                    "Sacramento R Deep Water Ship Channel near Freeport" = "11455095",
                    "Sacramento R Deep Water Ship Channel near Clarksburg" = "11455136",
                    "Toe Drain at Liberty Island near Courtland CA" = "11455140",
                    "Sacramento R Deep Water Ship Channel near Courtland" = "11455142",
                    "Sacramento R Deep Water Ship Channel Marker 51 CA" = "11455338",
                    "Shag Slough at Liberty Island near Courtland CA" = "11455276",
                    "Ulatis C at Browns Rd near Elmira CA" = "11455261",
                    "Cache Slough at South Liberty Island near Rio Vista CA" = "11455315",
                    "Sutter Slough at Courtland CA" = "11447830",
                    "Steamboat Slough near Walnut Grove CA" = "11447850"
                  ),
                  "Central Delta" = list(
                    "Staten Island near Terminous CA" = "380924121301901",
                    "Staten Island Drain near Terminous CA" = "380711121323401",
                    "Mokelumne R at Andrus Island near Terminous CA" = "11336930",
                    "Little Potato Slough at Terminous CA" = "11336790",
                    "Threemile Slough near Rio Vista CA" = "11337080",
                    "False River near Oakley CA" = "11313440",
                    "Old R at Franks Tract near Terminous CA" = "11313452",
                    "Old R at Quimby Island near Bethel Island CA" = "11313434",
                    "Holland Cut near Bethel Island CA" = "11313431",
                    "Delta RMP CENT-014" = "380347121334001",
                    "Delta RMP CONF-004" = "380118121440701",
                    "Delta RMP CONF-003" = "380146121451401",
                    "Delta RMP CONF-002" = "380333121492001",
                    "Delta RMP CONF-001" = "380228121492901",
                    "Dutch Slough below Jersey Island Rd near Jersey Island CA" = "11313433"
                  ),
                  
                  "South Delta" = list(
                    "Middle R near Holt CA" = "11312685",
                    "Middle R at Middle River CA" = "11312676",
                    "Old R at Bacon Island CA" = "11313405",
                    "Delta RMP CENT-013" = "375633121333301",
                    "Old R near Byron CA" = "11313315",
                    "Victoria Canal near Byron CA" = "11312672",
                    "Grant Line Canal near Tracy CA" = "11313240",
                    "Old R near Delta Mendota Canal CA" = "11312968",
                    "Delta-Mendota Canal at Tracy PP near Tracy CA" = "11313000"
                  ),
                  
                  "West Delta" = list(
                    "San Francisco Bay Water Quality Project Site 3" = "380306121524801",
                    "Suisun Bay at Van Sickle Island near Pittsburg CA" = "11455508",
                    "Combined Delta Outflow at Suisun Bay near Pittsburg CA" = "380245121532301",
                    "Suisun Bay at Mallard Island CA" = "11185185",
                    "Suisun Bay at Channel Marker 24A near Bay Point CA" = "380318121571501",
                    "Suisun Bay at Buoy 19 near Port Chicago CA" = "380337122000301",
                    "San Francisco Bay at Roe Island CA" = "380354122020601",
                    "Suisun Bay at Channel Marker 16 near Port Chicago CA" = "380356122023701",
                    "First Mallard Branch near Fairfield CA" = "381142122015801",
                    "Grizzly Bay at Suisun Slough near Avon CA" = "380631122032201",
                    "Suisun Bay at Benicia Bridge near Benicia CA" = "11455780"
                  ),
                  
                  "Carquinez Strait" = list(
                    "Carquinez Strait at Martinez CA" = "11182450",
                    "San Francisco Bay at Martinez CA" = "380148122090601",
                    "San Francisco Bay Water Quality Project Site 10" = "380336122123001",
                    "Carquinez Strait at Carquinez Bridge near Crockett CA" = "11455820"
                  ),
                  
                  "Napa River" = list(
                    "Napa River near Napa CA" = "11458000"
                  ),
                  
                  "San Francisco Bay" = list(
                    "San Francisco Bay at Richmond-San Rafael Bridge CA" = "375607122264701",
                    "San Francisco Bay at Richmond-San Rafael Bridge Pier 47" = "375603122254401",
                    "San Francisco Bay at NE Shore Alcatraz Island CA" = "374938122251801",
                    "San Francisco Bay at Pier 17 San Francisco CA" = "374811122235001",
                    "San Francisco Bay 5.21 mi N of San Mateo Bridge CA" = "374027122130401",
                    "San Francisco Bay 2.8 mi N of San Mateo Bridge CA" = "373751122143601",
                    "San Francisco Bay 0.57 mi N of San Mateo Bridge CA" = "373642122120401",
                    "SF Bay at San Mateo Bridge near Foster City CA" = "11162765",
                    "San Francisco Bay ST AC7 near Alameda C CA" = "373540122094301",
                    "San Francisco Bay at Bair Island near Foster City CA" = "373336122122501",
                    "San Francisco Bay Site 10 near Union City CA" = "373401122092201",
                    "San Francisco Bay at Greco Island near San Carlos CA" = "373158122102701",
                    "San Francisco Bay at Ravenswood Slough near E Palo Alto" = "373033122092801",
                    "South San Francisco Bay at Dumbarton Bridge CA" = "373015122071000",
                    "San Francisco Bay at Old Dumbarton Bridge near Newark CA" = "373025122065901",
                    "San Francisco Bay ST AC2 near Mowery C CA" = "372930122043401",
                    "South SF Bay at Channel Marker 17 near Palo Alto CA" = "372844122043800",
                    "San Francisco Bay at Hooks Point near E Palo Alto CA" = "372804122053501",
                    "Coyote C near Alviso CA" = "372750122012701"
                  )
                )),
    #selectInput("parameter", "Select Parameter:", 
                #choices = c("Temperature (Water, °C)" = "00010", 
                            #"Discharge (cubic feet per second)" = "00060", 
                            #"Discharge (tidally filtered, cubic feet per second)" = "72401")),
    #uiOutput("temp_column_selector"),  # Dynamically display temperature column options
    dateRangeInput("date_filter", "Filter Dates:", 
                   start = Sys.Date() - 30, end = Sys.Date()),
    actionButton("update_data", "Update Data")
  ),
  dashboardBody(
    fluidRow(
      column(
        width = 6,
        box(title = "Water Temperature Plot", width = NULL, status = "primary", solidHeader = TRUE,
            plotOutput("water_temp_plot"))
      ),
      column(
        width = 6,
        box(title = "Discharge Plot", width = NULL, status = "primary", solidHeader = TRUE,
            plotOutput("discharge_plot"))
      )
    ),
    fluidRow(
      box(title = "Interactive Map", width = 12, status = "primary", solidHeader = TRUE,
          leafletOutput("map"))
    )
  )
)

# Define Server
server <- function(input, output, session) {
  # Fetch NWIS data
  nwis_data <- reactive({
    req(input$update_data)
    site <- input$waterway
    parameters <- c("00010", "00060", "72401")  # Water temperature, regular discharge, tidally filtered discharge
    start_date <- input$date_filter[1]
    end_date <- input$date_filter[2]
    
    readNWISdata(
      sites = site,
      startDate = start_date,
      endDate = end_date,
      parameterCd = parameters,
      service = "uv"  # Unit values (high resolution)
    )
  })
  
  # Plot Water Temperature
  output$water_temp_plot <- renderPlot({
    data <- nwis_data()
    
    temp_col <- grep("00010_00000$", colnames(data), value = TRUE)
    temp_col <- temp_col[!grepl("_cd$", temp_col)]
    
    if (length(temp_col) == 0) {
      showNotification("Water temperature data is not available for this site.", type = "error")
      return(NULL)
    }
    
    ggplot(data, aes(x = dateTime, y = .data[[temp_col[1]]])) +
      geom_line(color = "black") +
      labs(title = NULL, x = NULL, y = "Water Temperature (°C)") +
      theme_minimal()
  })
  
  # Plot Discharge (Regular and Tidally Filtered)
  output$discharge_plot <- renderPlot({
    data <- nwis_data()
    
    discharge_cols <- list(
      regular = grep("00060_00000$", colnames(data), value = TRUE),
      tidally_filtered = grep("72401_00000$", colnames(data), value = TRUE)
    )
    
    discharge_cols$regular <- discharge_cols$regular[!grepl("_cd$", discharge_cols$regular)]
    discharge_cols$tidally_filtered <- discharge_cols$tidally_filtered[!grepl("_cd$", discharge_cols$tidally_filtered)]
    
    if (length(discharge_cols$regular) == 0 && length(discharge_cols$tidally_filtered) == 0) {
      showNotification("Neither regular discharge nor tidally filtered discharge data is available for this site.", type = "error")
      return(NULL)
    }
    
    gg <- ggplot(data)
    
    # Add Regular Discharge Plot if Available
    if (length(discharge_cols$regular) > 0) {
      gg <- gg + geom_line(aes(x = dateTime, y = .data[[discharge_cols$regular[1]]]), 
                           color = "black", linetype = "solid", size = 1, alpha = 0.8, 
                           show.legend = TRUE, inherit.aes = FALSE) +
        labs(color = "Discharge Type")
    }
    
    # Add Tidally Filtered Discharge Plot if Available
    if (length(discharge_cols$tidally_filtered) > 0) {
      gg <- gg + geom_line(aes(x = dateTime, y = .data[[discharge_cols$tidally_filtered[1]]]), 
                           color = "grey", linetype = "dashed", size = 1, alpha = 0.8, 
                           show.legend = TRUE, inherit.aes = FALSE)
    }
    
    gg <- gg +
      labs(title = NULL, x = NULL, y = "Discharge (cfs)") +
      theme_minimal()
    
    gg
  })
  
  # Render Example Map (Without Sites)
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -121.5, lat = 38.5, zoom = 7)  # Centered on Central Valley, California
  })
}

# Run the application
shinyApp(ui = ui, server = server)