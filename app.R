#integrated with Shiny
library(shiny)
library(leaflet)
library(ggplot2)
library(dplyr)
library(tibble)
library(rgdal)
library(sp)


mabpld_sp <- readOGR(dsn ="data/mabpld_sp","mabpld_sp")
cleantable <- read.csv("data/cleantable.csv", sep = ",", header=TRUE)
top10 <- read.csv("data/top10.csv", sep = ",", header=TRUE)
countysum <- read.csv("data/countysum.csv", sep = ",", header=TRUE)


# Choices for drop-downs
vars <- c(
  "Total Foreign-Born Population" = "Total",
  "China" = "CN_pct",
  "Dominican Republic" = "DO_pct",
  "Brazil" = "BR_pct",
  "India" = "IN_pct",
  "Haiti" = "HT_pct",
  "El Salvado" = "SV_pct",
  "Portugal" = "PT_pct",
  "Vietnam" = "VT_pct",
  "Guatemala" = "GT_pct",
  "Canada" = "CA_pct"
)

ui <- fluidPage(
  navbarPage("Foreign Born in Massachusetts", id="nav", 
    tabPanel("Interactive map",
      div(class="outer",
          tags$head(
            # Include our custom CSS
            includeCSS("styles.css"),
          ),
      leafletOutput("map", width="100%", height="100%"),
      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE, 
                    draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto", width = 400, height = "auto",
                    
                    h3("TOP 10 COUNTRIES"),
                    
                    selectInput(inputId = "countryselected", label = "Select Country", choices = vars, selected = "Total"),
                    
                    plotOutput("barplot", height = 300),
                    plotOutput("countyplot", height = 350)
             )
            )
          ),
    
    # Data Explorer 
    tabPanel("Data explorer", 
      fluidRow(
        column(3, selectInput("states", "States", c("Massachusetts"=""))),
        column(3, selectInput(inputId = "counties", label = "Counties", choices = c("All counties"=""), multiple=TRUE)),
        column(3,selectInput(inputId = "subcounties", label = "County Subdivisions", 
                             choices = c("All County Subdivisions"=""), multiple=TRUE))
      ),
      hr(),
      DT::dataTableOutput("bpldtable")
      ),
      conditionalPanel("false", icon("crosshair"))
  ))

server <- function(input, output, session) {
  output$map <- renderLeaflet({
    
    #create leaflet
    leaflet(mabpld_sp) %>%
      addTiles() %>% 
      setView(lng = -71.38, lat = 42.15, zoom = 9)
  })
  
  output$barplot <- renderPlot({
    ggplot(data = top10, aes(x = reorder(Country, Population), y = Population)) + 
      geom_bar(stat = "identity", fill = "seagreen", width = 0.3) + 
      coord_flip() + 
      theme(axis.title.y = element_blank(), 
            axis.text.y = element_text(color = "grey20", size = 12), 
            axis.text.x = element_text(color = "grey20", size = 12)) +
      ggtitle("Top 10 Country of Birth \nAmong Foreign Born in Massachusetts")
  })
  output$countyplot <- renderPlot({
    options(scipen = 999)
    ggplot(data = countysum, aes(x = reorder(County, Population), y = Population)) + 
      geom_bar(stat = "identity", fill = "seagreen", width = 0.3) + 
      coord_flip() +
      theme(axis.title.y = element_blank(), 
            axis.text.y = element_text(color = "grey20", size = 12), 
            axis.text.x = element_text(color = "grey20", size = 12))+
      ggtitle("Foreign Born Population \nin Massachusetts Counties")
  })
  
  observe({
    countryBy <- input$countryselected
    countryData <- mabpld_sp[[countryBy]]
    pal <- colorBin(palette = "YlOrRd", domain = countryData, bins=8, reverse = FALSE) 
    
    leafletProxy("map", data = mabpld_sp) %>%
      addPolygons(layerId =~GISJOIN, color = "#444444", weight = 1, smoothFactor = 0.5, 
                  opacity = 1.0, fillOpacity = 0.5, fillColor = ~pal(countryData),
                  highlightOptions = highlightOptions(color = "Black", weight = 2, bringToFront = TRUE)) %>%
      addLegend("bottomleft", pal = pal, values = ~countryData, opacity = 0.7, 
                title = countryBy, layerId="Legend")
  })
  
  clickpopup <- function(GISJOIN, lat, lng) {
    selectedpop <- mabpld_sp[mabpld_sp$GISJOIN == GISJOIN,]
    content <- as.character(tagList(
      tags$h4(selectedpop$NAME_E),
      tags$strong(HTML(sprintf("Total Foreign-Born Population: %s", selectedpop$Total))), tags$br(),
      sprintf("China: %s", selectedpop$AJ7OE049), tags$br(),
      sprintf("Dominican Republic: %s", selectedpop$AJ7OE130), tags$br(),
      sprintf("Brazil: %s", selectedpop$AJ7OE151), tags$br(),
      sprintf("India: %s", selectedpop$AJ7OE059), tags$br(),
      sprintf("Haiti: %s", selectedpop$AJ7OE132), tags$br(),
      sprintf("El Salvado: %s", selectedpop$AJ7OE142), tags$br(),
      sprintf("Portugal: %s", selectedpop$AJ7OE024), tags$br(),
      sprintf("Vietnam: %s", selectedpop$AJ7OE076), tags$br(),
      sprintf("Guatemala: %s", selectedpop$AJ7OE143), tags$br(),
      sprintf("Canada: %s", selectedpop$AJ7OE161), tags$br()
    )
    )
    leafletProxy("map") %>% addPopups(lng, lat, content, layerId = GISJOIN)
  }
  
  # When map is clicked, show a popup with countysub info. clickmap only returns ID, LAT, LNG, so we need to give clickpop content.
  observe({
    leafletProxy("map") %>% clearPopups()
    event <- input$map_shape_click
    if (is.null(event))
      return()
    
    isolate({
      clickpopup(event$id, event$lat, event$lng)
    })
  })
  
  ## Data Explorer ###########################################
  observe({
    counties <- cleantable$County %>%
        unique() %>%
        sort()
    stillSelected <- isolate(input$counties[input$counties %in% counties])
    updateSelectizeInput(session, "counties", choices = counties,
                         selected = stillSelected, server = TRUE)
  })
  
  observe({
    subcounties <- if (is.null(input$counties)) character(0) else {
      dplyr::filter(cleantable, County %in% input$counties) %>%
        `$`('CountySubdivision') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$subcounties[input$subcounties %in% subcounties])
    updateSelectizeInput(session, "subcounties", choices = subcounties,
                         selected = stillSelected, server = TRUE)
  })
  
  output$bpldtable <- DT::renderDataTable({
    df <- cleantable %>%
      dplyr::filter(
        is.null(input$counties) | County %in% input$counties,
        is.null(input$subcounties) | CountySubdivision %in% input$subcounties
      ) 
  })
}

shinyApp(ui, server)