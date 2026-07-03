# app.r - Shiny app for interactive ancestry census map

library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(tidyr)
library(RColorBrewer)
library(ggplot2)

# Load geographic data
states <- readRDS("data/states.rds")
counties <- readRDS("data/counties.rds")

# Convert to sf if not already
if (!inherits(states, "sf")) {
  states <- st_as_sf(states)
}
if (!inherits(counties, "sf")) {
  counties <- st_as_sf(counties)
}

# Filter counties for Illinois
illinois_counties <- counties %>%
  filter(STATEFP == "17")  # FIPS code for Illinois

# Create sample ancestry data for Illinois counties
illinois_ancestry_data <- illinois_counties %>%
  st_drop_geometry() %>%
  mutate(
    german = runif(n(), 8, 20),
    irish = runif(n(), 4, 16),
    english = runif(n(), 6, 18),
    italian = runif(n(), 2, 10),
    polish = runif(n(), 1, 12),
    french = runif(n(), 1, 5),
    mexican = runif(n(), 1, 30),
    chinese = runif(n(), 0.2, 3),
    african_american = runif(n(), 5, 40),
    asian = runif(n(), 1, 8)
  ) %>%
  select(GEOID, german, irish, english, italian, polish, french, mexican, chinese, african_american, asian)

# Join Illinois county data with geography
illinois_data <- illinois_counties %>%
  left_join(illinois_ancestry_data, by = "GEOID")
set.seed(42)
ancestry_data <- states %>%
  st_drop_geometry() %>%
  mutate(
    german = runif(n(), 10, 25),
    irish = runif(n(), 5, 18),
    english = runif(n(), 8, 20),
    italian = runif(n(), 2, 12),
    polish = runif(n(), 1, 8),
    french = runif(n(), 2, 6),
    mexican = runif(n(), 2, 35),
    chinese = runif(n(), 0.5, 4),
    african_american = runif(n(), 2, 40),
    asian = runif(n(), 1, 10)
  ) %>%
  select(GEOID, german, irish, english, italian, polish, french, mexican, chinese, african_american, asian)

# Join data with geography
states_data <- states %>%
  left_join(ancestry_data, by = "GEOID")

# Pre-compute rankings data for faster loading
top_ancestry_by_state <- states_data %>%
  st_drop_geometry() %>%
  select(NAME, german, irish, english, italian, polish, french, mexican, chinese, african_american, asian) %>%
  pivot_longer(cols = -NAME, names_to = "ancestry", values_to = "percentage") %>%
  arrange(NAME, desc(percentage)) %>%
  group_by(NAME) %>%
  slice(1) %>%
  ungroup() %>%
  arrange(desc(percentage)) %>%
  mutate(ancestry = gsub("_", " ", tools::toTitleCase(ancestry)))

# Pre-compute diversity scores
diversity_scores <- states_data %>%
  st_drop_geometry() %>%
  select(NAME, german, irish, english, italian, polish, french, mexican, chinese, african_american, asian) %>%
  pivot_longer(cols = -NAME, names_to = "ancestry", values_to = "percentage") %>%
  group_by(NAME) %>%
  summarise(
    top_percentage = max(percentage),
    diversity_index = 100 - max(percentage),
    .groups = 'drop'
  ) %>%
  arrange(diversity_index) %>%
  head(15) %>%
  mutate(
    top_percentage = round(top_percentage, 1),
    diversity_index = round(diversity_index, 1)
  )
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body {
        font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
        margin: 0;
        padding: 0;
      }
      .header {
        background-color: #1a1a1a;
        color: white;
        padding: 20px;
        text-align: center;
      }
      .header h1 {
        margin: 0;
        font-size: 32px;
      }
      .header p {
        margin: 5px 0 0 0;
        font-size: 14px;
        color: #999;
      }
      .controls {
        background-color: #f5f5f5;
        padding: 20px;
        border-bottom: 1px solid #ddd;
      }
      #map {
        width: 100%;
        height: 700px;
      }
      .legend-title {
        font-weight: bold;
        margin-bottom: 10px;
      }
      .info-box {
        padding: 20px;
        background-color: #f9f9f9;
        border-top: 1px solid #ddd;
        text-align: center;
      }
    "))
  ),
  
  # Header
  div(class = "header",
    h1("US Ancestry Census Map"),
    p("Interactive visualization of ancestry distribution across US counties")
  ),
  
  # Controls
  div(class = "controls",
    fluidRow(
      column(6,
        selectInput("ancestry", 
          label = "Select Ancestry Group:",
          choices = c(
            "German" = "german",
            "Irish" = "irish",
            "English" = "english",
            "Italian" = "italian",
            "Polish" = "polish",
            "French" = "french",
            "Mexican" = "mexican",
            "Chinese" = "chinese",
            "African American" = "african_american",
            "Asian" = "asian"
          ),
          selected = "german"
        )
      ),
      column(6,
        p("Hover over states to see detailed information. Darker colors indicate higher percentages of the selected ancestry group.")
      )
    )
  ),
  
  # Tabset Panel
  tabsetPanel(
    # Tab 1: Interactive Map
    tabPanel("Interactive Map",
      leafletOutput("map")
    ),
    # Tab 2: All Ancestries Grid
    tabPanel("All Ancestries",
      fluidRow(
        column(12,
          p("Comparison of all ancestry groups across the United States")
        )
      ),
      plotOutput("ancestriesGridPlot", height = 800)
    ),
    # Tab 3: Illinois County Map
    tabPanel("Illinois Map",
      fluidRow(
        column(6,
          selectInput("ancestryIL", 
            label = "Select Ancestry Group:",
            choices = c(
              "German" = "german",
              "Irish" = "irish",
              "English" = "english",
              "Italian" = "italian",
              "Polish" = "polish",
              "French" = "french",
              "Mexican" = "mexican",
              "Chinese" = "chinese",
              "African American" = "african_american",
              "Asian" = "asian"
            ),
            selected = "german"
          )
        ),
        column(6,
          p("Illinois ancestry distribution by county. Darker colors indicate higher percentages.")
        )
      ),
      leafletOutput("illinoisMap")
    ),
    # Tab 4: Rankings
    tabPanel("Rankings by State",
      fluidRow(
        column(12,
          h3("Top Ancestry Group by State"),
          tableOutput("topAncestryTable")
        )
      ),
      br(),
      fluidRow(
        column(12,
          h3("Top Ancestry Distribution by State"),
          plotOutput("topAncestryPlot", height = 500)
        )
      ),
      br(),
      fluidRow(
        column(12,
          h3("Most Diverse States (Lowest Dominance)"),
          tableOutput("diversityTable")
        )
      )
    )
  ),
  
  # Info box
  div(class = "info-box",
    p("Data is sample data for demonstration. Connect Census API for real 2021 Census data by state."),
    p("Values represent estimated percentage of population belonging to each ancestry group.")
  )
)

# Define server
server <- function(input, output, session) {
  
  # Create a reactive timer for flashing effect (every 200ms)
  flash_timer <- reactiveTimer(200)
  
  # Reactive expression for flash color - cycles through rainbow colors
  flash_color <- reactive({
    flash_timer()
    colors <- c("#FF0000", "#FF7F00", "#FFFF00", "#00FF00", "#0000FF", "#4B0082", "#9400D3")
    colors[((as.numeric(Sys.time()) * 5) %% length(colors)) + 1]
  })
  
  # Reactive expression for selected ancestry data
  selected_ancestry <- reactive({
    states_data %>%
      mutate(value = get(input$ancestry))
  })
  
  # Create color palette
  color_palette <- reactive({
    data <- selected_ancestry()
    colorNumeric(
      palette = "YlOrRd",
      domain = c(0, max(data$value, na.rm = TRUE)),
      na.color = "#cccccc"
    )
  })
  
  # Render map
  output$map <- renderLeaflet({
    data <- selected_ancestry()
    pal <- color_palette()
    
    # Create labels
    labels <- sprintf(
      "<strong>%s</strong><br/>%s: %.1f%%",
      data$NAME,
      gsub("_", " ", tools::toTitleCase(input$ancestry)),
      data$value
    ) %>%
      lapply(HTML)
    
    leaflet(data = data) %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = -95, lat = 37.5, zoom = 4) %>%
      addPolygons(
        fillColor = ~pal(value),
        weight = 0.5,
        opacity = 0.5,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 2,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.9,
          bringToFront = TRUE
        ),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        )
      ) %>%
      addLegend(
        pal = pal,
        values = ~value,
        opacity = 0.7,
        title = paste(tools::toTitleCase(gsub("_", " ", input$ancestry)), "%"),
        position = "bottomright"
      )
  })
  
  # Illinois map rendering - initial render
  output$illinoisMap <- renderLeaflet({
    # Get selected ancestry for Illinois
    il_data <- illinois_data %>%
      mutate(value = get(input$ancestryIL))
    
    # Identify DuPage County
    il_data <- il_data %>%
      mutate(is_dupage = NAME == "DuPage")
    
    # Create color palette for Illinois
    pal_il <- colorNumeric(
      palette = "YlGnBu",
      domain = c(0, max(il_data$value, na.rm = TRUE)),
      na.color = "#cccccc"
    )
    
    # Create labels
    labels_il <- sprintf(
      "<strong>%s County, IL</strong><br/>%s: %.1f%%%s",
      il_data$NAME,
      gsub("_", " ", tools::toTitleCase(input$ancestryIL)),
      il_data$value,
      ifelse(il_data$is_dupage, "<br/><span style='color: rainbow;'><b>✨ DuPage County ✨</b></span>", "")
    ) %>%
      lapply(HTML)
    
    leaflet(data = il_data, elementId = "illinoisMapElement") %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = -89, lat = 40, zoom = 7) %>%
      addPolygons(
        fillColor = ~pal_il(value),
        weight = 1,
        opacity = 0.7,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.8,
        layerId = ~GEOID,
        highlight = highlightOptions(
          weight = 2,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.9,
          bringToFront = TRUE
        ),
        label = labels_il,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        )
      ) %>%
      addLegend(
        pal = pal_il,
        values = ~value,
        opacity = 0.7,
        title = paste(tools::toTitleCase(gsub("_", " ", input$ancestryIL)), "%"),
        position = "bottomright"
      )
  })
  
  # Update DuPage County with flashing colors
  observe({
    flash_timer()
    
    current_flash <- flash_color()
    
    # Get DuPage GEOID
    dupage_geoid <- illinois_data %>%
      filter(NAME == "DuPage") %>%
      pull(GEOID) %>%
      first()
    
    leafletProxy("illinoisMapElement") %>%
      setShapeStyle(
        layerId = dupage_geoid,
        fillColor = current_flash,
        color = current_flash,
        weight = 4,
        opacity = 1
      )
  })
  
  # All ancestries grid plot
  output$ancestriesGridPlot <- renderPlot({
    all_ancestry_data <- states_data %>%
      st_drop_geometry() %>%
      select(NAME, german, irish, english, italian, polish, french, mexican, chinese, african_american, asian) %>%
      pivot_longer(cols = -NAME, names_to = "ancestry", values_to = "percentage") %>%
      mutate(ancestry = gsub("_", " ", tools::toTitleCase(ancestry)))
    
    ggplot(all_ancestry_data, aes(x = reorder(NAME, percentage), y = percentage, fill = percentage)) +
      geom_col() +
      facet_wrap(~ancestry, nrow = 2, ncol = 5) +
      scale_fill_gradient(low = "lightblue", high = "darkblue") +
      theme_minimal() +
      theme(
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 8),
        strip.text = element_text(size = 10, face = "bold"),
        plot.title = element_text(size = 14, face = "bold"),
        legend.position = "bottom"
      ) +
      labs(
        title = "All Ancestry Groups by State",
        x = "",
        y = "Percentage (%)",
        fill = "Percentage"
      )
  })
  
  # Top ancestry by state table - using pre-computed data
  output$topAncestryTable <- renderTable({
    top_ancestry_by_state %>%
      select(NAME, ancestry, percentage) %>%
      mutate(percentage = round(percentage, 1)) %>%
      rename("State" = NAME, "Top Ancestry" = ancestry, "Percentage" = percentage)
  }, striped = TRUE, hover = TRUE)
  
  # Top ancestry bar plot - using pre-computed data
  output$topAncestryPlot <- renderPlot({
    ggplot(top_ancestry_by_state, aes(x = reorder(NAME, -percentage), y = percentage, fill = ancestry)) +
      geom_col() +
      scale_fill_brewer(palette = "Set3") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 8),
        plot.title = element_text(size = 12, face = "bold"),
        legend.position = "bottom",
        legend.text = element_text(size = 8)
      ) +
      labs(
        title = "Top Ancestry Group by State",
        x = "State",
        y = "Percentage (%)",
        fill = "Ancestry Group"
      )
  })
  
  # Diversity table - using pre-computed data
  output$diversityTable <- renderTable({
    diversity_scores %>%
      rename("State" = NAME, "Top Group %" = top_percentage, "Diversity Index" = diversity_index)
  }, striped = TRUE, hover = TRUE)
}

# Run app
shinyApp(ui, server)
