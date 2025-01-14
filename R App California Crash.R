# Title: California Crash Data Analysis (2020-2023)
# Description:
# Author: Yihang Luo
# Date: 12/5/2024


# =======================================================
# Packages 
# =======================================================
library(shiny)
library(tidyverse)    # data wrangling and graphics
library(lubridate)    # for working with dates
library(leaflet)      # web interactive maps
library(plotly)       # web interactive graphics

# =======================================================
# Import data
# =======================================================
crashes = read_csv(
  file = "crashes_california_2020_2023.csv", 
  col_types = list(
    col_double(),    #  1) CASE_ID
    col_double(),    #  2) ACCIDENT_YEAR
    col_date(),      #  3) COLLISION_DATE 
    col_double(),    #  4) COLLISION_TIME 
    col_double(),    #  5) HOUR 
    col_integer(),   #  6) DAY_OF_WEEK 
    col_character(), #  7) WEATHER_1 
    col_character(), #  8) WEATHER_2 
    col_character(), #  9) STATE_HWY_IND
    col_character(), # 10) COLLISION_SEVERITY 
    col_integer(),   # 11) NUMBER_KILLED 
    col_integer(),   # 12) NUMBER_INJURED 
    col_integer(),   # 13) PARTY_COUNT 
    col_character(), # 14) PCF_VIOL_CATEGORY 
    col_character(), # 15) TYPE_OF_COLLISION 
    col_character(), # 16) ROAD_SURFACE 
    col_character(), # 17) ROAD_COND_1 
    col_character(), # 18) ROAD_COND_2 
    col_character(), # 19) LIGHTING 
    col_character(), # 20) PEDESTRIAN_ACCIDENT 
    col_character(), # 21) BICYCLE_ACCIDENT 
    col_character(), # 22) MOTORCYCLE_ACCIDENT 
    col_character(), # 23) TRUCK_ACCIDENT 
    col_character(), # 24) NOT_PRIVATE_PROPERTY 
    col_character(), # 25) ALCOHOL_INVOLVED 
    col_character(), # 26) COUNTY 
    col_character(), # 27) CITY 
    col_character(), # 28) PO_NAME
    col_double(),    # 29) ZIP_CODE
    col_double(),    # 30) POINT_X 
    col_double()     # 31) POINT_Y 
  ))

collision_types = unique(crashes$TYPE_OF_COLLISION)

pal = colorFactor(
  palette = "Paired",       
  domain = collision_types
)




# ===============================================
# Define "ui" for application
# ===============================================
ui <- fluidPage(
  
  # Application title
  titlePanel("Califirnia Crash Data Visualization"),
  
  # -------------------------------------------------------
  # Input widgets 
  # Customize the following dummy widgets with your own inputs
  # -------------------------------------------------------
  sidebarLayout(
    sidebarPanel(
      # ---------------------------------------------
      # input widgets of first tab
      # (adapt code with widgets of your choice)
      # ---------------------------------------------
      conditionalPanel(
        condition = "input.tabselected==1",
        h4("Exploratory Analysis"),
        # replace with your widgets
        selectInput(
          inputId = "county",
          label = "Select County",
          choices = c("ALL", sort(unique(crashes$COUNTY))),
          selected = "ALAMEDA"
          ),
        sliderInput(
          inputId = "year_range",
          label = "Select Year Range",
          min = min(crashes$ACCIDENT_YEAR),
          max = max(crashes$ACCIDENT_YEAR),
          value = c(min(crashes$ACCIDENT_YEAR), max(crashes$ACCIDENT_YEAR)),
          step = 1
          ),
        radioButtons(
          inputId = "type",
          label = "Select Type of Collision",
          choices = c("ALL", unique(crashes$TYPE_OF_COLLISION))
          ),
        checkboxInput(
          inputId = "alcohol",
          label = strong("Facet by Alcohol Involved"),
        )
      ), # closes 1st panel
      
      # ---------------------------------------------
      # input widgets of second tab
      # ---------------------------------------------
      conditionalPanel(
        condition = "input.tabselected==2",
        h4("Map"),
        # replace with your widgets
        selectInput(
          inputId = "po",
          label = "Select Post Office",
          choices = c(sort(unique(crashes$PO_NAME))),
          selected = "Berkeley"
        ),
        selectInput(
          inputId = "year",
          label = "Select a Year",
          choices = c("ALL", sort(unique(crashes$ACCIDENT_YEAR)))
        ),
        selectInput(
          inputId = "category",
          label = "Select a Violation Category",
          choices = c("ALL", sort(unique(crashes$PCF_VIOL_CATEGORY)))
        )
      ), # closes 2nd panel
      
    ), # closes sidebarPanel
    
    
    # -------------------------------------------------------
    # Main Panel with 2 tabsets: 
    # tab1: exploratory analysis
    # tab2: map of crashes
    # -------------------------------------------------------
    mainPanel(
      tabsetPanel(
        type = "tabs",
        # first tab (graphic)
        tabPanel(title = "Explore",
                 value = 1,
                 plotlyOutput(outputId = "plot1"),
                 hr(),
                 plotOutput(outputId = "plot2"),
                 hr(),
                 plotOutput(outputId = "plot3"),
                 ),
        # second tab (map)
        tabPanel(title = "Map",
                 value = 2,
                 leafletOutput("map", height = 600)),
        # selected tab
        id = "tabselected"
        
      ) # closes tabsetPanel
    ) # closes mainPanel
    
    
  ) # closes sidebarLayout
) # closes fluidPage (UI)



# ===============================================
# Define server logic
# ===============================================
server <- function(input, output) {
  
  filtered_data1 = reactive({
    data = crashes
    if (input$county != "ALL") {
      data = data |> 
        filter(COUNTY == input$county)
    } 
    if (input$type != "ALL") {
      data = data |>
        filter(TYPE_OF_COLLISION == input$type)
    }
    data = data |> filter(
      ACCIDENT_YEAR >= input$year_range[1],
      ACCIDENT_YEAR <= input$year_range[2]
    )
    data
  })
  
  filtered_data2 = reactive({
    data = crashes
    if (input$year != "ALL") {
      data = data |> 
        filter(ACCIDENT_YEAR == input$year)
    }
    if (input$po != "ALL") {
      data = data |> 
        filter(PO_NAME == input$po)
    }
    if (input$category != "ALL") {
      data = data |> 
        filter(PCF_VIOL_CATEGORY == input$category)
    }
    data
  })
  
  # ------------------------------------------------
  # Output for first TAB (i.e. summary plots)
  # ------------------------------------------------
  output$plot1 <- renderPlotly({
    p1 = filtered_data1() |>
      count(COLLISION_SEVERITY, ALCOHOL_INVOLVED) |>
      ggplot(aes(x = COLLISION_SEVERITY, y = n, fill = COLLISION_SEVERITY)) +
      geom_col(show.legend = FALSE) +
      labs(title = "Crashes by Severity", 
           x = "Severity", 
           y = "Count"
           ) +
      theme_minimal()
    if (input$alcohol) {
      p1 = p1 + 
        facet_wrap(~ ALCOHOL_INVOLVED) + 
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1, size = 10))
      p1
    } else {
      p1
    }
  })

  
  output$plot2 <- renderPlot({
    p2 =  filtered_data1() |>
      group_by(DAY_OF_WEEK, ALCOHOL_INVOLVED) |>
      summarize(total_killed = sum(NUMBER_KILLED, na.rm = TRUE),
                .group="drop") |>
      mutate(DAY_OF_WEEK = factor(DAY_OF_WEEK, 
                                  levels = c(1, 2, 3, 4, 5, 6, 7),
                                  labels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))) |>
      ggplot(aes(x = DAY_OF_WEEK, y = total_killed)) +
      geom_col(fill = "firebrick") +
      labs(
        title = "Number of Killed Victims by Day of Week",
        x = "Day of Week",
        y = "Total Number of Killed"
        ) +
      theme_minimal()
    if (input$alcohol) {
      p2 = p2 + 
        facet_wrap(~ ALCOHOL_INVOLVED) + 
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1, size = 10))
      p2
    } else {
      p2
    }
  })
  
  
  output$plot3 <- renderPlot({
    filtered_data1() |>
      group_by(HOUR) |>
      summarize(
        num_crashes = n(), 
        .groups = "drop") |>
      ggplot(aes(x = HOUR, y = num_crashes)) +
      geom_line(color = "blue", size = 1.5) +
      labs(
        title = "Distribution of Crashes Across Hours",
        x = "Number of Crashes",
        y = "Hour of Day"
      ) +
      theme_minimal()
  })
  
  
  # -----------------------------------------------
  # Output for second TAB (i.e. map)
  # ----------------------------------------------
  output$map <- renderLeaflet({
    data = filtered_data2()
    data |>
      leaflet() |>
      addProviderTiles("CartoDB.DarkMatter") |> 
      addCircles(lng = ~POINT_X,
                 lat = ~POINT_Y,
                 color = ~pal(TYPE_OF_COLLISION), 
                 fillOpacity = 1,
                 label = ~TYPE_OF_COLLISION,
                 weight = 4) |> 
      addLegend(position = "bottomleft", 
                pal = pal,
                values = collision_types,
                title = "Collision Type",
                opacity = 1)
  })
  
  
} # closes server



# ===============================================
# Run the application
# ===============================================
shinyApp(ui = ui, server = server)
