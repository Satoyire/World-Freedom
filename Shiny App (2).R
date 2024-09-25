#Load Pacages
library(tidyverse)
library(lubridate)
library(readxl)
library(dplyr)
library(ggplot2)
library(sf)
library(ggthemes)
library(plotly)
library(shiny)
library(htmltools)


load("worldfreedomdata.RData")


ui <- fluidPage(
  titlePanel("Freedom Index Visualization"),
  
  sidebarLayout(
    
    # Define sidebarPanel
    sidebarPanel(
      # First input function - select input widget
      conditionalPanel(
        condition = "input.tabs == 'Freedom Map'",
        selectInput(inputId = "variable",
                    label= "Select 1st Variable",
                    choices = c("Civil Liberties",
                                "Political Rights",
                                "Overall_Score"),
                    selected = NULL)),
      
      conditionalPanel(
        condition = "input.tabs == 'Freedom Status Bar'",
        selectInput(inputId = "Year", 
                  label = "Select Year(s):", 
                  choices = as.factor(unique(NewDist$Year)),
                  selected = NULL,
                  multiple = TRUE),
        selectInput(inputId = "Status",
                  label= "Select Country Status(s)",
                  choices = as.factor(unique(NewDist$Freedom_Status)),
                  selected = NULL,
                  multiple = TRUE)),
      
    conditionalPanel(
      condition = "input.tabs == 'Scatter plot'",
      selectInput(inputId = "region",
                  label= "Select region (s) to view relationship",
                  choices = unique(Freedom_data$Region),
                  selected = NULL,
                  multiple = TRUE),
      
      selectInput(inputId = "Score1",
                  label= "Select 1st Variable",
                  choices = c("CL_Score",
                              "PR_Score",
                              "Overall_Score"),
                  selected = NULL),
      
      selectInput(inputId = "Score2",
                  label= "Select 2nd Variable",
                  choices = c("CL_Score",
                              "PR_Score",
                              "Overall_Score"),
                  selected = NULL)),
    
    # Add download button for RData file
    conditionalPanel(
      condition = "input.tabs == 'Data'",
      downloadButton("downloadData", "Download RData File"))
  ),
    
    # Define mainPanel
    mainPanel(
      tabsetPanel(id="tabs",
                  tabPanel("Introduction",
                           h2("Welcome to the World Freedom Index Visualization App!"), 
                           p("Freedom House rates people’s access to political rights and civil liberties in 210 
                             countries and territories through its annual Freedom in the World report. 
                             Individual freedoms—ranging from the right to vote to freedom of expression and 
                             equality before the law, can be affected by state or nonstate actors."),
                          p("This app allows you to visually explore various freedom index data for 195 countries 
                           obtained from Freedom House from 2006 to 2024. 
                           Use the tabs above to navigate through different plots and analyses. 
                           You can select different variables, regions, years, and freedom statuses to customize
                             the visualizations."),
                           br(),
                          strong("Plots"),
                           p("The freedom map shows the average Freedom score and status as assigned to 195 countries 
                             by Freedom House from 2006-2024. Select the variable of interest to view freedom status 
                             according to that variable. You can also click on the status in the legend and hover on 
                             the map to learn more."),
                           p("The bar plot shows the distribution of number of countries 
                             classified as free, partially free, and not free fromm 2006 to 2024"),
                           p("The scatter plot shows the relationship between various freedom index variables 
                             in one or more regions"),
                           br(),
                           strong("Note"),
                           p("PR - Political Rights"),
                           p("CL - Civil Liberties"),
                           p("Overall Score = PR +CL"),
                           br(),
                           strong("Data Sources"),
                           p(
                             "1.", 
                             tags$a(href="https://freedomhouse.org/report/freedom-world#Data",
                                    "World Freedom Data")
                             ),
                           uiOutput("definition_introduction")),
                  
                  tabPanel("Freedom Map",
                           plotlyOutput("map")),
                           
                  
                  tabPanel("Freedom Status Bar",
                           plotOutput(outputId = "bar_plot")),
                  
                  tabPanel("Scatter plot",
                           plotOutput(outputId = "scat_plot"),
                           uiOutput("definition_scat_plot")),
                  tabPanel("Data", tableOutput("dataView"))
      )
)
)
)

#Define Server
server <- function(input, output, session) {
  
  output$downloadData <- downloadHandler(
    filename = function() {
      "worldfreedomdata.RData"  # Name of the RData file that the user will download
    },
    content = function(file) {
      file.copy("worldfreedomdata.RData", file)  # Path to the RData file
    }
  )
  
  # Dynamic update of Score2 choices based on Score1 selection
  observeEvent(input$Score1, {
    # Filter out the selected variable from choices for Score2
    choices <- setdiff(c("CL_Score", "PR_Score", "Overall_Score"), input$Score1)
    # Update choices for Score2
    updateSelectInput(session, "Score2", choices = choices)
  })
  
  # Render World Map plot
  output$map <- renderPlotly({
    W_Freedom <- ggplot(Avg_World_Freedom) +
      geom_sf(aes(fill = Status, 
                  text = paste0("Country:", COUNTRY, "\n", input$variable, ":", .data[[input$variable]])))+
      coord_sf() +
      theme_map() +
      labs(
        title = paste("World Freedom Index based on", input$variable, "(2006-2024)"))+
      theme(plot.title = element_text(hjust = 0.5),
            legend.position = "bottom",  # You can adjust legend position here
            legend.title = element_text(size = 10))+
      scale_fill_manual(name = paste("Freedom Status"), 
                        values = c("Not Free" = "#fb6a4a", 
                                   "Partially Free" = "#fed976", 
                                   "Free" = "#238443"),
                        labels = c("FREE\nScore: 70-100", "NOT FREE\nScore: 0-39", "PARTLY FREE\nScore: 40-69"))
    
    ggplotly(
      W_Freedom, 
      tooltip = c("text"),
      width=900,        # Width of the plot in pixels
      height=600) %>%   # Height of the plot in pixels
      layout(legend = list(orientation = "h", x = 0, y = 0)
      )
  })
  
  
  # Render Bar Plot
  output$bar_plot <- renderPlot({
    
    filtered_data <- subset(NewDist, Year %in% input$Year & Freedom_Status %in% input$Status)
    
    bar <- ggplot(filtered_data, aes(x = as.factor(Year), y = Number, fill = Freedom_Status)) +
      geom_bar(stat = "identity", position = position_dodge(width = 0.7)) +
      labs(x = "Year",
           y = "Number of Countries",
           fill = "Freedom Status",
           title = "Number of Countries by Freedom Status ",
           caption = "Data Source: https://freedomhouse.org/report/freedom-world#Data") +
      theme_classic() +
      theme(legend.position = "right",
            legend.key.size = unit(0.5, "cm"),
            legend.direction = "vertical",
            axis.text.x = element_text(angle = 45, hjust = 1),
            plot.title = element_text(hjust = 0.5),
            plot.caption = element_text(hjust = 0.5))
    bar
  })
  
  
    
  output$scat_plot <- renderPlot({
    
    Scatter_data <- subset(Freedom_data, Region %in% input$region)
    
    Scat <- ggplot(data = Scatter_data) +
      geom_point(aes(x = !!sym(input$Score1), y = !!sym(input$Score2)), shape=16) +
      geom_smooth(aes(x = !!sym(input$Score1), y = !!sym(input$Score2)), method="lm", se=TRUE)+
      labs(x = input$Score1, 
           y = input$Score2,
           color = "Region",
           title = paste("Relationship between ", input$Score1, "and ", input$Score2),
           caption = "Data Source: https://freedomhouse.org/report/freedom-world#Data") +
      theme_classic() +
      theme(legend.position = "bottom",
            plot.title = element_text(hjust=0.5),
            plot.caption = element_text(hjust = 0.5))
    
    # # print(Scat)  # Print the plot to ensure it's created
    # 
    # # Return the plot
    return(Scat)    
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
