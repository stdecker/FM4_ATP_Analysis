library(shiny)
library(crosstalk)
library(d3scatter)
library(dplyr)

ui <- fluidPage(
  selectInput("species", "Species", levels(iris$Species), multiple = TRUE),
  fluidRow(
    column(6, d3scatterOutput("scatter1")),
    column(6, d3scatterOutput("scatter2"))
  ),
  h4("Summary of selected data"),
  verbatimTextOutput("summary")
)

server <- function(input, output, session) {
  # Subset the dataset based on user's choice of species
  user_iris <- reactive({
    readxl::read_xlsx("C://Users//u1159489//Box//FunaiLab//Data//Adenine Diet//High-Adenine O2K Experiments//Cohort3//AdenineDietFM4.xlsx", sheet = '1')
  })

  shared_iris <- SharedData$new(user_iris)

  output$scatter1 <- renderD3scatter({
    d3scatter(shared_iris, ~Time, ~Intensity, width = "100%")
  })

  output$scatter2 <- renderD3scatter({
    d3scatter(shared_iris, ~Time, ~Intensity, width = "100%")
  })

  output$summary <- renderPrint({
    df <- shared_iris$data(withSelection = TRUE) %>%
      filter(selected_ | is.na(selected_)) %>%
      mutate(selected_ = NULL)

    cat(nrow(df), "observation(s) selected\n\n")
    summary(df)
  })
}

shinyApp(ui, server)

#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(titlePanel("Analysis of ATP Production on FM4"),
                sidebarLayout(sidebarPanel(width = 2,
                                           fileInput('file1', 'Choose xlsx file',
                                                     accept = c(".xlsx", ".csv")),
                                           textInput("sheet_name",
                                                     "Sheet Name"),
                                           actionButton('add', "Add to Final Data")
                ),
                mainPanel(
                  tabsetPanel(
                    tabPanel("Graph", htmlOutput('graph')
                             #          fluidRow(column(9,plotOutput('graph', height = 'auto')),
                             #          column(3,numericInput("background_correction_start",
                             #                       "Background Correction Start Time (s)",
                             #                       min = 0,
                             #                       max = 1000000000000000000,
                             #                       value = 0),
                             #          numericInput("background_correction_end",
                             #                       "Background Correction End Time (s)",
                             #                       min = 0,
                             #                       max = 1000000000000000000,
                             #                       value = 0),
                             #          numericInput("adp1_slope_start",
                             #                       "Titration 1 Start Time (s)",
                             #                       min = 0,
                             #                       max = 1000000000000000000,
                             #                       value = 0),
                             #          numericInput("adp1_slope_end",
                             #                       "Titration 1 End Time (s)",
                             #                       min = 0,
                             #                       max = 1000000000000000000,
                             #                       value = 0),
                             #          numericInput("adp2_slope_start",
                             #                       "Titration 2 Start Time (s)",
                             #                       min = 0,
                             #                       max = 1000000000000000000,
                             #                       value = 0),
                             #          numericInput("adp2_slope_end",
                             #                       "Titration 2 End Time (s)",
                             #                       min = 0,
                             #                       max = 1000000000000000000,
                             #                       value = 0),
                             #          numericInput("adp3_slope_start",
                             #                       "Titration 3 Start Time (s)",
                             #                       min = 0,
                             #                       max = 1000000000000000000,
                             #                       value = 0),
                             #          numericInput("adp3_slope_end",
                             #                       "Titration 3 End Time (s)",
                             #                       min = 0,
                             #                       max = 1000000000000000000,
                             #                       value = 0),
                             #          numericInput("adp4_slope_start",
                             #                       "Titration 4 Start Time (s)",
                             #                       min = 0,
                             #                       max = 1000000000000000000,
                             #                       value = 0),
                             #          numericInput("adp4_slope_end",
                             #                       "Titration 4 End Time (s)",
                             #                       min = 0,
                             #                       max = 1000000000000000000,
                             #                       value = 0),
                             #          numericInput("adp5_slope_start",
                             #                       "Titration 5 Start Time (s)",
                             #                       min = 0,
                             #                       max = 1000000000000000000,
                             #                       value = 0),
                             #          numericInput("adp5_slope_end",
                             #                       "Titration 5 End Time (s)",
                             #                       min = 0,
                             #                       max = 1000000000000000000,
                             #                       value = 0),
                             # ))
                    ),
                    tabPanel("ATP Production Rates", tableOutput('rates'),),
                    tabPanel("Final Data", tableOutput('final')),
                    tabPanel("Raw Data", tableOutput('raw')),
                    downloadButton('download_graph',"Download Graph"),
                    downloadButton('download_rates',"Download ATP Production Rates"))
                )
                )
)

server <- function(input, output){
  output$raw <- renderTable({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    table1 <- readxl::read_excel(inFile$datapath, col_names = T, sheet = paste(input$sheet_name))
  }, hover = TRUE, striped = T, bordered = T, align = "c", caption = "Raw Values")
  
  
  
  
  output$graph <- renderPlotly({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    
    # tabsetPanel(numericInput("background_correction_start",
    #                          "Background Correction Start Time (s)",
    #                          min = 0,
    #                          max = 1000000000000000000,
    #                          value = 0),
    #             numericInput("background_correction_end",
    #                          "Background Correction End Time (s)",
    #                          min = 0,
    #                          max = 1000000000000000000,
    #                          value = 0),
    #             numericInput("adp1_slope_start",
    #                          "Titration 1 Start Time (s)",
    #                          min = 0,
    #                          max = 1000000000000000000,
    #                          value = 0),
    #             numericInput("adp1_slope_end",
    #                          "Titration 1 End Time (s)",
    #                          min = 0,
    #                          max = 1000000000000000000,
    #                          value = 0),
    #             numericInput("adp2_slope_start",
    #                          "Titration 2 Start Time (s)",
    #                          min = 0,
    #                          max = 1000000000000000000,
    #                          value = 0),
    #             numericInput("adp2_slope_end",
    #                          "Titration 2 End Time (s)",
    #                          min = 0,
    #                          max = 1000000000000000000,
    #                          value = 0),
    #             numericInput("adp3_slope_start",
    #                          "Titration 3 Start Time (s)",
    #                          min = 0,
    #                          max = 1000000000000000000,
    #                          value = 0),
    #             numericInput("adp3_slope_end",
    #                          "Titration 3 End Time (s)",
    #                          min = 0,
    #                          max = 1000000000000000000,
    #                          value = 0),
    #             numericInput("adp4_slope_start",
    #                          "Titration 4 Start Time (s)",
    #                          min = 0,
    #                          max = 1000000000000000000,
    #                          value = 0),
    #             numericInput("adp4_slope_end",
    #                          "Titration 4 End Time (s)",
    #                          min = 0,
    #                          max = 1000000000000000000,
    #                          value = 0),
    #             numericInput("adp5_slope_start",
    #                          "Titration 5 Start Time (s)",
    #                          min = 0,
    #                          max = 1000000000000000000,
    #                          value = 0),
    #             numericInput("adp5_slope_end",
    #                          "Titration 5 End Time (s)",
    #                          min = 0,
    #                          max = 1000000000000000000,
    #                          value = 0),
    # )
    
    pacman::p_load(readxl, tidyverse, ggplot2, ggprism, ggpubr, zoo, crosstalk, plotly, DT)
    
    data <- readxl::read_excel(inFile$datapath, col_names = T, sheet = paste(input$sheet_name))
    
    Coef <- function(Z) coef(lm(Intensity ~ Time, as.data.frame(Z)))    
    avg_results <- rollapplyr(zoo(data), 12, Coef, by.column = FALSE, fill = NA)
    
    data$Slope <- avg_results$Time
    
    data$Slope <- as.numeric(data$Slope)
    
    sd <- SharedData$new(data)
    
    fig <- plot_ly()
    # Add traces
    fig <- fig %>% 
      add_lines(data = sd, x = ~Time, y = ~Intensity, name = "Intensity", line = list(color='blue', width = 1)) %>%
      add_markers(data = sd, x = ~Time, y = ~Intensity, alpha = 0.01) %>%
      highlight("plotly_selected", dynamic = TRUE)
    
    ay <- list(
      tickfont = list(color = "black"),
      overlaying = "y",
      side = "right",
      title = "<b>Flux</b>")
    
    fig <- fig %>% 
      add_lines(data = sd, x = ~Time, y = ~Slope, name = "Flux", yaxis = "y2", line = list(color='red', width = 2.5)) %>%
      add_markers(data = sd, x = ~Time, y = ~Slope, alpha = 0.01, yaxis = "y2") %>%
      highlight("plotly_selected", dynamic = TRUE)
    
    # Set figure title, x and y-axes titles
    fig <- fig %>% layout(
      title = do.call(paste, list("<b>FM Data for Sample", input$sheet_name, collapse = "</b>")),
      yaxis2 = ay,
      xaxis = list(title="<b>Time (s)</b>"),
      yaxis = list(title="<b>Intensity</b>")
    )%>%
      layout(plot_bgcolor='white',
             xaxis = list(
               zerolinecolor = '#ffff',
               zerolinewidth = 2,
               gridcolor = 'ffff'),
             yaxis = list(
               zerolinecolor = '#ffff',
               zerolinewidth = 2,
               gridcolor = 'ffff')
      )
    
    options(persistent = TRUE)
    
    p <- datatable(sd)
    
    bscols(widths = c(6, 4), fig, p)
    
  })
  
  
  output$rates <- renderTable({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    
    pacman::p_load(readxl, tidyverse, ggplot2, ggprism, ggpubr, zoo, crosstalk, plotly, DT)
    
    data <- readxl::read_excel(inFile$datapath, col_names = T, sheet = paste(input$sheet_name))
    
    Coef <- function(Z) coef(lm(Intensity ~ Time, as.data.frame(Z)))    
    avg_results <- rollapplyr(zoo(data), 10, Coef, by.column = FALSE, fill = NA)
    
    data$Slope <- avg_results$Time
    
    data$Slope <- as.numeric(data$Slope)
    
    
  }, hover = TRUE, striped = T, bordered = T, align = "c", caption = paste("ATP Production Rates for Sample"))
  
  
  output$final <- renderTable({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    
    pacman::p_load(readxl, tidyverse, ggplot2, ggprism, ggpubr, zoo, crosstalk, plotly, DT)
    
    data <- readxl::read_excel(inFile$datapath, col_names = T, sheet = paste(input$sheet_name))
    
    Coef <- function(Z) coef(lm(Intensity ~ Time, as.data.frame(Z)))    
    avg_results <- rollapplyr(zoo(data), 10, Coef, by.column = FALSE, fill = NA)
    
    data$Slope <- avg_results$Time
    
    data$Slope <- as.numeric(data$Slope)
    
    
  }, hover = TRUE, striped = T, bordered = T, align = "c", caption = "All ATP Production Rates")
  
}

# Run the application 
shinyApp(ui = ui, server = server)

# library(plotly)
# library(crosstalk)
# library(DT)
# library(zoo)
# 
# data <- readxl::read_excel("C://Users//u1159489//Box//FunaiLab//Data//Adenine Diet//High-Adenine O2K Experiments//Cohort3//AdenineDietFM4.xlsx", sheet = '1')
# 
# 
# Coef <- function(Z) coef(lm(Intensity ~ Time, as.data.frame(Z)))    
# avg_results <- rollapplyr(zoo(data), 10, Coef, by.column = FALSE, fill = NA)
# 
# data$Slope <- avg_results$Time
# 
# data$Slope <- as.numeric(data$Slope)
# 
# 
# a <- plot_ly(sd, x = ~Time, y = ~Slope) %>% 
#   add_lines() 



