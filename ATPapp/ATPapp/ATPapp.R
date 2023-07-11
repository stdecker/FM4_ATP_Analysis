#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
pacman::p_load(readxl, tidyverse, ggplot2, ggprism, ggpubr, zoo, crosstalk, plotly, DT)

# Define UI for application that draws a histogram
ui <- fluidPage(titlePanel("Analysis of ATP Production on FM4"),
                sidebarLayout(sidebarPanel(width = 2,
                  fileInput('file1', 'Choose xlsx file',
                            accept = c(".xlsx", ".csv")),
                  textInput("sheet_name",
                               "Sheet Name"),
                  actionButton('add', "Add to Final Data"),
                  actionButton('reset_input', "Clear Time Values"),
                  downloadButton('download_atp',"Download Final Data"),
                  numericInput("background_correction_start",
                               "Background Correction Start Time (s)",
                               min = 0,
                               max = 1000000000000000000,
                               value = 0),
                  numericInput("background_correction_end",
                               "Background Correction End Time (s)",
                               min = 0,
                               max = 1000000000000000000,
                               value = 0),
                  numericInput("adp1_slope_start",
                               "Titration 1 Start Time (s)",
                               min = 0,
                               max = 1000000000000000000,
                               value = 0),
                  numericInput("adp1_slope_end",
                               "Titration 1 End Time (s)",
                               min = 0,
                               max = 1000000000000000000,
                               value = 0),
                  numericInput("adp2_slope_start",
                               "Titration 2 Start Time (s)",
                               min = 0,
                               max = 1000000000000000000,
                               value = 0),
                  numericInput("adp2_slope_end",
                               "Titration 2 End Time (s)",
                               min = 0,
                               max = 1000000000000000000,
                               value = 0),
                  numericInput("adp3_slope_start",
                               "Titration 3 Start Time (s)",
                               min = 0,
                               max = 1000000000000000000,
                               value = 0),
                  numericInput("adp3_slope_end",
                               "Titration 3 End Time (s)",
                               min = 0,
                               max = 1000000000000000000,
                               value = 0),
                  numericInput("adp4_slope_start",
                               "Titration 4 Start Time (s)",
                               min = 0,
                               max = 1000000000000000000,
                               value = 0),
                  numericInput("adp4_slope_end",
                               "Titration 4 End Time (s)",
                               min = 0,
                               max = 1000000000000000000,
                               value = 0),
                  numericInput("adp5_slope_start",
                               "Titration 5 Start Time (s)",
                               min = 0,
                               max = 1000000000000000000,
                               value = 0),
                  numericInput("adp5_slope_end",
                               "Titration 5 End Time (s)",
                               min = 0,
                               max = 1000000000000000000,
                               value = 0
                )
                  ),
                  mainPanel(
                    tabsetPanel(
                      tabPanel("Graph", htmlOutput('graph'),
                      ),
                      tabPanel("ATP Production Rates", tableOutput('rates'),),
                      tabPanel("Final Data", tableOutput('final')),
                      tabPanel("Raw Data", tableOutput('raw')))
                  )
                )
)

server <- function(input, output, session){
  
  observeEvent(input$reset_input, {
    updateNumericInput(session, "background_correction_start", value = 0)
    updateNumericInput(session, "background_correction_end", value = 0)
    updateNumericInput(session, "adp1_slope_start", value = 0)
    updateNumericInput(session, "adp1_slope_end", value = 0)
    updateNumericInput(session, "adp2_slope_start", value = 0)
    updateNumericInput(session, "adp2_slope_end", value = 0)
    updateNumericInput(session, "adp3_slope_start", value = 0)
    updateNumericInput(session, "adp3_slope_end", value = 0)
    updateNumericInput(session, "adp4_slope_start", value = 0)
    updateNumericInput(session, "adp4_slope_end", value = 0)
    updateNumericInput(session, "adp5_slope_start", value = 0)
    updateNumericInput(session, "adp5_slope_end", value = 0)
    
  })
  
  output$raw <- renderTable({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    table1 <- readxl::read_excel(inFile$datapath, col_names = T, sheet = paste(input$sheet_name))
  }, hover = TRUE, striped = T, bordered = T, align = "c", caption = "Raw Values")
  
  

  
  output$graph <- renderUI({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    
    data <- readxl::read_excel(inFile$datapath, col_names = T, sheet = paste(input$sheet_name))
    
    Coef <- function(Z) coef(lm(Intensity ~ Time, as.data.frame(Z)))    
    avg_results <- zoo::rollapplyr(zoo::zoo(data), 12, Coef, by.column = FALSE, fill = NA)
    
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
    
    p <- datatable(sd, rownames = NA)
    
    bscols(widths = c(6, 4), fig, p)
    
  })
  
  
adp_means_table <- reactive({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    
  data <- readxl::read_excel(inFile$datapath, col_names = T, sheet = paste(input$sheet_name))
  
  Coef <- function(Z) coef(lm(Intensity ~ Time, as.data.frame(Z)))    
  avg_results <- rollapplyr(zoo(data), 10, Coef, by.column = FALSE, fill = NA)
  
  data$Slope <- avg_results$Time
  
  data$Slope <- as.numeric(data$Slope)
  
  Sample <- paste(input$sheet_name)
  
  baseline <- data[c(input$background_correction_start:input$background_correction_end),]
  
  baseline_mean <- mean(baseline$Slope)
  
  adp1 <- data[c(input$adp1_slope_start:input$adp1_slope_end),]
  
  adp1_mean <- mean(adp1$Slope)
  
  adp2 <- data[c(input$adp2_slope_start:input$adp2_slope_end),]
  
  adp2_mean <- mean(adp2$Slope)
  
  adp3 <- data[c(input$adp3_slope_start:input$adp3_slope_end),]
  
  adp3_mean <- mean(adp3$Slope)
  
  adp4 <- data[c(input$adp4_slope_start:input$adp4_slope_end),]
  
  adp4_mean <- mean(adp4$Slope)
  
  adp5 <- data[c(input$adp5_slope_start:input$adp5_slope_end),]
  
  adp5_mean <- mean(adp5$Slope)
  
  adp_means <- data.frame(Sample, adp1_mean, adp2_mean, adp3_mean, adp4_mean, adp5_mean)
  
  colnames(adp_means) <- c('Sample', 'ADP 1', 'ADP 2', 'ADP 3', 'ADP 4', 'ADP 5')
  

  return(adp_means)
  })


output$rates <- renderTable({
  
  adp_means_table()
  
}, hover = TRUE, striped = T, bordered = T, align = "c", caption = paste("ATP Production Rates for Current Sample"))
  


rv <- reactiveValues(
  df = data.frame(
    Sample = character(),
    'ADP 1' = double(),
    'ADP 2' = double(),
    'ADP 3' = double(),
    'ADP 4' = double(),
    'ADP 5' = double()
  )
)

observeEvent(input$add, {
  rv$df <- rbind(rv$df, adp_means_table())
  
})

output$final <- renderTable({
  final_data <- data.frame(rv$df)
  
  colnames(final_data) <- c('Sample', 'ADP 1', 'ADP 2', 'ADP 3', 'ADP 4', 'ADP 5')
  
  thedata <- reactive(final_data)

  
  thedata()
  
}, hover = TRUE, striped = T, bordered = T, align = "c", caption = paste("ATP Production Rates for Group"))


output$download_atp <- downloadHandler(
  filename = function() {paste("ATP_analysis_",Sys.Date(),".csv", sep = "")}, 
  content = function(fname){
    write.csv(rv$df, fname, col.names = TRUE, row.names = FALSE)
  }
)

}

# Run the application 
shinyApp(ui = ui, server = server)


