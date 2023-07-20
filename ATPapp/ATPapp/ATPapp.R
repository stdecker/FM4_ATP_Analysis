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
ui <- fluidPage(titlePanel("Analysis of Fluoromax Data (Updated on 7/20/2023 by Stephen Decker)"),
                sidebarLayout(sidebarPanel(width = 2,
                                           fileInput('file1', 'Choose xlsx file',
                                                     accept = c(".xlsx", ".csv")),
                                           textInput("sheet_name",
                                                     "Sheet Name"),
                                           numericInput("averages",
                                                        "Number of Averages",
                                                        min = -3000000000000000000,
                                                        max = 3000000000000000000,
                                                        value = 4),
                                           numericInput("protein",
                                                        "Protein Concentration (\u03BCg)",
                                                        min = -3000000000000000000,
                                                        max = 3000000000000000000,
                                                        value = 5),
                                           numericInput("std_curve_value_x",
                                                        "Standard Curve x",
                                                        min = -3000000000000000000,
                                                        max = 3000000000000000000,
                                                        value = 4870815.931),
                                           numericInput("std_curve_value_b",
                                                        "Standard Curve b",
                                                        min = -3000000000000000000,
                                                        max = 3000000000000000000,
                                                        value = --1123036.603),
                                           checkboxInput("use_log", "Use Log?", value = TRUE),
                                           actionButton('add', "Add to Final Data"),
                                           actionButton('reset_input', "Clear Time Values"),
                                           downloadButton('download_atp',"Download Final Data"),
                                           numericInput("background_correction_start",
                                                        "Background Correction Start Time (s)",
                                                        min = 0,
                                                        max = 3000000000000000000,
                                                        value = 0),
                                           numericInput("background_correction_end",
                                                        "Background Correction End Time (s)",
                                                        min = 0,
                                                        max = 3000000000000000000,
                                                        value = 0),
                                           numericInput("adp1_slope_start",
                                                        "Titration 1 Start Time (s)",
                                                        min = 0,
                                                        max = 3000000000000000000,
                                                        value = 0),
                                           numericInput("adp1_slope_end",
                                                        "Titration 1 End Time (s)",
                                                        min = 0,
                                                        max = 3000000000000000000,
                                                        value = 0),
                                           numericInput("adp2_slope_start",
                                                        "Titration 2 Start Time (s)",
                                                        min = 0,
                                                        max = 3000000000000000000,
                                                        value = 0),
                                           numericInput("adp2_slope_end",
                                                        "Titration 2 End Time (s)",
                                                        min = 0,
                                                        max = 3000000000000000000,
                                                        value = 0),
                                           numericInput("adp3_slope_start",
                                                        "Titration 3 Start Time (s)",
                                                        min = 0,
                                                        max = 3000000000000000000,
                                                        value = 0),
                                           numericInput("adp3_slope_end",
                                                        "Titration 3 End Time (s)",
                                                        min = 0,
                                                        max = 3000000000000000000,
                                                        value = 0),
                                           numericInput("adp4_slope_start",
                                                        "Titration 4 Start Time (s)",
                                                        min = 0,
                                                        max = 3000000000000000000,
                                                        value = 0),
                                           numericInput("adp4_slope_end",
                                                        "Titration 4 End Time (s)",
                                                        min = 0,
                                                        max = 3000000000000000000,
                                                        value = 0),
                                           numericInput("adp5_slope_start",
                                                        "Titration 5 Start Time (s)",
                                                        min = 0,
                                                        max = 3000000000000000000,
                                                        value = 0),
                                           numericInput("adp5_slope_end",
                                                        "Titration 5 End Time (s)",
                                                        min = 0,
                                                        max = 3000000000000000000,
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
    data <- readxl::read_excel(inFile$datapath, col_names = T, sheet = paste(input$sheet_name))
    
    data$Time <- round(data$Time)
    
    Coef <- function(Z) coef(lm(Intensity ~ Time, as.data.frame(Z)))    
    avg_results <- rollapplyr(zoo(data), input$averages, Coef, by.column = FALSE, fill = NA)
    
    data$Slope <- avg_results$Time/input$averages
    
    data$Slope <- as.numeric(data$Slope)
    
    Sample <- paste(input$sheet_name)
    
    baseline <- data[c(which(data$Time == input$background_correction_start):which(data$Time == input$background_correction_end)),]
    
    baseline_correction <- mean(baseline$Intensity)
    
    data$Intensity_corrected <- data$Intensity - baseline_correction
    
    for(k in input$use_log){
      
      if(k == TRUE){
        
        data$ATP <- (0.001*1000000*((((data$Intensity_corrected) - input$std_curve_value_b)/input$std_curve_value_x)))/(input$protein*0.001)
      }
      
      if(k == FALSE){
        
        data$ATP <- (0.001*1000000*(10^(((data$Intensity_corrected) - input$std_curve_value_b)/input$std_curve_value_x)))/(input$protein*0.001)
      }
    }
    
    Coef <- function(Z) coef(lm(ATP ~ Time, as.data.frame(Z)))    
    ATP_results <- rollapplyr(zoo(data), input$averages, Coef, by.column = FALSE, fill = NA)
    
    data$ATP_Slope <- ATP_results$Time
    
    data$ATP_Slope <- as.numeric(data$ATP_Slope)
    
    return(data)
    
  }, hover = TRUE, striped = T, bordered = T, align = "c", caption = "Raw Values")
  
  
  
  
  output$graph <- renderUI({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    
    data <- readxl::read_excel(inFile$datapath, col_names = T, sheet = paste(input$sheet_name))
    
    data$Time <- round(data$Time)
    
    Coef <- function(Z) coef(lm(Intensity ~ Time, as.data.frame(Z)))    
    avg_results <- zoo::rollapplyr(zoo::zoo(data), input$averages, Coef, by.column = FALSE, fill = NA)
    
    data$Slope <- avg_results$Time/input$averages
    
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
  
  
  atp_flux_table <- reactive({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    
    
    data <- readxl::read_excel(inFile$datapath, col_names = T, sheet = paste(input$sheet_name))
    
    data$Time <- round(data$Time)
    
    Coef <- function(Z) coef(lm(Intensity ~ Time, as.data.frame(Z)))    
    avg_results <- rollapplyr(zoo(data), input$averages, Coef, by.column = FALSE, fill = NA)
    
    data$Slope <- avg_results$Time/input$averages
    
    data$Slope <- as.numeric(data$Slope)
    
    Sample <- paste(input$sheet_name)
    
    baseline <- data[c(which(data$Time == input$background_correction_start):which(data$Time == input$background_correction_end)),]
    
    baseline_correction <- mean(baseline$Intensity)
    
    data$Intensity_corrected <- data$Intensity - baseline_correction
    
    for(k in input$use_log){
      
      if(k == TRUE){
        
        data$ATP <- (0.001*1000000*(10^(((data$Intensity_corrected) - input$std_curve_value_b)/input$std_curve_value_x)))/(input$protein*0.001)
        
        Coef <- function(Z) coef(lm(ATP ~ Time, as.data.frame(Z)))    
        ATP_results <- rollapplyr(zoo(data), input$averages, Coef, by.column = FALSE, fill = NA)
        
        data$ATP_Slope <- ATP_results$Time
        
        data$ATP_Slope <- as.numeric(data$ATP_Slope)
        
        bl <- data[c(which(data$Time == input$background_correction_start):which(data$Time == input$background_correction_end)),]
        
        baseline_flux <- mean(bl$ATP_Slope)
        
        if(input$adp1_slope_end > 0){
          adp1 <- data[c(which(data$Time == input$adp1_slope_start):which(data$Time == input$adp1_slope_end)),]
        }
        
        if(input$adp1_slope_end == 0){
          adp1 <- NA
        }
        
        if(is.atomic(adp1) == FALSE){
          atp1_flux <- mean(adp1$ATP_Slope)
        }
        
        if(is.atomic(adp1) == TRUE){
          atp1_flux <- NA
        }
        
        if(input$adp2_slope_end > 0){
          adp2 <- data[c(which(data$Time == input$adp2_slope_start):which(data$Time == input$adp2_slope_end)),]
        }
        
        if(input$adp2_slope_end == 0){
          adp2 <- NA
        }
        
        if(is.atomic(adp2) == FALSE){
          atp2_flux <- mean(adp2$ATP_Slope)
        }
        
        if(is.atomic(adp2) == TRUE){
          atp2_flux <- NA
        }
        
        if(input$adp3_slope_end > 0){
          adp3 <- data[c(which(data$Time == input$adp3_slope_start):which(data$Time == input$adp3_slope_end)),]
        }
        
        if(input$adp3_slope_end == 0){
          adp3 <- NA
        }
        
        if(is.atomic(adp3) == FALSE){
          atp3_flux <- mean(adp3$ATP_Slope)
        }
        
        if(is.atomic(adp3) == TRUE){
          atp3_flux <- NA
        }
        
        if(input$adp4_slope_end > 0){
          adp4 <- data[c(which(data$Time == input$adp4_slope_start):which(data$Time == input$adp4_slope_end)),]
        }
        
        if(input$adp4_slope_end == 0){
          adp4 <- NA
        }
        
        if(is.atomic(adp4) == FALSE){
          atp4_flux <- mean(adp4$ATP_Slope)
        }
        
        if(is.atomic(adp4) == TRUE){
          atp4_flux <- NA
        }
        
        if(input$adp5_slope_end > 0){
          adp5 <- data[c(which(data$Time == input$adp5_slope_start):which(data$Time == input$adp5_slope_end)),]
        }
        
        if(input$adp5_slope_end == 0){
          adp5 <- NA
        }
        
        if(is.atomic(adp5) == FALSE){
          atp5_flux <- mean(adp5$ATP_Slope)
        }
        
        if(is.atomic(adp5) == TRUE){
          atp5_flux <- NA
        }
        
        atp_flux_means <- data.frame(Sample, baseline_flux, atp1_flux, atp2_flux, atp3_flux, atp4_flux, atp5_flux)
      }
      
      if(k == FALSE){
        
        data$ATP <- (0.001*1000000*((((data$Intensity_corrected) - input$std_curve_value_b)/input$std_curve_value_x)))/(input$protein*0.001)
        
        Coef <- function(Z) coef(lm(ATP ~ Time, as.data.frame(Z)))    
        ATP_results <- rollapplyr(zoo(data), input$averages, Coef, by.column = FALSE, fill = NA)
        
        data$ATP_Slope <- ATP_results$Time
        
        data$ATP_Slope <- as.numeric(data$ATP_Slope)
        
        bl <- data[c(which(data$Time == input$background_correction_start):which(data$Time == input$background_correction_end)),]
        
        baseline_flux <- mean(bl$ATP_Slope)
        
        if(input$adp1_slope_end > 0){
          adp1 <- data[c(which(data$Time == input$adp1_slope_start):which(data$Time == input$adp1_slope_end)),]
        }
        
        if(input$adp1_slope_end == 0){
          adp1 <- NA
        }
        
        if(is.atomic(adp1) == FALSE){
          atp1_flux <- mean(adp1$ATP_Slope)
        }
        
        if(is.atomic(adp1) == TRUE){
          atp1_flux <- NA
        }
        
        if(input$adp2_slope_end > 0){
          adp2 <- data[c(which(data$Time == input$adp2_slope_start):which(data$Time == input$adp2_slope_end)),]
        }
        
        if(input$adp2_slope_end == 0){
          adp2 <- NA
        }
        
        if(is.atomic(adp2) == FALSE){
          atp2_flux <- mean(adp2$ATP_Slope)
        }
        
        if(is.atomic(adp2) == TRUE){
          atp2_flux <- NA
        }
        
        if(input$adp3_slope_end > 0){
          adp3 <- data[c(which(data$Time == input$adp3_slope_start):which(data$Time == input$adp3_slope_end)),]
        }
        
        if(input$adp3_slope_end == 0){
          adp3 <- NA
        }
        
        if(is.atomic(adp3) == FALSE){
          atp3_flux <- mean(adp3$ATP_Slope)
        }
        
        if(is.atomic(adp3) == TRUE){
          atp3_flux <- NA
        }
        
        if(input$adp4_slope_end > 0){
          adp4 <- data[c(which(data$Time == input$adp4_slope_start):which(data$Time == input$adp4_slope_end)),]
        }
        
        if(input$adp4_slope_end == 0){
          adp4 <- NA
        }
        
        if(is.atomic(adp4) == FALSE){
          atp4_flux <- mean(adp4$ATP_Slope)
        }
        
        if(is.atomic(adp4) == TRUE){
          atp4_flux <- NA
        }
        
        if(input$adp5_slope_end > 0){
          adp5 <- data[c(which(data$Time == input$adp5_slope_start):which(data$Time == input$adp5_slope_end)),]
        }
        
        if(input$adp5_slope_end == 0){
          adp5 <- NA
        }
        
        if(is.atomic(adp5) == FALSE){
          atp5_flux <- mean(adp5$ATP_Slope)
        }
        
        if(is.atomic(adp5) == TRUE){
          atp5_flux <- NA
        }
        
        atp_flux_means <- data.frame(Sample, baseline_flux, atp1_flux, atp2_flux, atp3_flux, atp4_flux, atp5_flux)
      }
    }
    
    colnames(atp_flux_means) <- c('Sample', 'Basal', 'ADP 1', 'ADP 2', 'ADP 3', 'ADP 4', 'ADP 5')
    
    
    return(atp_flux_means)
  })
  
  
  output$rates <- renderTable({
    
    atp_flux_table()
    
  }, hover = TRUE, striped = T, bordered = T, align = "c", caption = paste("ATP Production Rates for Current Sample"))
  
  
  
  rv <- reactiveValues(
    df = data.frame(
      Sample = character(),
      'Basal' = double(),
      'ADP 1' = double(),
      'ADP 2' = double(),
      'ADP 3' = double(),
      'ADP 4' = double(),
      'ADP 5' = double()
    )
  )
  
  observeEvent(input$add, {
    rv$df <- rbind(rv$df, atp_flux_table())
    
  })
  
  output$final <- renderTable({
    final_data <- data.frame(rv$df)
    
    colnames(final_data) <- c('Sample', 'Basal','ADP1', 'ADP2', 'ADP3', 'ADP4', 'ADP5')
    
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
