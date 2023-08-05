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
ui <- fluidPage(navbarPage("Horiba Fluorometer Analysis", 
                           tabPanel("App", fluid = TRUE, icon = icon('calculator'),
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
                                                        value = 215516.6563),
                                           numericInput("std_curve_value_b",
                                                        "Standard Curve b",
                                                        min = -3000000000000000000,
                                                        max = 3000000000000000000,
                                                        value = 158967.0058),
                                           checkboxInput("use_log", "Use Log?", value = FALSE),
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
                    tabPanel("Flux Rates (pmol/sec/\u03BCg)", tableOutput('rates'),),
                    tabPanel("Final Data", tableOutput('final')),
                    tabPanel("User Guide", shiny::textOutput('guide'),),
                    tabPanel("Raw Data", tableOutput('raw')))
                )
                )

),
tabPanel("User Guide",icon = icon('book-open'), fluid = TRUE,
           tabPanel("User Guide"),
           fluidRow(
             h2(HTML("<b>Instructions</b>")),
             h5(HTML("<p>For version control and other questions, see my <a href='https://github.com/stdecker/FM4_ATP_Analysis'>GitHub repo</a>.</p>")),
             h4(HTML("<b>Uploading Data</b>")),
             h5("To upload data, Each Excel file must first be set up with 'Time' and 'Intensity' as the first two columns of each sheet, with the respective data in the underlying rows of each column."),
             h5("Once data are in the correct format, you can upload the data using the tab on the left"),
             h4(HTML("<b>Selecting a Sheet</b>")),
             h5("The app requires you to input a 'Sheet Name'. The default naming scheme in Excel follows Sheet[x], where the first sheet x = 1 (so, Sheet1, Sheet2, etc.). 
                I have set this up where data would be stored on different sheets within one single Excel file, so when analyzing the data, 
                all that needs to be done is to click 'Add to Final Data' and then proceed to the next sheet. As long as you have your data in one Excel file spread across different sheets, 
                you should not need to browse and upload a new excel file for every analysis. 
                However, I believe uploading a new Excel sheet should not clear the existing data (I'm not sure, though, as I haven't tested that)."),
             h4(HTML("<b>Updating Standard Curves</b>")),
             h5("It is important to update the standard cuyrve information for each analysis. I have the standard curve equation setup as 'y = mx + b'. 
                To update the curve, change the [x] and [b] values. If standard curve follows a log equation, check the box stating 'Log'"),
             h4(HTML("<b>Selecting Points on Plot</b>")),
             h5("To Zoom in on the plot, select the 'Zoom' tool at the top of the plot. Draw a box area to zoom in. Double click to reset plot size."),
             h5("To select points, select the 'Box Select' tool at the top of the plot. Draw a box with the points you wish to select, making sure that the area of the box includes the data points. 
                Double click to reset the data selected"),
             h4(HTML("<b>Marking Selected Data for Analysis</b>")),
             h5(HTML("Once data have been selected using the 'Box Select' tool, the table to the right of the graph will update and show <i>only</i> the data selected in the box area. 
                Type in (do not click the rows in the table or the data will reset) the first and last time points in the table in the respective 'Start' and 'End' inputs in the left column.
                For example, if I am selecting the background correction and my timestamps range from 120 to 135 seconds, 
                I would inpput '120' into the 'Background Correction Start' input box and '135' into the 'Background Correction End' input box'. 
                Once the input boxes have been updated, the data in the 'Flux' tab will automatically update. 
                <b><u>Please note that the data in the 'Flux' tab is not stored and will be deleted upon closing the app unless the 'Add to Final Data' button is presses and the data are downloaded (see below)</u></b>.")),
             h4(HTML("<b>Saving and Downloading Data</b>")),
             h5("Once the data are in the 'Flux' table, they should be checked for accuracy and adjusted accordingly (again, these will update automatically once the timestamps are changed."),
             h5("After data have been checked and verified, click the 'Add to Final Data' button. This will add all of the verified data to a separate data frame and stored until the app is closed.
                At this point, you may continue with the analysis of other sheets in your Excel File by changing the 'Sheet Name', selecting new timestamps, and adding the verified data to the 'Final Data' dataframe (which will be added as a new row with the corresponding Sheet Name as the first column)."),
             h5("Once the analysis is completely finished, make sure all of the data that have been analyzed are added to the 'Final Data' tab. Select the 'Download Final Data' button.
                This will prompt a new window where you can rename the CSV file and save it onto your local system. Hit save. The data should be stored in the selected location and the app may now be closed.")
           ))))

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
  
  # output$guide <- 
  
  output$raw <- renderTable({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    data <- readxl::read_excel(inFile$datapath, col_names = T, sheet = paste(input$sheet_name))
    
    name1 <- colnames(data[1])
    name2 <- colnames(data[2])
    
    colnames(data) <- c('x', 'y')
    
    data$x <- round(data$x)
    
    Coef <- function(Z) coef(lm(y ~ x, as.data.frame(Z)))    
    avg_results <- rollapplyr(zoo(data), input$averages, Coef, by.column = FALSE, fill = NA)
    
    data$Slope <- avg_results$x/input$averages
    
    data$Slope <- as.numeric(data$Slope)
    
    Sample <- paste(input$sheet_name)
    
    baseline <- data[c(which(data$x == input$background_correction_start):which(data$x == input$background_correction_end)),]
    
    baseline_correction <- mean(baseline$y)
    
    data$Intensity_corrected <- data$y - baseline_correction
    
    for(k in input$use_log){
      
      if(k == FALSE){
        
        data$ATP <- (0.001*1000000*((((data$Intensity_corrected) - input$std_curve_value_b)/input$std_curve_value_x)))/(input$protein*0.001)
      }
      
      if(k == TRUE){
        
        data$ATP <- (0.001*1000000*(10^(((data$Intensity_corrected) - input$std_curve_value_b)/input$std_curve_value_x)))/(input$protein*0.001)
      }
    }
    
    Coef <- function(Z) coef(lm(ATP ~ x, as.data.frame(Z)))    
    ATP_results <- rollapplyr(zoo(data), input$averages, Coef, by.column = FALSE, fill = NA)
    
    data$flux <- ATP_results$x
    
    data$flux <- as.numeric(data$flux)
    
    colnames(data) <- c(paste(name1), paste(name2), paste(name2, 'Slope'), 'Intensity Corrected', 'Concentration (pmol)', 'Flux (pmol/sec)')
    
    return(data)
    
  }, hover = TRUE, striped = T, bordered = T, align = "c", caption = "Raw Values")
  
  
  
  
  output$graph <- renderUI({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    data <- readxl::read_excel(inFile$datapath, col_names = T, sheet = paste(input$sheet_name))
    
    name1 <- colnames(data[1])
    name2 <- colnames(data[2])
    
    colnames(data) <- c('x', 'y')
    
    data$x <- round(data$x)
    
    Coef <- function(Z) coef(lm(y ~ x, as.data.frame(Z)))    
    avg_results <- rollapplyr(zoo(data), input$averages, Coef, by.column = FALSE, fill = NA)
    
    data$Slope <- avg_results$x/input$averages
    
    data$Slope <- as.numeric(data$Slope)
    
    Sample <- paste(input$sheet_name)
    
    baseline <- data[c(which(data$x == input$background_correction_start):which(data$x == input$background_correction_end)),]
    
    baseline_correction <- mean(baseline$y)
    
    data$Intensity_corrected <- data$y - baseline_correction
    
    for(k in input$use_log){
      
      if(k == FALSE){
        
        data$ATP <- (0.001*1000000*((((data$Intensity_corrected) - input$std_curve_value_b)/input$std_curve_value_x)))/(input$protein*0.001)
      }
      
      if(k == TRUE){
        
        data$ATP <- (0.001*1000000*(10^(((data$Intensity_corrected) - input$std_curve_value_b)/input$std_curve_value_x)))/(input$protein*0.001)
      }
    }
    
    Coef <- function(Z) coef(lm(ATP ~ x, as.data.frame(Z)))    
    ATP_results <- rollapplyr(zoo(data), input$averages, Coef, by.column = FALSE, fill = NA)
    
    data$flux <- ATP_results$x
    
    data$flux <- as.numeric(data$flux)
    
    sd <- SharedData$new(data)
    
    fig <- plot_ly()
    # Add traces
    fig <- fig %>% 
      add_lines(data = sd, x = ~x, y = ~y, name = paste(name2), line = list(color='blue', width = 1)) %>%
      add_markers(data = sd, x = ~x, y = ~y, alpha = 0.01) %>%
      highlight("plotly_selected", dynamic = TRUE)
    
    ay <- list(
      tickfont = list(color = "black"),
      overlaying = "y",
      side = "right",
      title = "<b>Flux</b>")
    
    fig <- fig %>% 
      add_lines(data = sd, x = ~x, y = ~Slope, name = "Flux", yaxis = "y2", line = list(color='red', width = 2.5)) %>%
      add_markers(data = sd, x = ~x, y = ~Slope, alpha = 0.01, yaxis = "y2") %>%
      highlight("plotly_selected", dynamic = TRUE)
    
    # Set figure title, x and y-axes titles
    fig <- fig %>% layout(
      title = do.call(paste, list("<b>FM Data for Sample", input$sheet_name, collapse = "</b>")),
      yaxis2 = ay,
      xaxis = list(title="<b>Time (s)</b>"),
      yaxis = list(title= do.call(paste, list('<b>', name2, '</b>')))
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
        
        bg_start <- input$background_correction_start
        bg_end <- input$background_correction_end
        adp1_start <- input$adp1_slope_start
        adp1_end <- input$adp1_slope_end
        adp2_start <- input$adp2_slope_start
        adp2_end <- input$adp2_slope_end
        adp3_start <- input$adp3_slope_start
        adp3_end <- input$adp3_slope_end
        adp4_start <- input$adp4_slope_start
        adp4_end <- input$adp4_slope_end
        adp5_start <- input$adp5_slope_start
        adp5_end <- input$adp5_slope_end
        
        atp_flux_means <- data.frame(Sample, baseline_flux, atp1_flux, atp2_flux, atp3_flux, atp4_flux, atp5_flux, 
                                     bg_start, bg_end, adp1_start, adp1_end, adp2_start, adp2_end, adp3_start, adp3_end, adp4_start, adp4_end, adp5_start, adp5_end)
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
        
        bg_start <- input$background_correction_start
        bg_end <- input$background_correction_end
        adp1_start <- input$adp1_slope_start
        adp1_end <- input$adp1_slope_end
        adp2_start <- input$adp2_slope_start
        adp2_end <- input$adp2_slope_end
        adp3_start <- input$adp3_slope_start
        adp3_end <- input$adp3_slope_end
        adp4_start <- input$adp4_slope_start
        adp4_end <- input$adp4_slope_end
        adp5_start <- input$adp5_slope_start
        adp5_end <- input$adp5_slope_end
        
        atp_flux_means <- data.frame(Sample, baseline_flux, atp1_flux, atp2_flux, atp3_flux, atp4_flux, atp5_flux, 
                                     bg_start, bg_end, adp1_start, adp1_end, adp2_start, adp2_end, adp3_start, adp3_end, adp4_start, adp4_end, adp5_start, adp5_end)
      }
    }
    
    colnames(atp_flux_means) <- c('Sample', 'Basal', 'ADP 1', 'ADP 2', 'ADP 3', 'ADP 4', 'ADP 5', 
                                  'Background Start', 'Background end', 'ADP1 Start', 'ADP1 end', 'ADP2 Start', 'ADP2 end', 'ADP3 Start', 'ADP3 end', 'ADP4 Start', 'ADP4 end', 'ADP5 Start', 'ADP5 end')
    
    
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
    
    colnames(final_data) <- c('Sample', 'Basal', 'ADP 1', 'ADP 2', 'ADP 3', 'ADP 4', 'ADP 5', 
                              'Background Start', 'Background end', 'ADP1 Start', 'ADP1 end', 'ADP2 Start', 'ADP2 end', 'ADP3 Start', 'ADP3 end', 'ADP4 Start', 'ADP4 end', 'ADP5 Start', 'ADP5 end')
    
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
