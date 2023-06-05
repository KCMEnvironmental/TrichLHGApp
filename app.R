# TrichAnalytics Shiny App: Otolith LHG App 
# Created 5 June 2023 KMill 
#
# 
#
# To run App: 
#   Click the 'Run App' button above
#
#

# Load Libraries ----
library(shiny)
library(shinythemes) 
library(shinyWidgets)
library(shinyFiles)
library(ggplot2)
library(tidyverse)
library(zip)
library(ggiraph)


# Define UI for application ----
ui <- fluidPage(
  theme = shinytheme("superhero"),
  
  titlePanel("TrichAnalytics Otolith Data"),
  hr(),
  
  h3("Upload Data File"), 
  
  fluidRow(
    column(3, 
           fileInput("file",
                     label = NULL)),
    column(9, 
           downloadButton(outputId = "download_btn",
                          label = "Download All Plots",
                          icon = icon("fish-fins")))), 
  
  
  h3("Explore Data"), 
  
  textOutput('text'),
  
  tags$head(tags$style("#text{font-style: italic;}")),
  
  fluidRow(uiOutput(outputId = "dropdown")), 
  fluidRow(
    column(6, girafeOutput(outputId = "plot_Zn")), 
    column(6, fluidRow(girafeOutput(outputId = "plot_SrBa")))))

# Define server logic ----
server <- function(input, output) {
  
  options(shiny.maxRequestSize = 30 * 1024^2)
  
  output$text <- renderText({
    req(is.null(input$file))
    "Upload file to browse plots"
  })
  
  ## Download button ----
  
  output$download_btn <- downloadHandler(
    filename = function() {
      paste(Sys.Date(), ".zip", sep = "")
    },
    content = function(file) {
      
      sampleID_list <- readxl::excel_sheets(input$file$datapath)
      temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
      dir.create(temp_directory)
      
      for (i in sampleID_list) {   
        
        data <- readxl::read_excel(input$file$datapath, 
                                   sheet = i, 
                                   range = readxl::cell_cols(1:19))
        
        ### Create and save zinc plot ----
        plot_Zn <- ggplot() + 
          geom_line(data = data, 
                    aes(x = as.numeric(time), 
                        y = as.numeric(`66Zn`)), 
                    colour = "darkgreen") + 
          theme_bw() +
          labs(y = "Zinc (ppm)", 
               x = "Time (s)", 
               title = i) +
          theme(aspect.ratio = 1, 
                plot.title = element_text(face = "bold", 
                                          size = 14)) +
          scale_y_continuous(limits = c(0, 250))
        
        ggsave(paste(temp_directory, "/", i, "_Zn.png", sep = ""), plot_Zn)
        
        ### Create and save strontium and barium plot ----
        
        plot_SrBa <- ggplot() + 
          geom_line(data = data, 
                    aes(x = as.numeric(time), 
                        y = as.numeric(`88Sr`), 
                        colour = "Strontium")) + 
          geom_line(data = data, 
                    aes(x = as.numeric(time), 
                        y = as.numeric(`137Ba`)*40, 
                        colour = "Barium")) + 
          theme_bw() +
          labs(x = "Time (s)", 
               title = i) +
          theme(aspect.ratio = 1, 
                plot.title = element_text(face = "bold", 
                                          size = 14, 
                                          margin = margin(0,0,-13,0)), 
                legend.title = element_blank(), 
                legend.position = "top", 
                legend.margin = margin(0,0,0,150),
                legend.box.spacing = unit(0, 'cm')) +
          scale_y_continuous(name = "Strontium (ppm)",
                             limits = c(0, 2000), 
                             sec.axis = sec_axis(~ . /40, name = "Barium (ppm)"))
        
        ggsave(paste(temp_directory, "/", i, "_SrBa.png", sep = ""), plot_SrBa)
      }
      
      zip::zip(
        zipfile = file,
        files = dir(temp_directory),
        root = temp_directory
      )}, 
    contentType = 'application/zip')
  
  
  # Drop down button ----
  output$dropdown <- renderUI({
    req(input$file)
    selectInput('sampleIDs', 
                "Sample ID", 
                readxl::excel_sheets(input$file$datapath))
  })
  
  ### Create zinc plot ----
  output$plot_Zn <- renderGirafe({
    
    req(input$file)
    data <- readxl::read_excel(input$file$datapath, 
                               sheet = input$sampleIDs, 
                               range = readxl::cell_cols(1:19))
    
    data$tp <- (paste0(round(data$time, 0), " s \n", round(data$`66Zn`, 0), " ppm"))
    
    plot_Zn <- ggplot(data = data, 
                      aes(x = as.numeric(time), 
                          y = as.numeric(`66Zn`))) + 
      geom_line(colour = "darkgreen") + 
      geom_point_interactive(aes(tooltip = tp), 
                             alpha = 0) +
      theme_bw() +
      labs(y = "Zinc (ppm)", 
           x = "Time (s)", 
           title = input$sampleIDs) +
      theme(aspect.ratio = 1, 
            plot.title = element_text(face = "bold", 
                                      size = 14)) +
      scale_y_continuous(limits = c(0, 250))
    
    ggiraph(code = print(plot_Zn))
    
  })
  
  
  ### Create Sr Ba plot ----
  output$plot_SrBa <- renderGirafe({
    
    req(input$file)
    data <- readxl::read_excel(input$file$datapath, 
                               sheet = input$sampleIDs, 
                               range = readxl::cell_cols(1:19))
    
    data$tp <- (paste0(round(data$time, 0), " s \n", round(data$`88Sr`, 0), " ppm Sr \n", round(data$`137Ba`, 1), " ppm Ba"))
    
    plot_SrBa <- ggplot() + 
      geom_line(data = data, 
                aes(x = as.numeric(time), 
                    y = as.numeric(`88Sr`), 
                    colour = "Strontium")) + 
      geom_point_interactive(data = data, 
                             alpha = 0, # set totally transparent so viewer sees only geom_line with tooltip and does not see geom_point. 
                             aes(x = as.numeric(time), 
                                 y = as.numeric(`88Sr`), 
                                 tooltip = tp)) + 
      geom_line(data = data, 
                aes(x = as.numeric(time), 
                    y = as.numeric(`137Ba`)*40, 
                    colour = "Barium")) + 
      geom_point_interactive(data = data, # duplicates tooltip for Ba series, so tooltip will show when hovering over any data point
                             alpha = 0, 
                             aes(x = as.numeric(time), 
                                 y = as.numeric(`137Ba`)*40, 
                                 tooltip = tp)) + 
      theme_bw() +
      labs(x = "Time (s)", 
           title = input$sampleIDs) +
      theme(aspect.ratio = 1, 
            plot.title = element_text(face = "bold", 
                                      size = 14, 
                                      margin = margin(0,0,-13,0)), 
            legend.title = element_blank(), 
            legend.position = "top", 
            legend.margin = margin(0,0,0,150),
            legend.box.spacing = unit(0, 'cm')) +
      scale_y_continuous(name = "Strontium (ppm)",
                         limits = c(0, 2000), 
                         sec.axis = sec_axis(~ . /40, name = "Barium (ppm)"))
    
    ggiraph(code = print(plot_SrBa))
    
  })
}


# Run the application ----
shinyApp(ui = ui, server = server)


