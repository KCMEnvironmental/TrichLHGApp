# TrichAnalytics Shiny App: Otolith LHG App 
# Created 5 June 2023 KMill 
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
library(viridis)
library(openxlsx)


# Define UI for application ----
ui <- fluidPage(
  theme = shinytheme("superhero"),
  
  titlePanel("TrichAnalytics Otolith LHG Data"),
  hr(),
  
  h3("Upload Data File"), 
  
  fluidRow(
    column(3, 
           fileInput("file",
                     label = NULL)),
    column(2, 
           downloadButton(outputId = "download_plots",
                          label = "Download All Plots",
                          icon = icon("fish-fins"))), 
    column(6, 
         downloadButton(outputId = "download_file",
                        label = "Download Data File",
                        icon = icon("table")))), 
  
  numericInput("len.int", label = h5("Enter length interval (µm)"), value = 10),
    
  h3("Explore Data"), 
  
  textOutput('text'),
  tags$head(tags$style("#text{font-style: italic;}")),
  
  fluidRow(uiOutput(outputId = "dropdown")), 
  
  fluidRow(girafeOutput(outputId = "plot")))

# Define server logic ----
server <- function(input, output) {
  
  options(shiny.maxRequestSize = 30 * 1024^2) #increases max size of file that can be uploaded
  
  output$text <- renderText({
    req(is.null(input$file))
    "Upload file to browse plots"
  })
  
  ## Download File button ----
  
  output$download_file <- downloadHandler(
    filename = function() {
      paste(input$file, "_Summary.xlsx", sep = "")
    },
    content = function(file) {
      
      sampleID_list <- readxl::excel_sheets(input$file$datapath)
      sampleID_list <- sampleID_list[sampleID_list %in% 
                                       c("Otolith Analysis",
                                         "Sample Set Information", 
                                         "COC", 
                                         "Sample Notes") == FALSE]
      
      file_output <- createWorkbook()
      
      for (i in sampleID_list) {   
        
        data <- readxl::read_excel(input$file$datapath, sheet = i)[-c(1:2),] %>% 
          rename("Length (µm)" = "Parameter") %>% 
          mutate(`Length (µm)` = as.numeric(`Length (µm)`)) %>% 
          mutate(ints = cut(`Length (µm)`, 
                            breaks = seq(min(`Length (µm)`), 
                                         nrow(.), input$len.int),
                            include.lowest = TRUE)) %>% 
          group_by(ints) %>% 
          summarise(across(everything(), median, .names = "{.col}_median"))
        
        addWorksheet(file_output, sheetName = i)
        writeData(file_output, sheet = i, x = data)
      }
      saveWorkbook(file_output, file)
    }
  )
  
  ## Download Plots button ----
  output$download_plots <- downloadHandler(
    filename = function() {
      paste(Sys.Date(), ".zip", sep = "")
    },
    content = function(file) {

      sampleID_list <- readxl::excel_sheets(input$file$datapath)
      sampleID_list <- sampleID_list[sampleID_list %in% 
                                       c("Otolith Analysis",
                                         "Sample Set Information", 
                                         "COC", 
                                         "Sample Notes") == FALSE]
      temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
      dir.create(temp_directory)
      
      for (i in sampleID_list) {   
        
        data <- readxl::read_excel(input$file$datapath, sheet = i)[-c(1:2),] %>% 
          rename("Length (µm)" = "Parameter") %>% 
          mutate(`Length (µm)` = as.numeric(`Length (µm)`)) %>% 
          mutate(ints = cut(`Length (µm)`, 
                            breaks = seq(min(`Length (µm)`), 
                                         nrow(.), input$len.int),
                            include.lowest = TRUE)) %>% 
          group_by(ints) %>% 
          summarise(across(everything(), median))
        
      # Create and save plot ----
        plot <- ggplot() + 
          geom_path(data = data, 
                    aes(x = as.numeric(`88Sr/Ca`), 
                        y = as.numeric(`137Ba/Ca`), 
                        colour = `Length (µm)`)) + 
          geom_point(data = data, 
                    aes(x = as.numeric(`88Sr/Ca`), 
                        y = as.numeric(`137Ba/Ca`), 
                        colour = `Length (µm)`)) + 
          theme_bw() +
          scale_color_gradientn(colours = rainbow(5), 
                                breaks = c(min(data$`Length (µm)`), 
                                           median(data$`Length (µm)`), 
                                           max(data$`Length (µm)`)), 
                                labels = c("Core", "Middle","Edge")) +
          # scale_color_viridis(option = "D", 
          #                     breaks = c(min(data_p$`Length (µm)`), 
          #                                median(data_p$`Length (µm)`), 
          #                                max(data_p$`Length (µm)`)), 
          #                     labels = c("Core", "Middle","Edge")) +
          labs(y = "Ba:Ca (µmol/mol)", 
               x = "Sr:Ca (µmol/mol)", 
               title = i) +
          theme(aspect.ratio = 1, 
                plot.title = element_text(face = "bold", 
                                          size = 14, 
                                          margin = margin(0,0,-13,0)), 
                legend.title = element_blank(), 
                legend.position = "top", 
                legend.margin = margin(0,0,0,250),
                legend.box.spacing = unit(0.1, 'cm')) 
        
        ggsave(paste(temp_directory, "/", i, "_LHG.png", sep = ""), plot)
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
    
    sampleID_list <- readxl::excel_sheets(input$file$datapath)
    sampleID_list <- sampleID_list[sampleID_list %in% 
                                     c("Otolith Analysis",
                                       "Sample Set Information", 
                                       "COC", 
                                       "Sample Notes") == FALSE]
    
    selectInput('sampleIDs', 
                "Sample ID", 
                sampleID_list)
  })
  
  ## Create 'Explore Data' plot ----
  output$plot <- renderGirafe({
    
    req(input$file)
    
    data_p <- readxl::read_excel(input$file$datapath, 
                               sheet = input$sampleIDs)[-c(1:2),] %>% 
      rename("Length (µm)" = "Parameter") %>% 
      mutate(`Length (µm)` = as.numeric(`Length (µm)`)) %>% 
      mutate(ints = cut(`Length (µm)`, 
                        breaks = seq(min(`Length (µm)`), 
                                     nrow(.), input$len.int),
                        include.lowest = TRUE)) %>% 
      group_by(ints) %>% 
      summarise(across(everything(), median))
    
    data_p$tp <- (paste0("Length ", round(data_p$`Length (µm)`, 0), " µm \n", 
                       "Sr:Ca ", round(data_p$`88Sr/Ca`, 0), " µmol/mol \n", 
                       "Ba:Ca ", round(data_p$`137Ba/Ca`, 0), " µmol/mol \n"))
    
    plot <- ggplot() + 
      geom_path(data = data_p, 
                             aes(x = as.numeric(`88Sr/Ca`), 
                                 y = as.numeric(`137Ba/Ca`), 
                                 colour = `Length (µm)`)) + 
      geom_point_interactive(data = data_p, 
                 aes(x = as.numeric(`88Sr/Ca`), 
                     y = as.numeric(`137Ba/Ca`), 
                     colour = `Length (µm)`, 
                     tooltip = tp)) + 
      theme_bw() +
      scale_color_gradientn(colours = rainbow(5), 
                            breaks = c(min(data_p$`Length (µm)`), 
                                       median(data_p$`Length (µm)`), 
                                       max(data_p$`Length (µm)`)), 
                            labels = c("Core", "Middle","Edge")) +
      # scale_color_viridis(option = "D", 
      #                     breaks = c(min(data_p$`Length (µm)`), 
      #                                median(data_p$`Length (µm)`), 
      #                                max(data_p$`Length (µm)`)), 
      #                     labels = c("Core", "Middle","Edge")) +
      labs(y = "Ba:Ca (µmol/mol)", 
           x = "Sr:Ca (µmol/mol)", 
           title = input$sampleIDs) +
      theme(aspect.ratio = 1, 
            plot.title = element_text(face = "bold", 
                                      size = 14, 
                                      margin = margin(0,0,-13,0)), 
            legend.title = element_blank(), 
            legend.position = "top", 
            legend.margin = margin(0,0,0,250),
            legend.box.spacing = unit(0.1, 'cm')) 
    
    girafe(code = print(plot))
    
  })
}


# Run the application ----
shinyApp(ui = ui, server = server)

