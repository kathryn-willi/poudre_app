library(shiny)
library(tidyverse)
library(sf)
library(leaflet)
library(shinythemes)
library(markdown)
library(bslib)
library(DT)
library(shinyWidgets)
library(plotly)
library(shinyBS)
library(ggridges)

source("prep.R")

col.list <- c('black','#0b6e8b','#0e94bb','#62BBA8','#4596B7','#40c8f0','#b7ebf9','#4B73B3','#5E4FA1')

col.years <- rev(c('black', "#095b73", "#0b6e8b", "#0d81a3", "#0e94bb", "#10a7d3", "#12baea",
                   "#28c2ee", "#40c8f0", "#58cff2", "#70d6f4", "#87ddf6", "#9fe4f8", "#b7ebf9"))

# Define UI for application that draws a histogram
ui <- navbarPage("Poudre Watershed Flows",
                 
                 tabPanel("River", fluid = TRUE,
                          
                          fluidPage(
                            
                            # theme = bslib::bs_theme(
                            #   bg = "white", fg="#353d42", primary = "#FCC780"),
                            
                            sidebarLayout(
                              sidebarPanel(
                                fluidRow(
                                  pickerInput(
                                    inputId = "river_1", 
                                    label = "Select River Sites",
                                    choices = sort(unique(data_1$Site)),
                                    selected = "Canyon Mouth",
                                    options = list('actions-box' = TRUE),
                                    multiple = TRUE))),
                              
                              # Show a plot of the flows
                              mainPanel(
                                plotlyOutput("flowPlot"))
                            ))),
                 
                 tabPanel("Explore by year",
                          fluidPage(
                            
                            # theme = bslib::bs_theme(
                            #   bg = "white", fg="#353d42", primary = "#FCC780"),
                            
                            
                            sidebarLayout(
                              sidebarPanel(
                                fluidRow(
                                  pickerInput(
                                    inputId = "river_2",
                                    label = "Select River Sites",
                                    choices = sort(unique(data_2$Site)),
                                    selected = "Canyon Mouth",
                                    options = list('actions-box' = TRUE),
                                    multiple = FALSE))),
                              
                              
                              # Show a plot years
                              mainPanel(
                                plotOutput("yearPlot"))
                            )))
)

server <- function(input, output, session) {
  
  filtered_data_1 <- reactive({
    
    if(!isTruthy(input$river_1)){data_1 <- filter(data_1,Site == "Bogus")
    }else{
      data_1 <- filter(data_1, Site %in% c(input$river_1))}
    
    data_1
    
  })
  
  output$flowPlot <- renderPlotly({
    
    plotly::plot_ly(data = filtered_data_1(), x = (~Date), y = (~q_cfs), mode = 'markers',
                    color = ~Site, colors = col.list) %>%
      add_lines()
  })
  
  filtered_data_2 <- reactive({
    
    if(!isTruthy(input$river_2)){data_2 <- filter(data_2,Site == "Bogus")
    }else{
      data_2 <- filter(data_2, Site %in% c(input$river_2)) %>%
        filter(year(Date) > 1999)}
    
    data_2
    
  })
  
  
  output$yearPlot <- renderPlot({
    
    sc <- (max(filtered_data_2()$q_cfs) / (min(filtered_data_2()$q_cfs)+1) / 10000000)
    
    ggplot(filtered_data_2(), aes(x = my, y = Year, height = q_cfs, fill = Year, group = Year)) +
      ggridges::geom_ridgeline(stat="identity", scale = 5 * sc, size = 0.5) +
      theme(axis.text.y = element_text(family="Arial Narrow", size=18),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.title = element_text(family="Arial Narrow", size=17),
            plot.title = element_text(family="Arial Narrow", size=21),
            plot.caption = element_text(color="#999999", size=12),
            legend.position = "none",
            panel.background = element_blank()) +
      scale_x_continuous(breaks = seq(1, 12, by = 1), expand=c(0.01,0)) +
      scale_y_reverse() +
      scale_fill_gradientn(colors = col.years) +
      labs(x = "Day of year", y = "Stream flow",
           title = paste0("Mean daily discharge at selected site"),
           caption = "Adapted from Lauren Steely and Madre DeZanjas")
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
