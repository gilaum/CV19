# Tom Alig
# April 3, 2020
# Shiny App Covid Data

#library(plyr)
library(tidyverse)
library(lubridate)
library(stringr)
library(scales)
library(mgcv)
#library(flexdashboard)
library(DT)
library(here)
library(shiny)
#library(shinydashboard)
library(hms)
library(ggridges)
#library(gganimate)
library(plotly)

source("dataset2.R")

ui <- fluidPage(
  
  fluidRow(
    
    valueBox(value = "ww_conf",
             subtitle = "Worldwide Confirmed",
             icon = "fa-user-md",
             color = clr.ww.conf),
    
    valueBox(value = "ww_death",
             subtitle = "Worldwide Deaths",
             icon = "fa-dizzy",
             color = clr.ww.dead),
    
    valueBox(value = "us_conf",
             subtitle = "U.S. Confirmed",
             icon = "fa-user-md",
             color = clr.us.conf),
    
    valueBox(value = "us_death",
             subtitle = "U.S. Deaths",
             icon = "fa-dizzy",
             color = clr.us.dead)
  ),
    

    
  navbarPage("COVID-19",
             tabPanel("Worldwide Totals",
                        h3(textOutput("RawNumbers")),
                         br(),
                mainPanel(
                         plotOutput("ww_raw_graph"),
                         br(),
                         br(),
                         plotOutput("ww_raw_death_graph"),
                        # h3(textOutput("PerMillionPop")),
                         br()
                  ) #end main panel        
                ), # end tabPanel Worldwide Totals
             
             tabPanel("Worldwide Per Million",
                      h3(textOutput("ww_mill_text")),
                      br(),
                      plotOutput("ww_mill_graph"),
                      br(),
                      br(),
                      plotOutput("ww_mill_death_graph")
             ), # end tabPanel Worldwide Per Million
                      
                      
                
                           tabPanel("U.S. Totals",
                                    h3(textOutput("RawNumbers2")),
                                    br(),
                                    plotOutput("us_raw_graph"),
                                    #h3(textOutput("SelectedStatesData")),
                                    br(),
                                    br(),
                                    plotOutput("us_raw_death_graph")
                           ), # end tabPanel Selected U.S. States
             
             tabPanel("U.S. Per Million",
                      h3(textOutput("us_mill_text")),
                      br(),
                      plotOutput("us_mill_graph"),
                      #h3(textOutput("SelectedStatesData")),
                      br(),
                      br(),
                      plotOutput("us_mill_death_graph")
             ) # end tabPanel Selected U.S. States
             
             
                ) #end navbarPage
    ) # end fluidPage
#  ) # end dashboardBody
 #               ) # end Fluid Page
                
                
server <- (function(input, output, session) {
  
  ##########################  
  
  # /Date Range Output
  ##########################  ##########################  
  
  
  output$RawNumbers <- renderText({
    ("Selected Countries"
    )
  })
  
  output$PerMillionPop <- renderText({
    ("Selected Countries"
    )
  })
  
  output$ww_mill_text <- renderText({
    ("Selected Countries"
    )
  })
  
  output$RawNumbers2 <- renderText({
    ("Selected States"
    )
  })
  
  output$us_mill_text <- renderText({
    ("Selected States"
    )
  })
  
  
  output$us_raw_graph <- renderPlot({
    graph.conf.us.raw
  })
  
  output$us_mill_graph<- renderPlot({
    graph.conf.us.mil
  })
  
  output$ww_raw_graph <- renderPlot({
    graph.conf.ww.raw
  })
  
  output$ww_raw_death_graph <- renderPlot({
    graph.death.ww.raw
  })
  
  output$us_raw_death_graph <- renderPlot({
    graph.death.us.raw
  })
  
  output$ww_mill_death_graph <- renderPlot({
    graph.death.ww.mil
  })
  
  output$us_mill_death_graph <- renderPlot({
    graph.death.us.mil
  })
  
  output$ww_mill_graph<- renderPlot({
    graph.conf.ww.mil
  })
  
 # output$ww_conf <- renderValueBox({
#    valueBox(curr.ww.counts$tot.conf,
#          icon = icon('fa-user-md'), 
#          color = clr.ww.conf)
#  })
  
 # output$ww_death <- renderValueBox({
#    valueBox(curr.ww.counts$tot.deaths,
#          icon = icon('fa-dizzy'), 
#          color = clr.ww.dead)
#  })

  
 # output$us_conf <- renderValueBox({
#    valueBox(curr.us.counts$tot.conf,
#          icon = icon('fa-user-md'), 
#          color = clr.us.conf)
#  })
  
 # output$us_death <- renderValueBox({
#    valueBox(curr.us.counts$tot.deaths,
#          icon = icon('fa-dizzy'), 
#          color = clr.us.dead)
#  })
  
  
    output$ww_conf<- renderText({ 
      format(curr.ww.counts$tot.conf, big.mark = ",")
    })
  
    output$ww_death<- renderText({ 
      paste(format(curr.ww.counts$tot.deaths, big.mark = ","), 
            " (", curr.ww.death.rate, "%)",
      sep = (""))
    })
  
    output$us_conf<- renderText({ 
      format(curr.us.counts$tot.conf, big.mark = ",")
    })
    
    output$us_death<- renderText({ 
      paste(format(curr.us.counts$tot.deaths, big.mark = ","), 
            " (", curr.us.death.rate, "%)",
            sep = "")
    })
    
  }) # end Server

shinyApp(ui = ui, server = server)

