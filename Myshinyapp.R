library(dplyr)
library(ggplot2)
library(readr)
library(shiny)
library(DT)
library(lubridate)
library(gapminder)
library(stringr)
library(knitr)


# Dataprep of Sewanee utilities and weather

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

rm(list = ls()) # clear environment first
dir() # look at files in your working directory

# weather ======================================================================
load('sewanee_weather.rds') # loads 3 datasets

# dataset #1: Monthly rainfall in Sewanee, 1895 - 2023
sewanee_rain %>% head
sewanee_rain %>% tail

# dataset #2: Monthly temperature in Sewanee, 1958 - 2023
# Note some years have wonky data
sewanee_temp$year %>% unique
# So let's take those rows out
sewanee_temp <- sewanee_temp %>% filter(!is.na(as.numeric(year)))
# Now take a look
sewanee_temp %>% head
sewanee_temp %>% tail

# dataset #3: Hourly weather (air temp, soil temp, humidity, rain) from Split Creek Observatory
# Aug 18, 2018 - June 14 2022
split_creek %>% head
split_creek %>% tail

# utilities  ===================================================================
load('utilities.rds') # loads two datasets

# dataset #1: Utilities data for every campus building (water, electricity, natural gas)
# caution: many rows have missing data
utilities %>% as.data.frame %>% head
utilities %>% as.data.frame %>% tail

# dataset #2: Same data for Fall 2025, but with residence hall occupancy information added
# broken down by gender
# caution again: many rows have missing data
fall2025 %>% as.data.frame %>% head
fall2025 %>% as.data.frame %>% tail


# making month year column in utilities

utilities <- utilities %>% mutate(therms_per_person = therms/capacity)
utilities <- utilities %>% mutate(gallons_per_person = gallons/capacity)

y <-
  utilities %>% pull(year) %>% as.character()
m <- 
  utilities %>% pull(month) %>% as.character()
ym <- paste0(y, "-", m)
ymdate <- ym %>% ym %>% as_date
utilities <- utilities %>% mutate(date = ymdate)




###################################################################
###################################################################
# Define UI for application that draws a histogram

ui <- fluidPage(
  titlePanel('Think of a title'),
  p('Here is a nice little tagline about my dashboard'),
  helpText('this is helpText, a little more subtle maybe?'),
  tabsetPanel(
    tabPanel(h5('Time series'),
             fluidRow(column(4, sliderInput(inputId = 'date',
                                            label = 'Select dates',
                                            min = min(utilities$date),
                                            max = max(utilities$date),
                                            value = range(utilities$date))),
                      column(4, 
                             uiOutput('building'),
                             radioButtons(inputId = 'rank',
                                          label = 'Rank dormatories ...',
                                          choices = c('Alphabetically', 'By therms'),
                                          selected = 'Alphabetically',
                                          inline = TRUE)),
                      column(4, radioButtons(inputId = 'yvar',
                                             label = 'Select variable',
                                             choices = names(utilities)[11:21],
                                             selected = 'therms',
                                             inline = TRUE))),

             br(),
             br(),
             fluidRow(column(1),
                      column(10, plotOutput("dormplot")),
                      column(1))
    ),
    tabPanel(h5('Data viewer'),
             fluidRow(column(12, DTOutput('dt1'))))
  )
)



###################################################################
###################################################################

server <- function(input, output) {
  rv <- reactiveValues()
  rv$dorms <- utilities
  
  observe({
    if(! is.null(input$building)){
      rv$dorms <- utilities %>% filter(building %in% input$building)
    }else{
      rv$dorms <- utilities
    }
  })
  
  output$building <- renderUI({
    # Create a default: alphabetical order
    (dorms <- utilities %>% pull(building) %>% unique %>% sort)
    
    # If input$rank is NOT "Alphabetical...
    if(input$rank == 'By therms'){
      # rank them by therms
      utilities %>% head
      dorms <- utilities %>%
        group_by(building) %>%
        summarize(therms = sum(therms)) %>%
        arrange(desc(therms)) %>%
        pull(building)
    }
    
    # then build up your selectInput
    selectInput(inputId = 'building',
                label = 'Select dorms',
                multiple = TRUE,
                choices = dorms,
                selected = 'Ayres Hall')
  })
  
  output$dormplot <- renderPlot({
    print(input$building)
    if(input$building %>% is.null){
      # this is where ALL dorms are shown
      ggplot(rv$dorms,
             aes_string(x= 'date', y= input$yvar, group = 'building')) +
        geom_path() +
        xlim(input$date)
    }else{
      # this is where only selected are shown
      ggplot(rv$dorms,
             aes_string(x='date', y=input$yvar, color= 'building')) + 
        geom_path() +
        xlim(input$date)
    }
  })
  
  output$dt1 <- renderDT({ rv$dorms })
}

# Run the application
shinyApp(ui = ui, server = server)

