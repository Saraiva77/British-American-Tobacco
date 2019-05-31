#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

require(shinythemes)
require(shiny)
library(rJava)
library(xlsx)
library(lubridate)
library(stringr)
library(shinyjs)
library(readxl)
require(maps)
require(OpenStreetMap)
library(googleway)
require(leaflet)
require(leaflet.extras)
require(splitstackshape)
require(htmltools)
require(tabulizer)
require(dplyr)
require(plotrix)
require(RColorBrewer)
require(randomcoloR)

require(shinyWidgets)
require(shinycssloaders)


fluidPage(
    titlePanel("Route Analysis 1.0"),
    sidebarLayout(
        sidebarPanel(
            fileInput('file1', 'Choose file to upload',
                      accept = c(
                          'text/csv',
                          'text/comma-separated-values',
                          'text/tab-separated-values',
                          'text/plain',
                          '.csv',
                          '.tsv'
                      )
            ),
            tags$hr(),
            checkboxInput('header', 'Header', TRUE),
            radioButtons('sep', 'Separator',
                         c(Comma=',',
                           Semicolon=';',
                           Pipe="|",
                           Tab='\t'),
                         ','),
            radioButtons('quote', 'Quote',
                         c(None='',
                           'Double Quote'='"',
                           'Single Quote'="'"),
                         '"'),
            tags$hr(),
            p('For this application you will need to pile the coordinates files',
              'with the follow informations:',
              'PLACA,DATA,LAT and LONG'
            )
        ),
        mainPanel(
            tabsetPanel(
                tabPanel("Data", tableOutput('contents')),
                tabPanel("Map",leafletOutput("mymap", width=860, height=620)))
            
        )
    )
)