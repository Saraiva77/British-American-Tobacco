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
require(DT)
require(shinyWidgets)
require(shinycssloaders)
options(warn=-1)

info_roubo <- data.frame(read_excel("./data/SWS Database.xlsx",1))
info_roubo$Hora <- format(info_roubo$Hora, "%H:%M")
info_roubo$Data.Transbordo <- format(info_roubo$Data.Transbordo, "%H:%M")

####SeparaÃ§Ã£o da coluna concatenada de veiculos envolvidos
info_roubo <- cSplit(info_roubo, 'Veiculos.SC..Placa.Rota.Tipo.', sep=";", type.convert=FALSE)
roubos_RJ <- info_roubo 

placas <- as.data.frame(cbind(roubos_RJ$Veiculos.SC..Placa.Rota.Tipo._01,roubos_RJ$UF.Evento))
colnames(placas)<- c("Veiculo.Roubado","Estado")

info_colabs <- strsplit(roubos_RJ$Envolvidos..Nome.Mat.CPF.Cargo.Tipo.,";")
info_colabs <- unlist(lapply(info_colabs, `[[`, 1)) # 1 para apenas nomes dos colabs
roubos_RJ$Colabor <- info_colabs

info_primaria=roubos_RJ[,c(1:12,18)]
tabela_roubos=cbind(info_primaria,info_colabs,placas)

bootstrapPage(theme = shinytheme("united"),
navbarPage("Route Analysis 1.1",id="nav",
    tabPanel("Data",
        div(class="outer",  
            sidebarLayout(
                sidebarPanel(
                    fileInput('file1', 'Choose file to upload',
                        accept = c(
                          'text/csv',
                          'text/comma-separated-values',
                          'text/tab-separated-values',
                          'text/plain',
                          '.csv',
                          '.tsv')),
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
                        'PLACA,DATA,LAT and LONG')
                ),
        tableOutput('contents')     
        ))),
        
    tabPanel("Map",leafletOutput("mymap", width=1360, height=600)),
            
    tabPanel("Data explorer",
             # Create a new Row in the UI for selectInputs
             fluidRow(
                 column(3,
                    selectInput("uf", "UF Eventos:",unique(as.character(tabela_roubos$UF.Evento)), multiple=TRUE)
                        ),
                 column(3,
                    conditionalPanel("input.uf",
                                     selectInput("placa", "Placas:", unique(as.character(tabela_roubos$Veiculo.Roubado)), multiple=TRUE)
                    )
             )),
             hr(),
             DT::dataTableOutput("tabela")
   
     )
)
)
