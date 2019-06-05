#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)
library(shinythemes)
library(rJava)
library(xlsx)
library(readxl)
library(lubridate)
library(stringr)
require(maps)
require(OpenStreetMap)
library(googleway)
require(leaflet)
require(splitstackshape)
require(htmltools)
require(tabulizer)
require(dplyr)
require(plotrix)
require(RColorBrewer)
require(randomcoloR)
require(leaflet.extras)
require(DT)
options(shiny.maxRequestSize = 9*1024^2)
options(warn=-1)
shinyServer(function(input, output, session){
idinfo <- showNotification(paste("Wait while application is loading... This may take a while",sep=""),type= "message", duration = NULL)
  
    output$contents <- renderTable({
        inFile <- input$file1
        if (is.null(inFile))
            return(NULL)

        read.csv(inFile$datapath, header = input$header,
                 sep = input$sep, quote = input$quote)
    })
 
info_roubo <- data.frame(read_excel("./data/SWS Database.xlsx",1))
info_roubo$Data <- as.Date.POSIXct(info_roubo$Data)

####FormataÃ§Ã£o da Hora
info_roubo$Hora <- format(info_roubo$Hora, "%H:%M")
info_roubo$Data.Transbordo <- format(info_roubo$Data.Transbordo, "%H:%M")

####SeparaÃ§Ã£o da coluna concatenada de veiculos envolvidos
info_roubo <- cSplit(info_roubo, 'Veiculos.SC..Placa.Rota.Tipo.', sep=";", type.convert=FALSE)
roubos_RJ <- info_roubo 

####Limpeza da variavel de rotas
CE_code <- roubos_RJ$Veiculos.SC..Placa.Rota.Tipo._02
CE_code <- gsub(" APOIO","",CE_code)
CE_code <- gsub("FROTA","",CE_code)
CE_code <- gsub(" ","",CE_code)

placas <- as.data.frame(cbind(roubos_RJ$Veiculos.SC..Placa.Rota.Tipo._01,roubos_RJ$UF.Evento))
colnames(placas)<- c("Veiculo.Roubado","Estado")
info_colabs <- strsplit(roubos_RJ$Envolvidos..Nome.Mat.CPF.Cargo.Tipo.,";")
info_colabs <- unlist(lapply(info_colabs, `[[`, 1)) # 1 para apenas nomes dos colabs
roubos_RJ$Colabor <- info_colabs

roubos_RJ$Veiculos.SC..Placa.Rota.Tipo._02 <- CE_code
roubos_RJ$Veiculos.SC..Placa.Rota.Tipo._01 <- paste(roubos_RJ$Veiculos.SC..Placa.Rota.Tipo._01,roubos_RJ$Data,sep=" ")
roubos_RJ <<- cbind(roubos_RJ,placas$Veiculo.Roubado)

#################################################
#################################################
output$mymap <- renderLeaflet({

  inFile <- input$file1
  coords<-read.csv(inFile$datapath, header = input$header,
                   sep = input$sep, quote = input$quote)
  #coords<- read.csv("./data/CSV BANGU.csv",sep=",", header=T)

listadata<-str_split(coords$Data,"/")

dataf=data.frame(matrix(data = NA,nrow=nrow(coords),ncol=3))
for(i in 1:nrow(coords)){
    for (j in 1:3){    
      dataf[i,j]=listadata[[i]][j]
    }
}

coords$Data<-as.Date(paste(dataf$X3,dataf$X2,dataf$X1,sep="-"),"%Y-%m-%d")

depara <- paste(coords$Placa,coords$Data,sep=" ")
nomes <-unique(depara)
coords=cbind(coords,depara)
roubo_select=c()
for(i in 1:nrow(roubos_RJ)){
    if(((roubos_RJ$Veiculos.SC..Placa.Rota.Tipo._01[i] %in% nomes))){
        roubo_sel <- roubos_RJ[i,]
        roubo_select= rbind(roubo_select,roubo_sel)
    }
}

vetorcor <- c("yellow","orange","red","green","blue","purple","pink",
              "lightgreen","lightblue","peru","tomato","darkred","brown",
              "greenyellow","magenta","cyan","aquamarine","violet","darkred","brown",
              "greenyellow","magenta","cyan","aquamarine","violet")

cor=c()
for(i in 1:length(nomes)){
  cor1=color.id(vetorcor[i])[1]
  cor=rbind(cor,cor1)
}
corfim=data.frame(cbind(nomes,cor))
colnames(corfim)<- c("depara","cor")

coords<-merge(corfim,coords)
coords$Latitude<-as.numeric(sub(",", ".", coords$Latitude, fixed = TRUE))
coords$Longitude<-as.numeric(sub(",", ".", coords$Longitude, fixed = TRUE))

coords<-coords[complete.cases(coords), ]


latrota <- na.omit(as.numeric(sub(",", ".", coords$Latitude, fixed = TRUE)))
longrota <- na.omit(as.numeric(sub(",", ".", coords$Longitude, fixed = TRUE)))

intervalo2=data.frame(table(coords$depara))
intervalo= c()
intervalo= as.vector(intervalo2[,2])

df <- data.frame(lat=coords$Latitude,
                 lon=coords$Longitude ,
                 group=coords$depara,
                 col=rep(hcl.colors(length(intervalo),palette = "viridis",alpha=NULL),times=intervalo))

map3 <- leaflet() %>% addTiles() %>% addProviderTiles(providers$CartoDB.Positron) %>% addMarkers(-43.281921, -22.790117)
for( group in levels(df$group)){
  map3 <- addPolylines(map3, lng=~lon,lat=~lat,data=df[df$group==group,], color=~col,
                       label = ~group)
}

roubo_select$Latitude.Evento<-na.omit(as.numeric(sub(",", ".", roubo_select$Latitude.Evento, fixed = TRUE)))
roubo_select$Longitude.Evento<-na.omit(as.numeric(sub(",", ".", roubo_select$Longitude.Evento, fixed = TRUE)))

local_roubo = makeIcon("./data/rob-icon.png", "rob-icon@2x.png", 24, 24)

markers <- data.frame(roubo_select$ID,roubo_select$Veiculos.SC..Placa.Rota.Tipo._01,roubo_select$Colabor)
markers <- paste(roubo_select$ID,roubo_select$Veiculos.SC..Placa.Rota.Tipo._01,roubo_select$Colabor, sep="<br/>")
#local de roubo#
  map3 <- addMarkers(map3,data=roubo_select,lng =~Longitude.Evento, lat =~Latitude.Evento,icon=local_roubo,
                           popup=markers)
map3

})

removeNotification(idinfo)


observeEvent(input$uf, {
  placa_f = filter(tabela_roubos,tabela_roubos$UF.Evento %in% input$uf)
  updateSelectInput(session, "placa",
                    label = "placa",
                    choices = unique(placa_f$Veiculo.Roubado))
})


output$tabela <- DT::renderDataTable({
  df <- tabela_roubos %>%
    filter(
      is.null(input$uf) | UF.Evento %in% input$uf,
      is.null(input$placa) | Veiculo.Roubado %in% input$placa
    )
    DT::datatable(df)
})

})
