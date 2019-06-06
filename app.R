## app.R ##
library("shiny")
library("shinydashboard")
library("leaflet")
library("lubridate")
library("dplyr")
library("zoo")
library("ggplot2")
library("forecast")
library("openair") #pour afficher un calendrier qui est adapte a la pollution de l'air
source("global.R")
#palette de couleurs
library("RColorBrewer")
library("psy")
library("corrplot")
library("latticeExtra")

#################################################################################################
# 
# UI
#
#################################################################################################

ui <- dashboardPage(
  dashboardHeader(title = "UC2: Bornes IRVE"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Carte interactive", tabName = "carto", icon = icon("map")),
      selectInput(inputId = "ville",
                  label = paste("Choisissez la ville (",nbVilles,")",""),
                  choice = listVilles),
      selectInput(inputId = "station",
                  label = paste("Choisissez la station (",nbstations,")",""),
                  choice = listStations),
      menuItem("Historique 2018", tabName = "histofacet", icon = icon("th")),
      menuItem("Histogrammes", tabName = "histo", icon = icon("th"))
    )
  ),
  dashboardBody(
    
    tabItems(
      # First tab content : carto
      tabItem(tabName = "carto",
              fluidRow(
                box(title="Carte interactive des bornes IRVE", width=12,status="primary",solidHeader=TRUE,
                  leafletOutput("map",height=500),
                  column(4,selectInput(inputId = "legend",
                                       label = h4("choix legende"),
                                       choice = c("TypeCharge","Consommationkwh","DureeChargemin","nbTransaction"))),
                  column(4,checkboxInput("quantile", "Quantile", FALSE)),
                  column(12,uiOutput("locationid"))
                )
               
              )
      ),      

      # Tab Content Historique 2018 : histofacet
      tabItem(tabName = "histofacet",
              fluidRow(
                column(4,selectInput(inputId = "varhist18",
                                     label = h4("choix variable"),
                                     choice = c("nbTransaction","Consommationkwh","DureeChargemin"))
                ),
                column(4,selectInput(inputId = "temps",
                                     label = h4("choix Mois / Semaine"),
                                     choice = c("mois","semaine"))
                ),
                column(12,textOutput("villesbornes")),
                box(title="Historique Annee 2018",status="primary",solidHeader=TRUE,width=12,
                    plotOutput("ggplotfacet",height = 1500)
                ),
                box(title="Historique Annee 2018, echelle y fixe",status="primary",solidHeader=TRUE,width=12,
                    plotOutput("ggplotfacet2",height = 1500)
                )
              )
      ),
      # Tab content Histogramme pour histo
      tabItem(tabName = "histo",
              fluidRow(
                column(4,selectInput(inputId = "varhisto",
                                     label = h4("choix variable"),
                                     choice = c("Consommationkwh","DureeChargemin","nbTransaction"))
                       ),
                column(4,selectInput(inputId = "annee",
                                      label = h4("choix annee"),
                                      choice = c("2017-2018","2018","2017"))
                        ),
                column(4,sliderInput(inputId = "nombre",
                                     label = h4("Nombre de bornes"),1,nbornes,30)
                 ),
                box(title="Saisonnalite mensuelle",status="primary",solidHeader=TRUE,width=12,
                    plotOutput("visuseasonal")
                ),
                box(title="Boxplot d'utilisation des bornes par mois",status="primary",solidHeader=TRUE,
                    plotOutput("ggboxpermois")
                ),
                box(title="Utilisation des bornes par mois",status="primary",solidHeader=TRUE,
                    plotOutput("ggpermois")
                ),
                box(title="Boxplot d'utilisation des bornes par type de jour",status="primary",solidHeader=TRUE,
                    plotOutput("ggboxperjour")
                ),
                box(title="Utilisation des bornes par type de jour",status="primary",solidHeader=TRUE,
                    plotOutput("ggperjour")
                ),
                box(title="Boxplot d'utilisation des bornes par heure",status="primary",solidHeader=TRUE,
                    plotOutput("ggboxperheure")
                ),
                box(title="Utilisation des bornes par heure",status="primary",solidHeader=TRUE,
                    plotOutput("ggperheure")
                )
            )
      )
    )
  )
)

#################################################################################################
# 
# SERVER
#
#################################################################################################

server <- function(input, output) {

  # Les bornes
  data <- reactive({
    x <- df
  })
  
  # les transactions
  trans <- reactive({
    t <- transactions
  })
  #les reservations
  output$resaggplot <- renderPlot ({
    #Calendrier
    calendarPlot(resa05, pollutant="nbResa",year = 2018,
                 month=6:12, cols=c("green","blue","orange","red"))
  })

  # CLIC sur la carte => PDC
  # click sur la carte => recuperation de la Borne PDC
  pdc <- reactive({
    validate(
      need(!is.null(input$map_marker_click), "Please select a location from the map above")
    )
    input$map_marker_click$id
  })
  ville <- reactive({
    validate(
      need(!is.null(input$map_marker_click), "Please select a location from the map above")
    )
    trans <- select(filter(df, CodePDC==pdc()),c("Ville"))
    return (trans[1]$Ville)
  })
  #### 6/12 ESSAI : click sur le bouton => recuperation de la Borne PDC
  pdc1 <- eventReactive(input$button_click, {
    input$map_marker_click$id
  })
  #output$locationid1 <- renderText({paste("Location Selected using popup select button resa click:", resa())})
  output$locationid <- renderUI({
    h4(paste("Vous avez choisi le point de charge :", pdc(), "de la ville", ville()))
    })

  
  ############################################################################################################
  ## VISU contextuelles de la cartographie => carto
  ############################################################################################################
  colorvar <- eventReactive(
    {
      input$variable
    }, 
    {
      if(input$variable == "Consommationkwh"){
        col <- "#589482"
      } else if(input$variable == "DureeChargemin"){
        col <- "#8C2423"
      } else if(input$variable == "nbTransaction"){
        col <- "#A6B06D"
      }
      return (col)
    })
  unitevar <- eventReactive(
    {
      input$variable
    }, 
    {
      if(input$variable == "Consommationkwh"){
        unite <- "kwh"
      } else if(input$variable == "DureeChargemin"){
        unite <- "minutes"
      } else if(input$variable == "nbTransaction"){
        unite <- "nombre"
      }
      return (unite)
    })
  #Utilisation des bornes par Mois
  output$histopermois <- renderPlot({
    input$variable
    datapdc <- filter(transactionsPerMoisPerBorne,Borne==pdc())
    title = paste(pdc()," : ",input$variable," : consolidation mois", "2017-2018","")
    ggplot(datapdc, aes_string("MoisFactorDebut",input$variable,group="MoisFactorDebut")) + 
      geom_col(fill=colorvar())+ labs(title = title, x = "mois", y=unitevar())
  })
  #Utilisation des bornes par Jour
  output$histoperjour <- renderPlot({
    input$variable
    datapdc <- filter(transactionsPerJour,Borne==pdc())
    title = paste(pdc()," : ",input$variable," : consolidation jour", "2017-2018","")
    ggplot(datapdc, aes_string("typeJourFactorDebut",input$variable,group="typeJourFactorDebut")) + 
      geom_col(fill=colorvar())+ labs(title = title, x = "Type de jour", y=unitevar())
  })
  #Utilisation des bornes par heure
  output$histoperheure <- renderPlot({
    input$variable
    datapdc <- filter(transactionsPerHeure,Borne==pdc())
    title = paste(pdc()," : ",input$variable," : consolidation heure", "2017-2018","")
    ggplot(datapdc, aes_string("HHFactor",input$variable,group="HHFactor")) + 
      geom_col(fill=colorvar()) + labs(title = title, x = "Heure", y=unitevar())
  })

  #PLOT mois
  output$histomois <- renderPlot({
    #PLOT mois
    datapdc <- filter(transactionsPerMoisPerBorne,Borne==pdc())
    title = paste(pdc()," : ",input$variable," : consolidation mois", input$annee,"")
    xlabtext = paste("mois annee(s)", "2017/2018","")
    plot(datapdc[["MonthDebut"]],datapdc[["Consommationkwh"]], type ="h",ylab="kwh", xlab=xlabtext, main=title,col="blue")
  })
  #HISTORIQUE BORNE : historique annee 2018 avec 3 variables 
  output$transbornemois <- renderPlot({
    #PLOT mois
    databorne <- filter(transactionsPerMoisPerBorne2018,Borne==pdc())
    title = paste("Nombre de transactions",pdc())
    p <-ggplot(databorne, aes_string("MonthDebut","nbTransaction")) +
      geom_col(colour="red",fill="orange")+
      labs(title = title, x = "Mois", y="nombre")
    p #limits=c("1","5","10","15","20","25","30","35","40","45","50","60"))
  })
  output$consobornemois <- renderPlot({
    #PLOT mois
    databorne <- filter(transactionsPerMoisPerBorne2018,Borne==pdc())
    title = paste("Consommation",pdc())#,nrow(databorne))
    ggplot(databorne, aes_string("MonthDebut","Consommationkwh")) +
      geom_col(colour="blue",fill="turquoise")+
      labs(title = title, x = "Mois", y="kwh")
  })
  output$chargebornemois <- renderPlot({
    #PLOT mois
    databorne <- filter(transactionsPerMoisPerBorne2018,Borne==pdc())
    title = paste("Temps d'utilisation",pdc())#,nrow(databorne))
    ggplot(databorne, aes_string("MonthDebut","DureeChargemin")) +
      geom_col(colour="purple",fill="pink")+
      labs(title = title, x = "Mois", y="minutes")
  })
  #HISTORIQUE VILLE : historique annee 2018 avec 3 variables 
  output$transvillemois <- renderPlot({
    #PLOT mois
    dataville <- filter(transactionsPerMoisPerVille2018,Ville==ville())
    title = paste("Nombre de transactions",ville())#,nrow(dataville))
    ggplot(dataville, aes_string("MonthDebut","nbTransaction")) +
      geom_col(colour="red",fill="orange")+
      labs(title = title, x = "Mois", y="nombre")
  })
  output$consovillemois <- renderPlot({
    #PLOT mois
    dataville <- filter(transactionsPerMoisPerVille2018,Ville==ville())
    title = paste("Consommation :",ville())#,nrow(dataville))
    ggplot(dataville, aes_string("MonthDebut","Consommationkwh")) +
      geom_col(colour="blue",fill="turquoise")+
      labs(title = title, x = "Mois", y="kwh")
  })
  output$chargevillemois <- renderPlot({
    #PLOT mois
    dataville <- filter(transactionsPerMoisPerVille2018,Ville==ville())
    title = paste("Temps d'utilisation",ville())#,nrow(dataville))
    ggplot(dataville, aes_string("MonthDebut","DureeChargemin")) +
      geom_col(colour="purple",fill="pink")+
      labs(title = title, x = "Mois", y="minutes")
  })
  ############################################################################################################
  ## Ajout de couleur au cercle :
  ##   rouge pour RAPIDE
  ##   bleu pour ACCELERE
  ##
  ## Ajout de couleur sur la borne en fonction de l'ecart a la moyenne : mois d'octobre
  ## par quantile : 1er quantile vert, 2eme bleu, 3eme turquoise, 4eme jaune
  ##
  ############################################################################################################
  output$map <- renderLeaflet({
      df <- data()
      
      # Ajout de couleur au cercle :
      #   rouge pour RAPIDE
      #   bleu pour ACCELERE
      pal = leaflet::colorFactor(palette=c("blue","red"), domain=df$Type)
      
      m <- leaflet(data = df) %>%
        addTiles() %>%
        addLegend(position="topleft",pal=pal, values= ~Type,title = "Type de charge",opacity=2) %>%
        addCircleMarkers(layerId= ~CodePDC,
                         lng = ~lon,lat = ~lat,
                         color = ~ pal(Type),
                         label = paste(df$Commune,":",df$Station),
                         labelOptions = labelOptions(noHide=F,direction='top', textsize='15px'),
                         #clusterOptions = markerClusterOptions(),
                         # Station;Commune;Adresse;Identifiant;Type;StationCode;Connecteur;ChargeBoxIdentity;CodePDC;PDL_IDC;
                         # INT Numero;CodeInsee
                         popup = ~paste("<b>","Commune :","</b>",as.character(df$Commune),"</br>",
                                        "<b>","Nom de la Station :","</b>",as.character(df$Station),"</br>",
                                        "<b>","Code PDC :","</b>",as.character(df$CodePDC),"</br>",
                                        "<b>","ID station :","</b>",as.character(df$Identifiant),"</br>","</br>",
                                        "<b>","Code Station :","</b>",as.character(df$StationCode),"</br>",
                                        "<b>","Connecteur :","</b>",as.character(df$Connecteur),"</br>",
                                        "<b>","Type :","</b>",as.character(df$Type),"</br>",
                                        "<b>","Adresse de la Station :","</b>",as.character(df$Adresse),"</br>","</br>",
                                        "<b>","ChargeBoxIdentity :","</b>",as.character(df$ChargeBoxIdentity),"</br>",
                                        "<b>","PDL ou IDC :","</b>",as.character(df$PDL_IDC),"</br>","</br>",
                                        "<b>","Numero :","</b>",as.character(df$Numero),"</br>",
                                        "<b>","Code INSEE :","</b>",as.character(df$CodeInsee),"</br>")
        )
      m
    })
  # Use a separate observer to recreate the legend as needed.
  observe({
    input$legend
    input$quantile
    proxy <- leafletProxy("map", data = df)
    # Remove any existing legend, and only if the legend is enabled, create a new one.
    proxy %>% clearMarkers() %>% clearControls() %>% clearShapes() 
    if (input$legend=="TypeCharge") {
      pal = leaflet::colorFactor(palette=c("blue","red"), domain=df$Type)
      proxy %>% addLegend(position = "topleft",
                          pal = pal, values = ~Type,title = "Type de borne",opacity=2)
      colorpal=~pal(Type)
      val=~Type
      cercle=10
    }
    else if (input$legend=="Consommationkwh")
    {
      if(input$quantile == TRUE)
      {
        pal <- leaflet::colorQuantile(
          palette = "Blues", 
          na.color = "#808080",
          domain = df$EcartConso)
      }
      else {
        pal <- leaflet::colorNumeric(
          palette = "Blues", 
          na.color = "#808080",
          domain = df$EcartConso)
      }
      proxy %>% addLegend(position = "topleft",
                         pal = pal, values = ~EcartConso,title = "Ecart annuel : Consommation en kwh",opacity=2)
      colorpal=~pal(EcartConso)
      val=~EcartConso
      cercle=~sqrt(Consommationkwh)
    }
    else if (input$legend=="nbTransaction")
    {
      if(input$quantile == TRUE)
      {
        pal <- leaflet::colorQuantile(
          palette = "Oranges", 
          domain = df$EcartTrans)
      }
      else
      {
        pal <- leaflet::colorNumeric(
          palette = "Oranges", 
          domain = df$EcartTrans)
      }
      proxy %>% addLegend(position = "topleft",
                          pal = pal, values = ~EcartTrans,title = "Ecart annuel : nombre de transactions",opacity=2)
      colorpal=~pal(EcartTrans)
      val=~EcartTrans
      cercle=~sqrt(nbTransaction)*2
    }
    else if (input$legend=="DureeChargemin")
    {
      if(input$quantile == TRUE)
      {
        pal <- leaflet::colorQuantile(
          palette = "RdYlBu", 
          na.color = "#808080",
          domain = df$EcartDuree)
      }
      else
      { 
        pal <- leaflet::colorNumeric(
          palette = "RdYlBu", #Blues, Greens, Reds, Oranges, RdYlBu
          na.color = "#808080",
          domain = df$EcartDuree)
      }
      proxy %>% addLegend(position = "topleft",
                          pal = pal, values = ~EcartDuree,title = "Ecart annuel :Duree en minutes",opacity=2)
      colorpal=~pal(EcartDuree)
      val=~EcartDuree
      cercle=~sqrt(DureeChargemin)/4
    }
    # Remove any existing circle
    proxy %>% addCircleMarkers(layerId= ~CodePDC,
                       lng = ~lon,lat = ~lat, fillOpacity=0.5,
                       radius = cercle,
                       color = colorpal, #Type
                       label = paste(df$Commune,":",df$Station), #Station
                       popup = ~paste("<b>","Commune :","</b>",as.character(df$Commune),"</br>",
                                     "<b>","Nom de la Station :","</b>",as.character(df$Station),"</br>",
                                      "<b>","Code PDC :","</b>",as.character(df$CodePDC),"</br>",
                                      "<b>","ID station :","</b>",as.character(df$Identifiant),"</br>","</br>",
                                      "<b>","Code Station :","</b>",as.character(df$StationCode),"</br>",
                                      "<b>","Connecteur :","</b>",as.character(df$Connecteur),"</br>",
                                      "<b>","Type :","</b>",as.character(df$Type),"</br>",
                                      "<b>","Adresse de la Station :","</b>",as.character(df$Adresse),"</br>","</br>",
                                      "<b>","ChargeBoxIdentity :","</b>",as.character(df$ChargeBoxIdentity),"</br>",
                                      "<b>","PDL ou IDC :","</b>",as.character(df$PDL_IDC),"</br>","</br>",
                                      "<b>","Numero :","</b>",as.character(df$Numero),"</br>",
                                      "<b>","Code INSEE :","</b>",as.character(df$CodeInsee),"</br>","</br>",
                                      "<b>","2018: ","</br>",
                                      "<b>","       Nombre de Transactions : ","</b>",as.character(df$nbTransaction),"</br>",
                                      "<b>","Ecart Nb Transactions : ","</b>",as.character(df$EcartTrans),"</br>",
                                      "<b>","Consommation : ","</b>",as.character(df$Consommationkwh),"kwh","</br>",
                                      "<b>","Ecart Consommation : ","</b>",as.character(df$EcartConso),"</br>",
                                      "<b>","Duree de charge : ","</b>",as.character(df$DureeChargemin),"minutes","</br>",
                                      "<b>","Ecart Duree de charge : ","</b>",as.character(df$EcartDuree),"</br>")
                        )
  })

  # A REUTILISER rectangle vert autour d'une ville
  #add a rectangle to display the selected City
  observe({
    input$ville
    laVille<-filter(df,Ville==input$ville)
    laVille <- laVille[1,]

    proxy <- leafletProxy("map", data = df)
    # Remove any existing legend, and only if the legend is enabled, create a new one.
    proxy %>% clearShapes() %>%
      addRectangles(
        lng1=laVille$lon-0.03, lat1=laVille$lat-0.03,
        lng2=laVille$lon+0.03, lat2=laVille$lat+0.03,
        color="green",
        fill = FALSE
        #highlightOptions = highlightOptions(color = "grey", weight = 2,
        #                                                 bringToFront = TRUE)

      )
  })
  #add a rectangle to display the selected station
  observe({
    input$station
    laStation<-filter(df,Station==input$station)
    laStation <- laStation[1,]
    
    proxy <- leafletProxy("map", data = df)
    # Remove any existing legend, and only if the legend is enabled, create a new one.
    proxy %>% clearShapes() %>%  
      addRectangles(
        lng1=laStation$lon-0.02, lat1=laStation$lat-0.02,
        lng2=laStation$lon+0.02, lat2=laStation$lat+0.02,
        color="green",
        fill = FALSE,
        highlightOptions = highlightOptions(color = "grey", weight = 2,
        bringToFront = TRUE)
      )
  })

  ###########################################################################################
  ## VISU FACET VILLES / BORNES => histofacet
  output$villesbornes <- renderText({
    "Les villes qui ont plusieurs bornes sont les suivantes : 
    Embrun, Tallard, Guillestre, Laragne-Monteglin, Savines-Le-Lac, L'Argentiere-La-Bessee,
    Le Devoluy, Le Monetier-Les-Bains, Les Orres, St Firmin, Veynes"
  })
  
  unite18 <- eventReactive(
    {
      input$varhist18
    }, 
    {
      if(input$varhist18 == "Consommationkwh"){
        unite <- "kwh"
      } else if(input$varhist18 == "DureeChargemin"){
        unite <- "minutes"
      } else if(input$varhist18 == "nbTransaction"){
        unite <- "nombre"
      }
      return (unite)
    })
  tempshist18 <- eventReactive(
    {
      input$temps
    },
    {
      if(input$temps == "mois")
      {
        vartemps <- "MonthDebut"
      }
      else
      {
        vartemps <- "week"
      }
      return (vartemps)
    })
  # Nb transaction : colour="red",fill="orange"
  # Consommation : colour="blue",fill="turquoise"
  # Charge : colour="purple",fill="pink"
  colorfill <- eventReactive(
    {
      input$varhist18
    }, 
    {
      if(input$varhist18 == "Consommationkwh"){
        col <- "turquoise"
      } else if(input$varhist18 == "DureeChargemin"){
        col <- "red"
      } else if(input$varhist18 == "nbTransaction"){
        col <- "orange"
      }
      return (col)
  })
  datahisto18 <- eventReactive(
    {
      input$temps
    },
    {
      if(input$temps == "mois")
      {
          data <- transactionsPerMoisPerVille2018
      }
      else
      {
          data <- transactionsPerSemainePerVille2018
      }
      return (data)
    })
  # Visualisation de toutes les VILLES
  # echelle Y adaptee 
  output$ggplotfacet <- renderPlot({
    input$varhist18 # nbTransaction - Consommationkwh - DureeChargemin

    input$temps #Mois ou Semaine
    vtitle = paste(input$varhist18,": consolidation",input$temps,"par Ville")
    #facet_wrap(~ Borne,scales="free_y",ncol=5) l'echelle est adaptee a chaque wrap
 
      # VISU VILLE
      ggplot(datahisto18(), aes_string(tempshist18(),input$varhist18)) +
        geom_col(colour="black",fill=colorfill())+ 
        facet_wrap(~ Ville,ncol=6,scales="free_y") +
        labs(title = vtitle, x = input$temps, y=unite18())
  })
  # Visualisation de toutes les VILLES
  # echelle Y FIXE 
  output$ggplotfacet2 <- renderPlot({
    input$varhist18 # nbTransaction - Consommationkwh - DureeChargemin
    input$temps #Mois ou Semaine
    
    vtitle = paste(input$varhist18,": consolidation",input$temps,"par Ville")
    #facet_wrap(~ Borne,scales="free_y",ncol=5) l'Ã©chelle est adaptee a chaque wrap
      # VISU VILLE
      ggplot(datahisto18(), aes_string(tempshist18(),input$varhist18)) +
        geom_col(colour="black",fill=colorfill())+ 
        facet_wrap(~ Ville,ncol=6) +
        labs(title = vtitle, x = input$temps, y=unite18())
    
  })
  
  ###########################################################################################
  ## VISU historique => histo
  ###########################################################################################
  unitevarhisto <- eventReactive(
    {
      input$varhisto
    }, 
    {
      if(input$varhisto == "Consommationkwh"){
        unite <- "kwh"
      } else if(input$varhisto == "DureeChargemin"){
        unite <- "minutes"
      } else if(input$varhisto == "nbTransaction"){
        unite <- "nombre"
      }
      return (unite)
    })
  # Nb transaction : couleur #A6B06D ou #98DB9C
  # Consommation : couleur #589482
  # Charge : couleur #8C2423
  colorvarhisto <- eventReactive(
    {
      input$varhisto
    }, 
    {
      if(input$varhisto == "Consommationkwh"){
        col <- "#589482"
      } else if(input$varhisto == "DureeChargemin"){
        col <- "#8C2423"
      } else if(input$varhisto == "nbTransaction"){
        col <- "#A6B06D"
      }
      return (col)
    })
  datavarmois <- eventReactive(
    {
      input$annee
      input$nombre
    }, 
    {
      if(input$annee == 2017){
        data <- transactionsPerMoisPerBorne2017
      } else if(input$annee == 2018){
        data <- transactionsPerMoisPerBorne2018
      } else {
        data <- transactionsPerMoisPerBorne
      }
      if (input$nombre <= length(data$Borne))
      {
        listBornes <- listBornes[1:input$nombre]
        data <- filter(data, Borne %in% listBornes)
      }
      return (data)
    })
  datavarjour <- eventReactive(
    {
      input$annee
      input$nombre
    }, 
    {
      if(input$annee == 2017){
        data <- transactionsPerJour2017
      } else if(input$annee == 2018){
        data <- transactionsPerJour2018
      } else {
        data <- transactionsPerJour
      }
      if (input$nombre <= length(data$Borne))
      {
        listBornes <- listBornes[1:input$nombre]
        data <- filter(data, Borne %in% listBornes)
      }
      return (data)
    })
  datavarheure <- eventReactive(
    {
      input$annee
      input$nombre
    }, 
    {
      if(input$annee == 2017){
        data <- transactionsPerHeure2017
      } else if(input$annee == 2018){
        data <- transactionsPerHeure2018
      } else {
        data <- transactionsPerHeure
      }
      if (input$nombre <= length(data$Borne))
      {
        listBornes <- listBornes[1:input$nombre]
        data <- filter(data, Borne %in% listBornes)
      }
      return (data)
    })
  #VISU SEASONAL
  output$visuseasonal <- renderPlot({
    input$varhisto
    
    #create a ts object
    tsMois <- ts(transactionsPerMois[[input$varhisto]], start=c(2017, 1), end=c(2018, 11), frequency=12)
    axey <- paste(input$varhisto,"en",unitevarhisto(),"Toutes les bornes","")
    visu<-seasonplot(tsMois,year.labels =TRUE,col=1:6, year.labels.left=TRUE, ylab=unitevarhisto() ,xlab="mois de janvier 2017 - novembre 2018",main=axey)
    return (visu)
  })
  #PLOT jour
  output$plotperjour <- renderPlot({
    input$varhisto
    input$annee
    title = paste(input$varhisto," : consolidation jour",input$annee,"")
    xlabtext = paste("jours annee(s)", input$annee,"")
    plot(datavarjour()[["DateDebut"]],datavarjour()[[input$varhisto]], type ="h", ylab=unitevarhisto(), xlab=xlabtext, main=title,col=colorvarhisto())
   })
  #PLOT mois
  output$plotpermois <- renderPlot({
    input$varhisto
    input$annee
    title = paste(input$varhisto," : consolidation mois", input$annee,"")
    xlabtext = paste("mois annee(s)", input$annee,"")
    plot(datavarmois()[["MonthDebut"]],datavarmois()[[input$varhisto]], type ="h",ylab=unitevarhisto(), xlab=xlabtext, main=title,col=colorvarhisto())
    #plot(transactionsPerMois[["MonthDebut"]],transactionsPerMois[[input$varhisto]], type ="h",ylab=unitevarhisto(), xlab=xlabtext, main=title,col=colorvar())
  })
  #Utilisation des bornes par Mois
  output$ggpermois <- renderPlot({
    input$varhisto
    input$annee
    title = paste(input$varhisto," : consolidation mois", input$annee,"")
    ggplot(datavarmois(), aes_string("MoisFactorDebut",input$varhisto,group="MoisFactorDebut")) + 
        geom_col(fill=colorvarhisto())+ labs(title = title, x = "mois", y=unitevarhisto())
  })
  #Boxplot d'utilisation des bornes par Mois
  output$ggboxpermois <- renderPlot({
    input$varhisto
    input$annee
    title = paste(input$varhisto," : consolidation mois", input$annee,"")
    ggplot(datavarmois(), aes_string("MoisFactorDebut",input$varhisto,group="MoisFactorDebut")) + 
        geom_boxplot(fill=colorvarhisto())+ labs(title = title, x = "mois", y=unitevarhisto()) +
        stat_summary(fun.y=mean, geom="point", shape=1, size=2,color="#191919")
  })
  #Utilisation des bornes par Jour
  output$ggperjour <- renderPlot({
    input$varhisto
    input$annee
    title = paste(input$varhisto," : consolidation jour", input$annee,"")
    ggplot(datavarjour(), aes_string("typeJourFactorDebut",input$varhisto,group="typeJourFactorDebut")) + 
      geom_col(fill=colorvarhisto())+ labs(title = title, x = "Type de jour", y=unitevarhisto())
  })
  #Boxplot d'utilisation des bornes par Jour
  output$ggboxperjour <- renderPlot({
    input$varhisto
    input$annee
    title = paste(input$varhisto," : consolidation jour", input$annee,"")
    ggplot(datavarjour(), aes_string("typeJourFactorDebut",input$varhisto,group="typeJourFactorDebut")) + 
      geom_boxplot(fill=colorvarhisto())+ labs(title = title, x = "Type de jour", y=unitevarhisto()) +
      stat_summary(fun.y=mean, geom="point", shape=1, size=2,color="#191919")
  })
  #Utilisation des bornes par heure
  output$ggperheure <- renderPlot({
    input$varhisto
    title = paste(input$varhisto," : consolidation heure", input$annee,"")
    ggplot(datavarheure(), aes_string("HHFactor",input$varhisto,group="HHFactor")) + 
      geom_col(fill=colorvarhisto()) + labs(title = title, x = "Heure", y=unitevarhisto())
  })
  #Boxplot d'utilisation des bornes par heure
  output$ggboxperheure <- renderPlot({
    input$varhisto
    title = paste(input$varhisto," : consolidation heure", input$annee,"")
    ggplot(datavarheure(), aes_string("HHFactor",input$varhisto,group="HHFactor")) + 
      geom_boxplot(fill=colorvarhisto()) + labs(title = title, x = "Heure", y=unitevarhisto())+
      stat_summary(fun.y=mean, geom="point", shape=23, size=1,color="#191919")
  })
}
shinyApp(ui, server)