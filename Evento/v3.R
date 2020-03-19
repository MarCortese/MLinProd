#versione funzionan
library(shinythemes)
library(shiny)
library(stringr)
library(plotly)
library(tidyr)
library(ape)
library(ggdendro)
source("reg.R")
ui <- fluidPage(
  theme=shinytheme("lumen"),
  themeSelector(),
  navbarPage(title="",
             tabPanel("Home",
                      h2(),
                      h2("PORTARE IL ML IN PRODUZIONE CON SHINY" ,align="center"),
                      tags$img(src="data:image/png;base64,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",height="100", width="100",align="right"),
                      tags$img(src="https://www.sccpre.cat/mypng/full/34-343872_r-shiny-logo-png.png",height="50", width="50"),
                      tags$img(src="https://www.rstudio.com/wp-content/uploads/2014/06/RStudio-Ball.png",height="50", width="50"),
                      h3("Ciao a tutti.Sono Marco Cortese Data Scientist in Healthy Reply SpA e oggi parleremo di come creare una web app interattiva che consente mettere in produzione un modello di ML scritto in codice R attraverso il package Shiny.",align="justify"),
                      tags$img(src="https://scontent-mxp1-1.xx.fbcdn.net/v/t1.0-9/69936177_2763676243654354_2428608864048906240_o.jpg?_nc_cat=106&_nc_oc=AQlT1euVQxbgQFP9fLQE9GGd2WRB47Cz0hatjWEMAjU3gG0bDTlTFjiwxiHDGuo8Nr4&_nc_ht=scontent-mxp1-1.xx&oh=4e5508c0828c199bfa901c272501e362&oe=5E110C75",height="400", width="800",style="display: block; margin-left: auto; margin-right: auto;"),
                      wellPanel(
                        helpText(   a("Profilo linkedin",     href="https://it.linkedin.com/in/marco-cortese-6b0375107")),
                        helpText(   a("Visita il sito 'Reply'",     href="https://www.reply.com/it/")
                        )
                      )
             ),#tabHome
             tabPanel("Esplorazione dati",
                      h5(),
                      h1("Tabella"),
                      h2("Il dataset utilizzato per questa demo risulta essere caratterizzato dalle seguenti dimensioni:"),
                      verbatimTextOutput("dim"),
                      h2("Mostriamo un estratto delle variabili presenti"),
                      verbatimTextOutput("names"),
                      h2(),
                      verbatimTextOutput("summary"),
                      h3("Valutiamo la distribuzioni delle variabili:"),
                      #nome variabile
                      h2("Provenienza geografica:"),
                      plotlyOutput("plot_Veneto",height = 500, width=700),
                      h2("Analizzando maggiormente nel dettaglio geografico delle provenienze extra Veneto:"),
                      plotlyOutput("plot_Altro",height = 700, width=1300),
                      h2("Distribuzione Eta per Genere:"),
                      plotlyOutput("plot_box", height = 400, width=500),
                      hr(),
                      h2("Le prestazioni sono suddivise su 4 livelli di priorita:"),
                      plotlyOutput("plot_Prio",height = 700, width=900),
                      h2("Valutiamo le diverse spese sostenute dalle due strutture presenti nel dataset"),
                      plotlyOutput("plot_SpesaMese",height = 700, width=1300),
                      h2("Nel dataset sono presenti prestazioni etichettate con 7 tipologia di branca differente.Analizzando la spesa per branca e periodo di erogazione possiamo notare che alcune branche mediche richiedono interventi maggiormente costosi rispetto ad altri."),
                      verbatimTextOutput("dataBranca")
             ),#tabEsplorazioneDati
             tabPanel("Cluster",
                      h5(),
                      h2("La seguente demo preve un modello di clustering gerarchico"),
                      h2("Scegli il numero di cluster che preferisci"),
                      numericInput("num", label = h3("Cluster"), value = 2, min = 2,max=100), 
                      fluidRow(verbatimTextOutput("valueNumeric")),
                      h5("Scegli la tipologia di distanza"),
                      radioButtons("CalcoloMetodo", label = h3("Distanze"), 
                                   choices = list("Euclidea" = 1, "Manhattan" = 2, "Maximum" = 3, "Canberra" = 4, "Binario" = 5, "Minkowski" = 6), selected = 1),
                      #fluidRow(verbatimTextOutput("TipologiaMetodo")),
                      verbatimTextOutput("TyD"),
                      h5("Scegli la tipologia di legame"),
                      radioButtons("CalcoloLegame", label = h3("Legami"), 
                                   choices = list("Ward" = 1, "Singolo" = 2, "Completo" = 3, "Medio" = 4, "McQuitty" = 5, "Mediano" = 6, "Centroide"= 7), selected = 1),
                      #fluidRow(verbatimTextOutput("TipologiaLegame")),
                      verbatimTextOutput("TyL"),
                      h3("Numerosita per ogni cluster"),
                      #verbatimTextOutput("ClusterFinale"),
                      verbatimTextOutput("gruppi"),
                      plotOutput("Dendogramma",height = 700, width=1100)
                      
             ),#,#tabCluster
             tabPanel("Regressione Lineare",
                      h2(),
                      h4("Seleziona le variabili e valuta il loro impatto sulla previsione della variabile risposta",align="left"),
                      
                      checkboxGroupInput("VariabiliScelte", label = h3("Variabili interne al modello"),
                                         choices = list("TRIMESTRE_EROG" = 'TRIMESTRE_EROG', "SESSO" = 'SESSO', "ETA"="ETA",
                                                        "RES_VEN"='FLAG_RESIDENZA',"FLAG_TIPO_RICETTA"='FLAG_TIPO_RICETTA',
                                                        "BRANCA"='Branca',"CLASSE_PRIORITA"='CLASSE_PRIORITA',
                                                        "FLG_NON_ESEN"='FLG_NON_ESEN',"FLG_OSPEDALE"='FLG_OSPEDALE',"GG_PRESCR_PRENOT"="GG_PRESCR_PRENOT",
                                                        "GG_PRENOT_EROG"="GG_PRENOT_EROG"),
                                         selected = 1),
                      verbatimTextOutput("VariabiliReg"),
                      radioButtons("Intercetta", label = h3("Intercetta?"),
                                   choices = list("Si" = "Si", "No" = "No"), selected = "Si"),
                      fluidRow(verbatimTextOutput("Out_Intercetta")),
                      verbatimTextOutput("model"),
                      h2(),
                      h4("Modello di Regressione Lineare Multipla", align="center"),
                      h4("Y = B0 + B1*X1 + B2*X2 + B3*X3 + B4*X4",align="center"),
                      h4("dove: ",br(),"Y = VariabileRisposta",br(),"X1 = Variabile1",br(), "X2 = Variabile2", br(), "X3 = variabile3", br(), "X4 = variabile4"),
                      
                      
                      hr(),
                      h2("Il modello scelto per la previsione puntuale risulta essere il seguente:"),
                      verbatimTextOutput("modelloscelto"),
                      h2("Inserisci i valori nei regressori per vedere come varia la previsione di spesa"),
                      # selectInput("TRIMESTRE_EROG", label = h5("Seleziona la modalita per la TRIMESTRE_EROG"),
                      #             choices = list("1 Trimestre" = "1 Trimestre", "2 Trimestre"="2 Trimestre", "3 Trimestre" = "3 Trimestre",
                      #                            "4 Trimestre" = "4 Trimestre"),
                      #             selected = 0),
                      selectInput("SESSO", label = h5("Seleziona i valori della variabile SESSO"),
                                  choices = list("Uomo" = 1, "Donna"=0),
                                  selected = 1),
                      verbatimTextOutput("SEX_Category"),
                      
                      selectInput("RES_VEN", label = h5("Seleziona i valori della variabile RES_VEN, 0(Fuori Regione) o 1(Residente in Veneto)"),
                                  choices = list("Fuori Regione" = 0, "Residente in Veneto"=1),
                                  selected = 1),
                      verbatimTextOutput("RES_VEN_Category"),
                      
                      numericInput("GG_PRESCR_PRENOT", "Seleziona i valori della variabile GG_PRESCR_PRENOT", 1, min = 0, max = 300  ),
                      verbatimTextOutput("GG_PRESCR_PRENOT_Category"),
                      
                      
                      numericInput("GG_PRENOT_EROG", "Seleziona i valori della variabile GG_PRENOT_EROG", 1, min = 0, max = 300  ),
                      verbatimTextOutput("GG_PRENOT_EROG_Category"),
                      
                      
                      # selectInput("FLAG_TIPO_RICETTA", label = h5("Seleziona la modalita per la tipologia della ricetta"),
                      #             choices = list("Ricetta DEMA"="Ricetta DEMA","Ricetta Rossa"="Ricetta Rossa"),
                      #             selected = 0),
                      
                      # selectInput("Branca", label = h5("Seleziona la modalita per la Branca medica della prestazione"),
                      #             choices = list("Dermosifilopatia" = 1, "Medician fisica e riabilitazione"=2,
                      #                            "Nefrologia" = 3,
                      #                            "Otorinolaringoiatra" = 4,
                      #                            "Ostetricia e ginecologia"=5,
                      #                            "Branca Generica"=6,
                      #                            "Oculistica" = 7,
                      #                            "Non indicaata"=8,
                      #                            "Lab analisi"=9,
                      #                            "Multibranca"=10,
                      #                            "Radiologia"=11,
                      #                            "Ortopedia"=12,
                      #                            "Radioterapia"=13,
                      #                            "Pneumologia"=14,
                      #                            "Chirurgia generale"=15,
                      #                            "Medicina Nucleare"=16,
                      #                            "Neurologia"=17,
                      #                            "Cardiologia"=18),
                      #             selected = 0),
                      selectInput("CLASSE_PRIORITA", label = h5("Seleziona la modalita per di CLASSE_PRIORITA"),
                                  choices = list("A" = 1, "B"=2,"C"=3, "Z"=4),
                                  selected = 1),
                      verbatimTextOutput("PRI_Category"),
                      selectInput("FLG_NON_ESEN", label = h5("Seleziona la modalita per di FLG_NON_ESEN"),
                                  choices = list("Si" =1, "No"=0),
                                  selected = 1),
                      verbatimTextOutput("ESE_Category"),
                      selectInput("FLG_OSPEDALE", label = h5("Seleziona la modalita per di FLG_OSPEDALE"),
                                  choices = list("Si" = 1, "No"=0),
                                  selected = 1),
                      verbatimTextOutput("OSP_Category"),
                      verbatimTextOutput("str"),
                      
                      
                      
                      verbatimTextOutput("prev")
                      
                      
                      
             )#tabRegressione
  )#navbar
)#fluidPage
server <- function(input, output) {
  #Caricare i dati
  
  dati<-read.csv("dati.csv",sep=",",dec=".",header=T,stringsAsFactors = F, fileEncoding="latin1")
  dati$TRIMESTRE_EROG<-as.factor(dati$TRIMESTRE_EROG)
  #data_altro<-read.csv("data_altro.csv",sep=",",dec=".",header=T,stringsAsFactors = F)
  Soggetti<-read.csv("Soggetti.csv",sep=",",dec=".",header=T,stringsAsFactors = F)
  #dataReg_Iniz<-read.csv("dataReg.csv",sep=",",dec=".",header=T)
  dataReg_Iniz<-reg(dati)
  
  
  
  
  #dati<-rbind(dati_2018,dati_2019)
  datiN<-dati[,c("MESE_EROG","TRIMESTRE_EROG","SEMESTRE_EROG","ANNO_EROG","REGIONE_RICETTA","AZIENDA_RICETTA","CHIAVE_RICETTA",
                 "STRUTTURA_EROG","SESSO","DATA_NASCITA","ETA","CLASSE_ETA","REG_RES","PROV_RES","AZIENDA_RES","DATA_EROG",
                 "CLASSE_PRIORITA","TIPOLOGIA_EROG","DIAGNOSI_RICETTA","CODICE_SOGGETTO","PRESTAZIONE","QUANTITA","IMP_PRESTAZIONE")]
  
  
  #Esplorazione Dati
  output$dim<-renderPrint({dim(dati)})
  output$names<-renderPrint({names(datiN)})
  output$summary<-renderPrint({summary(datiN)})
  
  
  
  
  SpesaGiornaliera<-aggregate(dati$SPESA,by=list(dati$DATA_EROG,dati$STRUTTURA_EROG),FUN=sum)
  names(SpesaGiornaliera)<-c("Data","Struttura","Spesa")
  
  
  Veneto<-vector()
  nrow(Soggetti[Soggetti$Regione_Res=="Veneto",])
  Residenza<-c("Veneto","Altro")
  Totali<-c(nrow(Soggetti[Soggetti$Regione_Res=="Veneto",]),nrow(Soggetti[Soggetti$Regione_Res!="Veneto",]))
  Donne<-c(nrow(Soggetti[Soggetti$Regione_Res=="Veneto" & Soggetti$Sesso==0,]),nrow(Soggetti[Soggetti$Regione_Res!="Veneto"& Soggetti$Sesso==0,]))
  Uomini<-c(nrow(Soggetti[Soggetti$Regione_Res=="Veneto" & Soggetti$Sesso==1,]),nrow(Soggetti[Soggetti$Regione_Res!="Veneto"& Soggetti$Sesso==1,]))
  data<-data.frame(Residenza,Totali,Donne,Uomini)
  
  
  output$plot_Veneto <- renderPlotly({
    plot_ly(data, x = ~Residenza, y = ~Donne, type = 'bar', name = 'Donne', marker = list(color = 'rgb(255,105,180)')) %>%
      add_trace(y = ~Uomini, name = 'Uomini',marker = list(color = 'rgb(32,178,170)')) %>%
      layout(title = "Distribuzione Genere") %>%
      layout(yaxis = list(title = 'Pazienti'), barmode = 'stack')
  })
  
  
  data_altro<-aggregate(Seq~Regione_Res+Sesso,data=Soggetti,sum)
  data_altro<-spread(data_altro,Sesso,Seq,fill=0,convert=TRUE)
  names(data_altro)<-c("Residenza","Donne","Uomini")
  data_altro<-data_altro[-which(data_altro$Residenza=="Veneto"),]
  data_altro$Totale<-data_altro$Donne+data_altro$Uomini
  
  
  
  output$plot_Altro <- renderPlotly({
    plot_ly(data_altro, x = ~Residenza, y = ~Donne, type = 'bar', name = 'Donne' ,marker = list(color = 'rgb(255,228,196)')) %>%
      add_trace(y = ~Uomini, name = 'Uomini',marker = list(color = 'rgb(100,149,237)')) %>%
      layout(title = "Distribuzione Genere") %>%
      layout(yaxis = list(title = 'Pazienti'), barmode = 'stack')
  })
  
  
  output$plot_box <- renderPlotly({
    plot_ly(type = 'box') %>%
      add_boxplot(y = dati$ETA[dati$SESSO==0], jitter = 0.3, pointpos = -1.8, boxpoints = FALSE,
                  marker = list(color = 'rgb(255,228,225)'),
                  line = list(color = 'rgb(218,112,214)'),
                  name = "Donne") %>%
      add_boxplot(y = dati$ETA[dati$SESSO==1], name = "Uomini", boxpoints = FALSE,
                  marker = list(color = 'rgb(175,238,238)'),
                  line = list(color = 'rgb(0,0,128)'))   
  })
  
  
  Prio<-aggregate(Seq~CLASSE_PRIORITA+MESE_EROG,data=dati,sum)
  Prio<-spread(Prio,CLASSE_PRIORITA,Seq,fill=0,convert=TRUE)
  Prio$Mese<-factor(Prio$MESE_EROG, levels=c("Gennaio","Febbraio","Marzo","Aprile","Maggio","Giugno","Luglio","Agosto","Settembre","Ottobre","Novembre","Dicembre"))
  Prio<-Prio[,-1]
  
  
  
  output$plot_Prio <- renderPlotly({
    plot_ly(Prio, x = ~Mese, y = ~A, type = 'bar', name = 'Priorita A', marker = list(color = 'rgb(0,128,0)')) %>%
      add_trace(y = ~B, name = 'Priorita B',marker = list(color = 'rgb(184,134,11)')) %>%
      add_trace(y = ~C, name = 'Priorita C',marker = list(color = 'rgb(139,0,0)')) %>%
      add_trace(y = ~Z, name = 'Priorita Z',marker = list(color = 'rgb(105,105,105)')) %>%
      layout(title = "Distribuzione Priorita") %>%
      layout(yaxis = list(title = 'Prestazioni'), barmode = 'group')
  })
  
  SP<-aggregate(SPESA~MESE_EROG+STRUTTURA_EROG,data=dati,sum)
  SP<-spread(SP,STRUTTURA_EROG,SPESA,fill=NA,convert=TRUE)
  SP[is.na(SP)]<-0
  
  colnames(SP)[2:ncol(SP)]<-paste0("Struttura_",colnames(SP)[2:ncol(SP)])
  colnames(SP)[1]<-"Mese"
  SP$Mese2<-factor(SP$Mese, levels=c("Gennaio","Febbraio","Marzo","Aprile","Maggio","Giugno","Luglio","Agosto","Settembre","Ottobre","Novembre","Dicembre"))
  SP<-SP[,-1]
  colnames(SP)[15]<-"Mese"
  
  
  
  output$plot_SpesaMese <- renderPlotly({
    plot_ly(SP, x = ~Mese, y = ~Struttura_101, type = 'bar', name = 'Struttura_000101.', marker = list(color = 'rgb(0,128,128)')) %>%
      add_trace(y = ~Struttura_201, name = 'Struttura_000201',marker = list(color = 'rgb(192,192,192)')) %>%
      add_trace(y = ~Struttura_201, name = 'Struttura_000201',marker = list(color = 'rgb(222,184,135)')) %>%
      add_trace(y = ~Struttura_301, name = 'Struttura_000301',marker = list(color = 'rgb(255,127,80)')) %>%
      add_trace(y = ~Struttura_401, name = 'Struttura_000401',marker = list(color = 'rgb(0,139,139)')) %>%
      add_trace(y = ~Struttura_501, name = 'Struttura_000501',marker = list(color = 'rgb(184,134,11)')) %>%
      add_trace(y = ~Struttura_602, name = 'Struttura_000602',marker = list(color = 'rgb(143,188,143)')) %>%
      add_trace(y = ~Struttura_701, name = 'Struttura_000701',marker = list(color = 'rgb(178,34,34)')) %>%
      add_trace(y = ~Struttura_1001, name = 'Struttura_001001',marker = list(color = 'rgb(218,165,32)')) %>%
      add_trace(y = ~Struttura_1101, name = 'Struttura_001101',marker = list(color = 'rgb(75,0,130)')) %>%
      add_trace(y = ~Struttura_1401, name = 'Struttura_001401',marker = list(color = 'rgb(240,128,128)')) %>%
      add_trace(y = ~Struttura_1501, name = 'Struttura_001501',marker = list(color = 'rgb(32,178,170)')) %>%
      add_trace(y = ~Struttura_2401, name = 'Struttura_002401',marker = list(color = 'rgb(147,112,219)')) %>%
      add_trace(y = ~Struttura_40001, name = 'Struttura_040001',marker = list(color = 'rgb(255,69,0)')) %>%
      add_trace(y = ~Struttura_40101, name = 'Struttura_040101',marker = list(color = 'rgb(220,20,60)')) %>%
      layout(title = "Importo per mese e struttura di erogazione") %>%
      layout(yaxis = list(title = 'Importo'), barmode = 'group')
  })
  
  
  
  output$dataBranca<-renderPrint({
    aggregate(SPESA~BRANCA_RIELABORATA+ANNO_EROG+SEMESTRE_EROG,data=dati,sum)
    
  })
  
  
  #PER CLUSTER
  output$valueNumeric <- renderPrint({ input$num })
  
  dati2 <- data.frame(dati$CODICE_SOGGETTO,dati$SEMESTRE_EROG,dati$REGIONE_RICETTA,
                      dati$SESSO,dati$DATA_NASCITA,dati$REG_RES,dati$TIPO_SPEC_MEDBASE,
                      dati$DISCIPLINA,dati$CLASSE_PRIORITA,dati$RICETTA_RIPETUTA,dati$NUMERO_RIPETIZIONI,
                      dati$IMP_PRESTAZIONE)
  
  #Metodo
  output$TipologiaMetodo <- renderPrint({ input$CalcoloMetodo })
  TypeDistanza<- reactive({
    switch(as.character(input$CalcoloMetodo),
           "1"="euclidean",
           "3"="maximum",
           "2"="manhattan",
           "4"="canberra",
           "5"="binary",
           "6"="minkowski"
    )
  })
  output$TyD<-renderPrint({TypeDistanza()})
  
  #Legame
  output$TipologiaLegame <- renderPrint({ input$CalcoloLegame })
  TypeLegame<- reactive({
    switch(as.character(input$CalcoloLegame),
           "1"="ward.D",
           "2"="single",
           "3"="complete",
           "4"="average",
           "5"="mcquitty",
           "6"="median",
           "7"="centroid"
    )
  })
  output$TyL<-renderPrint({TypeLegame()})
  
  
  MatriceDist <- reactive({
    suppressWarnings(dist(dati2, method = TypeDistanza()))
    
  })
  output$MatrDist <- renderPrint({MatriceDist()})
  
  Cluster_creato <- reactive({
    hclust(MatriceDist(),method = TypeLegame())
    
  })
  output$Cluster <- renderPrint({Cluster_creato()})
  
  Dati_Cluster <- reactive({
    cbind(dati,"Cluster"=cutree(Cluster_creato(), k=input$num))
  })
  
  Valori4Cluster<- reactive({
    cutree(Cluster_creato(), k=input$num)
  })
  
  output$Dendogramma<-renderPlot({
    plot(Cluster_creato(), main = "Distribuzione unita nei cluster", ylab = "Distanza", hang =
           + 0.1,frame.plot = TRUE)
    rect.hclust(Cluster_creato(), k=input$num, border ="cornflowerblue")
    
  })
  output$gruppi<-renderPrint({table(Valori4Cluster())})
  
  
  
  ###REGRESSIONE
  output$VariabiliReg <- renderPrint({ input$VariabiliScelte })
  output$Out_Intercetta <- renderPrint({ input$Intercetta })
  
  #output$TRIM_Category <- renderPrint({ input$TRIMESTRE_EROG })
  #output$ETA_Category <- renderPrint({ input$ETA })
  output$SEX_Category <- renderPrint({ input$SESSO })
  output$RES_VEN_Category <- renderPrint({ input$RES_VEN })
  #output$RIC_Category <- renderPrint({ input$FLAG_TIPO_RICETTA })
  output$GG_PRESCR_PRENOT_Category <- renderPrint({ input$GG_PRESCR_PRENOT })
  output$GG_PRENOT_EROG_Category <- renderPrint({ input$GG_PRENOT_EROG })
  #output$BRA_Category <- renderPrint({ input$Branca})
  output$PRI_Category <- renderPrint({ input$CLASSE_PRIORITA })
  #output$PRE_Category <- renderPrint({ input$PRESTAZIONE })
  output$ESE_Category <- renderPrint({ input$FLG_NON_ESEN })
  output$OSP_Category <- renderPrint({ input$FLG_OSPEDALE })
  
  
  regFormula_0 <- reactive({
    as.formula(paste("dataReg_Iniz$SPESA", '~+1'))
  })
  regFormula_1_int <- reactive({
    as.formula(paste("dataReg_Iniz$SPESA", '~+',input$VariabiliScelte))
  })
  regFormula_2_int<- reactive({
    {as.formula(paste("dataReg_Iniz$SPESA",'~+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[1],'+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[2]))}
  })
  regFormula_3_int<- reactive({
    {as.formula(paste("dataReg_Iniz$SPESA",'~+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[1],'+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[2],'+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[3]))}
  })
  regFormula_4_int<- reactive({
    {as.formula(paste("dataReg_Iniz$SPESA",'~+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[1],'+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[2],'+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[3],'+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[4]))}
  })
  regFormula_5_int<- reactive({
    {as.formula(paste("dataReg_Iniz$SPESA",'~+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[1],'+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[2],'+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[3],'+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[4],'+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[5]))}
  })
  regFormula_6_int<- reactive({
    {as.formula(paste("dataReg_Iniz$SPESA",'~+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[1],'+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[2],'+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[3],'+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[4],'+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[5],'+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[6]))}
  })
  regFormula_7_int<- reactive({
    {as.formula(paste("dataReg_Iniz$SPESA",'~+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[1],'+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[2],'+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[3],'+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[4],'+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[5],'+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[6],'+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[7]))}
  })
  regFormula_8_int<- reactive({
    {as.formula(paste("dataReg_Iniz$SPESA",'~+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[1],'+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[2],'+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[3],'+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[4],'+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[5],'+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[6],'+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[7],'+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[8]))}
  })
  regFormula_9_int<- reactive({
    {as.formula(paste("dataReg_Iniz$SPESA",'~+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[1],'+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[2],'+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[3],'+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[4],'+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[5],'+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[6],'+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[7],'+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[8],'+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[9]))}
  })
  regFormula_10_int<- reactive({
    {as.formula(paste("dataReg_Iniz$SPESA",'~+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[1],'+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[2],'+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[3],'+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[4],'+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[5],'+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[6],'+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[7],'+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[8],'+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[9],'+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[10]))}
  })
  regFormula_11_int<- reactive({
    {as.formula(paste("dataReg_Iniz$SPESA",'~+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[1],'+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[2],'+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[3],'+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[4],'+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[5],'+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[6],'+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[7],'+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[8],'+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[9],'+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[10],'+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[11]))}
  })
  regFormula_12_int<- reactive({
    {as.formula(paste("dataReg_Iniz$SPESA",'~+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[1],'+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[2],'+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[3],'+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[4],'+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[5],'+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[6],'+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[7],'+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[8],'+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[9],'+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[10],'+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[11],'+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[12]))}
  })
  
  regFormula_1_Nint <- reactive({
    as.formula(paste("dataReg_Iniz$SPESA", '~-1+',input$VariabiliScelte))
  })
  regFormula_2_Nint<- reactive({
    {as.formula(paste("dataReg_Iniz$SPESA",'~-1+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[1],'+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[2]))}
  })
  regFormula_3_Nint<- reactive({
    {as.formula(paste("dataReg_Iniz$SPESA",'~-1+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[1],'+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[2],'+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[3]))}
  })
  regFormula_4_Nint<- reactive({
    {as.formula(paste("dataReg_Iniz$SPESA",'~-1+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[1],'+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[2],'+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[3],'+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[4]))}
  })
  regFormula_5_Nint<- reactive({
    {as.formula(paste("dataReg_Iniz$SPESA",'~-1+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[1],'+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[2],'+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[3],'+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[4],'+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[5]))}
  })
  regFormula_6_Nint<- reactive({
    {as.formula(paste("dataReg_Iniz$SPESA",'~-1+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[1],'+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[2],'+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[3],'+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[4],'+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[5],'+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[6]))}
  })
  regFormula_7_Nint<- reactive({
    {as.formula(paste("dataReg_Iniz$SPESA",'~-1+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[1],'+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[2],'+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[3],'+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[4],'+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[5],'+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[6],'+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[7]))}
  })
  regFormula_8_Nint<- reactive({
    {as.formula(paste("dataReg_Iniz$SPESA",'~-1+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[1],'+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[2],'+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[3],'+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[4],'+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[5],'+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[6],'+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[7],'+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[8]))}
  })
  regFormula_9_Nint<- reactive({
    {as.formula(paste("dataReg_Iniz$SPESA",'~-1+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[1],'+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[2],'+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[3],'+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[4],'+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[5],'+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[6],'+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[7],'+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[8],'+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[9]))}
  })
  regFormula_10_Nint<- reactive({
    {as.formula(paste("dataReg_Iniz$SPESA",'~-1+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[1],'+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[2],'+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[3],'+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[4],'+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[5],'+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[6],'+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[7],'+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[8],'+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[9],'+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[10]))}
  })
  regFormula_11_Nint<- reactive({
    {as.formula(paste("dataReg_Iniz$SPESA",'~-1+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[1],'+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[2],'+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[3],'+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[4],'+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[5],'+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[6],'+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[7],'+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[8],'+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[9],'+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[10],'+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[11]))}
  })
  regFormula_12_Nint<- reactive({
    {as.formula(paste("dataReg_Iniz$SPESA",'~-1+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[1],'+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[2],'+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[3],'+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[4],'+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[5],'+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[6],'+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[7],'+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[8],'+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[9],'+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[10],'+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[11],'+',as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," ")))[12]))}
  })
  
  regFormula_0_Nint <- renderPrint({"inserire un regressore"})
  
  # regFormula_PrevPunt<- reactive({
  #   {as.formula("dataReg$SPESA~+V1+V2+V3+V4")}
  # })
  # mod_Punt<- reactive({
  #   lm(regFormula_PrevPunt(),data=dataReg)
  # })
  
  output$modelloscelto<-renderPrint({"dataReg_Iniz$SPESA~-1+FLAG_RESIDENZA+CLASSE_PRIORITA+FLG_NON_ESEN+FLG_OSPEDALE+GG_PRESCR_PRENOT+GG_PRENOT_EROG+SESSO"})
  
  selectedData <- reactive({
    dataReg_Iniz[1:nrow(dataReg_Iniz),1:ncol(dataReg_Iniz)]
  })
  
  
  # bivariate model
  model_int <- reactive({
    switch(as.character(length(as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," "))))),
           "0"=lm(regFormula_0(), data = selectedData()),
           "1"=lm(regFormula_1_int(), data = selectedData()),
           "2"=lm(regFormula_2_int(), data = selectedData()),
           "3"=lm(regFormula_3_int(), data = selectedData()),
           "4"=lm(regFormula_4_int(), data = selectedData()),
           "5"=lm(regFormula_5_int(), data = selectedData()),
           "6"=lm(regFormula_6_int(), data = selectedData()),
           "7"=lm(regFormula_7_int(), data = selectedData()),
           "8"=lm(regFormula_8_int(), data = selectedData()),
           "9"=lm(regFormula_9_int(), data = selectedData()),
           "10"=lm(regFormula_10_int(), data = selectedData()),
           "11"=lm(regFormula_11_int(), data = selectedData()),
           "12"=lm(regFormula_12_int(), data = selectedData())
           
    )
  })
  
  
  model_Nint<-reactive({
    switch(as.character(length(as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," "))))),
           "1"=lm(regFormula_1_Nint(), data = selectedData()),
           "2"=lm(regFormula_2_Nint(), data = selectedData()),
           "3"=lm(regFormula_3_Nint(), data = selectedData()),
           "4"=lm(regFormula_4_Nint(), data = selectedData()),
           "5"=lm(regFormula_5_Nint(), data = selectedData()),
           "6"=lm(regFormula_6_Nint(), data = selectedData()),
           "7"=lm(regFormula_7_Nint(), data = selectedData()),
           "8"=lm(regFormula_8_Nint(), data = selectedData()),
           "9"=lm(regFormula_9_Nint(), data = selectedData()),
           "10"=lm(regFormula_10_Nint(), data = selectedData()),
           "11"=lm(regFormula_11_Nint(), data = selectedData()),
           "12"=lm(regFormula_12_Nint(), data = selectedData())
           
    )
  })
  
  output$int<-renderPrint({ as.character(input$Intercetta)})
  
  output$model <- renderPrint({
    switch(as.character(input$Intercetta),
           "Si"=summary(model_int()),
           "No"= (switch(as.character(length(as.vector(unlist(strsplit(as.character(input$VariabiliScelte)," "))))),
                         "1"=summary(model_Nint()),
                         "2"=summary(model_Nint()),
                         "3"=summary(model_Nint()),
                         "4"=summary(model_Nint()),
                         "5"=summary(model_Nint()),
                         "6"=summary(model_Nint()),
                         "7"=summary(model_Nint()),
                         "8"=summary(model_Nint()),
                         "9"=summary(model_Nint()),
                         "10"=summary(model_Nint()),
                         "11"=summary(model_Nint()),
                         "12"=summary(model_Nint()),
                         "0"=regFormula_0_Nint())))
  })
  
  
  
  
  crea <- reactive({
    data.frame("FLAG_RESIDENZA"=as.factor(input$RES_VEN),"CLASSE_PRIORITA"=as.factor(input$CLASSE_PRIORITA), "FLG_NON_ESEN"=as.factor(input$FLG_NON_ESEN),"FLG_OSPEDALE"=as.factor(input$FLG_OSPEDALE),"GG_PRESCR_PRENOT"=as.numeric(input$GG_PRESCR_PRENOT),"GG_PRENOT_EROG"=as.numeric(input$GG_PRENOT_EROG),"SESSO"=as.factor(input$SESSO))
  })
  
  output$str<-renderPrint({str(crea())})
  
  output$modelloscelto<-renderPrint({paste("dataReg_Iniz$SPESA~1+FLAG_RESIDENZA+CLASSE_PRIORITA+FLG_NON_ESEN+FLG_OSPEDALE+GG_PRESCR_PRENOT+GG_PRENOT_EROG+SESSO")})
  
  modelloscelto_for<-reactive({
    paste("dataReg_Iniz$SPESA~1+FLAG_RESIDENZA+CLASSE_PRIORITA+FLG_NON_ESEN+FLG_OSPEDALE+GG_PRESCR_PRENOT+GG_PRENOT_EROG+SESSO")
  })
  regFormula_PrevPunt<- reactive({
    {as.formula(modelloscelto_for())}
  })
  mod_Punt<- reactive({
    lm(regFormula_PrevPunt(),data=selectedData())
  })
  
  previsioni<- reactive ({predict(mod_Punt(),newdata=crea(), interval="predict")
  })
  output$prev <- renderPrint({abs(previsioni()[1])})
  
  
}
shinyApp(ui = ui, server = server)



