setwd("C:/Users/m.cortese/Desktop/Evento")
library(dplyr)
library(stringr)


#h3("Ciao a tutti.Sono Marco Cortese Data Scientist in Healthy Reply SpA e oggi vi parlerò di come creare una web app interattiva che consente mettere in produzione un modello di ML scritto in codice R attraverso il package Shiny.",align="justify"),

library(plotly)
#dati<-read_xlsx("Export.xlsx")
dati_2018<-read.csv("File2018.csv",sep=";",dec=",",header=T,stringsAsFactors = F)
dati_2019<-read.csv("File2019.csv",sep=";",dec=",",header=T,stringsAsFactors = F)



# variabili utili 4-5-6 regione ricetta azienda ricetta struttura_erog sesso eta regione_res classe priorita tipologia erog 
#imponibile prestazione codice_soggetto id_ricetta diagnosi_ricetta

dati<-rbind(dati_2018,dati_2019)

dati<-dati[,c("MESE_EROG","TRIMESTRE_EROG","SEMESTRE_EROG","ANNO_EROG","REGIONE_RICETTA","AZIENDA_RICETTA","CHIAVE_RICETTA",
           "STRUTTURA_EROG","SESSO","DATA_NASCITA","ETA","REG_RES","PROV_RES","AZIENDA_RES","DATA_EROG",
           "CLASSE_PRIORITA","TIPOLOGIA_EROG","DIAGNOSI_RICETTA","CODICE_SOGGETTO","PRESTAZIONE","QUANTITA","IMP_PRESTAZIONE")]

elaborazione<-function(dati){

    SPESA<-vector()
    for(i in 1:nrow(dati)){
      SPESA[i]<-as.numeric(dati$QUANTITA[i])*as.numeric(dati$IMP_PRESTAZIONE[i])
    }
    
    
    dati$SPESA<-SPESA
    cod_sog<-unique(dati$CODICE_SOGGETTO)
    for(i in 1:length(cod_sog)){
      #print(paste0(cod_sog[i]," ripetuto ", nrow(dati[which(dati$CODICE_SOGGETTO==cod_sog[i]),])," volte" ))
      #print(i)
      dati$RIPETIZIONI[dati$CODICE_SOGGETTO==cod_sog[i]]<-nrow(dati[which(dati$CODICE_SOGGETTO==cod_sog[i]),])
    }
    
    #length(unique(dati$CODICE_SOGGETTO))
    #length(unique(dati$DATA_EROG))
    
    for(i in 1:nrow(dati)){
      dati$STRUTTURA_EROG[i]<-unlist(strsplit(dati$STRUTTURA_EROG[i]," "))[1]
    }
    
    SpesaGiornaliera<-aggregate(dati$SPESA,by=list(dati$DATA_EROG,dati$STRUTTURA_EROG),FUN=sum)
    names(SpesaGiornaliera)<-c("Data","Struttura","Spesa")
    #unique(dati$STRUTTURA_EROG)
    
    dati$SESSO<-as.numeric(as.factor(dati$SESSO))-1
    # Soggetti<-aggregate(dati$SESSO,by=list(dati$CODICE_SOGGETTO,dati$REG_RES,dati$PROV_RES,dati$DATA_NASCITA,dati$RIPETIZIONI,dati$Classe),FUN=mean)
    # names(Soggetti)<-c("Codice_Soggetto","Regione_Res","Prov_Res","Data_Nascita","Ripetizioni","Sesso")
    
    # table(Soggetti$Ripetizioni)
    # 
    # Soggetti$Regione_Res~Soggetti$Sesso
    # 
    # Veneto<-vector()
    # nrow(Soggetti[Soggetti$Regione_Res=="Veneto",])
    # Residenza<-c("Veneto","Altro")
    # Totali<-c(nrow(Soggetti[Soggetti$Regione_Res=="Veneto",]),nrow(Soggetti[Soggetti$Regione_Res!="Veneto",]))
    # Donne<-c(nrow(Soggetti[Soggetti$Regione_Res=="Veneto" & Soggetti$Sesso==0,]),nrow(Soggetti[Soggetti$Regione_Res!="Veneto"& Soggetti$Sesso==0,]))
    # Uomini<-c(nrow(Soggetti[Soggetti$Regione_Res=="Veneto" & Soggetti$Sesso==1,]),nrow(Soggetti[Soggetti$Regione_Res!="Veneto"& Soggetti$Sesso==1,]))
    # data<-data.frame(Residenza,Totali,Donne,Uomini)
    # 
    # 
    # p <- plot_ly(data, x = ~Residenza, y = ~Donne, type = 'bar', name = 'Donne', marker = list(color = 'rgb(255,105,180)')) %>%
    #   add_trace(y = ~Uomini, name = 'Uomini',marker = list(color = 'rgb(32,178,170)')) %>%
    #   layout(title = "Distribuzione Genere") %>%
    #   layout(yaxis = list(title = 'Pazienti'), barmode = 'stack')
    # p
    # 
    # 
    # 
    data_altro<-data.frame()
    regioni<-unique(Soggetti$Regione_Res[Soggetti$Regione_Res!="Veneto"])
    for(i in 1:length(regioni)){
      Residenza<-regioni[i]
      Totali<-c(nrow(Soggetti[Soggetti$Regione_Res==regioni[i],]))
      Donne<-c(nrow(Soggetti[Soggetti$Regione_Res==regioni[i] & Soggetti$Sesso==0,]))
      Uomini<-c(nrow(Soggetti[Soggetti$Regione_Res==regioni[i] & Soggetti$Sesso==1,]))
      data_temp<-data.frame(Residenza,Totali,Donne,Uomini)
      data_altro<-rbind(data_altro,data_temp)
    }
    # 
    # p_altro <- plot_ly(data_altro, x = ~Residenza, y = ~Donne, type = 'bar', name = 'Donne' ,marker = list(color = 'rgb(255,228,196)')) %>%
    #   add_trace(y = ~Uomini, name = 'Uomini',marker = list(color = 'rgb(100,149,237)')) %>%
    #   layout(title = "Distribuzione Genere") %>%
    #   layout(yaxis = list(title = 'Pazienti'), barmode = 'stack')
    # p_altro
    # 
    # c<-boxplot(dati$ETA~dati$SESSO, xlab ="Genere", ylab="Età", names=c("Donne","Uomini"),col=c("#f08080","#20b2aa"),main="Distribuzione Età")
    # 
    # 
    # 
    # Struttura<-c("000501","000201")
    # TotaleStrut<-c(nrow(dati[dati$STRUTTURA_EROG=="000501.",]),nrow(dati[dati$STRUTTURA_EROG=="000201.",]))
    # PrioritaA<-c(nrow(dati[dati$STRUTTURA_EROG=="000501." & dati$CLASSE_PRIORITA=="A",]),nrow(dati[dati$STRUTTURA_EROG=="000201." & dati$CLASSE_PRIORITA=="A",]))
    # PrioritaB<-c(nrow(dati[dati$STRUTTURA_EROG=="000501." & dati$CLASSE_PRIORITA=="B",]),nrow(dati[dati$STRUTTURA_EROG=="000201." & dati$CLASSE_PRIORITA=="B",]))
    # PrioritaC<-c(nrow(dati[dati$STRUTTURA_EROG=="000501." & dati$CLASSE_PRIORITA=="C",]),nrow(dati[dati$STRUTTURA_EROG=="000201." & dati$CLASSE_PRIORITA=="C",]))
    # Prio<-data.frame(Struttura,TotaleStrut,PrioritaA,PrioritaB,PrioritaC)
    # 
    # 
    # p_prio <- plot_ly(Prio, x = ~Struttura, y = ~TotaleStrut, type = 'bar', name = 'TotalePrestazioni', marker = list(color = 'rgb(46,139,87)')) %>%
    #   add_trace(y = ~PrioritaA, name = 'PrioritaA',marker = list(color = 'rgb(255,69,0)')) %>%
    #   add_trace(y = ~PrioritaB, name = 'PrioritaB',marker = list(color = 'rgb(221,160,221)')) %>%
    #   add_trace(y = ~PrioritaC, name = 'PrioritaC',marker = list(color = 'rgb(65,105,225)')) %>%
    #   layout(title = "Distribuzione Priorità") %>%
    #   layout(yaxis = list(title = 'Prestazioni'), barmode = 'group')
    # p_prio
    
    #lista<-list(p,p_altro,c,p_prio)
    
    lista<-list(dati,data_altro,Soggetti)
    return(lista)

}
