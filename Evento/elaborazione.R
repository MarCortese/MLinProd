dati<-read.csv("dati.csv",sep=",",dec=".",header=T,stringsAsFactors = F)
dati3<-read.csv("file3.csv",sep=";",dec=",",header=T,stringsAsFactors = F)
dati3<-read.csv("File2018.csv",sep=";",dec=",",header=T,stringsAsFactors = F)
dati3<-read.csv("File2019.csv",sep=";",dec=",",header=T,stringsAsFactors = F)

dati<-dati3

elaborazione<-function(dati,dati3){
  #dati3 è quello nuovo, dati è quello già esistente
  SPESA<-vector()
  for(i in 1:nrow(dati3)){
    SPESA[i]<-as.numeric(dati3$QUANTITA[i])*as.numeric(dati3$IMP_PRESTAZIONE[i])
  }
  
  
  dati3$SPESA<-SPESA
  cod_sog<-unique(dati3$CODICE_SOGGETTO)
  for(i in 1:length(cod_sog)){
    #print(paste0(cod_sog[i]," ripetuto ", nrow(dati[which(dati$CODICE_SOGGETTO==cod_sog[i]),])," volte" ))
    #print(i)
    dati3$RIPETIZIONI[dati3$CODICE_SOGGETTO==cod_sog[i]]<-nrow(dati3[which(dati3$CODICE_SOGGETTO==cod_sog[i]),])
  }
  
  #length(unique(dati$CODICE_SOGGETTO))
  #length(unique(dati$DATA_EROG))
  
  for(i in 1:nrow(dati3)){
    dati3$STRUTTURA_EROG[i]<-unlist(strsplit(dati3$STRUTTURA_EROG[i]," "))[1]
  }
  

  #unique(dati$STRUTTURA_EROG)
  
  dati3$SESSO<-as.numeric(as.factor(dati3$SESSO))-1
  dati3$CLASSE_ETA_CAT<-as.numeric(as.factor(dati3$CLASSE_ETA))
  
  
  dati<-rbind(dati,dati3)
  SpesaGiornaliera<-aggregate(dati$SPESA,by=list(dati$DATA_EROG,dati$STRUTTURA_EROG),FUN=sum)
  names(SpesaGiornaliera)<-c("Data","Struttura","Spesa")
  
  Soggetti<-aggregate(dati$SESSO,by=list(dati$CODICE_SOGGETTO,dati$REG_RES,dati$PROV_RES,dati$DATA_NASCITA,dati$CLASSE_ETA_CAT,dati$RIPETIZIONI),FUN=mean)
  names(Soggetti)<-c("Codice_Soggetto","Regione_Res","Prov_Res","Data_Nascita","Classe_Eta","Ripetizioni","Sesso")
  
  dati$Seq<-rep(1,nrow(dati))
  Soggetti$Seq<-rep(1,nrow(Soggetti))
  
  
  # data_altro<-aggregate(Seq~Regione_Res+Sesso,data=Soggetti,sum)
  # data_altro<-spread(data_altro,Sesso,Seq,fill=0,convert=TRUE)
  # names(data_altro)<-c("Residenza","Donne","Uomini")
  # data_altro<-data_altro[-which(data_altro$Residenza=="Veneto"),]
  # data_altro$Totale<-data_altro$Donne+data_altro$Uomini
  

  
  # SP<-aggregate(SPESA~MESE_EROG+STRUTTURA_EROG,data=dati,sum)
  # SP<-spread(SP,STRUTTURA_EROG,SPESA,fill=NA,convert=TRUE)
  # SP[is.na(SP)]<-0
  # 
  # colnames(SP)[2:ncol(SP)]<-paste0("Struttura_",colnames(SP)[2:ncol(SP)])
  # colnames(SP)[1]<-"Mese"
  # SP$Mese2<-factor(SP$Mese, levels=c("Gennaio","Febbraio","Marzo","Aprile","Maggio","Giugno","Luglio","Agosto","Settembre","Ottobre","Novembre","Dicembre"))
  # SP<-SP[,-1]
  # colnames(SP)[15]<-"Mese"

  dataReg<-dati[,c("SPESA","TRIMESTRE_EROG", "MESE_EROG" , "SESSO" , "CLASSE_ETA_CAT",
                      "REG_RES","AZIENDA_RES","TIPO_MED_PRESCR","FLAG_TIPO_RICETTA",
                      "DISCIPLINA","CLASSE_PRIORITA","PRESTAZIONE",
                      "FLG_NON_ESEN","FLG_OSPEDALE","BRANCA_RIELABORATA")]
  dataReg$REG_RES[dataReg$REG_RES=="Provincia di Bolzano" |dataReg$REG_RES== "Provincia di Trento"]<-"Trentino Alto Adige"

  for(i in 1:nrow(dataReg)){
    dataReg$MESE_EROG[i]<-switch(dataReg$MESE_EROG[i],
                              "Gennaio"="1",
                              "Febbraio"="2",
                              "Marzo"="3",
                              "Aprile"="4",
                              "Maggio"="5",
                              "Giugno"="6",
                              "Luglio"="7",
                              "Agosto"="8",
                              "Settembre"="9",
                              "Ottobre"="10",
                              "Novembre"="11",
                              "Dicembre"="12"
                              )
    dataReg$PRESTAZIONE_NUM[i]<-unlist(strsplit(dataReg$PRESTAZIONE[i]," -"))[1]
  }
  names(dataReg)[c(12,16)]<-c("PRESTAZIONE_TOTALE","PRESTAZIONE")
  dataReg$CLASSE_PRIORITA<-as.factor(as.numeric(as.factor(dataReg$CLASSE_PRIORITA)))
  dataReg$FLG_NON_ESEN<-as.factor(as.numeric(as.factor(dataReg$FLG_NON_ESEN))-1)
  dataReg$FLG_OSPEDALE<-as.factor(as.numeric(as.factor(dataReg$FLG_OSPEDALE))-1)
  
  
  dataReg<-dataReg[,c(1:11,16,13:15,12)]
  names(dataReg)[5]<-"CLASSE_ETA"
  
  dataReg$GG_PRESCR_PRENOT<-as.numeric(difftime(as.Date(dati$DATA_PRENOT,"%d/%m/%Y"),as.Date(dati$DATA_PRESCR,"%d/%m/%Y"),units = "days"))
  dataReg$GG_PRENOT_EROG<-as.numeric(difftime(as.Date(dati$DATA_EROG,"%d/%m/%Y"),as.Date(dati$DATA_PRESCR,"%d/%m/%Y"),units = "days"))
  
  dataReg$FLAG_RESIDENZA<-as.factor(as.numeric(as.factor(dati$FLAG_RESIDENZA_VENETO))-1)
  dataReg$TIPO_MED_PRESCR<-as.factor(as.numeric(as.factor(dataReg$TIPO_MED_PRESCR)))
  dataReg$MESE_EROG<-as.numeric(dataReg$MESE_EROG)
  dataReg$Branca<-as.factor(as.numeric(as.factor(dataReg$BRANCA_RIELABORATA)))
  dataReg$CLASSE_PRIORITA<-as.factor(dataReg$CLASSE_PRIORITA)
  dataReg$ETA<-dati$ETA
  dataReg$SESSO<-as.factor(dataReg$SESSO)
  dataReg$FLAG_TIPO_RICETTA[dataReg$FLAG_TIPO_RICETTA=="Altra ricetta (15 caratteri)"]<-"Altro"
  dataReg$TRIMESTRE_EROG<-str_remove_all(dataReg$TRIMESTRE_EROG,"°")
  unique(dati$TRIMESTRE_TRASM)
  dati$BRANCA_RIELABORATA
  
  dati$BRANCA_RIELABORATA[dati$BRANCA_RIELABORATA=="non indicata"]<-"Non Indicata"
  dati$BRANCA_RIELABORATA[dati$BRANCA_RIELABORATA=="56. Medicina fisica e riabilitazione - recupero e riabilitazione funzionale dei motulesi e neurolesi"]<-"Medicina e riabilitazione"
  dati$BRANCA_RIELABORATA[dati$BRANCA_RIELABORATA=="29. Nefrologia"]<-"Nefrologia"
  dati$BRANCA_RIELABORATA[dati$BRANCA_RIELABORATA=="37. Ostetricia e ginecologia"]<-"Ostetricia e Ginecologia"
  dati$BRANCA_RIELABORATA[dati$BRANCA_RIELABORATA=="00. Branca Generica"]<-"Branca generica"
  dati$BRANCA_RIELABORATA[dati$BRANCA_RIELABORATA=="52. Dermosifilopatia"]<-"Dermosifilopatia"
  dati$BRANCA_RIELABORATA[dati$BRANCA_RIELABORATA=="38. Otorinolaringoiatria"]<-"Otorinolaringoiatria"
  dati$BRANCA_RIELABORATA[dati$BRANCA_RIELABORATA=="34. Oculistica"]<-"Oculistica"
  dati$BRANCA_RIELABORATA[dati$BRANCA_RIELABORATA=="03. Laboratorio analisi chimico cliniche e microbiologiche - microbiologia - virologia - anatomia e istologia patologica - genetica - immunoematologia e servizio trasfusionale"]<-"Laboratorio Analisi"
  dati$BRANCA_RIELABORATA[dati$BRANCA_RIELABORATA=="69. Diagnostica per immagini: radiologia diagnostica"]<-"Radiologia"
  dati$BRANCA_RIELABORATA[dati$BRANCA_RIELABORATA=="36. Ortopedia e traumatologia"]<-"Ortopedia e Traumatologia"
  dati$BRANCA_RIELABORATA[dati$BRANCA_RIELABORATA=="70. Radioterapia"]<-"Radioterapia"
  dati$BRANCA_RIELABORATA[dati$BRANCA_RIELABORATA== "68. Pneumologia" ]<-"Pneumologia"
  dati$BRANCA_RIELABORATA[dati$BRANCA_RIELABORATA=="09. Chirurgia generale"]<-"Chirurgia generale"
  dati$BRANCA_RIELABORATA[dati$BRANCA_RIELABORATA=="61. Diagnostica per immagini: medicina nucleare"]<-"Medicina Nucleare"
  dati$BRANCA_RIELABORATA[dati$BRANCA_RIELABORATA=="32. Neurologia"]<-"Neurologia"
  dati$BRANCA_RIELABORATA[dati$BRANCA_RIELABORATA=="08. Cardiologia"]<-"Cardiologia"
  dati<-dati[-which(str_length(dati$DATA_PRENOT)==0),]
  dati<-dati2[-which(str_length(dati$DATA_PRESCR)==0),]
  

  #eliminoPrestazioneTotale
  dataReg<-dataReg[,-16]
  str(dataReg)
  

  
  lista<-list(dati,Soggetti,dataReg)
  return(lista)
  
}


write.csv(dati,"dati.csv",col.names = T,row.names = F,sep=";",dec=",")
write.csv(lista[[2]],"Soggetti.csv",sep=";",dec=",",row.names = F)
write.csv(lista[[3]],"dataReg.csv",sep=";",dec=",",row.names = F)
dati$TRIMESTRE_EROG<-str_remove_all(dati$TRIMESTRE_EROG,"°")
dati$SEMESTRE_EROG<-str_remove_all(dati$SEMESTRE_EROG,"°")

uniqu(dati$SEMESTRE_EROG)

dataReg[1:2,1:ncol(dataReg)]
dataReg$Branca<-as.factor(as.numeric(as.factor(dataReg$`dati$BRANCA_RIELABORATA`)))
dataReg$CLASSE_PRIORITA<-as.factor(dataReg$CLASSE_PRIORITA)

dataReg<-cbind(dataReg,dati$BRANCA_RIELABORATA)

mod<-lm(dataReg$SPESA~dataReg$ETA+dataReg$SESSO+dataReg$FLAG_RESIDENZA+dataReg$GG_PRESCR_PRENOT+dataReg$GG_PRENOT_EROG+
          dataReg$FLAG_TIPO_RICETTA+dataReg$CLASSE_PRIORITA+dataReg$FLG_NON_ESEN+dataReg$FLG_OSPEDALE+dataReg$Branca)
summary(mod)

mod<-lm(dataReg$SPESA~dataReg$ETA+dataReg$SESSO+dataReg$FLAG_RESIDENZA+dataReg$GG_PRESCR_PRENOT+dataReg$GG_PRENOT_EROG+
          dataReg$FLAG_TIPO_RICETTA+dataReg$CLASSE_PRIORITA+dataReg$FLG_OSPEDALE)
summary(mod)

mod<-lm(dataReg$SPESA~dataReg$ETA+dataReg$FLAG_RESIDENZA+dataReg$GG_PRESCR_PRENOT+dataReg$GG_PRENOT_EROG+
          dataReg$FLAG_TIPO_RICETTA+dataReg$CLASSE_PRIORITA+dataReg$FLG_OSPEDALE+dataReg$Branca+dataReg$TRIMESTRE_EROG)
summary(mod)

dataReg$Branca[1:100]

unique(dataReg$TRIMESTRE_EROG)
table(dataReg$FLAG_TIPO_RICETTA)
# install.packages("fastDummies",dependencies = T)
# 
# check_type <- function(.data) {
#   if (data.table::is.data.table(.data)) {
#     data_type <- "is_data_table"
#   } else if (tibble::is_tibble(.data)) {
#     data_type <- "is_tibble"
#   } else {
#     data_type <- "is_data_frame"
#   }
#   
#   return(data_type)
# }
# 
# fix_data_type <- function(.data, data_type) {
#   if (data_type == "is_data_frame") {
#     .data <- as.data.frame(.data)
#   } else if (data_type == "is_tibble") {
#     .data <- tibble::as_tibble(.data)
#   }
#   
#   return(.data)
# }

# 
# library("fastDummies")
# help(fastDummies)
# ??fastDummies
# crime <- data.frame(city = c("SF", "SF", "NYC"),
#                     year = c(1990, 2000, 1990),
#                     crime = 1:3)
# dummy_cols(crime)
# # Include year column
# dummy_cols(crime, select_columns = c("city", "year"))
# # Remove first dummy for each pair of dummy columns made
# dummy_cols(crime, select_columns = c("city", "year"),
#            remove_first_dummy = TRUE)
# 
# d<-dummy_cols(dataReg)
# 
# write.csv(lista[[1]],"dati.csv",col.names = T,row.names = F,sep=";",dec=",")
# write.csv(lista[[2]],"data_altro.csv",sep=";",dec=",",row.names = F)
# write.csv(lista[[3]],"Soggetti.csv",sep=";",dec=",",row.names = F)
# write.csv(lista[[4]],"dataReg.csv",sep=";",dec=",",row.names = F)
# 
# dati_2019<-read.csv("dati.csv",sep=";",dec=",",header=T,stringsAsFactors = F)
# 
# data_altro<-read.csv("data_altro.csv",sep=",",dec=".",header=T,stringsAsFactors = F)
