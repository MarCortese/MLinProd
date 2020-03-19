reg<-function(dati){
  dataReg<-dati[,c("SPESA","TRIMESTRE_EROG", "MESE_EROG" , "SESSO" , "CLASSE_ETA_CAT",
                   "REG_RES","AZIENDA_RES","TIPO_MED_PRESCR","FLAG_TIPO_RICETTA",
                   "DISCIPLINA","CLASSE_PRIORITA","PRESTAZIONE",
                   "FLG_NON_ESEN","FLG_OSPEDALE","BRANCA_RIELABORATA")]
  dataReg$REG_RES[dataReg$REG_RES=="Provincia di Bolzano" |dataReg$REG_RES== "Provincia di Trento"]<-"Trentino Alto Adige"
  dataReg$PRESTAZIONE<-as.character((dataReg$PRESTAZIONE))
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
  dataReg$FLAG_TIPO_RICETTA<-as.factor(dataReg$FLAG_TIPO_RICETTA)
  #dataReg$TRIMESTRE_EROG<-as.factor(dataReg$TRIMESTRE_EROG)
  
  
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
  
  #eliminoPrestazioneTotale
  dataReg<-dataReg[,-16]
  return(dataReg)
}


