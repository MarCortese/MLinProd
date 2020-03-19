# Machine Learning in Produzione
Esempio di sviluppo di una Shiny app con ML

Questa app, sviluppata in shiny con linguaggio R, è stata creata per un Meetup presso il Talent garden di Rende

![alt text](https://raw.githubusercontent.com/MarCortese/MLinProd/master/Evento/Locandina.jfif)

È articolata in 4 sezioni:

* Home
* Esplorazione dati
* Cluster
* Regressione
  
## Home
![alt text](https://raw.githubusercontent.com/MarCortese/MLinProd/master/Evento/Screen/Home.jpg)

## Esplorazione dati

Questa tab ha l'obbiettivo di mostrare le prime fasi che bisogna eseguire in una qualsiasi analisi. Tutto parte da un'analisi del fenomeno rappresentato dai dati cercando quindi di capire le variabili presenti nel dataset e la rispettiva distribuzione

##### Analisi delle variabili

![alt text](https://raw.githubusercontent.com/MarCortese/MLinProd/master/Evento/Screen/Esplora1.jpg)

##### Alcuni grafici descrittivi delle variabili.

![alt text](https://raw.githubusercontent.com/MarCortese/MLinProd/master/Evento/Screen/Esplora2.jpg)

![alt text](https://raw.githubusercontent.com/MarCortese/MLinProd/master/Evento/Screen/Esplora3.jpg)


## Cluster

La cluster è una tecnica di ML con apprendimento non supervisionato. Lo scopo di questa tecnica è quello di andare alla ricerca di gruppi di unità simili tra di loro. Alla base di un algoritmo di cluster gerarchico vi sono alcuni parametri ovvero la scelta della distanza da utilizzare, del legame e del numero di cluster che si vogliono cercare tra i dati. Per la scelta del valore da attribuire a queste variabili, in questa app sono stati scelti 2 radioButtons, che consentono una solo scelta per volta, e di un numeric input.

##### Scelta parametri 

![alt text](https://raw.githubusercontent.com/MarCortese/MLinProd/master/Evento/Screen/Cluster1.jpg)

##### Output 

Una volta attribuito un valore è possibile rappresentare l'output grafico, in questo caso un semplice dendogramma

![alt text](https://raw.githubusercontent.com/MarCortese/MLinProd/master/Evento/Screen/Cluster2.jpg)


## Regressione

La regressione, invece,  è una tecnica di ML con apprendimento supervisionato, nella quale quindi vi è la presenza della variabile target. Lo scopo di questa tecnica è quello di andare ad capire l'importanza delle variabile indipendenti nell'attribuzione del valore della variabile dipendente o target. La regressione, quindi, è caratterizzata da una prima fase nella quale si crea il modello con le variabili significative e una seconda fase nella quale, attribuendo un valore alle variabili del modello, si cerca di predire il valore della nostra variabile target. La scelta delle variabile deve prevedere la possibilità di inserire più variabili perciò è stata scelto un checkgroup.

##### Scelta variabili

![alt text](https://raw.githubusercontent.com/MarCortese/MLinProd/master/Evento/Screen/reg1.jpg)

##### Creazione modello

![alt text](https://raw.githubusercontent.com/MarCortese/MLinProd/master/Evento/Screen/reg2.jpg)

##### Previsioni

![alt text](https://raw.githubusercontent.com/MarCortese/MLinProd/master/Evento/Screen/reg3.jpg)





######## Link

App deployata su un account free perciò limitata nel tempo e se ancora disponibile può essere raggiunta al seguente link:

                                      https://marcocortese.shinyapps.io/MLinProd/
