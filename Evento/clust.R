library(dplyr)
# ensuring reproducibility for sampling
set.seed(40)
# generating random variable set
# specifying ordered factors, strings will be converted to factors when using data.frame()
# customer ids come first, we will generate 200 customer ids from 1 to 200
id.s <- c(1:200) %>%
  factor()
budget.s <- sample(c("small", "med", "large"), 200, replace = T) %>%
  factor(levels=c("small", "med", "large"), 
         ordered = TRUE)
origins.s <- sample(c("x", "y", "z"), 200, replace = T, 
                    prob = c(0.7, 0.15, 0.15))
area.s <- sample(c("area1", "area2", "area3", "area4"), 200, 
                 replace = T,
                 prob = c(0.3, 0.1, 0.5, 0.2))
source.s <- sample(c("facebook", "email", "link", "app"), 200,   
                   replace = T,
                   prob = c(0.1,0.2, 0.3, 0.4))
## day of week - probabilities are mocking the demand curve
dow.s <- sample(c("mon", "tue", "wed", "thu", "fri", "sat", "sun"), 200, replace = T,
                prob = c(0.1, 0.1, 0.2, 0.2, 0.1, 0.1, 0.2)) %>%
  factor(levels=c("mon", "tue", "wed", "thu", "fri", "sat", "sun"), 
         ordered = TRUE)
# dish 
dish.s <- sample(c("delicious", "the one you don't like", "pizza"), 200, replace = T)

# by default, data.frame() will convert all the strings to factors
synthetic.customers <- data.frame(id.s, budget.s, origins.s, area.s, source.s, dow.s, dish.s)

synthetic.customers <- data.frame(dati_2018$CODICE_SOGGETTO,dati_2018$SEMESTRE_EROG,dati_2018$REGIONE_RICETTA,dati_2018$STRUTTURA_EROG,
                                  dati_2018$SESSO,dati_2018$DATA_NASCITA,dati_2018$REG_RES,dati_2018$TIPO_SPEC_MEDBASE,
                                  dati_2018$DISCIPLINA,dati_2018$CLASSE_PRIORITA,dati_2018$RICETTA_RIPETUTA,dati_2018$NUMERO_RIPETIZIONI,
                                  dati_2018$IMP_PRESTAZIONE)


#----- Dissimilarity Matrix -----#
library(cluster) 
# to perform different types of hierarchical clustering
# package functions used: daisy(), diana(), clusplot()
gower.dist <- daisy(synthetic.customers[ ,2:7], metric = c("gower"))
gower.dist <- daisy(synthetic.customers[ ,2:13], metric = c("gower"))

# class(gower.dist) 
## dissimilarity , dist

divisive.clust <- diana(as.matrix(gower.dist), 
                        diss = TRUE, keep.diss = TRUE)
plot(divisive.clust, main = "Divisive")

aggl.clust.c <- hclust(gower.dist, method = "complete")
plot(aggl.clust.c,
     main = "Agglomerative, complete linkages")

synthetic.customers<-cbind(synthetic.customers,cutree(aggl.clust.c, k=33))
cluster1<-synthetic.customers[synthetic.customers$`cutree(aggl.clust.c, k = 11)`==1,]
cluster2<-synthetic.customers[synthetic.customers$`cutree(aggl.clust.c, k = 11)`==2,]
cluster3<-synthetic.customers[synthetic.customers$`cutree(aggl.clust.c, k = 11)`==3,]
cluster4<-synthetic.customers[synthetic.customers$`cutree(aggl.clust.c, k = 11)`==4,]
cluster5<-synthetic.customers[synthetic.customers$`cutree(aggl.clust.c, k = 11)`==5,]
cluster6<-synthetic.customers[synthetic.customers$`cutree(aggl.clust.c, k = 11)`==6,]
cluster7<-synthetic.customers[synthetic.customers$`cutree(aggl.clust.c, k = 11)`==7,]
cluster8<-synthetic.customers[synthetic.customers$`cutree(aggl.clust.c, k = 11)`==8,]
cluster9<-synthetic.customers[synthetic.customers$`cutree(aggl.clust.c, k = 11)`==9,]
cluster10<-synthetic.customers[synthetic.customers$`cutree(aggl.clust.c, k = 11)`==10,]
cluster11<-synthetic.customers[synthetic.customers$`cutree(aggl.clust.c, k = 11)`==11,]


install.packages("fpc")






MatriceDist <- reactive({
  dist<-dist(dati2, method = "euclidean")
  #daisy(dati2[ ,2:13], metric = c("euclidean"))
})
output$MatrDist <- renderPrint({MatriceDist()})

Cluster_creato <- reactive({
 hc<- hclust(dist,method = "complete")
})
output$Cluster <- renderPrint({Cluster_creato()})

Dati_Cluster <- reactive({
  t<-cbind(dati,"Cluster"=cutree(hc, k=30))
})

Valori4Cluster<- reactive({
  cutree(Cluster_creato(), k=input$num)
})
colors = c("coral","darkorchid3", "darkgoldenrod2", "cornflowerblue","navy","darkmagenta","deeppink","green3","navajowhite1","peru")
output$Dendogramma<-renderPlot({
  plot(Cluster_creato(), main = "Distribuzione unita nei cluster", ylab = "Distanza", hang =
         + 0.1,frame.plot = TRUE)
  rect.hclust(Cluster_creato(), k=input$num, border =colors[1:input$num])
  
})
output$gruppi<-renderPrint({table(Valori4Cluster())})


tree<-hc
k=30
library(fpc)
cstats.table <- function(dist, tree, k) {
  clust.assess <- c("cluster.number","n","within.cluster.ss","average.within","average.between",
                    "wb.ratio","dunn2","avg.silwidth")
  clust.size <- c("cluster.size")
  stats.names <- c()
  row.clust <- c()
  output.stats <- matrix(ncol = k, nrow = length(clust.assess))
  cluster.sizes <- matrix(ncol = k, nrow = k)
  for(i in c(1:k)){
    row.clust[i] <- paste("Cluster-", i, " size")
  }
  for(i in c(2:k)){
    stats.names[i] <- paste("Test", i-1)
    
    for(j in seq_along(clust.assess)){
      output.stats[j, i] <- unlist(cluster.stats(d = dist, clustering = cutree(tree, k = i))[clust.assess])[j]
      
    }
    
    for(d in 1:k) {
      cluster.sizes[d, i] <- unlist(cluster.stats(d = dist, clustering = cutree(tree, k = i))[clust.size])[d]
      dim(cluster.sizes[d, i]) <- c(length(cluster.sizes[i]), 1)
      cluster.sizes[d, i]
      
    }
  }
  output.stats.df <- data.frame(output.stats)
  cluster.sizes <- data.frame(cluster.sizes)
  cluster.sizes[is.na(cluster.sizes)] <- 0
  rows.all <- c(clust.assess, row.clust)
  # rownames(output.stats.df) <- clust.assess
  output <- rbind(output.stats.df, cluster.sizes)[ ,-1]
  colnames(output) <- stats.names[2:k]
  rownames(output) <- rows.all
  is.num <- sapply(output, is.numeric)
  output[is.num] <- lapply(output[is.num], round, 2)
  output
}
# I am capping the maximum amout of clusters by 7
# I want to choose a reasonable number, based on which I will be able to see basic differences between customer groups as a result
stats.df.divisive <- cstats.table(gower.dist, divisive.clust, 30)
stats.df.divisive


library(ggplot2)
# Elbow
# Divisive clustering
ggplot(data = data.frame(t(cstats.table(gower.dist, divisive.clust, 15))), 
       aes(x=cluster.number, y=within.cluster.ss)) + 
  geom_point()+
  geom_line()+
  ggtitle("Divisive clustering") +
  labs(x = "Num.of clusters", y = "Within clusters sum of squares (SS)") +
  theme(plot.title = element_text(hjust = 0.5))


# Agglomerative clustering,provides a more ambiguous picture
ggplot(data = data.frame(t(cstats.table(gower.dist, aggl.clust.c, 15))), 
       aes(x=cluster.number, y=within.cluster.ss)) + 
  geom_point()+
  geom_line()+
  ggtitle("Agglomerative clustering") +
  labs(x = "Num.of clusters", y = "Within clusters sum of squares (SS)") +
  theme(plot.title = element_text(hjust = 0.5))





ggplot(data = data.frame(t(cstats.table(gower.dist, divisive.clust, 15))), 
       aes(x=cluster.number, y=avg.silwidth)) + 
  geom_point()+
  geom_line()+
  ggtitle("Divisive clustering") +
  labs(x = "Num.of clusters", y = "Average silhouette width") +
  theme(plot.title = element_text(hjust = 0.5))


ggplot(data = data.frame(t(cstats.table(gower.dist, aggl.clust.c, 15))), 
       aes(x=cluster.number, y=avg.silwidth)) + 
  geom_point()+
  geom_line()+
  ggtitle("Agglomerative clustering") +
  labs(x = "Num.of clusters", y = "Average silhouette width") +
  theme(plot.title = element_text(hjust = 0.5))



library("ggplot2")
library("reshape2")
library("purrr")
library("dplyr")
# let's start with a dendrogram
library("dendextend")
dendro <- as.dendrogram(aggl.clust.c)
dendro.col <- dendro %>%
  set("branches_k_color", k = 7, value =   c("darkslategray", "darkslategray4", "darkslategray3", "gold3", "darkcyan", "cyan3", "gold3")) %>%
  set("branches_lwd", 0.6) %>%
  set("labels_colors", 
      value = c("darkslategray")) %>% 
  set("labels_cex", 0.5)
ggd1 <- as.ggdend(dendro.col)
ggplot(ggd1, theme = theme_minimal()) +
  labs(x = "Num. observations", y = "Height", title = "Dendrogram, k = 7")