---
title: "TFM"
author: "Maximilien Scocozza"
date: "September 17, 2017"
---

```{r setup, include = FALSE}
knitr::opts_chunk$set(eval = FALSE)
```

### Instaladores y paquetes importantes
```{r}
doInstall <- TRUE  # Change to FALSE if you don't want packages installed.
toInstall <- c("ROAuth", "streamR", "ggplot2", "stringr",
	"maps", "Rfacebook", "topicmodels", "devtools", "quanteda",
	"igraph", "rvest", "httr", "yaml", "reshape", "formatR", "readtext",
	"glmnet", "doMC", "FactorMineR","psych","nFactors")
```
## Recopilación y tratamiento de los datos
### Obtención de los datos: Versión Streaming 
```{r}
## cargar programas
library(netdemR)
library(streamR)
library(jsonlite)

# Cargar autorización
load("credentials/twitter-token.Rdata")

#Términos a trackear
tracking <- c("climate","climate change", "global warming")

#Definir corte del loop
finalizacion <- as.numeric(format(Sys.time(),"%M"))+3
formatofechacorte <- "%Y_%m_%d_%H_%M"

#loop para streamear 
while (as.numeric(format(Sys.time(),"%M"))< finalizacion) {
  
## definir frecuencia de corte para los archivos
current.time <- format(Sys.time(), formatofechacorte)
file.name <- paste("tfm/json/prueba", current.time, ".json", sep="")

## capture tweets
filterStream( file=file.name, track= tracking , oauth=my_oauth, timeout = 30)
}
```
### Recuperación de los archivos JSON desde archivo y unificación de la base de datos
```{r}
library(netdemR)
library(streamR)
library(igraph)

# Vector con los nombre de los archivos
filenames <- list.files("C:/Users/aximi/Documents/R/tfm/json", pattern="*.json", full.names=TRUE) 

### abrir cada archivo JSON y reinterpretar columnas de twitter
    tweets <- parseTweets(filenames[1], simplify = FALSE, verbose = TRUE)
### filtro idioma inglés
    tweets.en <- tweets[tweets$lang=="en", ]
### identificación de Bots
    tweets.en$notbot <- grepl('//twitter.com', tweets.en$source, ignore.case=TRUE)
### identificación de RT
    tweets.en$RT <- grepl('^RT @', tweets.en$text, ignore.case=TRUE)
### creación matriz discurso
    discurso <- data.frame(texto=tweets.en$text, IDtweet=tweets.en$id_str, usuario=tweets.en$screen_name, notbot=tweets.en$notbot, replytoidstr=tweets.en$in_reply_to_status_id_str, stringsAsFactors = FALSE)[tweets.en$RT=="FALSE",]
### identificación de usuario y ID
    listausuario <- data.frame(usuario=tweets.en$user_id_str, nombreusr=tweets.en$screen_name, stringsAsFactors = FALSE)
### red de RT
    redRT <- data.frame(
                          fromedge = tweets.en$screen_name[tweets.en$RT=="TRUE"],  
                          toedge = gsub(
                                        pattern = '.*RT @([a-zA-Z0-9_]+):? ?.*', 
                                        x = tweets.en$text[tweets.en$RT=="TRUE"], 
                                        repl="\\1"),
                          IDtweet = tweets.en$id_str,
                          stringsAsFactors=F)
### TimeLine
TL <- data.frame(IDtweet=tweets.en$id_str, fecha=as.Date(x = paste(
                                    substr(tweets.en$created_at, 27, 30), #year
                                    ifelse(test = "Oct"==substr(tweets.en$created_at, 5, 7),yes = as.integer(10),no = ifelse(test = "Nov"==substr(tweets.en$created_at, 5, 7),yes = as.integer(11),no = as.integer(12))), 
                                    substr(tweets.en$created_at, 9, 10), #day
                                    sep = "/")), stringsAsFactors = F)


# Sumar los distintos archivos en un solo objeto 
for(i in 2:NROW(filenames)){
### abrir cada archivo JSON y reinterpretar columnas de twitter                
                tweets <- parseTweets(filenames[i], simplify = FALSE, verbose = TRUE)
### filtro idioma inglés
                tweets.en <- tweets[tweets$lang=="en", ]
### identificación de Bots
                tweets.en$notbot <- grepl('//twitter.com', tweets.en$source, ignore.case=TRUE)
### identificación de RT
                tweets.en$RT <- grepl('^RT @', tweets.en$text, ignore.case=TRUE)
### creación matriz discurso
                discurso <- rbind(discurso, data.frame(texto=tweets.en$text, IDtweet=tweets.en$id_str, usuario=tweets.en$screen_name, notbot=tweets.en$notbot, replytoidstr=tweets.en$in_reply_to_status_id_str, stringsAsFactors = FALSE)[tweets.en$RT=="FALSE",])
### identificación de usuario y ID
                listausuario <- rbind(listausuario, data.frame(usuario=tweets.en$user_id_str, nombreusr=tweets.en$screen_name, stringsAsFactors = FALSE))
### red de RT
                redRT <- rbind(redRT, data.frame(
                          fromedge = tweets.en$screen_name[tweets.en$RT=="TRUE"],  
                          toedge = gsub(
                                        pattern = '.*RT @([a-zA-Z0-9_]+):? ?.*', 
                                        x = tweets.en$text[tweets.en$RT=="TRUE"], 
                                        repl="\\1"),
                          IDtweet = tweets.en$id_str,
                          stringsAsFactors=F))
### TimeLine
TL <- rbind(TL, data.frame(IDtweet=tweets.en$id_str, fecha=as.Date(x = paste(
                                    substr(tweets.en$created_at, 27, 30), #year
                                    ifelse(test = "Oct"==substr(tweets.en$created_at, 5, 7),yes = as.integer(10),no = ifelse(test = "Nov"==substr(tweets.en$created_at, 5, 7),yes = as.integer(11),no = as.integer(12))), 
                                    substr(tweets.en$created_at, 9, 10), #day
                                    sep = "/")), stringsAsFactors = F))                
###Limpiar memoria RAM
                rm(tweets,tweets.en)
}
discurso <- unique(discurso)
listausuario <- unique(listausuario)
redRT <- unique(redRT)
redRT$tipo <- rep("retweet",NROW(redRT))

save(listausuario, file = "C:/Users/aximi/Documents/R/tfm/listausuario", compress = "gzip")
save(discurso, file = "C:/Users/aximi/Documents/R/tfm/discurso", compress = "gzip")
save(redRT, file = "C:/Users/aximi/Documents/R/tfm/redRT", compress = "gzip")
save(TL, file = "C:/Users/aximi/Documents/R/tfm/TL", compress = "gzip")

```


### Guardado de la base de datos en formato Excel
```{r}

write.csv(discurso, file="C:/Users/aximi/Documents/R/tfm/discurso.csv", row.names=FALSE)
```

## Análisis Discursivo
### preparado material de texto

Corpus
```{r}
###### creación de archivo texto
load(file = "C:/Users/aximi/Documents/R/tfm/discurso")

##corregir codificación
discurso$texto <- iconv(discurso$texto, "latin1", "ASCII", sub="")

###remover links 
discurso$texto <- gsub(pattern = "(^|\\s)http.*($|\\s)",
                       replacement = "", 
                       x = discurso$texto
                       )


remover <- c("t.co", "https", "http", "t.c")
for (p in remover) discurso$texto <- gsub (
                          pattern = p,
                          replacement ="", 
                          x = discurso$texto
                          )
rm(remover)

### eliminar textos repetidos y crear nuevo ID (transformar en factores )

texto_ID <- data.frame(texto=discurso$texto, IDtweet=discurso$IDtweet, stringsAsFactors = T)
unico_texto <- as.character(unique(texto_ID$texto))


library(quanteda)
discursocorpus <- corpus(unico_texto)

save(discursocorpus, file = "C:/Users/aximi/Documents/R/tfm/discursocorpus", compress = "gzip")

# limpieza de las oraciones y transformación en tokens
twdfm <- dfm(
              x = discursocorpus, 
              tolower = TRUE,
              remove_punct = TRUE, 
              remove_numbers= TRUE,
              remove= stopwords("english"),
              ngrams= 1:3,
              verbose=TRUE)


rm(discursocorpus)
rm(discurso)

### eliminar palabras no repetidas
twdfm <- dfm_trim(twdfm, min_docfreq = 2)


### graficar
textplot_wordcloud(twdfm, rot.per=0, scale=c(3.5, .75), max.words=100)

save(twdfm, file = "C:/Users/aximi/Documents/R/tfm/twdfm", compress = "gzip")

```

### topic modeling para creación de discursos
crear función para análisis de número de tópicos a partir de matriz de discurso
```{r eval=FALSE}
# we now export to a format that we can run the topic model with
twtm <- convert(twdfm, to="topicmodels")



## Choosing the number of topics
library(topicmodels)
library(cvTools)

cvLDA <- function(Ntopics,twtm,K=10) {
            folds<-cvFolds(nrow(twtm),K,1)
            perplex <- rep(NA,K)
            llk <- rep(NA,K)
  for(i in unique(folds$which)){
    cat(i, " ")
    which.test <- folds$subsets[folds$which==i]
    which.train <- {1:nrow(twtm)}[-which.test]
    twtm.train <- twtm[which.train,]
    twtm.test <- twtm[which.test,]
    lda.fit <- LDA(twtm.train, k=Ntopics, method="Gibbs",
        control=list(verbose=50L, iter=100))
    perplex[i] <- perplexity(lda.fit,twtm.test)
    llk[i] <- logLik(lda.fit)
  }
  return(list(K=Ntopics,perplexity=perplex,logLik=llk))
}
```

Análisis de número de tópicos
```{r eval=FALSE}
K <- c(20, 30, 40, 50, 60, 70, 80, 90, 100)

results <- list()

i = 1
for (k in K){
    cat("\n\n\n##########\n ", k, "topics", "\n")
    res <- cvLDA(k, twtm)
    results[[i]] <- res
    i = i + 1
}
```

graficar análisis de número de tópicos 
```{r eval=FALSE}
## plot
df <- data.frame(
    k = rep(K, each=10),
    perp =  unlist(lapply(results, '[[', 'perplexity')),
    loglk = unlist(lapply(results, '[[', 'logLik')),
    stringsAsFactors=F)

min(df$perp)
df$ratio_perp <- df$perp / max(df$perp)
df$ratio_lk <- df$loglk / min(df$loglk)

df <- data.frame(cbind(
    aggregate(df$ratio_perp, by=list(df$k), FUN=mean),
    aggregate(df$ratio_perp, by=list(df$k), FUN=sd)$x,
    aggregate(df$ratio_lk, by=list(df$k), FUN=mean)$x,
    aggregate(df$ratio_lk, by=list(df$k), FUN=sd)$x),
    stringsAsFactors=F)
names(df) <- c("k", "ratio_perp", "sd_perp", "ratio_lk", "sd_lk")
library(reshape)
pd <- melt(df[,c("k","ratio_perp", "ratio_lk")], id.vars="k")
pd2 <- melt(df[,c("k","sd_perp", "sd_lk")], id.vars="k")
pd$sd <- pd2$value
levels(pd$variable) <- c("Perplexity", "LogLikelihood")

library(ggplot2)
library(grid)

p <- ggplot(pd, aes(x=k, y=value, linetype=variable))
pq <- p + geom_line() + geom_point(aes(shape=variable), 
        fill="white", shape=21, size=1.40) +
    geom_errorbar(aes(ymax=value+sd, ymin=value-sd), width=4) +
    scale_y_continuous("Ratio wrt worst value") +
    scale_x_continuous("Number of topics", 
        breaks=K) +
    theme_bw() 
pq
```

correr algoritmo LDA final
```{r message = FALSE}

K <- 80 #Nro de tópicos óptima

library(topicmodels)

lda <- LDA(twtm, k = K, method = "Gibbs", 
                control = list(verbose=25L, seed = 123, burnin = 100, iter = 500))

save(lda, file = "C:/Users/aximi/Documents/R/tfm/lda", compress = "gzip")

rm(twtm,twdfm)
```

extracción de los topicos resultantes
```{r}
load(file = "C:/Users/aximi/Documents/R/tfm/lda")
terms <- get_terms(lda, 50)
save(terms, file = "C:/Users/aximi/Documents/R/tfm/terms", compress = "gzip")
```

extraer probabilidad de tópicos x texto y topico más probable
```{r}
load("R/tfm/lda")
topicprob <- as.data.frame(lda@gamma)
topics <- as.data.frame(get_topics(lda))


#crear columna con el número de fila correspondiente a los tweets del df discurso
topicprob$textnr <- as.integer(sub("text","",lda@documents))
topics$textnr <- as.integer(sub("text","",lda@documents))


save(topicprob, file = "C:/Users/aximi/Documents/R/tfm/topicprob", compress = "gzip")
save(topics, file = "C:/Users/aximi/Documents/R/tfm/topics", compress = "gzip")
rm(lda)
```
### De discurso x texto >> discurso x individuo
#### consolidar topicos en matriz de discursos
```{r message = FALSE}
### sumar columna de topicos
load(file = "C:/Users/aximi/Documents/R/tfm/discurso")
load(file = "C:/Users/aximi/Documents/R/tfm/topicprob")
#agregar columna con número de fila

texto_ID <- data.frame(texto=discurso$texto, 
                       IDtweet=discurso$IDtweet, 
                       stringsAsFactors = F)

unico_texto <- data.frame(texto = as.character(unique(texto_ID$texto)),
                          textnr = as.integer(seq(1:NROW(unique(texto_ID$texto)))), 
                          stringsAsFactors = F)

unico_texto <- dplyr::left_join(unico_texto,topicprob, by= "textnr")
texto_ID <- dplyr::left_join(texto_ID, unico_texto, by= "texto")
texto_ID$texto <- NULL
texto_ID$textonr <- NULL

#agregar tópico a partir de la unión entre discurso y topicos según columna de número de fila
discurso <- dplyr::left_join(discurso,texto_ID, by= "IDtweet")


save(discurso, file = "C:/Users/aximi/Documents/R/tfm/discurso", compress = "gzip")
rm(topicprob)

```

#### comprimir en usuarios
```{r message = FALSE}
load(file = "C:/Users/aximi/Documents/R/tfm/discurso")
###descartar tweets no topificables (estos fueron descartados en la tokenización ya que no tienen información suficiente para ser catalogados según su contenido)

discurso <- discurso[which(is.na(discurso$V1)=="FALSE"),]

#crear matriz con usuarios y agregar la frecuencia promedio de cada tópico
usrtopic <- data.frame(aggregate(x = discurso[,7],
                                 by=list(discurso$usuario), 
                                 FUN=mean),
                       stringsAsFactors = F
                       )
for (i in 8:86){
  usrtopic <- cbind(usrtopic, aggregate(x = discurso[,i],
                                 by=list(discurso$usuario), 
                                 FUN=mean)[,2]
                  )
}

names(usrtopic)[1] <- "usuario"
names(usrtopic)[2:81] <- as.list(paste0(rep("D"),seq(1:80)))

### Agregar cantidad de tweets maxtopic en cada topico (son n columnas más. donde n es la cantidad de tópicos)

### agregar columna con cantidad de tweets originales por cada usuario (ponderador)
discurso$Qtw <- rep(1,NROW(discurso$texto))

usrtopic$Qtw <- aggregate(x = discurso$Qtw,
                                 by=list(discurso$usuario), 
                                 FUN=sum)[,2]

### agregar ID de usuario

load(file = "C:/Users/aximi/Documents/R/tfm/listausuario")
listausuario$usrID <- listausuario$usuario
listausuario$usuario <- listausuario$nombreusr
listausuario$nombreusr <- NULL

usrtopic <- dplyr::left_join(x = usrtopic, 
                             y = listausuario,
                             by = "usuario"
                             )
 
### agregar topico más probable de cada usuario
usrtopic$maxtopic <- sapply(row.names(usrtopic) ,function(x) max(usrtopic[x,2:81]))

save(usrtopic, file = "C:/Users/aximi/Documents/R/tfm/usrtopic")

```

##### agregar Qmaxtopic de cada topico x usuario
```{r}
doParallel::registerDoParallel(makeCluster(detectCores()-1, type='PSOCK'))

load(file = "C:/Users/aximi/Documents/R/tfm/discurso")

###descartar tweets no topificables (estos fueron descartados en la tokenización ya que notienen información suficiente para ser catalogados según su contenido)

discurso <- discurso[which(is.na(discurso$V1)=="FALSE"),]

load(file = "C:/Users/aximi/Documents/R/tfm/usrtopic")

# contar maxtopic Di x Usuario
for (i in 1:80){
  usrtopic[i,84+i] sapply(X = seq(1:NROW(usrtopic)), FUN = function(persona) NROW(which(discurso$usuario==persona & discurso$maxtopic==1)))
}

```



### Análisis de los tópicos
#### recuperar topico maximo de cada tweet
```{r message = FALSE}

load(file = "C:/Users/aximi/Documents/R/tfm/discurso")
load(file = "C:/Users/aximi/Documents/R/tfm/topics")


### eliminar tweets sin topico
discurso <- discurso[which(is.na(discurso$V1)=="FASLE"),]

### agregar topico máximo a cada tweet
topics$maxtopic <- topics[,1]
topics[,1] <- NULL
discurso <- dplyr::left_join(x = discurso, 
                             y = topics, 
                             by = "textnr")


save(discurso, file = "C:/Users/aximi/Documents/R/tfm/discurso")
```

#### Análisis cualitativo de principales tweets de cada tópico
```{r message = FALSE}
###Imprimir para cada tópico top 50 palabras clave y top 20 tweets aleatorios
twtopic <- data.frame(D1= rep(0,50), stringsAsFactors = F)

#extraer filas donde Di es maximo, ordenar de mayor a menor y guardar 40 primeros mas 10 aleatorios
for (i in 1:80){
  tabla <-  data.frame(texto=discurso[which(discurso$maxtopic==i),1],ptop=discurso[which(discurso$maxtopic==i),i+6], stringsAsFactors = F)
  
  head <- as.data.frame(head(x = tabla[order(-tabla[,2]),1:2],n = 40),
                        stringsAsFactors = F)
 
  matchmuestra <- match(x = sample(x = tabla[!tabla[,1]%in%head[,1],1],
                                   size = ifelse(NROW(tabla)>=50,
                                                 10,
                                                 NROW(tabla)-NROW(head)
                                                 )
                                   ),
                        table = tabla[,1])
  
  head <- as.data.frame(paste0("discurso_",
                              i,
                              " (p=",
                              round(x = head[,2],
                                    digits = 2),
                              ") top:",
                              head[,1]
                              ),
                        stringsAsFactors = F)
   colnames(head) <- paste0("D",i)

  muestra <- as.data.frame(paste0("discurso_",
                                 i,
                                 " (p=",
                                 round(x = tabla[matchmuestra,2],
                                       digits = 2),
                                 ") aleatorio:",
                                 tabla[matchmuestra,1]),
                           stringsAsFactors = F)
  colnames(muestra) <- paste0("D",i)
  
  vacio <- as.data.frame(rep("",50-NROW(head)-NROW(muestra)),stringsAsFactors = F)
  colnames(vacio) <- paste0("D",i)
  
  twtopic[,i] <- rbind(head,muestra,vacio)
  colnames(twtopic)[i] <- paste0("D",i)
  rm(tabla,muestra,head,matchmuestra,vacio)
}

# colapsar terminos de cada tópico en la última fila de una gran matriz de topicos
load(file = "C:/Users/aximi/Documents/R/tfm/terms")
twtopic[51,] <- sapply(X = seq(1:80),FUN = function(topico) paste("## discurso ",topico, " ##",paste(terms[,topico], collapse = "; ")))


discursos_consolidados <- list()

### eliminar links de los textos
for (i in 1:80){
discursos_consolidados[[paste0("D",i)]]$tweets <- sapply(X = 1:50,FUN = function(fila) ifelse(test = grepl(pattern = "(^|\\s)http.*($|\\s)", x = twtopic[fila,i])==TRUE, yes = sub(pattern = "(^|\\s)http.*($|\\s)",replacement = "$link$", x = twtopic[fila,i]), no = twtopic[fila,i]))
}


for (i in 1:80){
discursos_consolidados[[paste0("D",i)]]$keywords <- twtopic[51,i]
}


save(discursos_consolidados, file = "C:/Users/aximi/Documents/R/tfm/discursos_consolidados")
save(twtopic, file = "C:/Users/aximi/Documents/R/tfm/twtopic")
write.csv(twtopic, file = "C:/Users/aximi/Documents/R/tfm/twtopic.csv")

```

#### Análisis cuantitativo (distribución en tweets, personas y comunidades)
```{r message = FALSE}
load(file = "C:/Users/aximi/Documents/R/tfm/discursos_consolidados")

### análisis cuantitativo (distribución en tweets, personas y comunidades)

# Q tweet x topico
discurso$Qtw <- rep(1,NROW(discurso$texto))
for (i in 1:80){
q_tweets <- sum(discurso$Qtw[which(discurso$maxtopic==i)])

discursos_consolidados[[paste0("D",i)]]["Q_tweets"] <- list(c(paste(round(q_tweets/NROW(discurso)*100,
                                                                          1),
                                                                    "% de tweets totales",
                                                                    sep = ""),
                                                              q_tweets)
                                                            )
rm(q_tweets)
}

#probabilidad media de cada topico en cada tweet
for (i in 1:80){
discursos_consolidados[[paste0("D",i)]]["probabilidad_media_tweet"] <- list(c(paste(round(((mean(discurso[,i+6])/(1/80)-1)*100),1),"% de la media",sep = ""),round(mean(discurso[,i+6]),4)))
}

# Q usuarios x topico
usrtopic$Qusr <- rep(1,NROW(usrtopic$usuario))
for (i in 1:80){
q_usr <- sum(usrtopic$usuario[which(usrtopic$maxtopic==i)])

discursos_consolidados[[paste0("D",i)]]["Q_usr"] <- list(c(paste(round(q_usr/NROW(usrtopic)*100,
                                                                 1
                                                                 ),
                                                           "% de usuarios totales",
                                                           sep = ""),
                                                         q_usr)
                                                         )
rm(q_usr)
}

### probabilidad media de cada Discurso por usuario 
for (i in 1:80){
mediatopico <- mean(usrtopic[,i+1])
discursos_consolidados[[paste0("D",i)]]["probabilidad_media_usuario"] <- list(c(paste(round(((mediatopico/(1/80)-1)*100),1),"% de la media",sep = ""),round(mediatopico,4)))
rm(mediatopico)
}

### ordenar discursos según cantidad de tweets totales
discursos_consolidados <- discursos_consolidados[order(-sapply(names(discursos_consolidados),function(x) as.numeric(discursos_consolidados[[x]]$Q_tweets[2])))]


save(discursos_consolidados, 
     file = "C:/Users/aximi/Documents/R/tfm/discursos_consolidados")

```

#### TABLA con datos de distribución >> topicos x tweets
```{r}
load(file = "C:/Users/aximi/Documents/R/tfm/discursos_consolidados")
##creo tabla con Qtweets
dist_topic_tweet <- as.data.frame(t(as.data.frame(sapply(discursos_consolidados,
                                                                             function(x) x$Q_tweets
                                                                             ),
                                                                      stringsAsFactors = F)
                                    ),stringsAsFactors = F
                                  )

#agrego datos de distribucion de tweets
dist_topic_tweet$dif_pvalue <- sapply(discursos_consolidados, function(x) x$probabilidad_media_tweet[1])
dist_topic_tweet$pvalue_medio <- sapply(discursos_consolidados, function(x) x$probabilidad_media_tweet[2])
#agrego datos de Q usr
dist_topic_tweet$Qusrtot <- sapply(discursos_consolidados, function(x) x$Q_usr[1])
dist_topic_tweet$Qusrtop <- sapply(discursos_consolidados, function(x) x$Q_usr[2])
#agrego datos de distribucion de usuarios
dist_topic_tweet$dif_pvalue_usr <- sapply(discursos_consolidados, function(x) x$probabilidad_media_usuario[1])
dist_topic_tweet$pvalue_medio_usr <- sapply(discursos_consolidados, function(x) x$probabilidad_media_usuario[2])

dist_topic_tweet <- dist_topic_tweet[order(-as.numeric(as.data.frame(t(as.data.frame(sapply(discursos_consolidados,function(x) x$Q_tweets), stringsAsFactors = F)), stringsAsFactors = F)$V2)),]


colnames(dist_topic_tweet) <- c("%Q de tweets",
                                "Q de tweets",
                                "%dif tw pvalue",
                                "tw pvalue medio", 
                                "%Q de usr",
                                "Q de usr",
                                "%dif usr pvalue", 
                                "usr pvalue medio")

save(dist_topic_tweet, 
     file = "C:/Users/aximi/Documents/R/tfm/dist_topic_tweet")

```

#### Graficar probabilidad de topicos vs frecuencia
```{r}


tabla <- data.frame(pvalue = as.numeric(sapply(names(discursos_consolidados), function(x) discursos_consolidados[[x]]$probabilidad_media_tweet[2])),
                    Qtweets = as.numeric(sapply(names(discursos_consolidados), function(x) discursos_consolidados[[x]]$Q_tweets[2])),
                       stringsAsFactors = F)
row.names(tabla) <- names(discursos_consolidados)
plot(x = tabla$Qtweets, y = tabla$pvalue)


```


#### imprimir información discursos
```{r}
load(file = "C:/Users/aximi/Documents/R/tfm/discursos_consolidados")
load(file = "C:/Users/aximi/Documents/R/tfm/dist_topic_tweet")

sink("C:/Users/aximi/Documents/R/tfm/discursos_consolidados.txt")
print(discursos_consolidados)

sink()

write.csv(x = dist_topic_tweet, file = "C:/Users/aximi/Documents/R/tfm/dist_topic_tweet.csv")

```

 
 
## Sección Relacional
### identificar comunidades a partir de retweets y menciones
#### Time line
```{r message = FALSE}
### definir marco temporal de la red
load(file = "C:/Users/aximi/Documents/R/tfm/TL")

TL$count  <- rep(1,NROW(TL$IDtweet))

freqtwdias <- aggregate(TL$count, by=list(TL$fecha), FUN=sum)

names(freqtwdias) <- c("fecha","Qtweets")

filtrodias <- TL$IDtweet[TL$fecha==freqtwdias$fecha[11]]

```
#### crear red con retweets y menciones
```{r message = FALSE}
load(file = "C:/Users/aximi/Documents/R/tfm/discurso")

# creación de red de menciones
redMC <- data.frame(IDtweet=discurso$IDtweet, texto=discurso$texto, stringsAsFactors = FALSE)
save(redMC, file = "C:/Users/aximi/Documents/R/tfm/redMC")
rm(list = ls())
.rs.restartR()

load(file = "C:/Users/aximi/Documents/R/tfm/redMC")
library(plyr)
redMC <- ddply(redMC, c("IDtweet"), 
               function(x){
                  mention <- unlist(stringr::str_extract_all(x$texto, "@\\w+"))
                  if (length(mention) > 0)
                    {return(data.frame(mention = mention, stringsAsFactors=FALSE))} 
                  else
                    {return(data.frame(mention = NA))}
                            }
               )
redMC$mention <- stringr::str_replace(redMC$mention,"@","")
redMC <- redMC[which(!redMC$mention=="NA"),]
save(redMC, file = "C:/Users/aximi/Documents/R/tfm/redMC")
rm(list = ls())
.rs.restartR()


load(file = "C:/Users/aximi/Documents/R/tfm/discurso")
tweetyusr <- data.frame(IDtweet=discurso$IDtweet, usuario=discurso$usuario, stringsAsFactors = FALSE)
rm(discurso)
.rs.restartR()


load(file = "C:/Users/aximi/Documents/R/tfm/redMC")
redMC <- dplyr::left_join(redMC,tweetyusr, by= "IDtweet")
####  no eliminar ID por ahora... redMC$IDtweet <- NULL
rm(tweetyusr)

redMC$tipo <- rep("menciones",NROW(redMC))

redMC <- data.frame(fromedge=redMC$usuario, toedge=redMC$mention, IDtweet=redMC$IDtweet, tipo=redMC$tipo,stringsAsFactors = FALSE)
save(redMC, file = "C:/Users/aximi/Documents/R/tfm/redMC")

load(file = "C:/Users/aximi/Documents/R/tfm/redRT")
redtweeter <- rbind(redMC,redRT)
save(redtweeter, file = "C:/Users/aximi/Documents/R/tfm/redtweeter")

```

#### Construir grapho
```{r message = FALSE}
load(file = "C:/Users/aximi/Documents/R/tfm/redtweeter")
load(file = "C:/Users/aximi/Documents/R/tfm/redRT")
library(igraph)

redMC <- redMC[redMC$IDtweet%in%filtrodias,]

g <- graph_from_data_frame(d=redRT, directed=TRUE)
g <- simplify(g, remove.multiple = F, remove.loops = T) 
V(g)$label <- V(g)$name
V(g)$name <- seq(1:NROW(V(g)$name))

#### identifico los componentes dentro del grafo y creo una matriz con la asignacion de cada usuario
comp <- components(g)
usuariocantidad <- dplyr::left_join(data.frame(usuario=V(g)$name,
                                                 componente=as.integer(comp$membership), 
                                                 stringsAsFactors = F),
                                      data.frame(cantidad=comp$csize,
                                                 componente=unique(as.integer(comp$membership)), 
                                                 stringsAsFactors = F), by = "componente")



#Con cuantos componentes me quedo?
cantidadcomponente <- data.frame(cantidad=comp$csize,
                                 componente=unique(as.integer(comp$membership)),
                                 stringsAsFactors = F)

cantidadcomponente$porcentaje <- cantidadcomponente$cantidad/NROW(usuariocantidad)
cantidadcomponente$porcentaje[which(cantidadcomponente==max(cantidadcomponente$cantidad))]

##los siguientes aportes son>>
distribucioncomponentes <- data.frame(f=as.numeric(format(x = sort(cantidadcomponente$porcentaje,decreasing = T)*100,nsmall = 4,digits = 4)),stringsAsFactors = F)
#
#
#
##el primer componente tiene mas del 95% de los casos y el segundo componente menos del 0,02% con solo
#
#
#



#eliminar componentes con menos de 1000 individuos
#vamos a calcular todo con esto excepto las comunidades que se va a hacer con el grafo no dirigido
giantdirected <- delete.vertices(g, usuariocantidad$cantidad<1000)
save(giantdirected,file = "C:/Users/pegASUS/Documents/R/tfm/giantdirected")

giant <- igraph::as.undirected(giantdirected)
giant <- delete_vertex_attr(graph = giant, name = "label")
save(giant,file = "C:/Users/pegASUS/Documents/R/tfm/giant")

rm(usuariocantidad)


grado <- data.frame(grado = as.numeric(V(giant)$degree), count= as.numeric(V(giant)$count), stringsAsFactors = F )

grado <- aggregate(x = grado$count, by = list(grado$grado), FUN=sum)
plot(grado)

for (i in  as.list(vertex_attr_names(giant))) {max(V(giant)$i)}

edge_density(giant)

reciprocity(giant)

transitivity(giant)

save(giant,file = "C:/Users/aximi/Documents/R/tfm/giant")


sort(x = authority_score(graph = g,
                         scale = T)$vector, 
     decreasing = TRUE, 
     na.last = TRUE)[1:30]

sort(x = page_rank(graph = g,
                   directed = T)$vector, 
     decreasing = T,
     na.last = T)[1:50]

sort(x = betweenness(graph = giant, directed = T, nobigint = F, normalized = T),
     decreasing = TRUE,
     na.last = T)[1:50]
```

#### identificar comunidades
```{r message = FALSE}
comm_fg <- cluster_fast_greedy(graph = giant, 
                               merges = T, 
                               modularity = T, 
                               membership = T)

```

#### Extraer parametros de las comunidades y crear consolidado


```{r message = FALSE}
rm(list = ls())
library(igraph)
load(file="C:/Users/aximi/Documents/R/tfm/comm_fg")
load(file="C:/Users/aximi/Documents/R/tfm/giant")
load(file="C:/Users/aximi/Documents/R/tfm/giantdirected")


###Crear matriz de comparacion de comunidades segun distintos atributos
###Cual es mas democratica?
###Crear función con todos los parametros a evaluar en cada comunidad
comparacioncomunidades <- function(comm,graph, verbose=TRUE){

#1: agrego identificacion de la comunidad en el grafo desde el algoritmo de deteccion de comunidades
    V(graph)$comm <- comm$membership
#2: creo objeto con las columnas a completar pero sin datos adentro
    consolidado_comm <- data.frame(comunidad= as.integer(unique(comm$membership)), stringsAsFactors = F)

###calcular cantidad de nodos en cada comunidad
    cuentanodos <- as.data.frame(x = sizes(comm),stringsAsFactors = F)
    cuentanodos$comunidad <- as.integer(cuentanodos$Community.sizes)
    cuentanodos$Qnodos <- cuentanodos$Freq
    cuentanodos$Community.sizes <- NULL
    cuentanodos$Freq <- NULL
    consolidado_comm <- dplyr::left_join(consolidado_comm, cuentanodos, by= "comunidad")

rm(cuentanodos)


   
    consolidado_comm$centr_btw <- rep(0,NROW(consolidado_comm$comunidad))
    consolidado_comm$centr_dg_in <- rep(0,NROW(consolidado_comm$comunidad))
    consolidado_comm$centr_dg_out <- rep(0,NROW(consolidado_comm$comunidad))
    consolidado_comm$centr_clo_in <- rep(0,NROW(consolidado_comm$comunidad))
    
    consolidado_comm$centr_btw_tm <- rep(0,NROW(consolidado_comm$comunidad))
    consolidado_comm$centr_dg_in_tm <- rep(0,NROW(consolidado_comm$comunidad))
    consolidado_comm$centr_dg_out_tm <- rep(0,NROW(consolidado_comm$comunidad))
    consolidado_comm$centr_clo_in_tm <- rep(0,NROW(consolidado_comm$comunidad))
    
    consolidado_comm$diametro <- rep(0,NROW(consolidado_comm$comunidad))
    consolidado_comm$densidad <- rep(0,NROW(consolidado_comm$comunidad))
    consolidado_comm$transitividad <- rep(0,NROW(consolidado_comm$comunidad))
    consolidado_comm$distanciamedia <- rep(0, NROW(consolidado_comm$comunidad))
    consolidado_comm$conductancia <- rep(0,NROW(consolidado_comm$comunidad))
      

    porcentaje  <- 1
    
###calcular Caracterisiticas de cada comunidad
    for(i in consolidado_comm$comunidad){
      #aislo la comunidad i
      subgrafo <- induced_subgraph(graph = graph, v = which(V(graph)$comm== i))
      
      
      
      #Centralidad Betweeness
      cen_btw <- centralization.betweenness(graph = subgrafo, directed = T, normalized = T)
      consolidado_comm[which(consolidado_comm$comunidad==i),which(names(consolidado_comm)=="centr_btw")] <-   cen_btw$centralization
      consolidado_comm[which(consolidado_comm$comunidad==i),which(names(consolidado_comm)=="centr_btw_tm")] <-   cen_btw$theoretical_max
      rm(cen_btw)
      
      #Centralidad Grado (cuán referido es)
      cen_deg_in <- centralization.degree(graph = subgrafo, normalized = T, mode = "in")
      consolidado_comm[which(consolidado_comm$comunidad==i),which(names(consolidado_comm)=="centr_dg_in")] <-   cen_deg_in$centralization
      consolidado_comm[which(consolidado_comm$comunidad==i),which(names(consolidado_comm)=="centr_dg_in_tm")] <-   cen_deg_in$theoretical_max
      rm(cen_deg_in)
      
      #Centralidad Grado (cuán activo retwiteando es)
      cen_deg_out <- centralization.degree(graph = subgrafo, normalized = T, mode = "out")
      consolidado_comm[which(consolidado_comm$comunidad==i),which(names(consolidado_comm)=="centr_dg_out")] <-   cen_deg_out$centralization
      consolidado_comm[which(consolidado_comm$comunidad==i),which(names(consolidado_comm)=="centr_dg_out_tm")] <-   cen_deg_out$theoretical_max
      rm(cen_deg_out)
      
      #Centralidad cercania (expansión del mensaje asimetricamente)
      cen_clo <- centralization.closeness(graph = subgrafo, mode = "in", normalized = T)
      consolidado_comm[which(consolidado_comm$comunidad==i),which(names(consolidado_comm)=="centr_clo_in")] <-   cen_clo$centralization
      consolidado_comm[which(consolidado_comm$comunidad==i),which(names(consolidado_comm)=="centr_clo_in_tm")] <-   cen_clo$theoretical_max
      rm(cen_clo)
      
      #Diametro
      consolidado_comm[which(consolidado_comm$comunidad==i),which(names(consolidado_comm)=="diametro")] <- NROW(get.diameter(graph = subgrafo, directed = T,unconnected = TRUE))
      
      #densidad
      consolidado_comm[which(consolidado_comm$comunidad==i),which(names(consolidado_comm)=="densidad")] <- edge_density(graph = subgrafo, loops = F)
      
      #transitividad
      consolidado_comm[which(consolidado_comm$comunidad==i),which(names(consolidado_comm)=="transitividad")] <- transitivity(graph = subgrafo, type = "global", isolates = NaN)
      
      #distancia media
      consolidado_comm[which(consolidado_comm$comunidad==i),which(names(consolidado_comm)=="distanciamedia")] <- average.path.length(graph = subgrafo, directed = T, unconnected = TRUE)
      
      #Conductancia
      
      consolidado_comm[which(consolidado_comm$comunidad==i),which(names(consolidado_comm)=="conductancia")] <- sum(degree(subgrafo, mode = "total", loops = F, normalized = F))/sum(degree(graph = graph, v=which(V(graph)$comm== i),mode = "total", loops = F, normalized = F))
      
      
      
      # trancking
      avance <- match(x = i, 
                     table = consolidado_comm$comunidad)/length(consolidado_comm$comunidad)
      
      
        
      if(verbose == TRUE 
         & avance*100 >= porcentaje){
        redondo <- round(x = avance*100,digits = 0)
        cat(redondo, "% de las comunidades registradas.", "\n")
      
      porcentaje <- redondo+1
      }
      
    }

    rm(subgrafo,porcentaje)
    return(consolidado_comm)
}

### realizar reporte de comparación de comunidades
consolidado_comm <- comparacioncomunidades(comm = comm_fg,
                       graph = giantdirected, 
                       verbose = T)



save(consolidado_comm, file="C:/Users/aximi/Documents/R/tfm/consolidado_comm", compress = "gzip")
write.csv(consolidado_comm, file="C:/Users/aximi/Documents/R/tfm/consolidado_comm.csv", row.names=FALSE)

```




## CONSOLIDACiÓN de las Bases
### Consolidación de la base en COMUNIDADES
#### comprimir discurso x usuario en comunidades 
##### Unir discurso y comunidades por usuarios
```{r message = FALSE}
rm(list=ls())
load(file = "C:/Users/aximi/Documents/R/tfm/usrtopic")
load(file = "C:/Users/aximi/Documents/R/tfm/comm_fg")
load(file = "C:/Users/aximi/Documents/R/tfm/giantdirected")
load(file = "C:/Users/aximi/Documents/R/tfm/listausuario")

library(igraph)
### tomar label de giantdirected y pasarlo a giant
giant <- igraph::as.undirected(giantdirected)

### asignar comunidad a cada usr
usrcomm <- data.frame(comunidad = comm_fg$membership, 
                      Qusr = rep(1,NROW(V(giant)$label)),
                      usuario = V(giant)$label,
                      stringsAsFactors = F)

### unir usuario-topico con usuario-comunidad 
usr_topic_comm <- dplyr::left_join(x = usrcomm, y = usrtopic, by= "usuario")
"se pierden usuarios que no son retweeteados o retweetean
los más problemáticos son los observadores que no interactuan ya que podrían incluirse con followers... 
out of scope of this research"
### eliminar las bases pesadas...
rm(usrtopic,comm_fg,giant)


### compactar topicos de usuario a comunidad
#descartar usuarios no topificables (son los que sólo retweetean)
"acá se pierde la posibilidad de analizar comunidades que sólo retweetean cosas de otras comunidades...
Algunas opciones son:
1. Asignar topico a partir de la deteccion de tweet original dentro del RT
2. Asignar comunidad según conecciones externas de la comunidad -fantasma- (puede estar conectada a varias comunidades)"
usr_topic_comm <- usr_topic_comm[which(is.na(usr_topic_comm$D1)=="FALSE"),]
save(usr_topic_comm, file = "C:/Users/aximi/Documents/R/tfm/usr_topic_comm")
```

##### Agregar usuarios en comunidades
```{r}
rm(list = ls())
load(file = "C:/Users/aximi/Documents/R/tfm/usr_topic_comm")

#crear matriz con comunidades y agregar la frecuencia promedio de cada tópico
commtopic <- data.frame(aggregate(x = usr_topic_comm[,match(x = "D1",table = colnames(usr_topic_comm))],
                                  by=list(usr_topic_comm$comunidad), 
                                  FUN=mean),
                        stringsAsFactors = F)

for (i in 1:79){
  commtopic <- cbind(commtopic, aggregate(x = usr_topic_comm[,match(x = "D1",table = colnames(usr_topic_comm))+i],
                                          by=list(usr_topic_comm$comunidad), 
                                          FUN=mean)[,2]
                     )}

names(commtopic)[1] <- "comunidad"
names(commtopic)[2:81] <- as.list(paste0(rep("D"),seq(1:80)))

save(commtopic, file = "C:/Users/aximi/Documents/R/tfm/commtopic")
```

##### Agregar variables con información consolidada y GUARDAR
```{r}
rm(list = ls())
load(file = "C:/Users/aximi/Documents/R/tfm/commtopic")
load(file = "C:/Users/aximi/Documents/R/tfm/usr_topic_comm")

### agregar columna con cantidad de usuarios por cada comunidad (ponderador)
commtopic$Qusr_tworiginal <- aggregate(x = usr_topic_comm$Qusr,
                                 by=list(usr_topic_comm$comunidad), 
                                 FUN=sum)[,2]

### agregar columna con cantidad de tweets originales por cada comunidad (ponderador)
commtopic$Qtw <- aggregate(x = usr_topic_comm$Qtw,
                                 by=list(usr_topic_comm$comunidad), 
                                 FUN=sum)[,2]

### agregar topico más probable de cada comunidad
commtopic$maxtopic <- sapply(1:NROW(commtopic) ,
                             function(fila) match(x = max(commtopic[fila,2:81]),
                                                  table = commtopic[fila,2:81])
                             )

rm(usr_topic_comm)

save(commtopic, file = "C:/Users/aximi/Documents/R/tfm/commtopic")
```

##### Agregar variables relacionales
```{r}
load(file="C:/Users/aximi/Documents/R/tfm/consolidado_comm")
load(file = "C:/Users/aximi/Documents/R/tfm/commtopic")

comunidades_rel_dis <- dplyr::left_join(consolidado_comm, 
                                        commtopic, 
                                        by = "comunidad")
  
save(comunidades_rel_dis, file = "C:/Users/aximi/Documents/R/tfm/comunidades_rel_dis")
```


### Unir Características de Discurso y Características de Relacionamiento
```{r message = FALSE}
load(file = "C:/Users/aximi/Documents/R/tfm/commtopic")
load(file = "C:/Users/aximi/Documents/R/tfm/consolidado_comm")

commtopic <- dplyr::left_join(x = consolidado_comm,
                              y = commtopic,
                              by = "comunidad")

#Construir variables relativas para eliminar sesgos por tamaño de comunidad
commtopic$distmedia_std <- commtopic$distanciamedia/commtopic$diametro
commtopic$Qusr_tworig_std <- commtopic$Qusr_tworiginal/commtopic$Qnodos
commtopic$Qusr <- NULL
commtopic$Qusr_tworiginal <-NULL
commtopic$centr_clo_in_tm <- NULL
commtopic$centr_dg_in_tm <- NULL
commtopic$centr_dg_out_tm <- NULL
commtopic$centr_btw_tm <- NULL
commtopic$distanciamedia <- NULL

save(commtopic, file = "C:/Users/aximi/Documents/R/tfm/commtopic")
write.csv(commtopic, file = "C:/Users/aximi/Documents/R/tfm/commtopic.csv")
```


## Análisis y Resultados
### análisis descriptivo de las variables
```{r}
#tenes que encontrar la base NO normalizada....
Resumen_VAR_comunidades <- psych::describe.by(x = comunidades_rel_dis[which(comunidades_rel_dis$Qnodos>10),],
                                              group = "comunidad", 
                                              mat = T) #solo analizo las comunidades con más de 10 miembros
xlsx::write.xlsx(x = Resumen_VAR_comunidades,file = "C:/Users/aximi/Documents/R/tfm/archivos extra/descripcion_factores.xlsx",sheetName = "res_var",append = T)
```

### Análsis de Componentes principales = FACTORIZACION de las VARIABLES
#### FACTOMINER -  con variables suplementarias 
#####Preparo base
```{r}
rm(list = ls())
load(file = "C:/Users/aximi/Documents/R/tfm/comunidades_rel_dis")

#guardo comunidades como rownames
row.names(x = comunidades_rel_dis) <-  comunidades_rel_dis$comunidad

#elimino topicos irrelevantes desde excel analizado
library(xlsx)
topicanalisis <- read.xlsx(file = "C:/Users/aximi/Documents/R/tfm/archivos extra/dist_topic_tweet.xlsx", 
          sheetName = "tablaR", 
          colIndex = c(1:4))

# elimino ultima fila vacía
topicanalisis <- topicanalisis[-61,]

# eliminar comunidades sin discursos
comunidades_rel_dis <- comunidades_rel_dis[which(is.na(comunidades_rel_dis$D1)=="FALSE"),]

#estandarizar variables (igual no las usas)
comunidades_rel_dis$Qusr_tworig_std <- comunidades_rel_dis$Qusr_tworig/comunidades_rel_dis$Qnodos
comunidades_rel_dis$distmedia_std <- comunidades_rel_dis$distanciamedia/comunidades_rel_dis$Qnodos

# acá se sacan solo los discursos irrelevantes
comunidades_rel_dis <- subset(x = comunidades_rel_dis ,
                              select = c("comunidad","Qnodos", "Qtw", "diametro", "centr_btw","centr_dg_in", "centr_dg_out", "centr_clo_in", "densidad", "transitividad", "conductancia", colnames(comunidades_rel_dis)[which(paste0("D",1:80)%in%topicanalisis$topico=="TRUE")+(match(x = "D1",colnames(comunidades_rel_dis))-1)], "maxtopic")) 

row.names(x = comunidades_rel_dis) <-  comunidades_rel_dis$comunidad
comunidades_rel_dis$comunidad <- NULL

commtopic_estimate <- comunidades_rel_dis


# limpiar la base (transformar NaN y NA en promedios de la variable) 
for (i in 1:NROW(colnames(comunidades_rel_dis))){
  comunidades_rel_dis[,i] <- sapply(X =comunidades_rel_dis[,i],
                          FUN = function(x) ifelse(is.nan(x)=="TRUE",
                                                   0
                                                   ,x))
  comunidades_rel_dis[,i] <- sapply(X =comunidades_rel_dis[,i],FUN = function(x) ifelse(is.na(x)=="TRUE",
                                                                    0
                                                                    ,x))
}

### NORMALIZACIÓN de VARIABLES
# variables lineales
for(i in 1:(NROW(colnames(comunidades_rel_dis))-1)){
  comunidades_rel_dis[,i] <-  as.data.frame(sapply(X = 1:NROW(comunidades_rel_dis),
                                         FUN = function(dato)  comunidades_rel_dis[dato,i]/max(comunidades_rel_dis[,i])))
}

# separar variables cuanti (sacar variables irrelevantes o suma lineal de otras) 
commtopic_estimate <- subset(x = comunidades_rel_dis ,select = c("centr_btw","centr_dg_in", "centr_dg_out", "centr_clo_in", "diametro", "transitividad", "conductancia"))


save(comunidades_rel_dis,file = "C:/Users/aximi/Documents/R/tfm/comunidades_rel_dis")
                                  
rm(topicanalisis)
```

#####defino cantidad de factores
```{r}

prueba <- FactoMineR::PCA(X= commtopic_estimate,
                          ncp = NCOL(commtopic_estimate), 
                          scale.unit = T,
                          graph = F
                          ) 
summary(prueba)
rm(commtopic_estimate)

```

#####Ejecuto analisis solo relacional
```{r message = FALSE}
rm(prueba)
load(file = "C:/Users/aximi/Documents/R/tfm/comunidades_rel_dis")
library(FactoMineR)

columnas_discurso <- colnames(comunidades_rel_dis)[seq(from= match(x = "D1",colnames(comunidades_rel_dis)),to = NCOL(comunidades_rel_dis)-1)]


result <- FactoMineR::PCA(X= comunidades_rel_dis,
                          ncp = 4, 
                          scale.unit = T, 
                          quali.sup = as.vector(sapply(X = c("maxtopic"),
                                                       FUN = function(columna) match(x = columna,
                                                                                     table = colnames(comunidades_rel_dis)))),
                          quanti.sup = as.vector(sapply(X = c("Qtw","Qnodos",columnas_discurso),FUN = function(columna) match(x = columna,table = colnames(comunidades_rel_dis)))), 
                          graph = F,
                          ind.sup = which(x = comunidades_rel_dis$Qnodos<=4.01381E-05) # Acá saco las comunidades con menos d 10 personas
) 


### agregar valor de los factores en PCA
commfact <- result$call$X

save(result, file = "C:/Users/aximi/Documents/R/tfm/result")
save(commfact, file = "C:/Users/aximi/Documents/R/tfm/commfact")

### agregar valor de los factores en PCA
commfact <- result$call$X

save(result, file = "C:/Users/aximi/Documents/R/tfm/result")
save(commfact, file = "C:/Users/aximi/Documents/R/tfm/commfact")
```

Test de Kaiser, Meyer, Olkin
```{r}

test_KMO <- psych::KMO(r = comunidades_rel_dis[which(comunidades_rel_dis$Qnodos>4.01381E-05),rownames(result$var$coord)])
test_KMO$MSA #debe ser mayor a 0.5 para que sea aceptable una factorización

test_Bartlett <- psych::cortest.bartlett(R = test_KMO$Image, 
                                         n = NROW(which(comunidades_rel_dis$Qnodos>4.01381E-05))) # asumo que las observaciones utilizadas son una muestra del universo con el fin de testear la hipótesis nula de no intercorrelacionalidad
test_Bartlett$p.value # p<0.05 indica que las correlaciones son significativas

```


Visualizar
```{r}
rm(list = ls())
load(file = "C:/Users/aximi/Documents/R/tfm/result")
library(FactoMineR)

result$eig

options("scipen"=10)

FactoMineR::plot.PCA(x = result, 
                     axes = c(1,2),
                     choix = "ind",
                     label = "none",
                     invisible = c("quali","quanti.sup","ind.sup"),
                     title = "Comunidades y discursos")

FactoMineR::graph.var(x = result,
          axes = c(1,2),
          col.var = "green", col.sup = "red", draw = c("var"), lim.cos2.var = 0)

```

guardar descripción de factores
```{r}
rm(list = ls())
load(file = "C:/Users/aximi/Documents/R/tfm/result")

lista <- dimdesc(res = result,axes = seq(1:(NCOL(result$var$coord))),proba = 0.05)

descripcion_factores <- data.frame(lista[[1]]$quanti)
descripcion_factores$factor <- 1
descripcion_factores$variable <- row.names(descripcion_factores)
row.names(descripcion_factores) <- NULL

for(i in 2:NCOL(result$var$coord)){
  
df <- data.frame(lista[[i]]$quanti)
df$factor <- i
df$variable <- row.names(df)
row.names(df) <- NULL
descripcion_factores <- rbind(descripcion_factores, df)
rm(df)
}
rm(lista, i)
Coordenadas_Discursos <- result$quanti.sup$coord

write.csv(x = Coordenadas_Discursos, file = "C:/Users/aximi/Documents/R/tfm/archivos extra/Coordenadas_Discursos.csv")


write.csv(x = descripcion_factores, file = "C:/Users/aximi/Documents/R/tfm/archivos extra/descripcion_factores.csv")

```


### Análisis de Clasificación Automática - DIMENSIONALIZACION de las COMUNIDADES
#### Clusters Jerarquicos
##### correr modelo
```{r}
rm(list = ls())
load(file = "C:/Users/aximi/Documents/R/tfm/result")



re_HCPC <- FactoMineR::HCPC(res = result,
                            nb.clust = -1, 
                            min = 6,
                            consol = T,
                            metric = "euclidean", 
                            method = "ward", 
                            order = T, 
                            graph.scale = "inertia",
                            description = T)

save(re_HCPC, file = "C:/Users/aximi/Documents/R/tfm/re_HCPC")
```



##### Analizar los clusters
```{r}
load(file = "C:/Users/aximi/Documents/R/tfm/re_HCPC")
resultados_HCPC <- re_HCPC$desc.var

###analisis por Variables
dt <- as.data.frame(re_HCPC$desc.var$quanti[[1]])
dt$variable <- rownames(dt)
dt$Cluster <- 1
analisis_HCPC <- dt

for (i in 2:length(re_HCPC$desc.var$quanti)){
  dt <- as.data.frame(re_HCPC$desc.var$quanti[[i]])
  dt$variable <- rownames(dt)
  dt$Cluster <- i
  analisis_HCPC <- rbind(analisis_HCPC,dt)
  rm(dt)
 }


xlsx::write.xlsx(x = analisis_HCPC,
                 file = "C:/Users/aximi/Documents/R/tfm/archivos extra/analisis_HCPC.xlsx")

###analisis por Dimensiones
dt <- as.data.frame(re_HCPC$desc.axes$quanti[[1]])
dt$variable <- rownames(dt)
dt$Cluster <- 1
analisis_HCPC_Dim <- dt

for (i in 2:length(re_HCPC$desc.axes$quanti)){
  dt <- as.data.frame(re_HCPC$desc.axes$quanti[[i]])
  dt$variable <- rownames(dt)
  dt$Cluster <- i
  analisis_HCPC_Dim <- rbind(analisis_HCPC_Dim,dt)
  rm(dt)
 }


xlsx::write.xlsx(x = analisis_HCPC_Dim,
                 file = "C:/Users/aximi/Documents/R/tfm/archivos extra/analisis_HCPC_Dim.xlsx")





# append cluster assignment
commfacttopic <- as.data.frame(re_HCPC$data.clust)
commfacttopic <- cbind(data.frame(comunidad= as.integer(rownames(commfacttopic)),
                                  cluster = as.integer(commfacttopic$clust), 
                                  stringsAsFactors = F),
                       commfacttopic[,1:(NCOL(commfacttopic)-1)])
rownames(commfacttopic) <- NULL

save(commfacttopic ,file = "C:/Users/aximi/Documents/R/tfm/commfacttopic")

xlsx::write.xlsx(x = commfacttopic, file = "C:/Users/aximi/Documents/R/tfm/archivos extra/commfacttopic.xlsx")


FactoMineR::plot.HCPC(x = re_HCPC, 
                      axes = c(3,4), 
                      ind.names = F,
                      choice = "map",
                      new.plot = T, 
                      centers.plot = T,
                      draw.tree = F,
                      xlim = c(-6,5),
                      title = "Clusterización de las Comunidades según Factores Relacionales")

### cuáles son los casos más representativos y los casos específicos?
# representativos
re_HCPC$desc.ind$para

# especificos
re_HCPC$desc.ind$dist

```
### Tweets de los parangones tipológicos
```{r}
load("R/tfm/discurso")
load("R/tfm/usr_topic_comm")
load("R/tfm/re_HCPC")


for (cluster in 2:6){
  comunidades <- as.character(rownames(as.data.frame(re_HCPC$desc.ind$para[cluster])))
  
  for (comu in comunidades){
    DF <- usr_topic_comm[which(usr_topic_comm$comunidad==comu),
                                       c(3,
                                         NCOL(usr_topic_comm)-1,
                                         NCOL(usr_topic_comm))]

    DF$cluster <- rep(x = cluster, NROW(DF))
    DF$comunidad <- rep(x = comu, NROW(DF))
    DF$maxtopic_usr <- DF$maxtopic 
    DF$maxtopic <- NULL
    
    if (exists("usr_cluster")){
      usr_cluster <- rbind(usr_cluster,DF)  
    }  else {
      usr_cluster <- DF}
  }  
  if (exists("usr_parangones")){
    usr_parangones <- rbind(usr_parangones,usr_cluster)  
  }  else {
    usr_parangones <- usr_cluster}
  
  rm(cluster,comu,comunidades,DF,usr_cluster)   
}



tweets_parangones <- dplyr::left_join(usr_parangones, discurso, by="usuario")

write.csv(x = tweets_parangones, file= "C:/Users/aximi/Documents/R/tfm/archivos extra/tweets_parangones.csv")
```

### exportar grafos al gephi!!!
Grafo usuarios...  es muy pesado... sólo se exportan los subgrafos..
```{r message = FALSE}
rm(list=ls())
library(igraph)
load(file="C:/Users/aximi/Documents/R/tfm/comm_fg")
load(file="C:/Users/aximi/Documents/R/tfm/giantdirected")
load(file = "C:/Users/aximi/Documents/R/tfm/commfacttopic")
load(file = "C:/Users/aximi/Documents/R/tfm/usrtopic")


###incluir caracterisitcas de Commtopic a grafo
  commclust <- data.frame(usuario= V(giantdirected)$label, 
                          comunidad=comm_fg$membership, 
                          stringsAsFactors = F)
  
  commclust <- dplyr::left_join(commclust, commfacttopic[1:2])
  commclust$cluster[which(is.na(commclust$cluster) == T)] <- 99
  commclust <- dplyr::left_join(commclust, usrtopic[which(duplicated(x = usrtopic$usuario)==F),c(1,NCOL(usrtopic))])
  
  # comunidad
  V(giantdirected)$comunidad <- commclust$comunidad
  
  # cluster
  V(giantdirected)$cluster <- commclust$cluster
  
  # Topico
  commclust$maxtopic[which(is.na(commclust$maxtopic)==T)] <- 99
  V(giantdirected)$maxtopic <- commclust$maxtopic
  

### construir grafo usuarios
E(giantdirected)$count = 1

grafo_usuarios <- simplify(giantdirected, remove.loops=T, 
                       edge.attr.comb=list(count="sum", 
                                           "ignore"))
E(grafo_usuarios)$weight <- E(grafo_usuarios)$count

rm(commclust,commfacttopic,usrtopic,comm_fg, giantdirected)
  




### función para transformar un grafo en formato GEPHI
grafo_a_matrices = function(g, folder_path = NULL, file_nodes = "nodes", file_edges = "edges"){
  require(igraph)
  
  # gexf nodes require two column data frame (id, label)
  # check if the input vertices has label already present
  # if not, just have the ids themselves as the label
  if(is.null(V(g)$label))
    V(g)$label <- as.character(V(g))
  
  # similarily if edges does not have weight, add default 1 weight
  if(is.null(E(g)$weight))
    E(g)$weight <- rep.int(1, ecount(g))
  
  
  edges <- t(Vectorize(ends, vectorize.args='es')(g, 1:ecount(g), names = T))
 
  edges <- data.frame(source=edges[,1], 
                      target = edges[,2], 
                      weight = E(g)$weight, 
                      stringsAsFactors = F)
  rownames(edges) <- NULL
  
  # combine all node attributes into a matrix (and take care of & for xml)
  nodes<- data.frame(sapply(list.vertex.attributes(g)[which(list.vertex.attributes(g)!="label")], function(attr) as.integer(get.vertex.attribute(g, attr))), 
                     stringsAsFactors = F)
  
  nodes <- cbind(data.frame(id = nodes$name,
                      label = V(g)$label, 
                      stringsAsFactors = F), 
                 nodes[,2:NCOL(nodes)]
                 )
  rownames(nodes) <- NULL
  ### Saving
  
  if(is.null(folder_path))
    folder_path <- getwd()
  
  
  write.csv(x = nodes, file =  paste0(folder_path,"/",file_nodes,".csv"),row.names = F)
  
  write.csv(x = edges, file =  paste0(folder_path,"/",file_edges,".csv"), row.names = F)
  
}

load(file = "C:/Users/aximi/Documents/R/tfm/re_HCPC")


### crear y guardar archivo gephi para cada cluster
for (i in 2:6){
### subdividir en 6 subgrafos con los parangons de cada cluster
  grafo_parangons <- induced_subgraph(graph = grafo_usuarios,
                                      vids = do.call(c,
                                                     sapply(X = as.numeric(names(re_HCPC$desc.ind$para[[i]])), 
                                                            FUN = function(x) which(V(grafo_usuarios)$comunidad== x))), impl = "auto"
  )
  
  grafo_a_matrices(g = grafo_parangons, 
                   folder_path = "C:/Users/aximi/Documents/R/tfm/gephi", 
                   file_nodes = paste0("nodos_usuarios_cluster_",i), 
                   file_edges = paste0("aristas_usuarios_cluster_",i))
}
rm(list = ls())

```

Grafo comunidades
```{r message = FALSE}
rm(list=ls())
library(igraph)
load(file="C:/Users/aximi/Documents/R/tfm/comm_fg")
load(file="C:/Users/aximi/Documents/R/tfm/giant")
load(file="C:/Users/aximi/Documents/R/tfm/giantdirected")
load(file="C:/Users/aximi/Documents/R/tfm/consolidado_comm")




### construir grafo comunidades
E(giantdirected)$count = 1
grafo_comunidades <- contract.vertices(graph = giantdirected, 
                                       mapping = comm_fg$membership,
                                       vertex.attr.comb=list(size="sum",
                                                             "ignore")
                                       )

grafo_comunidades <- simplify(grafo_comunidades, remove.loops=T, 
                       edge.attr.comb=list(count="sum", 
                                           "ignore"))
E(grafo_comunidades)$weight <- E(grafo_comunidades)$count

# caracteristicas de centralidad y demás
# agregar todo a grafo
grafo_comunidades <- `vertex.attributes<-`(grafo_comunidades, 
                                           index= V(grafo_comunidades),
                                           consolidado_comm[,c(2:6,11:15)])


###incluir caracterisitcas de Commtopic a grafo
# comunidad
V(giantdirected)$comunidad <- comm_fg$membership




#asignar valores en una tabla
tablausuario <- data.frame(usuario= V(giantdirected)$name,
                           comunidad= comm_fg$membership,
                           stringsAsFactors = F)

tablausuario <- dplyr::left_join(tablausuario, 
                                 consolidado_comm, 
                                 by="comunidad")



### función para transformar un grafo en formato GEPHI



saveAsGEXF = function(g, filepath="converted_graph.gexf"){
  require(igraph)
  require(rgexf)
  
  # gexf nodes require two column data frame (id, label)
  # check if the input vertices has label already present
  # if not, just have the ids themselves as the label
  if(is.null(V(g)$label))
    V(g)$label <- as.character(V(g))
  
  # similarily if edges does not have weight, add default 1 weight
  if(is.null(E(g)$weight))
    E(g)$weight <- rep.int(1, ecount(g))
  
  nodes <- data.frame(cbind(V(g), V(g)$label))
  edges <- t(Vectorize(ends, vectorize.args='es')(g, 1:ecount(g), names = F))
  
  # combine all node attributes into a matrix (and take care of & for xml)
  vAttrNames <- setdiff(list.vertex.attributes(g), "label") 
  nodesAtt <- data.frame(sapply(vAttrNames, function(attr) sub("&", "&",get.vertex.attribute(g, attr))))
  
  # combine all edge attributes into a matrix (and take care of & for xml)
  eAttrNames <- setdiff(list.edge.attributes(g), "weight") 
  edgesAtt <- data.frame(sapply(eAttrNames, function(attr) sub("&", "&",get.edge.attribute(g, attr))))
  
  # combine all graph attributes into a meta-data
  graphAtt <- sapply(list.graph.attributes(g), function(attr) sub("&", "&",get.graph.attribute(g, attr)))
  
  # generate the gexf object
  output <- write.gexf(nodes, edges, 
                       edgesWeight=E(g)$weight,
                       edgesAtt = edgesAtt,
                       nodesAtt = nodesAtt,
                       meta=c(list(creator="Maximiliano Scocozza", description="grafo de comunidades", keywords="igraph, gexf, R, rgexf"), graphAtt))
  
  print(output, filepath, replace=T)
}

### crear y guardar archivo gephi
## nivel Comunidades
saveAsGEXF(grafo_comunidades,
           filepath = "C:/Users/aximi/Documents/R/tfm/Gephi/grafo_comunidades.gexf")

```

