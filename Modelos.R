
library("tm")
library("class")
library(plyr)
library(dplyr)
#Cargamos el dataset

db<- read.csv('train.csv')
text_predict<-c('efectivos','adecuados','ineficaces')

path<-db$discourse_text

#Limpiamos el texto


TextDoc <- Corpus(VectorSource(db$discourse_text))
#Replacing "/", "@" and "|" with space
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
TextDoc <- tm_map(TextDoc, toSpace, "/")
TextDoc <- tm_map(TextDoc, toSpace, "@")
TextDoc <- tm_map(TextDoc, toSpace, "\\|")

TextDoc<-tm_map(TextDoc,removePunctuation)
TextDoc<-tm_map(TextDoc,stripWhitespace)
TextDoc<-tm_map(TextDoc,tolower)
TextDoc<-tm_map(TextDoc,removeWords,stopwords('english'))



TextDoc_dtm <- TermDocumentMatrix(TextDoc)

generateTDM <-function(text_predict,path){
  
  
  result<-list(name=text_predict,tdm=TextDoc_dtm)
}

tdm<-lapply(text_predict, generateTDM)

bindCandidateToTDM<-function(tdm){
  s.mat<-t(data.matrix(tdm[['tdm']]))
  s.df<-as.data.frame(s.mat,stringsAsFactors=FALSE)
  s.df<-cbind(s.df,rep(tdm[['name']],nrow(s.df)))
  colnames(s.df)[ncol(s.df)]<-'target'
  return(s.df)
  
}


candTDM<-lapply(tdm, bindCandidateToTDM)

tdm.stack<-do.call(rbind.fill,candTDM)
tdm.stack[is.na(tdm.stack)]<-0



train.idx<-sample(nrow(tdm.stack),ceiling(nrow(tdm.stack)*0.7))
test.idx<-(1:nrow(tdm.stack))[-train.idx]

train.idx<-head(train.idx,5)
test.idx<-head(test.idx,5)
#Model KNN

tdm.cand<- tdm.stack[,'target']
tdm.stack.nl <-tdm.stack[,!colnames(tdm.stack) %in% 'target']

knn.pred<- knn(tdm.stack.nl[train.idx,],tdm.stack.nl[test.idx,],tdm.cand[train.idx])


#Acurracy

conf.mat<- table('predictions'=knn.pred, Actual=tdm.cand[test.idx])

#-------------------------------------------------------------------------------------------------new

# Palabras objetivo
text_predict <- c('efectivos', 'adecuados', 'ineficaces')

# Limpiamos el texto
clean_text <- function(text) {
  text <- tolower(text)
  text <- removePunctuation(text)
  text <- removeWords(text, stopwords('english'))
  return(text)
}

db$cleaned_text <- sapply(db$discourse_text, clean_text)

# Crear una matriz de términos y documentos
corpus <- Corpus(VectorSource(db$cleaned_text))
tdm <- DocumentTermMatrix(corpus)

# Función para generar TDM para una palabra objetivo
generateTDM <- function(text_predict, tdm) {
  target_rows <- grep(paste(text_predict, collapse = "|"), rownames(tdm))
  return(as.data.frame(tdm[target_rows, ]))
}

# Crear una lista de TDM para cada palabra objetivo
tdm_list <- lapply(text_predict, generateTDM, tdm)

# Combina las TDM en un único marco de datos
tdm_combined <- do.call(rbind.fill, tdm_list)

# Agregar la columna target
tdm_combined$target <- rep(text_predict, each = nrow(tdm_list[[1]]))

# Muestra del conjunto de datos
head(tdm_combined)

# Partición de datos en conjuntos de entrenamiento y prueba
set.seed(123)
train_idx <- sample(nrow(tdm_combined), ceiling(nrow(tdm_combined) * 0.7))
train_data <- tdm_combined[train_idx, ]
test_data <- tdm_combined[-train_idx, ]

# Modelo KNN
knn_pred <- knn(train_data[, -ncol(train_data)], test_data[, -ncol(test_data)], train_data$target, k = 3)

# Evaluación del modelo
conf_mat <- table('predictions' = knn_pred, 'Actual' = test_data$target)
print(conf_mat)




