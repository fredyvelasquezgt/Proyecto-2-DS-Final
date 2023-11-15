
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




