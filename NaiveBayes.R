#Import libraries


naiveBayesFun<-function(textoPred){
  

library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer) 
library(e1071)         #For Naive Bayes
library(caret)         #For the Confusion Matrix

library(gmodels) #provides CrossTable() function for comparison

db<-read.csv('train.csv')

test_txt<-textoPred


db<-db[,c(3,5)]
colnames(db)<-c('Msg','Tag')

db_ade<-subset(db,db$Tag=='Adequate')
db_in<-subset(db,db$Tag=='Ineffective')
db_ef<-subset(db,db$Tag=='Effective')

db_ade<-head(db_ade,300)
db_in<-head(db_in,300)
db_ef<-head(db_ef,300)

db<-rbind(db_ade,db_in,db_ef)

db[nrow(db)+1,]<-c(test_txt,'Ineffective')


db$Tag<-factor(db$Tag)

# creating our corpus

# db<-tail(db,200)
text_corpus <- VCorpus(VectorSource(db$Msg))

# Viewing the content of more than one texts using lapply() function
lapply(text_corpus[1:5], as.character) 

cleanCorpus <- tm_map(text_corpus, content_transformer(tolower)) # lowercase all texts
cleanCorpus <- tm_map(cleanCorpus, removeNumbers) # remove all numbers
cleanCorpus <- tm_map(cleanCorpus, removeWords, stopwords('english')) # remove all common words such as to, but and etc.
cleanCorpus <- tm_map(cleanCorpus, removePunctuation) # remove all punctuation
cleanCorpus <- tm_map(cleanCorpus, stripWhitespace) # remove all whitespace

text_dtm <- DocumentTermMatrix(cleanCorpus)
# inspect(text_dtm)

# Creating train and test portions 
porcentaje<-0.7
set.seed(123)
corte <- sample(nrow(text_dtm),nrow(text_dtm)*porcentaje)

train <- text_dtm[corte,] # 70% for training
test <- text_dtm[-corte, ] # 30% for testing
train_type <- db[corte, ]$Tag
test_type <- db[-corte, ]$Tag


#training portion
tbl_train <- prop.table(table(train_type))

#testing portion
tbl_test <- prop.table(table(test_type))

# spamText <- subset(db, Tag == "Adequate") 
# wordcloud(spamText$Msg, max.words = 50, scale = c(5, 0.3),random.order = FALSE, rot.per = 0.15, colors = brewer.pal(8, "Dark2") )
# 
# hamText <- subset(db, Tag =="Ineffective") # selecting ham texts
# wordcloud(hamText$Msg, max.words = 50, scale = c(5, 0.3),random.order = FALSE, rot.per = 0.15, colors = brewer.pal(8, "Dark2"))

freq_words <- findFreqTerms(train, 5) 


# Selecting only the frequent words from the train and test datasets
freq_words_train <- train[ , freq_words]
freq_words_test <- test[ , freq_words]


# creating a function for conversion
convert <- function(x) {x <- ifelse(x > 0, "y", "n")} 
train_conve <- apply(freq_words_train, MARGIN = 2, convert)
test <- apply(freq_words_test, MARGIN = 2, convert)
# str(train) # verifying the conversion



# Creating a Naive Bayes classifier
sms_classifier <- naiveBayes(train_conve, train_type)




# Making prediction & evaluation with the classifier
test_prediction <- predict(sms_classifier, test)

prediccionCorrecta<-tail(test_prediction,1)
# 
drt<-confusionMatrix(data=test_prediction, reference = test_type)

drtFinal<-drt$overall[1]

return(prediccionCorrecta)

}

naiveBayesFun_Ac<-function(textoPred){
  
  
  library(tm)
  library(SnowballC)
  library(wordcloud)
  library(RColorBrewer) 
  library(e1071)         #For Naive Bayes
  library(caret)         #For the Confusion Matrix
  
  library(gmodels) #provides CrossTable() function for comparison
  
  db<-read.csv('train.csv')
  
  test_txt<-textoPred
  
  
  db<-db[,c(3,5)]
  colnames(db)<-c('Msg','Tag')
  
  db_ade<-subset(db,db$Tag=='Adequate')
  db_in<-subset(db,db$Tag=='Ineffective')
  db_ef<-subset(db,db$Tag=='Effective')
  
  db_ade<-head(db_ade,300)
  db_in<-head(db_in,300)
  db_ef<-head(db_ef,300)
  
  db<-rbind(db_ade,db_in,db_ef)
  
  db[nrow(db)+1,]<-c(test_txt,'Ineffective')
  
  
  db$Tag<-factor(db$Tag)
  
  # creating our corpus
  
  # db<-tail(db,200)
  text_corpus <- VCorpus(VectorSource(db$Msg))
  
  # Viewing the content of more than one texts using lapply() function
  lapply(text_corpus[1:5], as.character) 
  
  cleanCorpus <- tm_map(text_corpus, content_transformer(tolower)) # lowercase all texts
  cleanCorpus <- tm_map(cleanCorpus, removeNumbers) # remove all numbers
  cleanCorpus <- tm_map(cleanCorpus, removeWords, stopwords('english')) # remove all common words such as to, but and etc.
  cleanCorpus <- tm_map(cleanCorpus, removePunctuation) # remove all punctuation
  cleanCorpus <- tm_map(cleanCorpus, stripWhitespace) # remove all whitespace
  
  text_dtm <- DocumentTermMatrix(cleanCorpus)
  # inspect(text_dtm)
  
  # Creating train and test portions 
  porcentaje<-0.7
  set.seed(123)
  corte <- sample(nrow(text_dtm),nrow(text_dtm)*porcentaje)
  
  train <- text_dtm[corte,] # 70% for training
  test <- text_dtm[-corte, ] # 30% for testing
  train_type <- db[corte, ]$Tag
  test_type <- db[-corte, ]$Tag
  
  
  #training portion
  tbl_train <- prop.table(table(train_type))
  
  #testing portion
  tbl_test <- prop.table(table(test_type))
  
  # spamText <- subset(db, Tag == "Adequate") 
  # wordcloud(spamText$Msg, max.words = 50, scale = c(5, 0.3),random.order = FALSE, rot.per = 0.15, colors = brewer.pal(8, "Dark2") )
  # 
  # hamText <- subset(db, Tag =="Ineffective") # selecting ham texts
  # wordcloud(hamText$Msg, max.words = 50, scale = c(5, 0.3),random.order = FALSE, rot.per = 0.15, colors = brewer.pal(8, "Dark2"))
  
  freq_words <- findFreqTerms(train, 5) 
  
  
  # Selecting only the frequent words from the train and test datasets
  freq_words_train <- train[ , freq_words]
  freq_words_test <- test[ , freq_words]
  
  
  # creating a function for conversion
  convert <- function(x) {x <- ifelse(x > 0, "y", "n")} 
  train_conve <- apply(freq_words_train, MARGIN = 2, convert)
  test <- apply(freq_words_test, MARGIN = 2, convert)
  # str(train) # verifying the conversion
  
  
  
  # Creating a Naive Bayes classifier
  sms_classifier <- naiveBayes(train_conve, train_type)
  
  
  
  
  # Making prediction & evaluation with the classifier
  test_prediction <- predict(sms_classifier, test)
  
  prediccionCorrecta<-tail(test_prediction,1)
  # 
  drt<-confusionMatrix(data=test_prediction, reference = test_type)
  
  drtFinal<-drt$overall[1]
  
  return(drtFinal)
  
}



# #Modelo 2
# 
# test_prediction_improved <- predict(sms_classifier_improved, test)
# 
# CrossTable(test_prediction_improved, test_type, 
#            prop.chisq = FALSE, prop.t = FALSE,
#            dnn = c('predicted', 'actual'))


#Predecir un texto

# test_txt<-"though some say that life on Mars does exist, I think that there is no life on Mars."
# 
# text_corpus_txt <- VCorpus(VectorSource(test_txt))
# 
# 
# 
# cleanCorpus_txt <- tm_map(text_corpus_txt, content_transformer(tolower)) # lowercase all texts
# cleanCorpus_txt <- tm_map(cleanCorpus_txt, removeNumbers) # remove all numbers
# cleanCorpus_txt <- tm_map(cleanCorpus_txt, removeWords, stopwords('english')) # remove all common words such as to, but and etc.
# cleanCorpus_txt <- tm_map(cleanCorpus_txt, removePunctuation) # remove all punctuation
# cleanCorpus_txt <- tm_map(cleanCorpus_txt, stripWhitespace) # remove all whitespace
# 
# text_dtm_txt <- DocumentTermMatrix(cleanCorpus_txt)
# inspect(text_dtm_txt)
# 
# test_txt <- text_dtm_txt
# freq_words_txt <- findFreqTerms(test_txt, 5) 
# 
# freq_words_test_txt <- test_txt[ , freq_words_txt]
# 
# test_txt <- apply(freq_words_test_txt, MARGIN = 2, convert)
# 
# 
# test_prediction <- predict(sms_classifier, test_txt)
# test_prediction
# 



# 
# 
# test_txt_df<-paste(as.String(test_txt[1]),sep = ' ' )
# test_df<-as.data.frame(test_txt_df)
# 
# 
# colnames(test_df)<-c('Msg')
# 
# 
# text_corpus_test <- VCorpus(VectorSource(test_df$Msg))
# 
# 
# lapply(text_corpus_test, as.character) 
# 
# cleanCorpus_test_df <- tm_map(text_corpus_test, content_transformer(tolower)) # lowercase all texts
# cleanCorpus_test_df <- tm_map(cleanCorpus_test_df, removeNumbers) # remove all numbers
# cleanCorpus_test_df <- tm_map(cleanCorpus_test_df, removeWords, stopwords('english')) # remove all common words such as to, but and etc.
# cleanCorpus_test_df <- tm_map(cleanCorpus_test_df, removePunctuation) # remove all punctuation
# cleanCorpus_test_df <- tm_map(cleanCorpus_test_df, stripWhitespace) # remove all whitespace
# 
# text_dtm_test <- DocumentTermMatrix(cleanCorpus_test_df)
# 
# 
# inspect(text_dtm_test)
# 
# 
# 
# 
# test_prediction_test <- predict(sms_classifier, test_df$Msg)
# 
# # example <- confusionMatrix(data=predicted_value, reference = expected_value)


