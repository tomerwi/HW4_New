
setwd("D:\\HW4")

library(readr)
train <- read_csv("train.csv")




results <- data.frame(queryId = numeric(), simTitle = numeric(),simDescription = numeric(),qGramTitle = numeric(),qGramDescription = numeric())



train1 <- read_csv("train_after_stemming.csv")
train1$ds<-NULL

library("stringdist")



for (i in 1:10158)

  {
  #calculate q-grams
  
qGramTitle<-stringdist(train1$query[i], train1$product_title[i], method='qgram', q=4)  
  
qGramDescription<-stringdist(train1$query[i], train1$product_description[i], method='qgram', q=4)  


#calculate cosSimilarity

simTitle<-stringdist(train1$query[i], train1$product_title[i], method='cosine', q=2)

simDescription<-stringdist(train1$query[i], train1$product_description[i], method='cosine', q=2)


 results<-rbind(results, data.frame(queryId =train$id[i] , simTitle = simTitle,simDescription=simDescription,qGramTitle=qGramTitle,qGramDescription=qGramDescription))



}


results["Median_Rating"] <- train1$median_relevance


library(RWeka)
library(partykit)

results$queryId<-NULL

m1<-J48(Median_Rating~., data = results)

summary(m1)

#load test set
test <- read_csv("test.csv")

test_featuers<-data.frame(simTitle = numeric(), simDescription = numeric())

for (i in 1:22513)
{
  
  #preproccesing to query
  queries <- paste(test$query[i], collapse=" ")
  query=NULL
  if (length(queries)==1)
  {
    query=queries
  } else {
    queries_source <- VectorSource(queries)
    
    corpus <- Corpus(queries_source)
    
    corpus <- tm_map(corpus, content_transformer(tolower))
    corpus <- tm_map(corpus, removePunctuation)
    
    corpus <- tm_map(corpus, stripWhitespace)
    
    corpus <- tm_map(corpus, removeWords, stopwords("english"))
    
    
    dtm <- DocumentTermMatrix(corpus)
    
    query<-dtm$dimnames$Terms
  }
  
  
  
  
  #preproccesing to title
  title<-paste(test$product_title[i], collapse=" ")
  titlep=NULL
  
  if (length(queries)==1)
  {
    titlep=title
  } else {
    queries_source <- VectorSource(title)
    
    corpus <- Corpus(queries_source)
    corpus <- tm_map(corpus, content_transformer(tolower))
    corpus <- tm_map(corpus, removePunctuation)
    
    corpus <- tm_map(corpus, stripWhitespace)
    
    corpus <- tm_map(corpus, removeWords, stopwords("english"))
    
    
    dtm <- DocumentTermMatrix(corpus)
    titlep<-dtm$dimnames$Terms
  }
  
  
  #cosSimilarity between title and query
  
  write( query, file=paste(td, "D1", sep="/"))
  write( titlep, file=paste(td, "D2", sep="/"))
  myMatrix = textmatrix(td, minWordLength=1)
  
  similaryTitleQuery <- lsa::cosine(myMatrix[,1], myMatrix[,2])
  
  
  #preproccesing to description
  desciption<-paste(test$product_description[i], collapse=" ")
  d=TRUE
  if(desciption!=""){
    d=FALSE
  }
  
  queries_source <- VectorSource(desciption)
  
  corpus <- Corpus(queries_source)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removePunctuation)
  
  corpus <- tm_map(corpus, stripWhitespace)
  
  corpus <- tm_map(corpus, removeWords, stopwords("english"))
  
  
  dtm <- DocumentTermMatrix(corpus)
  desciption<-dtm$dimnames$Terms
  desciption<-strsplit(gsub("[^[:alnum:] ]", "", desciption), " +")
  desciption<-as.character(desciption)
  
  
  write( desciption, file=paste(td, "D2", sep="/"))
  #totalSimilarity<-0 #initialization
  if(d==FALSE && length(desciption)!=0)
  {
    myMatrix = textmatrix(td, minWordLength=1)
    
    similaryContentQuery <- lsa::cosine(myMatrix[,1], myMatrix[,2])
    
    #   totalSimilarity<-0.8*similaryTitleQuery[1,1]+0.2*similaryContentQuery[1,1]
    
  }else {
    #   totalSimilarity<-similaryTitleQuery[1,1]
    similaryContentQuery<-0
  }
  
  test_featuers<-rbind(test_featuers, data.frame(simTitle = similaryTitleQuery,simDescription=similaryContentQuery))
  
  
  
}

write.csv(file="test1.csv", x=test_featuers)
test_featuer <- read_csv("test1.csv")
test_featuer$ds=NULL

library("dplyr")
library("randomForest")
predictions <- predict(m1, test_featuer)
# submit_data<-data.frame(id=numeric(),prediction=numeric())
submit_data <- select(test,id)
submit_data["prediction"] <- predictions
write.csv(submit_data, file = "Submission.csv",row.names=F)

m1<-randomForest(Median_Rating~., data = train1)

summary(m1)

#load test set
test <- read_csv("test.csv")

test_featuers<-data.frame(simTitle = numeric(), simDescription = numeric())


