# HW4_New

Steps that we took:

1. Preprocessing to the train and to the test - Stemming, converting to lower letters, remove stopwords, remove punctuation.
    We wrote a new files which are called "train_after_stemming" and "test_after_stemming".
    Link to the files:
    https://github.com/tomerwi/HW4_New/blob/master/test_after_stemming.csv
    https://github.com/tomerwi/HW4_New/blob/master/train_after_stemming.csv
    
    #The code:
    


library(readr)
train <- read_csv("train.csv")



train_After_Stemming <- data.frame(id = numeric(), query = character(),product_title = character(), product_description = character(), median_relevance = numeric(), relevance_variance = numeric())

library(tm)
library(lsa)


for (i in 1:10158)
{

  #preproccesing to query
  
  queries <- paste(train$query[i], collapse=" ")
  query=NULL
  if (length(queries)==1)
  {
    current_query=queries
  } else {
    queries_source <- VectorSource(queries)

    corpus <- Corpus(queries_source)

    corpus <- tm_map(corpus, content_transformer(tolower))
    corpus <- tm_map(corpus, removePunctuation)

    corpus <- tm_map(corpus, stripWhitespace)

    corpus <- tm_map(corpus, removeWords, stopwords("english"))


    dtm <- DocumentTermMatrix(corpus)

    current_query<-dtm$dimnames$Terms
  }



  #preproccesing to title
  
  title<-paste(train$product_title[i], collapse=" ")

  queries_source <- VectorSource(title)

  corpus <- Corpus(queries_source)
  corpus <- tm_map(corpus, content_transformer(tolower)) 
  corpus <- tm_map(corpus, removePunctuation)

  corpus <- tm_map(corpus, stripWhitespace)

  corpus <- tm_map(corpus, removeWords, stopwords("english"))


  dtm <- DocumentTermMatrix(corpus)
  current_title<-dtm$dimnames$Terms

  total_title=""
  for (word in current_title)
  {
    total_title<-paste(total_title,word)
  }


  #preproccesing to description
  
  desciption<-paste(train$product_description[i], collapse=" ")
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
  current_desc<-as.character(desciption)
  
  total_desc=""
  for (word in current_desc)
  {
    total_desc<-paste(total_desc,word)
  }
  
  
  median <- train$median_relevance[i]
  var <- train$relevance_variance[i]
  
  #train_After_Stemming<-rbind(train_After_Stemming, data.frame(id =train$id[i] , query = current_query,product_title = title, product_description = desciption, median_relevance = median, relevance_variance = var))
  train_After_Stemming<-rbind(train_After_Stemming, data.frame(id =train$id[i] , query = current_query,product_title = total_title, product_description = total_desc, median_relevance = median, relevance_variance = var))

}



write.csv(file="train_after_stemming.csv", x=train_After_Stemming, row.names = F)





#Test

train <- read_csv("test.csv")

test_After_Stemming <- data.frame(id = numeric(), query = character(),product_title = character(), product_description = character())

for (i in 1:22513)
{
  
  #preproccesing to query
  
  queries <- paste(train$query[i], collapse=" ")
  query=NULL
  if (length(queries)==1)
  {
    current_query=queries
  } else {
    queries_source <- VectorSource(queries)
    
    corpus <- Corpus(queries_source)
    
    corpus <- tm_map(corpus, content_transformer(tolower))
    corpus <- tm_map(corpus, removePunctuation)
    
    corpus <- tm_map(corpus, stripWhitespace)
    
    corpus <- tm_map(corpus, removeWords, stopwords("english"))
    
    
    dtm <- DocumentTermMatrix(corpus)
    
    current_query<-dtm$dimnames$Terms
  }
  
  
  
  #preproccesing to title
  
  title<-paste(train$product_title[i], collapse=" ")
  
  queries_source <- VectorSource(title)
  
  corpus <- Corpus(queries_source)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removePunctuation)
  
  corpus <- tm_map(corpus, stripWhitespace)
  
  corpus <- tm_map(corpus, removeWords, stopwords("english"))
  
  
  dtm <- DocumentTermMatrix(corpus)
  current_title<-dtm$dimnames$Terms
  
  total_title=""
  for (word in current_title)
  {
    total_title<-paste(total_title,word)
  }
  
  
  #preproccesing to description
  
  desciption<-paste(train$product_description[i], collapse=" ")
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
  current_desc<-as.character(desciption)
  
  total_desc=""
  for (word in current_desc)
  {
    total_desc<-paste(total_desc,word)
  }
  
  


  test_After_Stemming<-rbind(test_After_Stemming, data.frame(id =train$id[i] , query = current_query,product_title = total_title, product_description = total_desc))
  
}


write.csv(file="test_after_stemming.csv", x=test_After_Stemming, row.names = F)



2. We started submitting our results to the site
    a. First Submission - We did cossineSimilarity with J48 model. 
    
    Link to the file: https://github.com/tomerwi/HW4_New/blob/master/cosSimJ48-1/cosSimilaritySubmission.R
  
  Screenshot:
 ![alt tag](/cosSimJ48-1/screenShot1.PNG)


    b. we changed the model to randomForest.
    
   Link to the file: https://github.com/tomerwi/HW4_New/blob/master/cosSimRandomForest-2/cosSimilaritySubmissionRandomForest.R
  
  Screenshot:
   ![alt tag](/cosSimRandomForest-2/screenShot2.PNG)
    
    c. We added another attribute to the J48 model - the attribute are Qgrams and Jaccard
    
   Link: https://github.com/tomerwi/HW4_New/blob/master/Qgram_Cosin_Jacard_J48-3/Qgram_Cosin_Jacard_J48.R
   
  Screenshot: 
   ![alt tag](/Qgram_Cosin_Jacard_J48-3/rank.JPG)
   
    d. We removed the jaccard becuase we noticed that it doesnt improve the performance. 
    
 Link: https://github.com/tomerwi/HW4_New/blob/master/Qgram_Cosin_J48-4/qGramJ48.R
 
  ![alt tag](/Qgram_Cosin_J48-4/Rank.JPG)
  
 
    e. We used Randomforest. It improved our position in one place :)
    
  Link: https://github.com/tomerwi/HW4_New/blob/master/Qgram_Cosin_RandomForest-5/Qgram_Cosin_RandomForest.R
   
    ![alt tag](/Qgram_Cosin_RandomForest-5/rank.JPG)
    
