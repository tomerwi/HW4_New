
setwd("C:\\Users\\Tomer\\Documents\\GitHub\\HW4_New")

library(readr)





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


 results<-rbind(results, data.frame(queryId =train1$id[i] , simTitle = simTitle,simDescription=simDescription,qGramTitle=qGramTitle,qGramDescription=qGramDescription))



}


results["Median_Rating"] <- train1$median_relevance

results$Median_Rating<-as.factor(results$Median_Rating)

library(RWeka)
library(partykit)

results$queryId<-NULL

m1<-J48(Median_Rating~., data = results)

summary(m1)

#load test set
test <- read_csv("test_after_stemming.csv")
test$ds<-NULL
test_featuers<-data.frame(simTitle = numeric(), simDescription = numeric(), qGramTitle = numeric(),qGramDescription = numeric())

for (i in 1:22513)
{
  
  #preproccesing to query
  qGramTitle<-stringdist(test$query[i], test$product_title[i], method='qgram', q=4)  
  
  qGramDescription<-stringdist(test$query[i], test$product_description[i], method='qgram', q=4)  
  
  
  #calculate cosSimilarity
  
  simTitle<-stringdist(test$query[i], test$product_title[i], method='cosine', q=2)
  
  simDescription<-stringdist(test$query[i], test$product_description[i], method='cosine', q=2)
  
  
  
  
  test_featuers<-rbind(test_featuers, data.frame(simTitle = simTitle,simDescription=simDescription, qGramTitle=qGramTitle,qGramDescription=qGramDescription ))
  
  
  
}




library("dplyr")

predictions <- predict(m1, test_featuers)
# submit_data<-data.frame(id=numeric(),prediction=numeric())
submit_data <- select(test,id)
submit_data["prediction"] <- predictions
write.csv(submit_data, file = "Submission3.csv",row.names=F)




