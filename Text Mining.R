#RETAIL ANALYTICS FINAL PROJECT: GROUP 1
## Analysis of Project Success Rate on Kickstarter
##Phuong Nguyen: Text Mining

#WORKSPACE PRELIMINARIES
rm(list = ls()) # clear workspace 
setwd("~/Desktop/Retail Analytics FInal Kickstarter Project")
kickstarter_cleaned <- read.csv("~/Desktop/Retail Analytics FInal Kickstarter Project/kickstarter_cleaned.csv", stringsAsFactors=TRUE)
install.packages("tm")
install.packages("SnowballC")
library(tm) 
library(SnowballC) 
library(caret)
library(dplyr)
Sys.setlocale('LC_ALL','C') #avoid (language) settings to interfere with this script.
install.packages("wordcloud")
library(wordcloud)
library("stringr")library(tm)
library(ggplot2)
library(stringr)
############USE PROJECT NAME TO DETERMINE IF A PROJECT WILL SUCCEED #########
# Document-Term Matrix   
#The ds is too large, use a random sample of 0.1% of the file
0.1*192546
sample <-sample_n(kickstarter_cleaned,19255)
sample$name <- str_replace_all(string=sample$name, pattern= "[&â€¦™ðŸ¥]" , replacement= "")
text_corpus <- VCorpus(VectorSource(sample$name))
text_corpus <- tm_map(text_corpus,removePunctuation)
text_corpus <- tm_map(text_corpus,removeNumbers) 
text_corpus <- tm_map(text_corpus,content_transformer(tolower)) # convert to lower case 
text_corpus <- tm_map(text_corpus,removeWords,c(stopwords("en"),"project"))
text_corpus <- tm_map(text_corpus,stemDocument, language = "english")
toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
text_corpus <- tm_map(text_corpus,stripWhitespace)
######WORDCLOUD########
#Build Term-document Matrix
doc_mat <- TermDocumentMatrix(text_corpus)
m <- as.matrix(doc_mat)
v <- sort(rowSums(m), decreasing = TRUE)
d_Rcran <- data.frame(word = names(v), freq = v)
head(d_Rcran, 25) #Top 25 most commonly seen word in project Names
wordcloud(words = d_Rcran$word, 
          freq = d_Rcran$freq, 
          min.freq = 1,
          max.words = 100, 
          random.order = FALSE,
          rot.per = 0.0, # proportion words with 90 degree rotation
          colors = brewer.pal(4, "Set1"))

##############
dtm <- DocumentTermMatrix(text_corpus)  
inspect(dtm) # sneak peak
dtm <- weightTfIdf(dtm) # tfidf representation
inspect(dtm)

### estimation preliminaries  
dtm_s50 = removeSparseTerms(dtm, sparse = 0.000000000001) # keep terms that appear in at least 50% of docs
dtm_s60 = removeSparseTerms(dtm, sparse = .6) # keep terms that appear in at least 40% of docs
sample$terms_s50 = as.matrix(dtm_s50) # attach dtm to ds -- need to declare dtm as a matrix (technical requirement) 
sample$terms_s60 = as.matrix(dtm_s60)
dtm_s50
set.seed(420) # set random seed
idx <- sample(2,nrow(sample),replace=TRUE,prob=c(.6,.4)) # 60/40 training/validation split


#####LINEAR MODEL TO PREDICT PROJECT SUCCESS ########
# linear model 1 (uses terms_s50)
lm1 = glm(success~terms_s50+ backers_count+category+sub_category+converted_pledged_amount+country+goal+is_starrable+staff_pick+static_usd_rate+usd_pledged, 
          family=gaussian, data=sample[idx==1,]) # uses training data only

summary(lm1) # view parameter estimates
lm1_pred = predict(lm1,newdata=ds) # predicted ratings  
lm1_pred = ifelse(lm1_pred<0,0,lm1_pred) # truncate at bounds
lm1_pred = ifelse(lm1_pred>100,100,lm1_pred)  
postResample(pred = lm1_pred[idx==1], obs = ds$review_ratings[idx==1]) # in-sample 
postResample(pred = lm1_pred[idx==2], obs = ds$review_ratings[idx==2]) # out-of-sample