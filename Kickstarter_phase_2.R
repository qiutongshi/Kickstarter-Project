rm(list = ls()) # clear workspace 

library("plyr")  
library("dplyr")                                                
library("readr")  

#merge all csv file into one dataframe in working directory
#files = list.files(pattern="*.csv")
# First apply read.csv, then rbind
#mergedKS = do.call(rbind, lapply(files, function(x) read.csv(x, stringsAsFactors = TRUE)))
mergedKS <- read.csv('kickstarter_final.csv',stringsAsFactors = TRUE)
mergedKS$id = as.factor(mergedKS$id)

#Confirming there's no missing value#Confirming there's no missiTng value
library("tidyverse")
glimpse(mergedKS) #data overview, remove columns with no values and unnesesay values. 
#mergedKS = subset(mergedKS, select=-c(created_at, creator, currency_symbol, currency_trailing_code,
 #                                     current_currency, deadline, is_starrable,launched_at, photo, 
  #                                   usd_type, usd_pledged, pledged))#col reduced from 35 to 21 vcairables 
sum(is.na (mergedKS)) #check for totaly missing values
mergedKS <- na.omit(mergedKS) #remove all missing values

#remove distinct projects in dataframe by id
uniq <- unique(mergedKS$id)#check number of distinct projects: 26912
mergedKS = distinct(mergedKS,id,.keep_all= TRUE)

#create a new dataframe consisting only of meaningful nonrepeated numeric values
num_KS <- select_if(mergedKS, is.numeric)# Subset numeric columns with dplyr
num_KS = subset(num_KS, select=-c(usd_pledged, X, static_usd_rate))

#corr plot of all numeric variables
library(corrplot)
cor_KS <- cor(num_KS)
#Positive correlations are displayed in blue and negative correlations in red color. 
#Color intensity and the size of the circle are proportional to the correlation coefficients. 
corrplot(cor_KS, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)

#visualize quantile and outliers of pledged_amouint, goal, and backers count
attach(num_KS)
par(mfrow=c(1,3))
boxplot(converted_pledged_amount, main = 'converted_pledged_amount')
abline(h=quantile(converted_pledged_amount, c(0.25, 0.75)), col="red")
quantile(converted_pledged_amount, c(0.25,0.5,0.75))


boxplot(goal, main = 'goal')
abline(h=quantile(goal, c(0.25, 0.75)), col="red")
quantile(goal, c(0.25,0.5,0.75))

boxplot(backers_count, main='backers_count')
abline(h=quantile(backers_count, c(0.25, 0.75)), col="red")
quantile(backers_count, c(0.25,0.5,0.75))


#from boxplot we can see that the numeric values suffer from serious outlier problem
#to get an understanding of any potential distribution for the purpose of descriptive analyses
#get rid of outlier
cpa_out <- which(converted_pledged_amount %in% c(boxplot.stats(converted_pledged_amount)$out))
goal_out<-which(goal %in% c(boxplot.stats(goal)$out))
bckcount_out<-which(backers_count %in% c(boxplot.stats(backers_count)$out))
table(cpa_out)
#subsetting new dataframe with no outliers
cpa_new <- num_KS[-cpa_out,]
goal_new <- num_KS[-goal_out,]
bckcount_new<-num_KS[-bckcount_out,]

par(mfrow=c(1,3))
hist(cpa_new$converted_pledged_amount)
hist(goal_new$goal)
hist(bckcount_new$backers_count)

par(mfrow=c(1,3))
boxplot(cpa_new$converted_pledged_amount, main='cpa after rm')
boxplot(goal_new$goal, main='cpa after rm')
boxplot(bckcount_new$backers_count, main='cpa after rm')

#correlation between categorical variables
library('rcompanion')
#country and currency
coun_curr = table(mergedKS$country, mergedKS$currency)
print(coun_curr)
cramerV(mergedKS$country, mergedKS$currency)
#spotlight and staff pick
spo_sp = table(mergedKS$spotlight, mergedKS$staff_pick)
print(spo_sp)
cramerV(mergedKS$spotlight, mergedKS$staff_pick)
#spotlight and state
spo_state = table(mergedKS$spotlight, mergedKS$state)
print(spo_state)
cramerV(mergedKS$spotlight, mergedKS$state)


#learning the distribution of each category
str(mergedKS$category)
levels( )[3]
kitty[1,]                               

