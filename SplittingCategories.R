rm(list = ls()) # clear workspace 
kick <- read.csv("kickstarter_final.csv")  # load data  
summary(kick)

install.packages("dplyr")
install.packages("stringr")
library("dplyr")
library("tidyr")
library("stringr")

df<-data.frame(x = c(kick$category))
split1=str_split_fixed(df$x,":",9) #splitting categories by the colon
split2=split1[,3] #third column of matrix is 'name' category
df2<-data.frame(x=c(split2))
split3=str_split_fixed(df2$x,",",2)
subcategory=split3[,1]

split4=split1[,4] #fourth column of matrix is 'slug' category
df3<-data.frame(x=c(split4))
split5=str_split_fixed(df3$x,",",2)
str_split_fixed(df3$x,",",2)
split5[,1]
