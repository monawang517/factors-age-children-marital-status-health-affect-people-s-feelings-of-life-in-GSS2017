#logistic
library(tidyverse)
library(survey)
data<-diamonds

####Build a model to predict diamond price category with carat, color, clarity, cut####

####step1(optional). Prepare the dataset (includes your cleaning code and other preparations)####
#Take a look at your data
str(data)
#If there are any NAs, drop it (in this example there are no NAs)
data<-na.omit(data)
#Sometimes NAs are stored as string, and the code above doesn't work, in that case, run data[data=="NA"]<-NA first
#If any categorical data are not factor type, convert it to factor 
#(in this example they are all factors, but just to show you how)
data$color<-as.factor(data$color)
#If they are factors but ordered (in this case they are), remove the order
data$color<- factor(data$color , ordered = FALSE )
data$cut<- factor(data$cut , ordered = FALSE )
data$clarity<- factor(data$clarity , ordered = FALSE )


#Step2 (optional) create your categorical Y (Ture/Fasle)
#you can either use relvel or define 1/0 as the value you need say 1=expensive
data$price.type<-as.factor(ifelse(data$price>10000,"Expensive","Regular")) # create your own category (you don't need to run this line if you already have variable you need)
data$price.type<- relevel(data$price.type, ref = "Regular")  #you don't need to run this line if predicting True/False or 1/0 because R always predict True(1)
#relevel tells R which category it should predict as "True", 
#by setting ref="Regular", we are telling R to predict prob of "Expensive"


#####step3. survey design####
#SRS
#Assuming Population N=100000
N=100000
n=length(data$price)
fpc.srs = rep(N, n)
example.design.srs <- svydesign(id=~1, data=data, fpc=fpc.srs)
svyglm.srs.logit <- svyglm(price.type ~ carat+color+cut+clarity, example.design.srs, family="binomial")
summary(svyglm.srs.logit) 
#Logistic regression cannot use R squared to evaluate model (AUC/ ROC Curve/ Confusion Matrix-- optional, do your own research)


#Stratified
#Assuing the data were collected using Stratified Sampling, Strata divided by color
#Assuming the population size per color is as following:
#D: 33875, E:48985, F:47710, G:56460, H: 41520, I:27110, J:14040 (get this info from User Guide or make your own proper assumptions)
data$fpc<-ifelse(data$color=="D",33875,ifelse(data$color=="E",48985,
                                              ifelse(data$color=="F",47710,ifelse(data$color=="G",56460,
                                                                                  ifelse(data$color=="H",41520,ifelse(data$color=="I",27110,ifelse(data$color=="J",14040,0)))))))

example.design.strs<-svydesign(id=~1,strata=~color, data=data, fpc=~fpc)
svyglm.strs.logit <- svyglm(price.type ~ carat+color+cut+clarity, example.design.strs, family="binomial")
summary(svyglm.strs.logit)

####Step4. How to interpret your result-notes####
