#linear
library(tidyverse)
library(survey)
data<-diamonds

####Build a model to predict diamond price with carat, color, clarity, cut####

####step1 (optional). Prepare the dataset (includes your cleaning code and other preparations)####
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


#####step2. survey design####
#SRS
#Assuming Population N=100000
N=100000
n=length(data$price)
fpc.srs = rep(N, n)
example.design.srs <- svydesign(id=~1, data=data, fpc=fpc.srs)
svyglm.srs <- svyglm(price ~ carat+color+cut+clarity, example.design.srs, family="gaussian")
summary(svyglm.srs) #There is no R squared
#To view R squared (use one of them):
# Method1
summary.lm(svyglm.srs) #to view R squared (0.916) 
# Method2
library(poliscidata)
fit.svyglm(svyglm.srs)
#Method 3
library(jtools)
summ(svyglm.srs)


#Stratified
# stratified sample
#Assuing the data were collected using Stratified Sampling, Strata divided by color
#Assuming the population size per color is as following:
#D: 33875, E:48985, F:47710, G:56460, H: 41520, I:27110, J:14040 (get this info from User Guide or make your own proper assumptions)
data$fpc<-ifelse(data$color=="D",33875,ifelse(data$color=="E",48985,
                                              ifelse(data$color=="F",47710,ifelse(data$color=="G",56460,
                                                                                  ifelse(data$color=="H",41520,ifelse(data$color=="I",27110,ifelse(data$color=="J",14040,0)))))))

example.design.strs<-svydesign(id=~1,strata=~color, data=data, fpc=~fpc)
svyglm.strs <- svyglm(price ~ carat+color+cut+clarity, example.design.strs, family="gaussian")
summary(svyglm.strs)
summary.lm(svyglm.strs) #to view R squared (0.916) 
#Step3. How to interpret your result-notes##
