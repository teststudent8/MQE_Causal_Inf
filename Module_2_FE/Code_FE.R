
library(stargazer)
library(lfe)

library(dplyr)

class<-c(1,2,3,4)
scores<-as.data.frame(class)
scores<-rbind(scores,scores,scores,scores,scores,scores,scores,scores,scores,scores)
scores$error<-rnorm(40, mean=0, sd=5)


scores$treat1<-rbinom(40,1,0.2)
scores$treat2<-rbinom(40,1,0.8)
scores$treat[scores$class%in%c(1,2)]<-scores$treat1[scores$class%in%c(1,2)]
scores$treat[scores$class%in%c(3,4)]<-scores$treat2[scores$class%in%c(3,4)]


scores<-scores%>%select(class,error,treat)
scores <- fastDummies::dummy_cols(scores, select_columns = "class")


scores$score<-80+15*scores$treat+85*scores$class_2+ -30*scores$class_3+
  -35*scores$class_4+scores$error


nofe<-felm(score~treat,scores)
dummies<-felm(score~treat+class_2+class_3+class_4, scores)
fe<-felm(score~treat|class,scores)

stargazer(nofe, dummies, fe, type='latex')


#calculateing the mean score in each classroom
cl_mean<-scores %>%
  group_by(class) %>%
  dplyr::summarize(Classmean = mean(score, na.rm=TRUE), treatmean=mean(treat, na.rm=TRUE))

#merging the means into full data
scores<-left_join(scores, cl_mean, by = "class")

#calculating the demeaned score
scores$demeansc<-scores$score-scores$Classmean
scores$demeantrt<-scores$treat-scores$treatmean

#running the basic regression on the demaned scores

regdemean<-felm(demeansc~demeantrt, scores)

stargazer(nofe, dummies, fe, regdemean, type='latex')

library(dplyr)
library(lfe)

class2<-c(1,2,3,4)
scores2<-as.data.frame(class2)
scores2<-rbind(scores2,scores2,scores2,scores2,scores2,scores2,scores2,scores2,scores2,scores2)
scores2$error<-rnorm(40, mean=0, sd=5)

scores2$treat[scores2$class%in%c(1,2)]<-0
scores2$treat[scores2$class%in%c(3,4)]<-1

scores2<-scores2%>%select(class2,error,treat)
scores2 <- fastDummies::dummy_cols(scores2, select_columns = "class2")

scores2$score<-80+15*scores2$treat+85*scores2$class2_2+ -30*scores2$class2_3+ -35*scores2$class2_4+scores2$error

nofe2<-felm(score~treat,scores2)
dummies2<-felm(score~treat+class2_2+class2_3+class2_4, scores2)
#fe2<-felm(score~treat|class2,scores2)

stargazer(nofe2, dummies2,  type='latex')



#install.packages("wooldridge")
library(wooldridge)
library(lfe)
#note: this dataset comes from the wooldridge textbook. Conveniently there is an R package that 
#includes all the wooldridge datasets. 

crime<-data('crime2')
crime<-crime2

regcrime<-felm(crmrte~unem, crime[crime$year=="87",])
summary(regcrime)



regcrime2<-felm(crmrte~unem+area+west+offarea+lawexpc+pcinc, crime[crime$year=="87",])
summary(regcrime2)


#note: the data does not have a unique city identifier. I am assuming the area of the city is 1)time-invariant and 2) uniquely identifies the 46 cities. The line of code below generates a unique identifier
crime <- transform(crime,city=as.numeric(factor(area)))
#I check that my assumptions were correct by seeing if I have 2 observations for 46 cities.
table(crime$city)

regcrime3<-felm(crmrte~unem+area+west+offarea+lawexpc+pcinc|city, crime)
summary(regcrime3)



regcrime4<-felm(crmrte~unem+area+west+offarea+lawexpc+pcinc|city+year, crime)
summary(regcrime4)


crime$city_year<-paste(crime$city, crime$year, sep="_")

#Note: the following regression will not run!
#regcrime5<-felm(crmrte~unem+area+west+offarea+lawexpc+pcinc|city_year, crime)
#summary(regcrime5)