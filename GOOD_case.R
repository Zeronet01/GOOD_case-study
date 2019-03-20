test<-read.csv("/Users/lingyitan/Desktop/test.csv")
cont<-read.csv("/Users/lingyitan/Desktop/control.csv")


######################Data cleaning######################
#########################################################
table(test$Status)
test1<-test[test$Status!="",]
cont1<-cont[cont$Status!="",]

table(test1$Status) #0.1782946 of disqualified
table(cont1$Status) #  0.2398374 of disqualified
#remove disqualified answers
test<-test1[test1$Status=="Complete",] #106
cont<-cont1[cont1$Status=="Complete",] #187
a<-prop.test(x=c(23,59),n=c(106,187),conf.level = 0.9)
a
#p-value = 0.09497
# More respondents in control group are having missing data
summary(test)
summary(cont)

#Check tables 
levels(test$Q1)

t1<-table(test$Q1)
t2<-table(cont$Q1)
t1
t2

prop.table(t1) #1.89%
prop.table(t2) #21.93%
res<-prop.test(x=c(2,41),n=c(106,187),conf.level = 0.9) #p-value =7.263e-6
res
#1.89% of respondents for test and 21.93% of respondents for control rated question 1 as top 2 box
#The difference between test and control is statistically significant at the 90% CI as the p-value for 2-sample test for equality of proportions is 7.263e-6, far less than  0.1

t1_2<-table(test$Q2)
t2_2<-table(cont$Q2)

prop.table(t1_2) #1.89%
prop.table(t2_2) #1.07%
res2<-prop.test(x=c(2,2),n=c(106,187),conf.level = 0.9) #p-value =7.263e-6
res2
#1.89% of respondents for test and 1.07% of respondents for control rated question 2 as top 2 box
#The difference between test and control is statistically insignificant at the 90% CI as the p-value for 2-sample test for equality of proportions is 0.9558, which is greater than 0.1

t1_3<-table(test$Q3)
t2_3<-table(cont$Q3)

prop.table(t1_3) #0.94%
prop.table(t2_3) #3.74%
res3<-prop.test(x=c(1,7),n=c(106,187),conf.level = 0.9) #p-value =7.263e-6
res3
#0.94% of respondents for test and 3.74% of respondents for control rated question 3 as top 2 box
#The difference between test and control is statistically insignificant at the 90% CI as the p-value for 2-sample test for equality of proportions is 0.2983, which is greater than 0.1


######################Data exploreation & data visulization######################
#################################################################################
test$cat<-"test"
cont$cat<-"control"
dat<-rbind(test,cont)
summary(dat)


Q1<-factor(dat$Q1,levels(dat$Q1)[c(1,4,3,5,2)])
dat$Q1<-Q1
Q2<-factor(dat$Q2,levels(dat$Q2)[c(1,4,3,5,2)])
dat$Q2<-Q2
Q3<-factor(dat$Q3,levels(dat$Q3)[c(1,3,4,2,5)])
dat$Q3<-Q3
Q4<-factor(dat$Q4,levels(dat$Q4)[c(1,5,2,4,6,3)])
dat$Q4<-Q4
Q5<-factor(dat$Q5,levels(dat$Q5)[c(1,4,5,3,2)])
dat$Q5<-Q5

#How the intervention effect different among feature categories 
table(dat$Q1,dat$Age,dat$cat)
table(dat$Q2,dat$Age,dat$cat)
table(dat$Q3,dat$Age,dat$cat)

dat$Q1_b[dat$Q1=="Of little importance"]<-1
dat$Q1_b[dat$Q1!="Of little importance"]<-0
dat$Q2_b[dat$Q2=="Of little importance"]<-1
dat$Q2_b[dat$Q2!="Of little importance"]<-0
dat$Q3_b[dat$Q3=="Disagree somewhat"]<-1
dat$Q3_b[dat$Q3!="Disagree somewhat"]<-0

#Crude visualization
library(ggplot2)
library(ggpubr)
p1<-ggplot(dat,aes(Q1))+geom_bar(aes(fill = as.factor(Q1_b)))+facet_wrap(~cat,dir="h")+scale_fill_manual("Top 2 box ratings", labels = c("no","yes"),values = c("#abd9e9","#fdae61"))+
  theme_bw()+theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5), panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  scale_x_discrete(label=c("2","3","4","5"))

p2<-ggplot(dat,aes(Q2))+geom_bar(aes(fill = as.factor(Q2_b)))+facet_wrap(~cat,dir="h")+scale_fill_manual("Top 2 box ratings", labels = c("no","yes"),values = c("#abd9e9","#fdae61"))+
  theme_bw()+theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5), panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  scale_x_discrete(label=c("2","3","4","5"))

p3<-ggplot(dat,aes(Q3))+geom_bar(aes(fill = as.factor(Q3_b)))+facet_wrap(~cat,dir="h")+scale_fill_manual("Top 2 box ratings", labels = c("no","yes"),values = c("#abd9e9","#fdae61"))+
  theme_bw()+theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5), panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  scale_x_discrete(label=c("2","3","4","5"))

ggarrange(p1,p2,p3,ncol=3)

#Stratified visualization
p1<-ggplot(dat,aes(Gender))+geom_bar(aes(fill = Q1))+facet_wrap(~cat,dir="h")+scale_fill_manual("Q1", labels=c("2","3","4","5"),values = c("#fbb4ae","#b3cde3","#ccebc5","#decbe4"))+
  theme_bw()+theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5), panel.grid.major = element_blank(),panel.grid.minor = element_blank())
p2<-ggplot(dat,aes(Gender))+geom_bar(aes(fill = Q2))+facet_wrap(~cat,dir="h")+scale_fill_manual("Q2", labels=c("2","3","4","5"),values = c("#fbb4ae","#b3cde3","#ccebc5","#decbe4"))+
  theme_bw()+theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5), panel.grid.major = element_blank(),panel.grid.minor = element_blank())
p3<-ggplot(dat,aes(Gender))+geom_bar(aes(fill = Q3))+facet_wrap(~cat,dir="h")+scale_fill_manual("Q3", labels=c("2","3","4","5"),values = c("#fbb4ae","#b3cde3","#ccebc5","#decbe4"))+
  theme_bw()+theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5), panel.grid.major = element_blank(),panel.grid.minor = element_blank())

ggarrange(p1,p2,p3,ncol=3)

p1<-ggplot(dat,aes(Age))+geom_bar(aes(fill = Q1))+facet_wrap(~cat,dir="h")+scale_fill_manual("Q1", labels=c("2","3","4","5"),values = c("#fbb4ae","#b3cde3","#ccebc5","#decbe4"))+
  theme_bw()+theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5), panel.grid.major = element_blank(),panel.grid.minor = element_blank())
p2<-ggplot(dat,aes(Age))+geom_bar(aes(fill = Q2))+facet_wrap(~cat,dir="h")+scale_fill_manual("Q2", labels=c("2","3","4","5"),values = c("#fbb4ae","#b3cde3","#ccebc5","#decbe4"))+
  theme_bw()+theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5), panel.grid.major = element_blank(),panel.grid.minor = element_blank())
p3<-ggplot(dat,aes(Age))+geom_bar(aes(fill = Q3))+facet_wrap(~cat,dir="h")+scale_fill_manual("Q3", labels=c("2","3","4","5"),values = c("#fbb4ae","#b3cde3","#ccebc5","#decbe4"))+
  theme_bw()+theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5), panel.grid.major = element_blank(),panel.grid.minor = element_blank())

ggarrange(p1,p2,p3,ncol=3)

p1<-ggplot(dat,aes(Q4))+geom_bar(aes(fill = Q1))+facet_wrap(~cat,dir="h")+scale_fill_manual("Q1", labels=c("2","3","4","5"),values = c("#fbb4ae","#b3cde3","#ccebc5","#decbe4"))+
  theme_bw()+theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5), panel.grid.major = element_blank(),panel.grid.minor = element_blank())
p2<-ggplot(dat,aes(Q4))+geom_bar(aes(fill = Q2))+facet_wrap(~cat,dir="h")+scale_fill_manual("Q2", labels=c("2","3","4","5"),values = c("#fbb4ae","#b3cde3","#ccebc5","#decbe4"))+
  theme_bw()+theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5), panel.grid.major = element_blank(),panel.grid.minor = element_blank())
p3<-ggplot(dat,aes(Q4))+geom_bar(aes(fill = Q3))+facet_wrap(~cat,dir="h")+scale_fill_manual("Q3", labels=c("2","3","4","5"),values = c("#fbb4ae","#b3cde3","#ccebc5","#decbe4"))+
  theme_bw()+theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5), panel.grid.major = element_blank(),panel.grid.minor = element_blank())

ggarrange(p1,p2,p3,ncol=3)

p1<-ggplot(dat,aes(Q5))+geom_bar(aes(fill = Q1))+facet_wrap(~cat,dir="h")+scale_fill_manual("Q1", labels=c("2","3","4","5"),values = c("#fbb4ae","#b3cde3","#ccebc5","#decbe4"))+
  theme_bw()+theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5), panel.grid.major = element_blank(),panel.grid.minor = element_blank())
p2<-ggplot(dat,aes(Q5))+geom_bar(aes(fill = Q2))+facet_wrap(~cat,dir="h")+scale_fill_manual("Q2", labels=c("2","3","4","5"),values = c("#fbb4ae","#b3cde3","#ccebc5","#decbe4"))+
  theme_bw()+theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5), panel.grid.major = element_blank(),panel.grid.minor = element_blank())
p3<-ggplot(dat,aes(Q5))+geom_bar(aes(fill = Q3))+facet_wrap(~cat,dir="h")+scale_fill_manual("Q3", labels=c("2","3","4","5"),values = c("#fbb4ae","#b3cde3","#ccebc5","#decbe4"))+
  theme_bw()+theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5), panel.grid.major = element_blank(),panel.grid.minor = element_blank())

ggarrange(p1,p2,p3,ncol=3)


###Stratification to adjust confounding

##Stratified by gender
#Q1
dat$cat<-as.factor(dat$cat)
summary(dat)
t1_f<-table(dat[dat$Gender=="Female"&dat$cat=="test","Q1"])
c1_f<-table(dat[dat$Gender=="Female"&dat$cat=="control","Q1"])
t1_f
c1_f
t1_m<-table(dat[dat$Gender=="Male"&dat$cat=="test","Q1"])
c1_m<-table(dat[dat$Gender=="Male"&dat$cat=="control","Q1"])
t1_m
c1_m

Q1_f<-prop.test(x=c(sum(t1_f[1:2]),sum(c1_f[1:2])),n=c(sum(t1_f),sum(c1_f)),conf.level = 0.9) 
Q1_f #p-value=0.01154

Q1_m<-prop.test(x=c(sum(t1_m[1:2]),sum(c1_m[1:2])),n=c(sum(t1_m),sum(c1_m)),conf.level = 0.9)
Q1_m #p-value=0.0003418
#Effect are significant for both male and female in Q1

t2_f<-table(dat[dat$Gender=="Female"&dat$cat=="test","Q2"])
t2_m<-table(dat[dat$Gender=="Male"&dat$cat=="test","Q2"])
c2_f<-table(dat[dat$Gender=="Female"&dat$cat=="control","Q2"])
c2_m<-table(dat[dat$Gender=="Male"&dat$cat=="control","Q2"])
t2_f
c2_f
t2_m
c2_m

Q2_f<-prop.test(x=c(sum(t2_f[1:2]),sum(c2_f[1:2])),n=c(sum(t2_f),sum(c2_f)),conf.level = 0.9) 
Q2_f #p-value=0.8493

Q2_m<-prop.test(x=c(sum(t2_m[1:2]),sum(c2_m[1:2])),n=c(sum(t2_m),sum(c2_m)),conf.level = 0.9)
Q2_m #p-value=1
#Effect are insignificant for both male and female in Q2

t3_f<-table(dat[dat$Gender=="Female"&dat$cat=="test","Q3"])
t3_m<-table(dat[dat$Gender=="Male"&dat$cat=="test","Q3"])
c3_f<-table(dat[dat$Gender=="Female"&dat$cat=="control","Q3"])
c3_m<-table(dat[dat$Gender=="Male"&dat$cat=="control","Q3"])
t3_f
c3_f
t3_m
c3_m

Q3_f<-prop.test(x=c(sum(t3_f[1:2]),sum(c3_f[1:2])),n=c(sum(t3_f),sum(c3_f)),conf.level = 0.9) 
Q3_f #p-value=1

Q3_m<-prop.test(x=c(sum(t3_m[1:2]),sum(c3_m[1:2])),n=c(sum(t3_m),sum(c3_m)),conf.level = 0.9)
Q3_m #p-value=0.1864
#Effect are insignificant for both male and female in Q3
#Check the stratified relative risk 

##Stratified by age
#Q1
dat$cat<-as.factor(dat$cat)
summary(dat)
t1_f<-table(dat[dat$Age=="18-29"&dat$cat=="test","Q1"])
c1_f<-table(dat[dat$Age=="18-29"&dat$cat=="control","Q1"])
t1_f
c1_f
t1_m<-table(dat[dat$Age!="18-29"&dat$cat=="test","Q1"])
c1_m<-table(dat[dat$Age!="18-29"&dat$cat=="control","Q1"])
t1_m
c1_m

Q1_f<-prop.test(x=c(sum(t1_f[1:2]),sum(c1_f[1:2])),n=c(sum(t1_f),sum(c1_f)),conf.level = 0.9) 
Q1_f #p-value=0.003533

Q1_m<-prop.test(x=c(sum(t1_m[1:2]),sum(c1_m[1:2])),n=c(sum(t1_m),sum(c1_m)),conf.level = 0.9)
Q1_m #p-value=0.001192
#Effect are significant for both age<30 and age>=30

t2_f<-table(dat[dat$Age=="18-29"&dat$cat=="test","Q2"])
t2_m<-table(dat[dat$Age!="18-29"&dat$cat=="test","Q2"])
c2_f<-table(dat[dat$Age=="18-29"&dat$cat=="control","Q2"])
c2_m<-table(dat[dat$Age!="18-29"&dat$cat=="control","Q2"])
t2_f
c2_f
t2_m
c2_m

Q2_f<-prop.test(x=c(sum(t2_f[1:2]),sum(c2_f[1:2])),n=c(sum(t2_f),sum(c2_f)),conf.level = 0.9) 
Q2_f #p-value=0.9326

Q2_m<-prop.test(x=c(sum(t2_m[1:2]),sum(c2_m[1:2])),n=c(sum(t2_m),sum(c2_m)),conf.level = 0.9)
Q2_m #p-value=NA
#Effect are insignificant for both age<30 and age>=30 in Q2

t3_f<-table(dat[dat$Age=="18-29"&dat$cat=="test","Q3"])
t3_m<-table(dat[dat$Age!="18-29"&dat$cat=="test","Q3"])
c3_f<-table(dat[dat$Age=="18-29"&dat$cat=="control","Q3"])
c3_m<-table(dat[dat$Age!="18-29"&dat$cat=="control","Q3"])
t3_f
c3_f
t3_m
c3_m

Q3_f<-prop.test(x=c(sum(t3_f[1:2]),sum(c3_f[1:2])),n=c(sum(t3_f),sum(c3_f)),conf.level = 0.9) 
Q3_f #p-value=1

Q3_m<-prop.test(x=c(sum(t3_m[1:2]),sum(c3_m[1:2])),n=c(sum(t3_m),sum(c3_m)),conf.level = 0.9)
Q3_m #p-value=0.3125
#Effect are insignificant for both age<30 and age>=30 in Q3
#Check the stratified relative risk 


#Stratified by Q5
t3_f<-table(dat[dat$Q5=="Not very often"&dat$cat=="test","Q3"])
t3_m<-table(dat[dat$Q5!="Not very often"&dat$cat=="test","Q3"])
c3_f<-table(dat[dat$Q5=="Not very often"&dat$cat=="control","Q3"])
c3_m<-table(dat[dat$Q5!="Not very often"&dat$cat=="control","Q3"])
t3_f
c3_f
t3_m
c3_m

Q3_f<-prop.test(x=c(sum(t3_f[1:2]),sum(c3_f[1:2])),n=c(sum(t3_f),sum(c3_f)),conf.level = 0.9) 
Q3_f #p-value=1

Q3_m<-prop.test(x=c(sum(t3_m[1:2]),sum(c3_m[1:2])),n=c(sum(t3_m),sum(c3_m)),conf.level = 0.9)
Q3_m #p-value=0.1506
#Effect are insignificant for both groups


##########After adjust for potential confoundings, dose the intervention influence the answer?###########
#########################################################################################################
MQ1<-lm(Q1_b~Age+Gender+Q4+Q5+cat,data=dat)
summary(MQ1)

MQ2<-lm(Q2_b~Age+Gender+Q4+Q5+cat,data=dat)
summary(MQ2)

MQ3<-lm(Q3_b~Age+Gender+Q4+Q5+cat,data=dat)
summary(MQ3)

# Cat variable is only significant in MQ1 model, which means after afjusting for gender, age and other potential confoundings,
# The impact of scientific content is only significant in Q1

