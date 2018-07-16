#This code aims to analyse the data we have


#Fin Maths Analysis
BUS2016H<-read.csv("BUS2016H.csv",header = TRUE, sep=";" )
BUS2016H<-na.omit(BUS2016H)
attach(BUS2016H)

no.protest<-subset(BUS2016H,RegAcadYear<2015)
protest<-subset(BUS2016H,RegAcadYear>=2015)

#Check if exam marks is normally distributed
exam.p<-protest$Exam.Mark
exam.np<-no.protest$Exam.Mark
library('moments') #install moments if need be
skewness(exam.p)
skewness(exam.np)
kurtosis(exam.p)
kurtosis(exam.np)

#figure out how to overlay normal curve
x<-seq(0,100,0.1)
hist(exam.np, freq = FALSE)
lines(dnorm(x, mean(exam.np),sd(exam.np)), x)

#qqplot
qqnorm(exam.np) #not too good at the tails
qqline(exam.np)

qqnorm(exam.p) #more like a normal than exam.np
qqline(exam.p)

#KS test
ks.test(exam.np,exam.p) #says cannot reject that they are from the same distribution
ks.test(exam.np, pnorm) #not normal
ks.test(exam.p,pnorm) #not normal

#shapiro
shapiro.test(exam.np) #not normal
shapiro.test(exam.p) #not normal

#Install package 'nortest'
#anderson darling test for normality
library(nortest)
ad.test(exam.np) #not normal
ad.test(exam.p) #normally dist

##going ahead with non-parametric testing
#Mann-whitney U test
#we assume the two populations have the same variance
wilcox.test(exam.np,exam.p) #H0: the medians are the same
#p is high, and cannot reject 


