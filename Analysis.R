#This code aims to analyse the data we have


#Fin Maths Analysis
BUS2016H<-read.csv("BUS2016H.csv",header = TRUE, sep=";" )
BUS2016H<-na.omit(BUS2016H)
attach(BUS2016H)

no.protest<-subset(BUS2016H,RegAcadYear<2015 & )
protest<-subset(BUS2016H,RegAcadYear>=2015)
