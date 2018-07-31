setwd("C:/Users/rohin/Dropbox/UCT Work/4th Year AcSci/BUS4029H Project/2018/GIT Repository/Thesis/Anthony Regression")
library(readxl)
rm(list = ls())

data = read_excel("BUS2016H.xlsx") 
data = subset(data, DEMOG !="Unknown")
data = subset(data, Gender !="T")
attach(data)

#setting data to right restrictions
RegAcadYear = as.numeric(RegAcadYear)
Gender = factor(Gender)
Population.Group = factor("Population Group")
DEMOG = factor(DEMOG)
Deferred = factor(Deferred)
RegProgram = factor(RegProgram)

n = length(AnonID)
protest_indicator=c()

for (i in 1:n)
{
  if (RegAcadYear[i] == 2012){
    protest_indicator[i] = 0
  }
  else if (RegAcadYear[i] == 2013){
    protest_indicator[i] = 0
  }
  
  else if (RegAcadYear[i] == 2014){
    protest_indicator[i] = 0
  }
  else {
    protest_indicator[i] = 1
  }
}


data = cbind(data, protest_indicator)
protest_indicator = as.numeric(protest_indicator)
protest_indicator2 = factor(protest_indicator)

#creating data sets
non_protest_years = subset(data,protest_indicator<1)
protest_years = subset(data,protest_indicator>0)

#data analysis
##looking at the marks in general
hist(Exam, freq = FALSE)
lines(density(Exam), col="red")
lines(seq(-40, 400, by=.5), dnorm(seq(-40, 400, by=.5),mean(Exam), sd(Exam)), col="blue")
ks.test((Exam - mean(Exam))/sd(Exam),"pnorm")
shapiro.test(Exam)

##looking at dbn of marks in protest years comparison
par(mfrow=c(1,1))
hist(non_protest_years$Exam, freq = FALSE)
lines(density(non_protest_years$Exam), col="red")
lines(seq(-40, 400, by=.5), dnorm(seq(-40, 400, by=.5),mean(non_protest_years$Exam), sd(non_protest_years$Exam)), col="blue")

hist(protest_years$Exam, freq = FALSE)
lines(density(protest_years$Exam), col="red")
lines(seq(-40, 400, by=.5), dnorm(seq(-40, 400, by=.5),mean(protest_years$Exam), sd(protest_years$Exam)), col="blue")

plot(0,type = "n",ylim = c(0,0.04),xlim = c(0,100))
lines(density(non_protest_years$Exam), col="red")
lines(density(protest_years$Exam), col="blue")

##looking how other factors are split
par(mfrow=c(1,1))
plot(Gender)
plot(DEMOG)
plot(RegProgram)
plot(Deferred)
plot(protest_indicator)

#some manipulation of data if needed
registered_program = c()
for (i in 1:n)
{
  if (RegProgram[i] == "CB003"){
    registered_program[i] = "CB003"
  }
  else {
    registered_program[i] = "Other"
  }
}
registered_program=factor(registered_program)
plot(registered_program)

#demographics = c()
#for (i in 1:n)
#{
#  if (DEMOG[i] == "White"){
#    demographics[i] = "White"
#  }
#  else if (DEMOG[i] == "Black"){
#    demographics[i] = "Black"
 # }
##  else if (DEMOG[i] == "Indian"){
#    demographics[i] = "Indian"
#  }
#  else if (DEMOG[i] == "International"){
#    demographics[i] = "International"
#  }
##  else if (DEMOG[i] == "Coloured"){
#    demographics[i] = "Coloured"
#  }
#  else if (DEMOG[i] == "Chinese"){
#    demographics[i] = "Chinese"
 # }
#  else {
#    demographics[i] = "Unknown"
#  }
#}

#demographics = factor(demographics)
#plot(demographics)
#data = cbind(data,demographics,registered_program)

#finding a model

##normal
fit_normal_1_1 = glm(Exam~protest_indicator, family = gaussian(link = ))
fit_normal_1_2 = glm(Exam~registered_program, family = gaussian(link = ))
fit_normal_1_3 = glm(Exam~Gender, family = gaussian(link = ))
fit_normal_1_4 = glm(Exam~DEMOG, family = gaussian(link = ))
fit_normal_1_5 = glm(Exam~Deferred, family = gaussian(link = ))

AIC(fit_normal_1_1,fit_normal_1_2,fit_normal_1_3,fit_normal_1_4,fit_normal_1_5) #from this we can choose gender to start
# best fit is DEMOG

fit_normal_2_1 = glm(Exam~DEMOG+protest_indicator, family = gaussian(link = )) 
fit_normal_2_2 = glm(Exam~DEMOG+registered_program, family = gaussian(link = )) 
fit_normal_2_3 = glm(Exam~DEMOG+Gender, family = gaussian(link = )) 
fit_normal_2_4 = glm(Exam~DEMOG+Deferred, family = gaussian(link = )) 

AIC(fit_normal_2_1,fit_normal_2_2,fit_normal_2_3,fit_normal_2_4)
#best fit is Gender
anova(fit_normal_1_4,fit_normal_2_3,test = "Chisq") #add gender

fit_normal_3_1 = glm(Exam~DEMOG+Gender + Gender*DEMOG, family = gaussian(link = ))

anova(fit_normal_2_3,fit_normal_3_1,test = "Chisq") #not significant
AIC(fit_normal_2_3,fit_normal_3_1) #do not add interaction effect

fit_normal_3_2 = glm(Exam~DEMOG+Gender + protest_indicator, family = gaussian(link = ))
fit_normal_3_3 = glm(Exam~DEMOG+Gender + registered_program, family = gaussian(link = ))
fit_normal_3_4 = glm(Exam~DEMOG+Gender + Deferred, family = gaussian(link = ))

AIC(fit_normal_3_2,fit_normal_3_3,fit_normal_3_4) #3.4 is lowest
anova(fit_normal_2_3,fit_normal_3_4,test = "Chisq") #add defferred

fit_normal_4_1 = glm(Exam~DEMOG+Gender + Deferred + protest_indicator, family = gaussian(link = ))
fit_normal_4_2 = glm(Exam~DEMOG+Gender + Deferred + registered_program, family = gaussian(link = ))
fit_normal_4_3 = glm(Exam~DEMOG+Gender + Deferred + Deferred*Gender, family = gaussian(link = ))
fit_normal_4_4 = glm(Exam~DEMOG+Gender + Deferred + Deferred*DEMOG, family = gaussian(link = ))
AIC(fit_normal_4_1,fit_normal_4_2,fit_normal_4_3,fit_normal_4_4)
anova(fit_normal_3_4,fit_normal_4_1,test = "Chisq") #do not add protest indicator
#stop at 3_4
#final model only consists of DEMOG, Gender and deferred
fit_normal_protest_indicator = glm(Exam~ protest_indicator, family = gaussian(link = ))

#comment
summary(fit_normal_3_4)
summary(fit_normal_protest_indicator)






