rm(list = ls())

BUS3018F_data=read.csv("BUS3018F.xlsx", header = TRUE, sep = ";" , na.strings = c("","NA"))
BUS3018F_data<-na.omit(BUS3018F_data)
BUS3018F_data<-subset(BUS3018F_data, DEMOG!="Unknown")
BUS3018F_data<-subset(BUS3018F_data, Gender!="T")
BUS3018F_data<-droplevels(BUS3018F_data)

levels(BUS3018F_data$Exemption)<-c(levels(BUS3018F_data$Exemption),1)
BUS3018F_data$Exemption[BUS3018F_data$Exemption=="E"]<-1
BUS3018F_data$Exemption<-droplevels(BUS3018F_data$Exemption)


registered_program = c()
n=length(BUS3018F_data$AnonID)
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

BUS3018F_data$RegProgram<-registered_program

exam_set=cbind(BUS3018F_data[1],BUS3018F_data[4], BUS3018F_data[5], BUS3018F_data[6],BUS3018F_data[7],
               BUS3018F_data[8],BUS3018F_data[9], BUS3018F_data[11], BUS3018F_data[12], BUS3018F_data[13],
               BUS3018F_data[14])


#creating the datasets for exam results
exam_set_2012 = exam_set[which(exam_set$RegAcadYear=="2012"),][-1]
exam_set_2013 = exam_set[which(exam_set$RegAcadYear=="2013"),][-1]
exam_set_2014 = exam_set[which(exam_set$RegAcadYear=="2014"),][-1]
exam_set_2015 = exam_set[which(exam_set$RegAcadYear=="2015"),][-1]
exam_set_2016 = exam_set[which(exam_set$RegAcadYear=="2016"),][-1]
exam_set_2017 = exam_set[which(exam_set$RegAcadYear=="2017"),][-1]

exam_set_non_protests = rbind(exam_set_2012, exam_set_2013, exam_set_2014) 
exam_set_non_protests = cbind(exam_set_non_protests,Protest=0)
exam_set_protests = rbind(exam_set_2015,exam_set_2016,exam_set_2017)
exam_set_protests = cbind(exam_set_protests,Protest=1)

exam_set_combined=rbind(exam_set_protests,exam_set_non_protests)

attach(exam_set_combined)

res_b1<-glm(Exemption~Gender, family=binomial(link="logit"))
res_b2<-glm(Exemption~DEMOG, family=binomial(link="logit")) #base is black
res_b3<-glm(Exemption~Deferred, family=binomial(link="logit"))
res_b4<-glm(Exemption~RegProgram, family=binomial(link="logit")) #base is cb003
res_b5<-glm(Exemption~Protest, family=binomial(link="logit"))

AIC(res_b1,res_b2,res_b3,res_b4,res_b5)
#demographic has the lowest AIC, start with this base

res_1<-glm(Exemption~DEMOG, family=binomial(link="logit"))
summary(res_1)
#Black, Indian and White are significant

res_2.1<-glm(Exemption~DEMOG+Gender, family=binomial(link="logit"))
res_2.2<-glm(Exemption~DEMOG+Deferred, family=binomial(link="logit"))
res_2.3<-glm(Exemption~DEMOG+RegProgram, family=binomial(link="logit"))
res_2.4<-glm(Exemption~DEMOG+Protest, family=binomial(link="logit"))

AIC(res_2.1,res_2.2,res_2.3,res_2.4)
#res2.1 is best, hence Gender is most valuable here

summary(res_2.1)
#gender is significant at 5%
anova(res_1, res_2.1, test="Chisq") #gender is significant
#Male has coefficient of 0.5... this means ?

res_3.1<-glm(Exemption~DEMOG+Gender+Deferred, family=binomial(link='logit'))
res_3.2<-glm(Exemption~DEMOG+Gender+RegProgram, family=binomial(link='logit'))
res_3.3<-glm(Exemption~DEMOG+Gender+Protest, family=binomial(link='logit'))

AIC(res_3.1,res_3.2, res_3.3)
#res_3.1 is best, hence Deferred is the next most important thing


summary(res_3.1)
#deferred is not significant. P value is 0.9853

anova(res_2.1,res_3.1,test="Chisq") 
#adding deferred is not significatn at 5% (p=0.07573)

res_4.1<-glm(Exemption~DEMOG+Gender+RegProgram, family=binomial(link='logit'))
res_4.2<-glm(Exemption~DEMOG+Gender+ Protest, family=binomial(link='logit'))

AIC(res_4.1, res_4.2)
#protests is only VERY slighly better

summary(res_4.2) 
#protest coefficient is not significant (p=0.37409)
anova(res_2.1, res_4.2, test="Chisq") #adding protetsts is not significant at 10% (p=0.3737)


res_5<-glm(Exemption~DEMOG+Gender+RegProgram, family=binomial(link='logit'))
summary(res_5)
#not siginiicant (p=0.7541)
anova(res_2.1, res_5, test = "Chisq" )
#not significant (p=0.7542)

#let us consider interaction terms:
res_6<-glm(Exemption~DEMOG+Gender+DEMOG*Gender, family=binomial(link='logit'))
summary(res_6) #none of the coefficients are significant
anova(res_2.1, res_6, test="Chisq") #not siginifcatn (p=0.7977)


#the final model consists of Demograhpy, gender
#protests is not a significant predictor variable
