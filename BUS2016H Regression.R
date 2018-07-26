rm(list = ls())

BUS2016H_data=read.csv("BUS2016H.csv", header = TRUE, sep = ";" , na.strings = c("","NA"))
BUS2016H_data<-na.omit(BUS2016H_data)
BUS2016H_data<-subset(BUS2016H_data, DEMOG!="Unknown")
BUS2016H_data<-subset(BUS2016H_data, Gender!="T")
BUS2016H_data<-droplevels(BUS2016H_data)

levels(BUS2016H_data$Exemption)<-c(levels(BUS2016H_data$Exemption),1)
BUS2016H_data$Exemption[BUS2016H_data$Exemption=="E"]<-1
BUS2016H_data$Exemption<-droplevels(BUS2016H_data$Exemption)
attach(BUS2016H_data)

registered_program = c()
n=length(AnonID)
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

BUS2016H_data$RegProgram<-registered_program

exam_set=cbind(BUS2016H_data[1],BUS2016H_data[4], BUS2016H_data[5], BUS2016H_data[6],BUS2016H_data[7],
               BUS2016H_data[8],BUS2016H_data[9], BUS2016H_data[11], BUS2016H_data[12], BUS2016H_data[13],
               BUS2016H_data[14])

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
#black, chinese, indian, white is significant

res_2.1<-glm(Exemption~DEMOG+Gender, family=binomial(link="logit"))
res_2.2<-glm(Exemption~DEMOG+Deferred, family=binomial(link="logit"))
res_2.3<-glm(Exemption~DEMOG+RegProgram, family=binomial(link="logit"))
res_2.4<-glm(Exemption~DEMOG+Protest, family=binomial(link="logit"))

AIC(res_2.1,res_2.2,res_2.3,res_2.4)
#res2.1 is best

summary(res_2.1)
#gender is significant 
anova(res_1, res_2.1, test="Chisq") #gender is significant
#Male is significant, it increases likelihood of getting exemption

res_3.1<-glm(Exemption~DEMOG+Gender+Deferred, family=binomial(link='logit'))
res_3.2<-glm(Exemption~DEMOG+Gender+RegProgram, family=binomial(link='logit'))
res_3.3<-glm(Exemption~DEMOG+Gender+Protest, family=binomial(link='logit'))

AIC(res_3.1,res_3.2, res_3.3)
#res_3.1 is best


summary(res_3.1)
#deferred is almost significant at 5% (p=0.051629)
anova(res_2.1,res_3.1,test="Chisq") 
#adding deferred is siginificant at 5%

res_4.1<-glm(Exemption~DEMOG+Gender+Deferred +RegProgram, family=binomial(link='logit'))
res_4.2<-glm(Exemption~DEMOG+Gender+Deferred + Protest, family=binomial(link='logit'))

AIC(res_4.1, res_4.2)
#protests is only VERY slighly better

summary(res_4.2) 
#protest coefficient is not significant (p=0.2)
anova(res_3.1, res_4.2, test="Chisq") #adding protetsts is not significant at 10% (p=0.22)
#not significant , p=0.2138

res_5<-glm(Exemption~DEMOG+Gender+Deferred  +RegProgram, family=binomial(link='logit'))
summary(res_5)
#not siginiicant
anova(res_3.1, res_5, test = "Chisq" )
#not significant

#let us consider interaction terms
res_6<-glm(Exemption~DEMOG+Gender+Deferred + DEMOG*Gender, family=binomial(link='logit'))
summary(res_6) #coeff not significant
anova(res_3.1, res_6, test="Chisq") #p=0.8182


#the final model consists of Demograhpy, gender and deferred
#protests is not a significant predictor variable
