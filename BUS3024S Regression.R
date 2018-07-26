rm(list = ls())

BUS3024S_data=read.csv("BUS3024S.xlsx", header = TRUE, sep = ";" , na.strings = c("","NA"))
BUS3024S_data<-na.omit(BUS3024S_data)
BUS3024S_data<-subset(BUS3024S_data, DEMOG!="Unknown")
BUS3024S_data<-subset(BUS3024S_data, Gender!="T")
BUS3024S_data<-droplevels(BUS3024S_data)

levels(BUS3024S_data$Exemption)<-c(levels(BUS3024S_data$Exemption),1)
BUS3024S_data$Exemption[BUS3024S_data$Exemption=="E"]<-1
BUS3024S_data$Exemption<-droplevels(BUS3024S_data$Exemption)


registered_program = c()
n=length(BUS3024S_data$AnonID)
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

BUS3024S_data$RegProgram<-registered_program

exam_set=cbind(BUS3024S_data[1],BUS3024S_data[4], BUS3024S_data[5], BUS3024S_data[6],BUS3024S_data[7],
               BUS3024S_data[8],BUS3024S_data[9], BUS3024S_data[11], BUS3024S_data[12], BUS3024S_data[13],
               BUS3024S_data[14])


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
#black, coloured, indian and white are significant

res_2.1<-glm(Exemption~DEMOG+Gender, family=binomial(link="logit"))
res_2.2<-glm(Exemption~DEMOG+Deferred, family=binomial(link="logit"))
res_2.3<-glm(Exemption~DEMOG+RegProgram, family=binomial(link="logit"))
res_2.4<-glm(Exemption~DEMOG+Protest, family=binomial(link="logit"))

AIC(res_2.1,res_2.2,res_2.3,res_2.4)
#res2.2 is best

summary(res_2.2)
#deferred is not significant
anova(res_1, res_2.2, test="Chisq")
#p=0.1245

res_3.1<-glm(Exemption~DEMOG+Gender, family=binomial(link='logit'))
res_3.2<-glm(Exemption~DEMOG+RegProgram, family=binomial(link='logit'))
res_3.3<-glm(Exemption~DEMOG+Protest, family=binomial(link='logit'))

AIC(res_3.1,res_3.2, res_3.3)
#res_3.2, reg programe


summary(res_3.2)
#RP is not significant (p=0.4006)
anova(res_1,res_3.2,test="Chisq") 
#p=0.4013, res_1 is still better. 

res_4.1<-glm(Exemption~DEMOG+Gender, family=binomial(link='logit'))
res_4.2<-glm(Exemption~DEMOG+Protest, family=binomial(link='logit'))

AIC(res_4.1, res_4.2)
#protests is only VERY slighly better

summary(res_4.2) 
#protest coefficient is not significant (p=0.62028)
anova(res_1, res_4.2, test="Chisq") #adding protetsts is not significant at 10% (p=0.22)
#not significant , p=0.6201

res_5<-glm(Exemption~DEMOG+Gender, family=binomial(link='logit'))
summary(res_5)
#gender is not signficant
anova(res_1, res_5, test = "Chisq" )
#not significant, p=0.8854


#the final model consists of Demograhpy
#protests is not a significant predictor variable
