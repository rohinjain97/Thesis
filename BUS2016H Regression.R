rm(list = ls())

BUS2016H_data=read.csv("BUS2016H.csv", header = TRUE, sep = ";" , na.strings = c("","NA"))
BUS2016H_data<-na.omit(BUS2016H_data)

levels(BUS2016H_data$Exemption)<-c(levels(BUS2016H_data$Exemption),1)
BUS2016H_data$Exemption[BUS2016H_data$Exemption=="E"]<-1
BUS2016H_data$Exemption<-droplevels(BUS2016H_data$Exemption)


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
res_b3<-glm(Exemption~SA.Citizenship.Status, family=binomial(link="logit"))
res_b4<-glm(Exemption~RegProgram, family=binomial(link="logit")) #base is cb003
res_b5<-glm(Exemption~Protest, family=binomial(link="logit"))

AIC(res_b1,res_b2,res_b3,res_b4,res_b5)
#demographic has the lowest AIC, start with this base

res_1<-glm(Exemption~DEMOG, family=binomial(link="logit"))
#black, chinese, indian, white is significant
res_2<-glm(Exemption~DEMOG+Gender, family=binomial(link="logit"))
summary(res_2)
anova(res_1, res_2, test="Chisq") #gender is significant
#Male is significant, it increases likelihood of getting exemption

res_3<-glm(Exemption~DEMOG+Gender+Protest, family=binomial(link='logit'))
summary(res_3)
anova(res_2,res_3,test="Chisq") #adding protest is not significant at 5%

res_4<-glm(Exemption~DEMOG+Gender+SA.Citizenship.Status, family=binomial(link='logit'))
summary(res_4) #NA in foreign level?? go fix??
anova(res_2, res_4, test="Chisq") #adding citizenship is not significant at 10% (p=0.22)

res_5<-glm(Exemption~DEMOG+Gender+RegProgram, family=binomial(link='logit'))
summary(res_5) #the other programs are not significant
anova(res_2, res_5, test="Chisq") #p=0.496

#the final model consists of Demograhpy, gender
#protests is not a significant predictor variable

res_6<-glm(Exemption~DEMOG+Gender+Gender*Protest, family=binomial(link='logit'))
summary(res_6) #interaction term not significant (p=0.919)
anova(res_2,res_6, test="Chisq") #p=0.1629

res_7<-glm(Exemption~DEMOG+Gender+DEMOG*Protest, family=binomial(link='logit'))
summary(res_7) #none of the interaction terms are significant
anova(res_2,res_7, test="Chisq") #p=0.4

#it seems like res_2 is the best  

res_all<-glm(Exemption~DEMOG+Gender+Protest+SA.Citizenship.Status+RegProgram, family=binomial(link='logit'))
summary(res_all) #it seems whichever factor you put first, that will receive significance
anova(res_all, test="Chisq")
