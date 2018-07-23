rm(list = ls())

BUS3024S_data=read.csv("BUS3024S.xlsx", header = TRUE, sep = ";" , na.strings = c("","NA"))
BUS3024S_data<-na.omit(BUS3024S_data)

levels(BUS3024S_data$Exemption)<-c(levels(BUS3024S_data$Exemption),1)
BUS3024S_data$Exemption[BUS3024S_data$Exemption=="E"]<-1
BUS3024S_data$Exemption<-droplevels(BUS3024S_data$Exemption)


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
res_b3<-glm(Exemption~SA.Citizenship.Status, family=binomial(link="logit"))
res_b4<-glm(Exemption~RegProgram, family=binomial(link="logit")) #base is cb003
res_b5<-glm(Exemption~Protest, family=binomial(link="logit"))

AIC(res_b1,res_b2,res_b3,res_b4,res_b5)
#demographic has the lowest AIC, start with this base

res_1<-glm(Exemption~DEMOG, family=binomial(link="logit"))
summary(res_1)
#white, coloured, indian are significant

res_2<-glm(Exemption~DEMOG+Gender, family=binomial(link="logit"))
summary(res_2)
anova(res_1, res_2, test="Chisq") #gender is not significant 
#male p value=0.79
#anova, p=0.7699

#res<-glm(Exemption~DEMOG+Protest, family=binomial(link="logit"))
#summary(res)
#anova(res_1, res, test="Chisq")
#even if you put protest as the next variable, its coeff is not significant
#even the ANOVA does not suggest that we add protest variable

res_3<-glm(Exemption~DEMOG+Protest, family=binomial(link='logit'))
summary(res_3)
anova(res_1,res_3,test="Chisq") #adding protest is not significant at 5% (p=0.6244)

res_4<-glm(Exemption~DEMOG+SA.Citizenship.Status, family=binomial(link='logit'))
summary(res_4) #NA in foreign level?? go fix?? I think it may be that F corresponds exactly with international
anova(res_1, res_4, test="Chisq") #P factor level is significant at p=0.362

res_5<-glm(Exemption~DEMOG+SA.Citizenship.Status+RegProgram, family=binomial(link='logit'))
summary(res_5) #None of the coeff are significant, however, ANOVA says p=0.0226
anova(res_4, res_5, test="Chisq") #But CB020 has 15 students, CB025 (11), CB026 (6)
#not sure if I should include Reg Prog

#the final model consists of Demograhpy, SA citizenship
#protests is not a significant predictor variable

res_6<-glm(Exemption~DEMOG+SA.Citizenship.Status+SA.Citizenship.Status*Protest, family=binomial(link='logit'))
summary(res_6) #interaction term not significant 
anova(res_4,res_6, test="Chisq") #p=0.8922

res_7<-glm(Exemption~DEMOG+SA.Citizenship.Status+DEMOG*Protest, family=binomial(link='logit'))
summary(res_7) #none of the interaction terms are significant
anova(res_4,res_7, test="Chisq") #p=0.9741

#it seems like res_4 is the best  

res_all<-glm(Exemption~Protest+DEMOG+Gender+SA.Citizenship.Status+RegProgram, family=binomial(link='logit'))
summary(res_all) 
anova(res_all, test="Chisq")
#even when you put the protest variable first, there is no significance