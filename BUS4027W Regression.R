rm(list = ls())

BUS4027W_data=read.csv("BUS4027W.xlsx", header = TRUE, sep = ";" , na.strings = c("","NA"))
BUS4027W_data<-na.omit(BUS4027W_data)

levels(BUS4027W_data$Exemption)<-c(levels(BUS4027W_data$Exemption),1)
BUS4027W_data$Exemption[BUS4027W_data$Exemption=="E"]<-1
BUS4027W_data$Exemption<-droplevels(BUS4027W_data$Exemption)


exam_set=cbind(BUS4027W_data[1],BUS4027W_data[4], BUS4027W_data[5], BUS4027W_data[6],BUS4027W_data[7],
               BUS4027W_data[8],BUS4027W_data[9], BUS4027W_data[11], BUS4027W_data[12], BUS4027W_data[13],
               BUS4027W_data[14])

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
res_b6<-glm(Exemption~Deferred,family=binomial(link="logit"))

AIC(res_b1,res_b2,res_b3,res_b4,res_b5, res_b6)
#demographic has the lowest AIC, start with this base

res_1<-glm(Exemption~DEMOG, family=binomial(link="logit"))
summary(res_1)
#all variables are significant at 10%

res_2<-glm(Exemption~DEMOG+Gender, family=binomial(link="logit"))
summary(res_2)
anova(res_1, res_2, test="Chisq") #gender is not significant 
#male p value=0.569
#anova, p=0.569

#res<-glm(Exemption~DEMOG+Protest, family=binomial(link="logit"))
#summary(res)
#anova(res_1, res, test="Chisq")
#even if you put protest as the next variable, its coeff is not significant
#even the ANOVA does not suggest that we add protest variable

res_3<-glm(Exemption~DEMOG+Protest, family=binomial(link='logit'))
summary(res_3)
anova(res_1,res_3,test="Chisq") #adding protest is not significant at 5% (p=0.5949)

res_4<-glm(Exemption~DEMOG+SA.Citizenship.Status, family=binomial(link='logit'))
summary(res_4) #NA in foreign level?? go fix?? I think it may be that F corresponds exactly with international
anova(res_1, res_4, test="Chisq") #P factor level is not significant at p=0.6435

res_5<-glm(Exemption~DEMOG+RegProgram, family=binomial(link='logit'))
summary(res_5) #None of the coeff are significant, however
anova(res_1, res_5, test="Chisq") #But CB020 has 9 students, CB025 (14), CB026 (2), CB019(13), CB018(14)
#not sure if I should include Reg Prog since so few students

res_de<-glm(Exemption~DEMOG+Deferred, family=binomial(link='logit'))
summary(res_de) #coefficient is not significant, however, only 17 students in dataset
anova(res_1, res_de, test="Chisq") #p=0.456

#the final model consists of Demograhpy, SA citizenship
#protests is not a significant predictor variable

#res_6<-glm(Exemption~DEMOG+SA.Citizenship.Status+SA.Citizenship.Status*Protest, family=binomial(link='logit'))
#summary(res_6) #interaction term not significant 
#anova(res_4,res_6, test="Chisq") #p=0.8922

res_7<-glm(Exemption~DEMOG+DEMOG*Protest, family=binomial(link='logit'))
summary(res_7) #none of the interaction terms are significant
anova(res_1,res_7, test="Chisq") #p=0.05202, since it is so close to the border, we may be tempted to accept
#but, we shall not since the coefficient are not significant (p is around 0.9)!!

#it seems like res_1 is the best  

res_all<-glm(Exemption~Protest+DEMOG+Gender+SA.Citizenship.Status+RegProgram, family=binomial(link='logit'))
summary(res_all)  #protest coefficient is not significant
anova(res_all, test="Chisq") #p=0.154
#even when you put the protest variable first, there is no significance
