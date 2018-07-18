rm(list = ls())

BUS2016H_data=read.csv("BUS2016H.csv", header = TRUE, sep = ";" , na.strings = c("","NA"))
BUS2016H_data<-na.omit(BUS2016H_data)

levels(BUS2016H_data$Exemption)<-c(levels(BUS2016H_data$Exemption),1)
BUS2016H_data$Exemption[BUS2016H_data$Exemption=="E"]<-1
BUS2016H_data$Exemption<-droplevels(BUS2016H_data$Exemption)

attach(BUS2016H_data)

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
exam_set_protests = rbind(exam_set_2015,exam_set_2016,exam_set_2017)

res_b1<-glm(Exemption~)

