rm(list = ls())

BUS4027W_data=read.csv("BUS4027W.xlsx", header = TRUE, sep = ";" )

#need to remove the NA observations
BUS4027W_data<-na.omit(BUS4027W_data)
attach(BUS4027W_data)

ID = BUS4027W_data[2]
exam_set = cbind(BUS4027W_data[1],BUS4027W_data[2], BUS4027W_data[4])

#creating the datasets for exam results
exam_set_2012 = exam_set[which(exam_set$RegAcadYear=="2012"),][-1]
exam_set_2013 = exam_set[which(exam_set$RegAcadYear=="2013"),][-1]
exam_set_2014 = exam_set[which(exam_set$RegAcadYear=="2014"),][-1]
exam_set_2015 = exam_set[which(exam_set$RegAcadYear=="2015"),][-1]
exam_set_2016 = exam_set[which(exam_set$RegAcadYear=="2016"),][-1]
exam_set_2017 = exam_set[which(exam_set$RegAcadYear=="2017"),][-1]

exam_set_non_protests = rbind(exam_set_2012, exam_set_2013, exam_set_2014)
exam_set_protests = rbind(exam_set_2015,exam_set_2016,exam_set_2017)

#plotting the histograms with curves fitted above it
par(mfrow=c(3,2))

hist(exam_set_2012$Exam,freq = FALSE, breaks = 10,xlim=c(0,100))
lines(density(exam_set_2012$Exam), col="red")
lines(seq(10, 100, by=.5), dnorm(seq(10, 100, by=.5),mean(exam_set_2012$Exam), sd(exam_set_2012$Exam)), col="blue")

hist(exam_set_2013$Exam,freq = FALSE, breaks = 10,xlim=c(0,100))
lines(density(exam_set_2013$Exam), col="red")
lines(seq(10, 100, by=.5), dnorm(seq(10, 100, by=.5),mean(exam_set_2013$Exam), sd(exam_set_2013$Exam)), col="blue")

hist(exam_set_2014$Exam,freq = FALSE, breaks = 10,xlim=c(0,100))
lines(density(exam_set_2014$Exam), col="red")
lines(seq(10, 100, by=.5), dnorm(seq(10, 100, by=.5),mean(exam_set_2014$Exam), sd(exam_set_2014$Exam)), col="blue")

hist(exam_set_2015$Exam,freq = FALSE, breaks = 10,xlim=c(0,100))
lines(density(exam_set_2015$Exam), col="red")
lines(seq(10, 100, by=.5), dnorm(seq(10, 100, by=.5),mean(exam_set_2015$Exam), sd(exam_set_2015$Exam)), col="blue")

hist(exam_set_2016$Exam,freq = FALSE, breaks = 10,xlim=c(0,100))
lines(density(exam_set_2016$Exam), col="red")
lines(seq(10, 100, by=.5), dnorm(seq(10, 100, by=.5),mean(exam_set_2016$Exam), sd(exam_set_2016$Exam)), col="blue")

hist(exam_set_2017$Exam,freq = FALSE, breaks = 10,xlim=c(0,100))
lines(density(exam_set_2017$Exam), col="red")
lines(seq(10, 100, by=.5), dnorm(seq(10, 100, by=.5),mean(exam_set_2017$Exam), sd(exam_set_2017$Exam)), col="blue")

boxplot(exam_set_2012$Exam)
boxplot(exam_set_2013$Exam)
boxplot(exam_set_2014$Exam)
boxplot(exam_set_2015$Exam)
boxplot(exam_set_2016$Exam)
boxplot(exam_set_2016$Exam)

par(mfrow=c(1,1))
hist(exam_set_non_protests$Exam,freq = FALSE, breaks = 10)
lines(density(exam_set_non_protests$Exam), col="red")
lines(seq(-40, 400, by=.5), dnorm(seq(-40, 400, by=.5),mean(exam_set_non_protests$Exam), sd(exam_set_non_protests$Exam)), col="blue")

hist(exam_set_protests$Exam,freq = FALSE, breaks = 10)
lines(density(exam_set_protests$Exam), col="red")
lines(seq(-40, 400, by=.5), dnorm(seq(-40, 400, by=.5),mean(exam_set_protests$Exam), sd(exam_set_protests$Exam)), col="blue")

#testing for normallity 
shapiro.test(exam_set_2012$Exam) #p is low, reject normality
ks.test((exam_set_2012$Exam - mean(exam_set_2012$Exam))/sd(exam_set_2012$Exam), 
        "pnorm") #KS says normal

shapiro.test(exam_set_2013$Exam) #p=0.24, accept normality
ks.test((exam_set_2013$Exam - mean(exam_set_2013$Exam))/sd(exam_set_2013$Exam),
        "pnorm") #p=0.6074

shapiro.test(exam_set_2014$Exam) #p=0.69, accept normality
ks.test((exam_set_2014$Exam - mean(exam_set_2014$Exam))/sd(exam_set_2014$Exam), 
        "pnorm") #p=0.9143

shapiro.test(exam_set_2015$Exam) 
ks.test((exam_set_2015$Exam - mean(exam_set_2015$Exam))/sd(exam_set_2015$Exam), 
        "pnorm")

shapiro.test(exam_set_2016$Exam)
ks.test((exam_set_2016$Exam - mean(exam_set_2016$Exam))/sd(exam_set_2016$Exam), "pnorm")

shapiro.test(exam_set_2017$Exam)
ks.test((exam_set_2017$Exam - mean(exam_set_2017$Exam))/sd(exam_set_2017$Exam), "pnorm")

shapiro.test(exam_set_non_protests$Exam)
ks.test((exam_set_non_protests$Exam - mean(exam_set_non_protests$Exam))/sd(exam_set_non_protests$Exam), "pnorm")

shapiro.test(exam_set_protests$Exam)
ks.test((exam_set_protests$Exam - mean(exam_set_protests$Exam))/sd(exam_set_protests$Exam), "pnorm")
# ks.test(exam_set_protests$Exam.Mark,exam_set_non_protests$Exam.Mark) #shows same distribution


#f the p-value is greater than the chosen alpha level, 
#then the null hypothesis that the data came from a normally distributed population can not be rejected
#seems that all are normally dbn except 2012 (and the protest data)

#comparing exam results of different years

#finding the means and sd
mean2012 = mean(exam_set_2012$Exam)
mean2013 = mean(exam_set_2013$Exam)
mean2014 = mean(exam_set_2014$Exam)
mean2015 = mean(exam_set_2015$Exam)
mean2016 = mean(exam_set_2016$Exam)
mean2017 = mean(exam_set_2017$Exam)
mean_non_protests = mean(exam_set_non_protests$Exam)
mean_protests = mean(exam_set_protests$Exam)

sd2012 = sd(exam_set_2012$Exam)
sd2013 = sd(exam_set_2013$Exam)
sd2014 = sd(exam_set_2014$Exam)
sd2015 = sd(exam_set_2015$Exam)
sd2016 = sd(exam_set_2016$Exam)
sd2017 = sd(exam_set_2017$Exam)
sd_non_protests =  sd(exam_set_non_protests$Exam)
sd_protests =  sd(exam_set_protests$Exam)

n1=length(exam_set_protests$Exam)-1 #these are the length less one
n2=length(exam_set_non_protests$Exam)-1

#######
#need to do non-parametic since non-protest is not normally dist
var.test(exam_set_protests$Exam,exam_set_non_protests$Exam, alternative = "two.sided")

F_non_protests_to_protests = (sd_protests^2)/(sd_non_protests^2)
F_test_stat_.1 =  qf(.95, df1=n1, df2=n2)
F_test_stat_.05 =  qf(.975, df1=n1, df2=n2)
F_test_stat_.01 =  qf(.995, df1=n1, df2=n2) #we can see that same variance at the 5% level but not at the 10%

#use function t.test, if the variance is not equal, change the arg "var.equal=FALSE"
t.test(exam_set_protests$Exam,exam_set_non_protests$Exam, alternative = "two.sided",var.equal = TRUE)

pulled_var = ((n1*sd_protests^2)+(n2*sd_non_protests^2))/(n1+n2)
pulled_sd = sqrt(pulled_var)
t_stat = (mean_non_protests-mean_protests)/(pulled_sd*sqrt((1/n1) + (1/n2)))
T_test_stat_.1 = qt(0.95, df= n1+n2)
T_test_stat_.05 = qt(0.975, df= n1+n2)
T_test_stat_.01 = qt(0.995, df= n1+n2) #there is no difference between the overall marks from protest years to non-protest years F and T tests, but data is not normal
#######

exam_set2 = cbind(BUS4027W_data[1],BUS4027W_data[2], BUS4027W_data[4],BUS4027W_data[5] ,BUS4027W_data[7], BUS4027W_data[11], BUS4027W_data[12], BUS4027W_data[14])

##### creating the datasets for exam results for gender devision
exam_set2_2012 = exam_set2[which(exam_set2$RegAcadYear=="2012"),][-1][-1]
exam_set2_2013 = exam_set2[which(exam_set2$RegAcadYear=="2013"),][-1][-1]
exam_set2_2014 = exam_set2[which(exam_set2$RegAcadYear=="2014"),][-1][-1]
exam_set2_2015 = exam_set2[which(exam_set2$RegAcadYear=="2015"),][-1][-1]
exam_set2_2016 = exam_set2[which(exam_set2$RegAcadYear=="2016"),][-1][-1]
exam_set2_2017 = exam_set2[which(exam_set2$RegAcadYear=="2017"),][-1][-1]
#####

#finding the means
mean_male_2012 = mean(exam_set2_2012[which(exam_set2_2012$Gender =="M"),1])
mean_female_2012 = mean(exam_set2_2012[which(exam_set2_2012$Gender =="F"),1])
number_male_2012 = length(exam_set2_2012[which(exam_set2_2012$Gender=="M"),1])
number_female_2012 = length(exam_set2_2012[which(exam_set2_2012$Gender=="F"),1])
average_gender_2012 = cbind(mean_male_2012,mean_female_2012)

mean_male_2013 = mean(exam_set2_2013[which(exam_set2_2013$Gender =="M"),1])
mean_female_2013 = mean(exam_set2_2013[which(exam_set2_2013$Gender =="F"),1])
number_male_2013 = length(exam_set2_2013[which(exam_set2_2013$Gender=="M"),1])
number_female_2013 = length(exam_set2_2013[which(exam_set2_2013$Gender=="F"),1])
average_gender_2013 = cbind(mean_male_2013,mean_female_2013)

mean_male_2014 = mean(exam_set2_2014[which(exam_set2_2014$Gender =="M"),1])
mean_female_2014 = mean(exam_set2_2014[which(exam_set2_2014$Gender =="F"),1])
number_male_2014 = length(exam_set2_2014[which(exam_set2_2014$Gender=="M"),1])
number_female_2014 = length(exam_set2_2014[which(exam_set2_2014$Gender=="F"),1])
average_gender_2014 = cbind(mean_male_2014,mean_female_2014)

mean_male_2015 = mean(exam_set2_2015[which(exam_set2_2015$Gender =="M"),1])
mean_female_2015 = mean(exam_set2_2015[which(exam_set2_2015$Gender =="F"),1])
number_male_2015 = length(exam_set2_2015[which(exam_set2_2015$Gender=="M"),1])
number_female_2015 = length(exam_set2_2015[which(exam_set2_2015$Gender=="F"),1])
average_gender_2015 = cbind(mean_male_2015,mean_female_2015)

mean_male_2016 = mean(exam_set2_2016[which(exam_set2_2016$Gender =="M"),1])
mean_female_2016 = mean(exam_set2_2016[which(exam_set2_2016$Gender =="F"),1])
number_male_2016 = length(exam_set2_2016[which(exam_set2_2016$Gender=="M"),1])
number_female_2016 = length(exam_set2_2016[which(exam_set2_2016$Gender=="F"),1])
average_gender_2016 = cbind(mean_male_2016,mean_female_2016)

mean_male_2017 = mean(exam_set2_2017[which(exam_set2_2017$Gender =="M"),1])
mean_female_2017 = mean(exam_set2_2017[which(exam_set2_2017$Gender =="F"),1])
number_male_2017 = length(exam_set2_2017[which(exam_set2_2017$Gender=="M"),1])
number_female_2017 = length(exam_set2_2017[which(exam_set2_2017$Gender=="F"),1])
average_gender_2017 = cbind(mean_male_2017,mean_female_2017)

total_average = c(mean(exam_set2_2012$Exam),mean(exam_set2_2013$Exam),mean(exam_set2_2014$Exam),mean(exam_set2_2015$Exam),mean(exam_set2_2016$Exam),mean(exam_set2_2017$Exam))
male_average=c(mean_male_2012,mean_male_2013,mean_male_2014,mean_male_2015,mean_male_2016,mean_male_2017)
female_average=c(mean_female_2012,mean_female_2013,mean_female_2014,mean_female_2015,mean_female_2016,mean_female_2017)
total_number_male = c(number_male_2012,number_male_2013,number_male_2014,number_male_2015,number_male_2016,number_male_2017)
total_number_female = c(number_female_2012,number_female_2013,number_female_2014,number_female_2015,number_female_2016,number_female_2017)
years=c(2012,2013,2014,2015,2016,2017)

#plotting
par(mfrow=c(1,2))
barplot(cbind(mean_male_2012,mean_male_2013,mean_male_2014,mean_male_2015,mean_male_2016,mean_male_2017),width = 0.2,ylim = c(0,70),col = c("blue","red"), xlab = "Means over the years")
barplot(cbind(mean_female_2012,mean_female_2013,mean_female_2014,mean_female_2015,mean_female_2016,mean_female_2017),width = 0.2,ylim = c(0,70),col = c("red"), xlab = "Means over the years")

plot(male_average~years, type = "p",pch=2,ylim=c(45,74))
plot(total_number_male~years, type = "p",pch=2, ylim=c(0,max(total_number_male)+5))
plot(female_average~years, type = "p",pch=2,ylim=c(45,74))
plot(total_number_female~years, type = "p",pch=2, ylim=c(0,max(total_number_female)+5))

par(mfrow=c(1,1))
plot(male_average~years,type = "p",pch=2,ylim=c(45,70))
points(female_average~years,pch=15)
points(total_average~years)

#testing differences
male_marks_non_protests = c(exam_set2_2012[which(exam_set2_2012$Gender =="M"),1] ,exam_set2_2013[which(exam_set2_2013$Gender =="M"),1],exam_set2_2014[which(exam_set2_2014$Gender =="M"),1])
female_marks_non_protests = c(exam_set2_2012[which(exam_set2_2012$Gender =="F"),1] ,exam_set2_2013[which(exam_set2_2013$Gender =="F"),1],exam_set2_2014[which(exam_set2_2014$Gender =="F"),1])
male_marks_protests = c(exam_set2_2015[which(exam_set2_2015$Gender =="M"),1],exam_set2_2016[which(exam_set2_2016$Gender =="M"),1] ,exam_set2_2017[which(exam_set2_2017$Gender =="M"),1] )
female_marks_protests = c(exam_set2_2015[which(exam_set2_2015$Gender =="F"),1],exam_set2_2016[which(exam_set2_2016$Gender =="F"),1] ,exam_set2_2017[which(exam_set2_2017$Gender =="F"),1] )

par(mfrow=c(1,2))
hist(male_marks_non_protests,freq = FALSE, breaks = 10)
lines(density(male_marks_non_protests), col="red")
lines(seq(-40, 400, by=.5), dnorm(seq(-40, 400, by=.5),mean(male_marks_non_protests), sd(male_marks_non_protests)), col="blue")
shapiro.test(male_marks_non_protests)
ks.test((male_marks_non_protests - mean(male_marks_non_protests))/sd(male_marks_non_protests), "pnorm")

hist(male_marks_protests,freq = FALSE, breaks = 10)
lines(density(male_marks_protests), col="red")
lines(seq(-40, 400, by=.5), dnorm(seq(-40, 400, by=.5),mean(male_marks_protests), sd(male_marks_protests)), col="blue")
shapiro.test(male_marks_protests)
ks.test((male_marks_protests - mean(male_marks_protests))/sd(male_marks_protests), "pnorm")

hist(female_marks_non_protests,freq = FALSE, breaks = 10)
lines(density(female_marks_non_protests), col="red")
lines(seq(-40, 400, by=.5), dnorm(seq(-40, 400, by=.5),mean(female_marks_non_protests), sd(female_marks_non_protests)), col="blue")
shapiro.test(female_marks_non_protests)
ks.test((female_marks_non_protests - mean(female_marks_non_protests))/sd(female_marks_non_protests), "pnorm")

hist(female_marks_protests,freq = FALSE, breaks = 10)
lines(density(female_marks_protests), col="red")
lines(seq(-40, 400, by=.5), dnorm(seq(-40, 400, by=.5),mean(female_marks_protests), sd(female_marks_protests)), col="blue")
shapiro.test(female_marks_protests)
ks.test((female_marks_protests - mean(female_marks_protests))/sd(female_marks_protests), "pnorm")

male_mean_non_protests = mean(male_marks_non_protests)
male_mean_protests = mean(male_marks_protests)
female_mean_non_protests = mean(female_marks_non_protests)
female_mean_protests = mean(female_marks_protests)

male_sd_non_protests = sd(male_marks_non_protests)
male_sd_protests = sd(male_marks_protests)
female_sd_non_protests = sd(female_marks_non_protests)
female_sd_protests = sd(female_marks_protests)

#####
#Cannot conduct male mean test since non protest years are not normal dist
#male test

n1=length(male_marks_non_protests)-1
n2=length(male_marks_protests)-1

F_non_protests_to_protests = (male_sd_non_protests^2)/(male_sd_protests^2)
F_test_stat_.1 =  qf(.95, df1=n1, df2=n2)
F_test_stat_.05 =  qf(.975, df1=n1, df2=n2)
F_test_stat_.01 =  qf(.995, df1=n1, df2=n2) #we cannot say they have the same variance

#t_stat = (male_mean_non_protests-male_mean_protests)/(sqrt((male_sd_non_protests^2/n1) + (male_sd_protests^2/n2)))
#n = (((male_sd_non_protests^2/(n1+1))+(male_sd_protests^2/(n2+1)))^2)/((((male_sd_non_protests^2/(n1+1))^2)/(n1+2))+(((male_sd_protests^2/(n2+1))^2)/(n2+2))) - 2

pulled_var = ((n1*male_sd_non_protests^2)+(n2*male_sd_protests^2))/(n1+n2)
pulled_sd = sqrt(pulled_var)
t_stat = (male_mean_non_protests-male_mean_protests)/(pulled_sd*sqrt((1/n1) + (1/n2)))
T_test_stat_.1 = qt(0.95, df= n1+n2)
T_test_stat_.05 = qt(0.975, df= n1+n2)
T_test_stat_.01 = qt(0.995, df= n1+n2) #no difference male test
#####
#female test

n1=length(female_marks_protests)-1
n2=length(female_marks_non_protests)-1

var.test(female_marks_non_protests, female_marks_protests, alternative = "two.sided")
#variance are equal

F_non_protests_to_protests = (female_sd_protests^2)/(female_sd_non_protests^2)
F_test_stat_.1 =  qf(.95, df1=n1, df2=n2)
F_test_stat_.05 =  qf(.975, df1=n1, df2=n2)
F_test_stat_.01 =  qf(.995, df1=n1, df2=n2) #we can say same variacne

pulled_var = ((n1*female_sd_protests^2)+(n2*female_sd_non_protests^2))/(n1+n2)
pulled_sd = sqrt(pulled_var)

t.test(female_marks_non_protests, female_marks_protests, alternative = "two.sided", var.equal = TRUE)
#p=0.4148.. there is a differences in the mean


t_stat = (female_mean_protests-female_mean_non_protests)/(pulled_sd*sqrt((1/n1) + (1/n2)))
T_test_stat_.1 = qt(0.95, df= n1+n2)
T_test_stat_.05 = qt(0.975, df= n1+n2)
T_test_stat_.01 = qt(0.995, df= n1+n2) #no differnce in means

#no change in mark for gender

#colour devisions:

mean_black_2012 = mean(exam_set2_2012[which(exam_set2_2012$Population.Group =="Black"),1])
number_black_2012 = length(exam_set2_2012[which(exam_set2_2012$Population.Group =="Black"),1])
mean_chinese_2012 = mean(exam_set2_2012[which(exam_set2_2012$Population.Group =="Chinese"),1])
number_chinese_2012 = length(exam_set2_2012[which(exam_set2_2012$Population.Group =="Chinese"),1])
mean_coloured_2012 = mean(exam_set2_2012[which(exam_set2_2012$Population.Group =="Coloured"),1])
number_coloured_2012 = length(exam_set2_2012[which(exam_set2_2012$Population.Group =="Coloured"),1])
mean_indian_2012 = mean(exam_set2_2012[which(exam_set2_2012$Population.Group =="Indian"),1])
number_indian_2012 = length(exam_set2_2012[which(exam_set2_2012$Population.Group =="Indian"),1])
mean_white_2012 = mean(exam_set2_2012[which(exam_set2_2012$Population.Group =="White"),1])
number_white_2012 = length(exam_set2_2012[which(exam_set2_2012$Population.Group =="White"),1])

mean_black_2013 = mean(exam_set2_2013[which(exam_set2_2013$Population.Group =="Black"),1])
number_black_2013 = length(exam_set2_2013[which(exam_set2_2013$Population.Group =="Black"),1])
mean_chinese_2013 = mean(exam_set2_2013[which(exam_set2_2013$Population.Group =="Chinese"),1])
number_chinese_2013 = length(exam_set2_2013[which(exam_set2_2013$Population.Group =="Chinese"),1])
mean_coloured_2013 = mean(exam_set2_2013[which(exam_set2_2013$Population.Group =="Coloured"),1])
number_coloured_2013 = length(exam_set2_2013[which(exam_set2_2013$Population.Group =="Coloured"),1])
mean_indian_2013 = mean(exam_set2_2013[which(exam_set2_2013$Population.Group =="Indian"),1])
number_indian_2013 = length(exam_set2_2012[which(exam_set2_2012$Population.Group =="Indian"),1])
mean_white_2013 = mean(exam_set2_2013[which(exam_set2_2013$Population.Group =="White"),1])
number_white_2013 = length(exam_set2_2012[which(exam_set2_2012$Population.Group =="White"),1])

mean_black_2014 = mean(exam_set2_2014[which(exam_set2_2014$Population.Group =="Black"),1])
number_black_2014 = length(exam_set2_2014[which(exam_set2_2014$Population.Group =="Black"),1])
mean_chinese_2014 = mean(exam_set2_2014[which(exam_set2_2014$Population.Group =="Chinese"),1])
number_chinese_2014 = length(exam_set2_2014[which(exam_set2_2014$Population.Group =="Chinese"),1])
mean_coloured_2014 = mean(exam_set2_2014[which(exam_set2_2014$Population.Group =="Coloured"),1])
number_coloured_2014 = length(exam_set2_2014[which(exam_set2_2014$Population.Group =="Coloured"),1])
mean_indian_2014 = mean(exam_set2_2014[which(exam_set2_2014$Population.Group =="Indian"),1])
number_indian_2014 = length(exam_set2_2014[which(exam_set2_2014$Population.Group =="Indian"),1])
mean_white_2014 = mean(exam_set2_2014[which(exam_set2_2014$Population.Group =="White"),1])
number_white_2014 = length(exam_set2_2014[which(exam_set2_2014$Population.Group =="White"),1])

mean_black_2015 = mean(exam_set2_2015[which(exam_set2_2015$Population.Group =="Black"),1])
number_black_2015 = length(exam_set2_2015[which(exam_set2_2015$Population.Group =="Black"),1])
mean_chinese_2015 = mean(exam_set2_2015[which(exam_set2_2015$Population.Group =="Chinese"),1])
number_chinese_2015 = length(exam_set2_2015[which(exam_set2_2015$Population.Group =="Chinese"),1])
mean_coloured_2015 = mean(exam_set2_2015[which(exam_set2_2015$Population.Group =="Coloured"),1])
number_coloured_2015 = length(exam_set2_2015[which(exam_set2_2015$Population.Group =="Coloured"),1])
mean_indian_2015 = mean(exam_set2_2015[which(exam_set2_2015$Population.Group =="Indian"),1])
number_indian_2015 = length(exam_set2_2015[which(exam_set2_2015$Population.Group =="Indian"),1])
mean_white_2015 = mean(exam_set2_2015[which(exam_set2_2015$Population.Group =="White"),1])
number_white_2015 = length(exam_set2_2015[which(exam_set2_2015$Population.Group =="White"),1])

mean_black_2016 = mean(exam_set2_2016[which(exam_set2_2016$Population.Group =="Black"),1])
number_black_2016 = length(exam_set2_2016[which(exam_set2_2016$Population.Group =="Black"),1])
mean_chinese_2016 = mean(exam_set2_2016[which(exam_set2_2016$Population.Group =="Chinese"),1])
number_chinese_2016 = length(exam_set2_2016[which(exam_set2_2016$Population.Group =="Chinese"),1])
mean_coloured_2016 = mean(exam_set2_2016[which(exam_set2_2016$Population.Group =="Coloured"),1])
number_coloured_2016 = length(exam_set2_2016[which(exam_set2_2016$Population.Group =="Coloured"),1])
mean_indian_2016 = mean(exam_set2_2016[which(exam_set2_2016$Population.Group =="Indian"),1])
number_indian_2016 = length(exam_set2_2016[which(exam_set2_2016$Population.Group =="Indian"),1])
mean_white_2016 = mean(exam_set2_2016[which(exam_set2_2016$Population.Group =="White"),1])
number_white_2016 = length(exam_set2_2016[which(exam_set2_2016$Population.Group =="White"),1])

mean_black_2017 = mean(exam_set2_2017[which(exam_set2_2017$Population.Group =="Black"),1])
number_black_2017 = length(exam_set2_2017[which(exam_set2_2017$Population.Group =="Black"),1])
mean_chinese_2017 = mean(exam_set2_2017[which(exam_set2_2017$Population.Group =="Chinese"),1])
number_chinese_2017 = length(exam_set2_2017[which(exam_set2_2017$Population.Group =="Chinese"),1])
mean_coloured_2017 = mean(exam_set2_2017[which(exam_set2_2017$Population.Group =="Coloured"),1])
number_coloured_2017 = length(exam_set2_2017[which(exam_set2_2017$Population.Group =="Coloured"),1])
mean_indian_2017 = mean(exam_set2_2017[which(exam_set2_2017$Population.Group =="Indian"),1])
number_indian_2017 = length(exam_set2_2017[which(exam_set2_2017$Population.Group =="Indian"),1])
mean_white_2017 = mean(exam_set2_2017[which(exam_set2_2017$Population.Group =="White"),1])
number_white_2017 = length(exam_set2_2017[which(exam_set2_2017$Population.Group =="White"),1])

black_average=c(mean_black_2012,mean_black_2013,mean_black_2014,mean_black_2015,mean_black_2016,mean_black_2017)
chinese_average=c(mean_chinese_2012,mean_chinese_2013,mean_chinese_2014,mean_chinese_2015,mean_chinese_2016,mean_chinese_2017)
coloured_average=c(mean_coloured_2012,mean_coloured_2013,mean_coloured_2014,mean_coloured_2015,mean_coloured_2016,mean_coloured_2017)
indian_average = c(mean_indian_2012,mean_indian_2013,mean_indian_2014,mean_indian_2015,mean_indian_2016,mean_indian_2017)
white_average = c(mean_white_2012,mean_white_2013,mean_white_2014,mean_white_2015,mean_white_2016,mean_white_2017)

black_number = c(number_black_2012,number_black_2013,number_black_2014,number_black_2015,number_black_2016,number_black_2017)
chinese_number = c(number_chinese_2012,number_chinese_2013,number_chinese_2014,number_chinese_2015,number_chinese_2016,number_chinese_2017)
coloured_number = c(number_coloured_2012,number_coloured_2013, number_coloured_2014, number_coloured_2015,number_coloured_2016,number_coloured_2017)
indian_number = c(number_indian_2012,number_indian_2013,number_indian_2014,number_indian_2015,number_indian_2016,number_indian_2017)
white_number = c(number_white_2012,number_white_2013,number_white_2014,number_white_2015,number_white_2016,number_white_2017)

years=c(2012,2013,2014,2015,2016,2017)
par(mfrow=c(1,2))
plot(black_average~years,type = "p",pch=2,ylim=c(45,74))
plot(black_number~years,type = "p",pch=2, ylim=c(0,max(black_number)+5))
plot(chinese_average~years,type = "p",pch=2,ylim=c(45,74))
plot(chinese_number~years,type = "p",pch=2, ylim=c(0,max(chinese_number)+5))
plot(coloured_average~years,type = "p",pch=2,ylim=c(45,74))
plot(coloured_number~years,type = "p",pch=2, ylim=c(0,max(coloured_number)+5))
plot(indian_average~years,type = "p",pch=2,ylim=c(45,74))
plot(indian_number~years,type = "p",pch=2, ylim=c(0,max(indian_number)+5))
plot(white_average~years,type = "p",pch=2,ylim=c(45,74))
plot(white_number~years,type = "p",pch=2 , ylim=c(0,max(white_number)+5))

par(mfrow=c(1,1))
plot(black_average~years,type = "p",pch=1,ylim=c(45,74))
points(indian_average~years,pch=4,col="green")
points(white_average~years,pch=5,col="purple")

white_non_protests = c(exam_set2_2012[which(exam_set2_2012$Population.Group =="White"),1],exam_set2_2013[which(exam_set2_2013$Population.Group =="White"),1],exam_set2_2014[which(exam_set2_2014$Population.Group =="White"),1])
white_protests = c(exam_set2_2015[which(exam_set2_2015$Population.Group =="White"),1],exam_set2_2016[which(exam_set2_2016$Population.Group =="White"),1],exam_set2_2017[which(exam_set2_2017$Population.Group =="White"),1])
black_non_protests = c(exam_set2_2012[which(exam_set2_2012$Population.Group =="Black"),1],exam_set2_2013[which(exam_set2_2013$Population.Group =="Black"),1],exam_set2_2014[which(exam_set2_2014$Population.Group =="Black"),1])
black_protests = c(exam_set2_2015[which(exam_set2_2015$Population.Group =="Black"),1],exam_set2_2016[which(exam_set2_2016$Population.Group =="Black"),1],exam_set2_2017[which(exam_set2_2017$Population.Group =="Black"),1])
indian_non_protests = c(exam_set2_2012[which(exam_set2_2012$Population.Group =="Indian"),1],exam_set2_2013[which(exam_set2_2013$Population.Group =="Indian"),1],exam_set2_2014[which(exam_set2_2014$Population.Group =="Indian"),1])
indian_protests = c(exam_set2_2015[which(exam_set2_2015$Population.Group =="Indian"),1],exam_set2_2016[which(exam_set2_2016$Population.Group =="Indian"),1],exam_set2_2017[which(exam_set2_2017$Population.Group =="Indian"),1])

mean_white_non_protests = mean(white_non_protests)
mean_white_protest = mean(white_protests)
mean_black_non_protests = mean(black_non_protests)
mean_black_protest = mean(black_protests)
mean_indian_non_protests = mean(indian_non_protests)
mean_indian_protest = mean(indian_protests)

protests_status = c("protest","non_protest")
xx=barplot(cbind(mean_white_non_protests,mean_white_protest),ylim = c(0,70),names.arg = c("Mean white no protests","Mean white protests"))
text(x = xx, y = cbind(mean_white_non_protests,mean_white_protest),labels = round(cbind(mean_white_non_protests,mean_white_protest),4), pos = 3, cex = 0.8, col = "red")

xx=barplot(cbind(mean_black_non_protests,mean_black_protest),ylim = c(0,70),names.arg = c("Mean black no protests","Mean black protests"))
text(x = xx, y = cbind(mean_black_non_protests,mean_black_protest),labels = round(cbind(mean_black_non_protests,mean_black_protest),4), pos = 3, cex = 0.8, col = "red")

xx=barplot(cbind(mean_indian_non_protests,mean_indian_protest),ylim = c(0,70),names.arg = c("Mean indian no protests","Mean indian protests"))
text(x = xx, y = cbind(mean_indian_non_protests,mean_indian_protest),labels = round(cbind(mean_indian_non_protests,mean_indian_protest),4), pos = 3, cex = 0.8, col = "red")

hist(black_non_protests,freq = FALSE, breaks = 10)
lines(density(black_non_protests), col="red")
lines(seq(-40, 400, by=.5), dnorm(seq(-40, 400, by=.5),mean(black_non_protests), sd(black_non_protests)), col="blue")
shapiro.test(black_non_protests)
ks.test((black_non_protests - mean(black_non_protests))/sd(black_non_protests), "pnorm")

hist(black_protests,freq = FALSE, breaks = 10)
lines(density(black_protests), col="red")
lines(seq(-40, 400, by=.5), dnorm(seq(-40, 400, by=.5),mean(black_protests), sd(black_protests)), col="blue")
shapiro.test(black_protests)
ks.test((black_protests - mean(black_protests))/sd(black_protests), "pnorm")

sd_black_non_protests = sd(black_non_protests)
sd_black_protests = sd(black_protests)

n1=length(black_non_protests)-1
n2=length(black_protests)-1

var.test(black_protests, black_non_protests, alternative = "two.sided")
#equal variances

F_non_protests_to_protests = (sd_black_non_protests^2)/(sd_black_protests^2)
F_test_stat_.1 =  qf(.95, df1=n1, df2=n2)
F_test_stat_.05 =  qf(.975, df1=n1, df2=n2)
F_test_stat_.01 =  qf(.995, df1=n1, df2=n2) #we can say same variacne

pulled_var = ((n1*sd_black_non_protests ^2)+(n2*sd_black_protests^2))/(n1+n2)
pulled_sd = sqrt(pulled_var)

t.test(black_protests, black_non_protests, alternative = "two.sided", var.equal = TRUE)
#same means

t_stat = (mean_black_non_protests-mean_black_protest)/(pulled_sd*sqrt((1/n1) + (1/n2)))
T_test_stat_.1 = qt(0.95, df= n1+n2)
T_test_stat_.05 = qt(0.975, df= n1+n2)
T_test_stat_.01 = qt(0.995, df= n1+n2) #no differnce in means

#creating database for exemption createria 
exemption_set = cbind(BUS4027W_data[1],BUS4027W_data[2], BUS4027W_data[5])

exemption_set_2012 = exemption_set[which(exemption_set$RegAcadYear=="2012"),][-1]
exemption_set_2013 = exemption_set[which(exemption_set$RegAcadYear=="2013"),][-1]
exemption_set_2014 = exemption_set[which(exemption_set$RegAcadYear=="2014"),][-1]
exemption_set_2015 = exemption_set[which(exemption_set$RegAcadYear=="2015"),][-1]
exemption_set_2016 = exemption_set[which(exemption_set$RegAcadYear=="2016"),][-1]
exemption_set_2017 = exemption_set[which(exemption_set$RegAcadYear=="2017"),][-1]

number_exemption_2012 = length(which(exemption_set_2012 == "E"))
number_exemption_2013 = length(which(exemption_set_2013 == "E"))
number_exemption_2014 = length(which(exemption_set_2014 == "E"))
number_exemption_2015 = length(which(exemption_set_2015 == "E"))
number_exemption_2016 = length(which(exemption_set_2016 == "E"))
number_exemption_2017 = length(which(exemption_set_2017 == "E"))

obs_exemption_2012 = nrow(exemption_set_2012)
obs_exemption_2013 = nrow(exemption_set_2013)
obs_exemption_2014 = nrow(exemption_set_2014)
obs_exemption_2015 = nrow(exemption_set_2015)
obs_exemption_2016 = nrow(exemption_set_2016)
obs_exemption_2017 = nrow(exemption_set_2017)

prop_exemption_2012 = number_exemption_2012/obs_exemption_2012
prop_exemption_2013 = number_exemption_2013/obs_exemption_2013
prop_exemption_2014 = number_exemption_2014/obs_exemption_2014
prop_exemption_2015 = number_exemption_2015/obs_exemption_2015
prop_exemption_2016 = number_exemption_2016/obs_exemption_2016
prop_exemption_2017 = number_exemption_2017/obs_exemption_2017

#plotting exemption graphs
year = cbind(2012,2013,2014,2015,2016,2017)
total_number_exemptions = cbind(number_exemption_2012,number_exemption_2013,number_exemption_2014,number_exemption_2015,number_exemption_2016,number_exemption_2017)

total_prop_exemptions = cbind(prop_exemption_2012,prop_exemption_2013,prop_exemption_2014,prop_exemption_2015,prop_exemption_2016,prop_exemption_2017)

par(mfrow=c(1,2))
plot(year, total_number_exemptions, type = "h", ylim=c(0, 50))
plot(year, total_prop_exemptions, type = "h", ylim=c(0, .8))

par(mfrow=c(1,1))
prop_protest_years = (number_exemption_2016+number_exemption_2017+number_exemption_2015)/(obs_exemption_2016+obs_exemption_2017+obs_exemption_2015)
prop_non_portest_years = (number_exemption_2012+number_exemption_2013+number_exemption_2014)/(obs_exemption_2012+obs_exemption_2013+obs_exemption_2014)
y=c(prop_protest_years,prop_non_portest_years)
xx=barplot(as.numeric(y),ylim = c(0,0.6),names.arg = c("Protest years","No Protest years"),axisnames = TRUE )
text(x = xx, y = y,labels = round(y,4), pos = 3, cex = 0.8, col = "red")

pulled_prop = (number_exemption_2016+number_exemption_2017+number_exemption_2015+number_exemption_2012+number_exemption_2013+number_exemption_2014)/(obs_exemption_2012+obs_exemption_2013+obs_exemption_2014+obs_exemption_2016+obs_exemption_2017+obs_exemption_2015)
n1=(obs_exemption_2012+obs_exemption_2013+obs_exemption_2014)^(-1)
n2= (obs_exemption_2016+obs_exemption_2017+obs_exemption_2015)^(-1)
x = sqrt(pulled_prop*(1-pulled_prop)*(n1+n2))
test_stat = (prop_non_portest_years-prop_protest_years)/(x) #thus no strong evedience to reject h0

exemption_set2 = cbind(BUS4027W_data[1],BUS4027W_data[2], BUS4027W_data[6],BUS4027W_data[9], BUS4027W_data[11], BUS4027W_data[12], BUS4027W_data[14])

#creating the datasets for exemption division stages
exemption_set2_2012 = exemption_set2[which(exemption_set2$RegAcadYear=="2012"),][-1][-1]
exemption_set2_2013 = exemption_set2[which(exemption_set2$RegAcadYear=="2013"),][-1][-1]
exemption_set2_2014 = exemption_set2[which(exemption_set2$RegAcadYear=="2014"),][-1][-1]
exemption_set2_2015 = exemption_set2[which(exemption_set2$RegAcadYear=="2015"),][-1][-1]
exemption_set2_2016 = exemption_set2[which(exemption_set2$RegAcadYear=="2016"),][-1][-1]
exemption_set2_2017 = exemption_set2[which(exemption_set2$RegAcadYear=="2017"),][-1][-1]

exemption_set2_2012 = exemption_set2_2012[which(exemption_set2_2012$Exemption=="E"),]
exemption_set2_2013 = exemption_set2_2013[which(exemption_set2_2013$Exemption=="E"),]
exemption_set2_2014 = exemption_set2_2014[which(exemption_set2_2014$Exemption=="E"),]
exemption_set2_2015 = exemption_set2_2015[which(exemption_set2_2015$Exemption=="E"),]
exemption_set2_2016 = exemption_set2_2016[which(exemption_set2_2016$Exemption=="E"),]
exemption_set2_2017 = exemption_set2_2017[which(exemption_set2_2017$Exemption=="E"),]

#finding the numbers
number_exemption_2012_male = length(exemption_set2_2012[which(exemption_set2_2012$Gender == "M"),1])
number_exemption_2012_female = length(exemption_set2_2012[which(exemption_set2_2012$Gender == "F"),1])
number_exemption_2012_black = length(exemption_set2_2012[which(exemption_set2_2012$Population.Group == "Black"),1])
number_exemption_2012_indian = length(exemption_set2_2012[which(exemption_set2_2012$Population.Group == "Indian"),1])
number_exemption_2012_white = length(exemption_set2_2012[which(exemption_set2_2012$Population.Group == "White"),1])

number_exemption_2013_male = length(exemption_set2_2013[which(exemption_set2_2013$Gender == "M"),1])
number_exemption_2013_female = length(exemption_set2_2013[which(exemption_set2_2013$Gender == "F"),1])
number_exemption_2013_black = length(exemption_set2_2013[which(exemption_set2_2013$Population.Group == "Black"),1])
number_exemption_2013_indian = length(exemption_set2_2013[which(exemption_set2_2013$Population.Group == "Indian"),1])
number_exemption_2013_white = length(exemption_set2_2013[which(exemption_set2_2013$Population.Group == "White"),1])

number_exemption_2014_male = length(exemption_set2_2014[which(exemption_set2_2014$Gender == "M"),1])
number_exemption_2014_female = length(exemption_set2_2014[which(exemption_set2_2014$Gender == "F"),1])
number_exemption_2014_black = length(exemption_set2_2014[which(exemption_set2_2014$Population.Group == "Black"),1])
number_exemption_2014_indian = length(exemption_set2_2014[which(exemption_set2_2014$Population.Group == "Indian"),1])
number_exemption_2014_white = length(exemption_set2_2014[which(exemption_set2_2014$Population.Group == "White"),1])

number_exemption_2015_male = length(exemption_set2_2015[which(exemption_set2_2015$Gender == "M"),1])
number_exemption_2015_female = length(exemption_set2_2015[which(exemption_set2_2015$Gender == "F"),1])
number_exemption_2015_black = length(exemption_set2_2015[which(exemption_set2_2015$Population.Group == "Black"),1])
number_exemption_2015_indian = length(exemption_set2_2015[which(exemption_set2_2015$Population.Group == "Indian"),1])
number_exemption_2015_white = length(exemption_set2_2015[which(exemption_set2_2015$Population.Group == "White"),1])

number_exemption_2016_male = length(exemption_set2_2016[which(exemption_set2_2016$Gender == "M"),1])
number_exemption_2016_female = length(exemption_set2_2016[which(exemption_set2_2016$Gender == "F"),1])
number_exemption_2016_black = length(exemption_set2_2016[which(exemption_set2_2016$Population.Group == "Black"),1])
number_exemption_2016_indian = length(exemption_set2_2016[which(exemption_set2_2016$Population.Group == "Indian"),1])
number_exemption_2016_white = length(exemption_set2_2016[which(exemption_set2_2016$Population.Group == "White"),1])

number_exemption_2017_male = length(exemption_set2_2017[which(exemption_set2_2017$Gender == "M"),1])
number_exemption_2017_female = length(exemption_set2_2017[which(exemption_set2_2017$Gender == "F"),1])
number_exemption_2017_black = length(exemption_set2_2017[which(exemption_set2_2017$Population.Group == "Black"),1])
number_exemption_2017_indian = length(exemption_set2_2017[which(exemption_set2_2017$Population.Group == "Indian"),1])
number_exemption_2017_white = length(exemption_set2_2017[which(exemption_set2_2017$Population.Group == "White"),1])

exemption_set2 = cbind(BUS4027W_data[1],BUS4027W_data[2], BUS4027W_data[6],BUS4027W_data[9], BUS4027W_data[11], BUS4027W_data[12], BUS4027W_data[14])

#creating the datasets for exam results for gender devision
exemption_set2_2012 = exemption_set2[which(exemption_set2$RegAcadYear=="2012"),][-1][-1]
exemption_set2_2013 = exemption_set2[which(exemption_set2$RegAcadYear=="2013"),][-1][-1]
exemption_set2_2014 = exemption_set2[which(exemption_set2$RegAcadYear=="2014"),][-1][-1]
exemption_set2_2015 = exemption_set2[which(exemption_set2$RegAcadYear=="2015"),][-1][-1]
exemption_set2_2016 = exemption_set2[which(exemption_set2$RegAcadYear=="2016"),][-1][-1]
exemption_set2_2017 = exemption_set2[which(exemption_set2$RegAcadYear=="2017"),][-1][-1]

obs_exemption_male_2012 = nrow(exemption_set2_2012[which(exemption_set2_2012$Gender=="M"),])
obs_exemption_female_2012 = nrow(exemption_set2_2012[which(exemption_set2_2012$Gender=="F"),])
obs_exemption_black_2012 = nrow(exemption_set2_2012[which(exemption_set2_2012$Population.Group=="Black"),])
obs_exemption_indian_2012 = nrow(exemption_set2_2012[which(exemption_set2_2012$Population.Group=="Indian"),])
obs_exemption_white_2012 = nrow(exemption_set2_2012[which(exemption_set2_2012$Population.Group=="White"),])

obs_exemption_male_2013 = nrow(exemption_set2_2013[which(exemption_set2_2012$Gender=="M"),])
obs_exemption_female_2013 = nrow(exemption_set2_2013[which(exemption_set2_2013$Gender=="F"),])
obs_exemption_black_2013 = nrow(exemption_set2_2013[which(exemption_set2_2013$Population.Group=="Black"),])
obs_exemption_indian_2013 = nrow(exemption_set2_2013[which(exemption_set2_2013$Population.Group=="Indian"),])
obs_exemption_white_2013 = nrow(exemption_set2_2013[which(exemption_set2_2013$Population.Group=="White"),])

obs_exemption_male_2014 = nrow(exemption_set2_2014[which(exemption_set2_2014$Gender=="M"),])
obs_exemption_female_2014 = nrow(exemption_set2_2014[which(exemption_set2_2014$Gender=="F"),])
obs_exemption_black_2014 = nrow(exemption_set2_2014[which(exemption_set2_2014$Population.Group=="Black"),])
obs_exemption_indian_2014 = nrow(exemption_set2_2014[which(exemption_set2_2014$Population.Group=="Indian"),])
obs_exemption_white_2014 = nrow(exemption_set2_2014[which(exemption_set2_2014$Population.Group=="White"),])

obs_exemption_male_2015 = nrow(exemption_set2_2015[which(exemption_set2_2015$Gender=="M"),])
obs_exemption_female_2015 = nrow(exemption_set2_2015[which(exemption_set2_2015$Gender=="F"),])
obs_exemption_black_2015 = nrow(exemption_set2_2015[which(exemption_set2_2015$Population.Group=="Black"),])
obs_exemption_indian_2015 = nrow(exemption_set2_2015[which(exemption_set2_2015$Population.Group=="Indian"),])
obs_exemption_white_2015 = nrow(exemption_set2_2015[which(exemption_set2_2015$Population.Group=="White"),])

obs_exemption_male_2016 = nrow(exemption_set2_2016[which(exemption_set2_2016$Gender=="M"),])
obs_exemption_female_2016 = nrow(exemption_set2_2016[which(exemption_set2_2016$Gender=="F"),])
obs_exemption_black_2016 = nrow(exemption_set2_2016[which(exemption_set2_2016$Population.Group=="Black"),])
obs_exemption_indian_2016 = nrow(exemption_set2_2016[which(exemption_set2_2016$Population.Group=="Indian"),])
obs_exemption_white_2016 = nrow(exemption_set2_2016[which(exemption_set2_2016$Population.Group=="White"),])

obs_exemption_male_2017 = nrow(exemption_set2_2017[which(exemption_set2_2017$Gender=="M"),])
obs_exemption_female_2017 = nrow(exemption_set2_2017[which(exemption_set2_2017$Gender=="F"),])
obs_exemption_black_2017 = nrow(exemption_set2_2017[which(exemption_set2_2017$Population.Group=="Black"),])
obs_exemption_indian_2017 = nrow(exemption_set2_2017[which(exemption_set2_2017$Population.Group=="Indian"),])
obs_exemption_white_2017 = nrow(exemption_set2_2017[which(exemption_set2_2017$Population.Group=="White"),])

prop_exemption_male_2012 = number_exemption_2012_male/obs_exemption_male_2012
prop_exemption_female_2012 = number_exemption_2012_female/obs_exemption_female_2012
prop_exemption_black_2012 = number_exemption_2012_black/obs_exemption_black_2012
prop_exemption_indian_2012 = number_exemption_2012_indian/obs_exemption_indian_2012
prop_exemption_white_2012 = number_exemption_2012_white/obs_exemption_white_2012

prop_exemption_male_2013 = number_exemption_2013_male/obs_exemption_male_2013
prop_exemption_female_2013 = number_exemption_2013_female/obs_exemption_female_2013
prop_exemption_black_2013 = number_exemption_2013_black/obs_exemption_black_2013
prop_exemption_indian_2013 = number_exemption_2013_indian/obs_exemption_indian_2013
prop_exemption_white_2013 = number_exemption_2013_white/obs_exemption_white_2013

prop_exemption_male_2014 = number_exemption_2014_male/obs_exemption_male_2014
prop_exemption_female_2014 = number_exemption_2014_female/obs_exemption_female_2014
prop_exemption_black_2014 = number_exemption_2014_black/obs_exemption_black_2014
prop_exemption_indian_2014 = number_exemption_2014_indian/obs_exemption_indian_2014
prop_exemption_white_2014 = number_exemption_2014_white/obs_exemption_white_2014

prop_exemption_male_2015 = number_exemption_2015_male/obs_exemption_male_2015
prop_exemption_female_2015 = number_exemption_2015_female/obs_exemption_female_2015
prop_exemption_black_2015 = number_exemption_2015_black/obs_exemption_black_2015
prop_exemption_indian_2015 = number_exemption_2015_indian/obs_exemption_indian_2015
prop_exemption_white_2015 = number_exemption_2015_white/obs_exemption_white_2015

prop_exemption_male_2016 = number_exemption_2016_male/obs_exemption_male_2016
prop_exemption_female_2016 = number_exemption_2016_female/obs_exemption_female_2016
prop_exemption_black_2016 = number_exemption_2016_black/obs_exemption_black_2016
prop_exemption_indian_2016 = number_exemption_2016_indian/obs_exemption_indian_2016
prop_exemption_white_2016 = number_exemption_2016_white/obs_exemption_white_2016

prop_exemption_male_2017 = number_exemption_2017_male/obs_exemption_male_2017
prop_exemption_female_2017 = number_exemption_2017_female/obs_exemption_female_2017
prop_exemption_black_2017 = number_exemption_2017_black/obs_exemption_black_2017
prop_exemption_indian_2017 = number_exemption_2017_indian/obs_exemption_indian_2017
prop_exemption_white_2017 = number_exemption_2017_white/obs_exemption_white_2017

#plotting exemption graphs
year = cbind(2012,2013,2014,2015,2016,2017)

total_number_exemptions_male = cbind(number_exemption_2012_male,number_exemption_2013_male,number_exemption_2014_male,number_exemption_2015_male,number_exemption_2016_male,number_exemption_2017_male)
total_number_exemptions_female = cbind(number_exemption_2012_female,number_exemption_2013_female,number_exemption_2014_female,number_exemption_2015_female,number_exemption_2016_female,number_exemption_2017_female)
total_number_exemptions_black = cbind(number_exemption_2012_black,number_exemption_2013_black,number_exemption_2014_black,number_exemption_2015_black,number_exemption_2016_black,number_exemption_2017_black)
total_number_exemptions_indian = cbind(number_exemption_2012_indian,number_exemption_2013_indian,number_exemption_2014_indian,number_exemption_2015_indian,number_exemption_2016_indian,number_exemption_2017_indian)
total_number_exemptions_white = cbind(number_exemption_2012_white,number_exemption_2013_white,number_exemption_2014_white,number_exemption_2015_white,number_exemption_2016_white,number_exemption_2017_white)

total_prop_exemptions_male = cbind(prop_exemption_male_2012,prop_exemption_male_2013,prop_exemption_male_2014,prop_exemption_male_2015,prop_exemption_male_2016,prop_exemption_male_2017)
total_prop_exemptions_female = cbind(prop_exemption_female_2012,prop_exemption_female_2013,prop_exemption_female_2014,prop_exemption_female_2015,prop_exemption_female_2016,prop_exemption_female_2017)
total_prop_exemptions_black = cbind(prop_exemption_black_2012,prop_exemption_black_2013,prop_exemption_black_2014,prop_exemption_black_2015,prop_exemption_black_2016,prop_exemption_black_2017)
total_prop_exemptions_indian = cbind(prop_exemption_indian_2012,prop_exemption_indian_2013,prop_exemption_indian_2014,prop_exemption_indian_2015,prop_exemption_indian_2016,prop_exemption_indian_2017)
total_prop_exemptions_white = cbind(prop_exemption_white_2012,prop_exemption_white_2013,prop_exemption_white_2014,prop_exemption_white_2015,prop_exemption_white_2016,prop_exemption_white_2017)

par(mfrow=c(1,2))
plot(year, total_number_exemptions_male, type = "h", ylim=c(0, 30))
plot(year, total_prop_exemptions_male, type = "h", ylim=c(0, .8))

plot(year, total_number_exemptions_female, type = "h", ylim=c(0, 15))
plot(year, total_prop_exemptions_female, type = "h", ylim=c(0, .6))

plot(year, total_number_exemptions_black, type = "h", ylim=c(0, 10))
plot(year, total_prop_exemptions_black, type = "h", ylim=c(0, .6))

plot(year, total_number_exemptions_indian, type = "h", ylim=c(0, 10))
plot(year, total_prop_exemptions_indian, type = "h", ylim=c(0, .8))

plot(year, total_number_exemptions_white, type = "h", ylim=c(0, 25))
plot(year, total_prop_exemptions_white, type = "h", ylim=c(0, .8))



#conclusion graphs
par(mfrow=c(1,2))
plot(male_average~years,type = "p",pch=2,ylim=c(45,70))
points(female_average~years,pch=15)
points(total_average~years)

par(mfrow=c(2,2))
plot(years, total_number_exemptions_male, type = "h", ylim=c(0, 30))
plot(years, total_prop_exemptions_male, type = "h", ylim=c(0, .8))
plot(male_average~years, type = "p",pch=2,ylim=c(45,74))
plot(total_number_male~years, type = "p",pch=2, ylim=c(0,max(total_number_male)+5))

plot(years, total_number_exemptions_female, type = "h", ylim=c(0, 15))
plot(years, total_prop_exemptions_female, type = "h", ylim=c(0, .6))
plot(female_average~years, type = "p",pch=2,ylim=c(45,74))
plot(total_number_female~years, type = "p",pch=2, ylim=c(0,max(total_number_female)+5))

plot(years, total_number_exemptions_black, type = "h", ylim=c(0, 10))
plot(years, total_prop_exemptions_black, type = "h", ylim=c(0, .6))
plot(black_average~years,type = "p",pch=2,ylim=c(45,74))
plot(black_number~years,type = "p",pch=2, ylim=c(0,max(black_number)+5))

plot(years, total_number_exemptions_indian, type = "h", ylim=c(0, 10))
plot(years, total_prop_exemptions_indian, type = "h", ylim=c(0, .8))
plot(indian_average~years,type = "p",pch=2,ylim=c(45,74))
plot(indian_number~years,type = "p",pch=2, ylim=c(0,max(indian_number)+5))

plot(years, total_number_exemptions_white, type = "h", ylim=c(0, 25))
plot(years, total_prop_exemptions_white, type = "h", ylim=c(0, .8))
plot(white_average~years,type = "p",pch=2,ylim=c(45,74))
plot(white_number~years,type = "p",pch=2 , ylim=c(0,max(white_number)+5))








