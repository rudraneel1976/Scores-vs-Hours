# Scores-vs-Hours
#Linear Regression with Scores Vs Hours of study
setwd("F:/internship")
library(readxl)
StudentDedication<-read_excel("Studentstime.xlsx")
summary(StudentDedication)
str(StudentDedication)
attach(StudentDedication) 
#StudentDedication$Hours
library(ggplot2)

# boxplots
boxplot(Hours)
boxplot(Scores)

ggplot(data=StudentDedication,aes(x=factor(0),y= Hours))+geom_jitter()+geom_boxplot(alpha=0.5)
ggplot(data=StudentDedication,aes(x=factor(0),y= Scores))+geom_jitter(aes(col="red"))+
                                                        geom_boxplot(alpha=0.5)
IQR(Hours)
OutHours<-summary(Hours)[["3rd Qu."]]+1.5*4.7
OutHours
OutHoursL<-summary(Hours)[["1st Qu."]]-1.5*4.7
OutHoursL

IQR(Scores)
OutScores<-summary(Scores)[["3rd Qu."]]+1.5*45
OutScores
OutScoresL<-summary(Hours)[["1st Qu."]]-1.5*45
OutScoresL


GPLOT<-ggplot(data=StudentDedication,aes(x=Hours,y=Scores))+
                       geom_point()+ geom_smooth(method = "lm", se = FALSE)

# se	Display confidence interval around smooth?                          
GPLOT
GPLOT2 =ggplot(data=StudentDedication,aes(x=Hours,y=Scores)) +
               geom_point()+ geom_smooth(method = "lm", se = TRUE)
GPLOT2
HistSD <- hist(StudentDedication$Scores)
HistSD
#install.packages("rcompanion")
library(rcompanion)

A1<-plotNormalHistogram(Scores, xlab ="Scores")

#HistggSD<-ggplot(StudentDedication, aes(x=Scores)) + 
#  geom_histogram(binwidth = 1, color="black", fill="white")
#HistggSD

#install.packages("e1071")
library(e1071) 
skewness(Hours)      # -1/2 TO -1 AND 1/2 TO 1 - MODERATE SKEWED
skewness(Scores)
Model1=lm(Scores~Hours)
summary(Model1)
