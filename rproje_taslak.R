library(tidyverse)
library(ggpubr)
library(rstatix)
library(car)
library(broom)
library(dplyr)
library(tidyr)
library(readxl)
heart<-read_excel("C:/Users/admin/Downloads/heart.xlsx")
summary(heart)
heart$Sex<- as.factor(heart$Sex)
heart$HeartDiseade <- as.factor(heart$HeartDisease)
summary(heart)

#Grup gozlem dagilimi
heart %>%
  group_by(HeartDisease) %>%
  dplyr::summarise(N = n())


#Degiskenlere Göre Ortalamalar ve Ortalama Çizimleri
heart %>% 
  group_by(HeartDisease) %>%
  summarise(across(-FastingBS, list(mean=mean,sd=sd)))

heart %>% 
  group_by(FastingBS) %>%
  summarise(across(-HeartDisease, list(mean=mean,sd=sd)))

library(gplots)
#hastalık icin :
plotmeans(Age~HeartDisease,xlab="yaş",ylab="hastalık", main="Mean Plot\nwith 95% CI",data=heart)
plotmeans(Cholesterol~HeartDisease, xlab="kolestrol",ylab="hastalık", main="Mean Plot\nwith 95% CI",data=heart)
plotmeans(MaxHR~HeartDiseade, xlab="max kalp atışı",ylab="hastalık", main="Mean Plot\nwith 95% CI",data=heart)

library(dplyr)

#Multivariate Normality
heart %>%
  dplyr::select(Age,Cholesterol,MaxHR) %>%
  mshapiro_test()

#Homogeneity of Covariances
install.packages("biotools")
library(biotools)
box_m(heart[, c("Age","Cholesterol","MaxHR")], heart$HeartDisease)

heart_manova <- manova(cbind(Age,Cholesterol,MaxHR) ~ HeartDisease,data=heart)
summary(heart_manova, test = "Hotelling-Lawley")
summary(heart_manova, test = "Wilks")
summary(heart_manova, test = "Pillai")
summary(heart_manova, test = "Roy")
