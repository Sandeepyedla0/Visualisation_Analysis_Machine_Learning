
library(dplyr)
library(tidyr)
library(ggplot2)
library(MASS)
library(lattice)
library(corrplot)
library(class)

library(lmtest)
library(Publish)
library(tibble)
library(GGally)
library(ROCR)
library(ineq)
library(mice)
library(naivebayes)
library(tidyverse)
library(caret)
library(reshape2)
library(RColorBrewer)
library(psych)
library(plyr)
library(arules)
library(pscl)
library(magrittr)
library(factoextra)
library(cluster)

library(rpart)
library(rpart.plot)
library(Metrics)
library(MLmetrics)
library(randomForest)
library(gridExtra) 
library(reshape2)

setwd("~/Desktop/GMU/GMU/STAT 515/Final_Project/May5")

led <- read_csv("Life_Expectancy_final.csv")


#----------------

orig<-led
torig_csv<-led   #      Original data
#view(Led)

#-----

colnames(torig_csv) = c("country","year","status","life_Expectancy",
                        "Adult_Mortality","Infant_mortality", "Alcohol",
                        "Percent_Expend","HepatitisB", "Measles","BMI","under5mor",
                        "Polio","Total_expenditure","Diphtheria","HIV","GDP",
                        "Population","Thin_10to19years","Thin5_9", "Income_resource",
                        "Yearsofschool")

#-----------Density plot for response variable---------


d<-ggplot(torig_csv, aes(x=life_Expectancy)) + 
  geom_density(alpha=0.5)+
  geom_vline(aes(xintercept=mean(life_Expectancy)),
             color="blue", linetype="dashed", size=1)+
  labs(title="Overall Density Distribution ",x="Life_Expectancy",y="Density")+
  theme(plot.title = element_text(hjust = 0.52))
d


#------------Corrplot
corplt <-subset(torig_csv,select=c(life_Expectancy, Adult_Mortality, Alcohol,Percent_Expend,Measles,Polio,Total_expenditure,HIV,GDP,Population))
ds_corr_gph <- cor(corplt)
corrplot(ds_corr_gph,type="upper", order="hclust",title='\n\n\n\n\n\n\n                                             Correlation Plot',col=(brewer.pal(n=9, name="Greens")))

#-------

#variable transformation
led$adult.mor = as.numeric(led$adult.mor)
led$infant.mor  = as.numeric(led$infant.mor)
led$measles  = as.numeric(led$measles)
led$under5mor  = as.numeric(led$under5mor)
led$H.B = as.numeric(led$H.B)
led$diphtheria = as.numeric(led$diphtheria)
led$polio = as.numeric(led$polio) 


colnames(led[,sapply(led, is.factor)])

table(led$year)
#--------- subsetting 2014

Uled2014<-(subset(led,year==2014))
#--------linear regression in 2014---
#view(Uled2014)
colnames(Uled2014)
life_selected<- subset(Uled2014, select=-c(country, year,status))

#--------------------
set.seed(123)


life_full <- lm(formula =  life.expect~., data = life_selected)
life_none <- lm(formula = life.expect ~1, data = life_selected)

#View(Uled2014)
life_model <- lm(formula = life.expect ~., data = life_selected)
#view(life_selected)
summary(life_model)

par(mfrow=c(2,2))
plot(life_model)

#------------------------

step.model <- stepAIC(life_full, direction = "both", trace = FALSE)

par(mfrow=c(2,2))
summary(step.model)

# Set seed for reproducibility

# Set up repeated k-fold cross-validation

train.control <- trainControl(method = "cv", number = 10)
# Train the model
step.model <- train(life.expect~., data = life_selected,
                    method = "leapBackward", 
                    tuneGrid = data.frame(nvmax = 1:8),
                    trControl = train.control)
step.model$results

step.model$bestTune

summary(step.model$finalModel)

Uled2014_model<-lm(formula =life.expect~adult.mor+H.B+tot.expend+HIV+thin.5.9+inc.resource, data=life_selected)
#Uled2014_model<-lm(formula =life.expect~adult.mor+tot.expend+HIV+inc.resource, data=life_selected)

summary(Uled2014_model)


plot(Uled2014_model)





#----------------------

data.num = led[,-c(1,2,3)]


dim(data.num)




#Split dataset by country status
developed = led[which(led$status == "Developed"),]
dim(developed)
developing = led[which(led$status == "Developing"),]
dim(developing)

#Missing value imputation
library(mice)
#md.pattern(data.num)
led.impute.developed = mice(led, method = "mean", seed = 1)
led.developed = complete(led.impute.developed)
sum(is.na(led.developed))
dim(led.developed)
led.impute.developing = mice(developing, method = "mean", seed = 1)
led.developing = complete(led.impute.developing)
sum(is.na(led.developing))
dim(led.developing)

#Numerical data
developed.num = led.developed[,-c(1,2,3)]
developing.num = led.developing[,-c(1,2,3)]

out.developed = winsor(developed.num, trim = 0.01)
out.developed.df = as.data.frame(out.developed)

out.developing = winsor(developing.num, trim = 0.01)
out.developing.df = as.data.frame(out.developing)

temp.data = rbind(out.developed, out.developing)

na.omit(led)
train1 = sample(1:nrow(life_selected),0.7*nrow(life_selected))

head(train1,5)
test1 = led$life.expect[-train1]

Rf_model<-randomForest(life.expect~.,data = life_selected,subset = train1,mtry=8,ntree=200,importance=TRUE)

Rf_model

importance(Rf_model)
varImpPlot(Rf_model,sort=TRUE)

Rf_model$importance

plot(Rf_model)








