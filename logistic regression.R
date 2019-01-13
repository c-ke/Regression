library(caret)
library(caTools)
library(ggplot2)
The_main_dataset = read.csv(file="C:/Chaitanya/titanic.csv",header=T,na.strings=c(""))
The_main_dataset = The_main_dataset[,2:7]
The_main_dataset[,4] = NULL

The_main_dataset= The_main_dataset[sample(nrow(The_main_dataset)),]
training_data = The_main_dataset[1:984,]
test_data=The_main_dataset[985:1313,]
model = glm(Survived~Sex,family = binomial(link = "logit"),data = training_data,control=list(maxit=50))
summary(model)
fitted.results <- predict(model,newdata=subset(test_data,select=c(1,3,4)),type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != test_data$Survived)
print(paste('Accuracy',1-misClasificError))
