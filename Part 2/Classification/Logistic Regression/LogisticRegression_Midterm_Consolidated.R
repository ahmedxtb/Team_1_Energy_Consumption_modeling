install.packages("leaps")
library("leaps")

ab <- read.csv("final_sample_format_Part1.csv")
#View(ab)

final_file <- data.frame()
#View(ab)

#Feature Transformation Converting Base_Hour_Flag to 0 & 1

ab$Base_hour_Flag <- as.character(ab$Base_hour_Flag)
ab$Base_hour_Flag[ab$Base_hour_Flag=='True'] <- 0
ab$Base_hour_Flag[ab$Base_hour_Flag=='False'] <- 1
ab$Base_hour_Flag <- as.numeric(ab$Base_hour_Flag)


#Feature Engineering

#Dropping Columns which are not required in the model & have Data issues
ab$X..address.y<-NULL
ab$area_floor._m.sqr.y<-NULL
ab$BuildingID <-NULL
ab$building<-NULL
ab$meternumb<-NULL
ab$airport_code<-NULL
ab$type<- NULL
ab$date<- NULL
ab$year<-NULL
ab$Base_hour_usage <- NULL
ab$Consumption <- NULL
#ab$Base_Hour_Class <- NULL
ab$VisibilityMPH <- NULL ## more than 50% data is negative
ab$Gust_SpeedMPH<-NULL    #601105 values = '-'
ab$PrecipitationIn<-NULL  #614116 values N/A
ab$Wind_Direction <- NULL # wind dir deg is numreical for wind_direction
ab$Events <- NULL         # 350626 values empty

#str(ab)

#Converting Categorical Variables into Factor to feed the Model
ab$Conditions <- factor(ab$Conditions)
ab$month <- factor(ab$month)
ab$day <- factor(ab$day)
ab$Day.of.Week <- factor(ab$Day.of.Week)
ab$hour <- factor(ab$hour)
ab$Base_hour_Flag <- as.numeric(ab$Base_hour_Flag)
ab$Weekday <- as.numeric(ab$Weekday)
ab$Holiday <- as.numeric(ab$Holiday)
ab$WindDirDegrees <- factor(ab$WindDirDegrees)


smp_size <- floor(0.75 * nrow(ab))


#Set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(ab)), size = smp_size)

train <- ab[train_ind, ]
test <- ab[-train_ind, ]

y=train$Base_Hour_Class


#Construct a logistic regression model for factor Base_Hour_Class using all other variables 
fit1 <- glm(y ~ day + Base_hour_Flag +Weekday + hour 
            + Holiday + TemperatureF +  WindDirDegrees 
            + month + Dew_PointF + Day.of.Week + Humidity + Sea_Level_PressureIn + Wind_SpeedMPH + Conditions,
            data=train, family=binomial(link="logit"))

summary(fit1)

## Forward Selection for Variable Selection

# regit.full <- regsubsets( Base_Hour_Class ~  day + Base_hour_Flag +Weekday + hour 
#                          + Holiday + TemperatureF +  WindDirDegrees 
#                          + month + Dew_PointF + Day.of.Week + Humidity + Sea_Level_PressureIn + Wind_SpeedMPH + Conditions,
#                          data=train,method="forward")
#plot(regit.full)


#Construct a logistic regression model for factor Base_Hour_Class 
#Using Selected Features

fit2 <- glm(y ~ Base_hour_Flag +Weekday  
            + Holiday + TemperatureF 
            + Dew_PointF,
            data=train, family=binomial(link="logit"))

summary(fit2)


test.prob <- predict(fit2, test, type="response")

pred <- rep("High",length(test.prob))


#Set Cutoff Value to 0.5
pred[test.prob >=0.5] <- "Low"

colnames(test) <- colnames(ab)

test$predicted_Flag <- pred

grouped_test = sqldf("SELECT day,month,Count(*) as count FROM test where predicted_Flag != Base_Hour_Class GROUP BY day,month ")
grouped_test$outlier_day <- ifelse(grouped_test$count > 6,'True','False')

grouped_test$count <- NULL

test <- merge(grouped_test, test, by=c("month","day"),all.y = TRUE )

test$outlier_day[is.na(test$outlier_day)] <- 'False'

#pred

#Creating Confusion matrix
library("caret")
confusion_matrix <- confusionMatrix(test$Base_Hour_Class, pred)

confusion_matrix <- as.data.frame.matrix(confusion_matrix$table)

model_Terms <- formula(fit2$terms)

confusion_matrix$Model <- as.character(model_Terms[3]) 

#View(confusion_matrix)
#confusion_matrix$BuildingID_MeterID <- unique(train$X28168_4.BuildingID_MeterID)

write.csv(confusion_matrix,"FullDataset_confusionMatrix.csv")
write.csv(test,"Logistic_FullDataset_output.csv")

library(ROCR)
prediction <- prediction(test.prob, test$Base_Hour_Class)
performance <- performance(prediction, measure = "tpr", x.measure = "fpr")
plot(performance, main="ROC curve", xlab="1-Specificity", ylab="Sensitivity")

plot(fit2)

