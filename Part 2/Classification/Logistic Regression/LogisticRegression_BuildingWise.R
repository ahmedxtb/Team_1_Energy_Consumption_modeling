install.packages("ROCR")
library(ROCR)
library("caret")

ab <- read.csv("final_sample_format_Part1.csv")
#View(ab)
final_file <- data.frame()
prediction_file <- data.frame()
#elect_file <- data.frame()
#prediction_elect_file <- data.frame()

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
#ab$type<- NULL
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


types_of_IDs <- split(ab,f=ab$type)
#View(buildings_type_elect)

buildings_type_elect <- data.frame(types_of_IDs[2])
buildings_type_heat <- data.frame(types_of_IDs[1])
colnames(buildings_type_elect) <- colnames(ab)
colnames(buildings_type_heat) <- colnames(ab)
#class(list_of_buildings_elect)
buildings_type_heat$type<-NULL
buildings_type_elect$type<-NULL


list_of_buildings_elect <- split(buildings_type_elect,f = buildings_type_elect$BuildingID_MeterID)
list_of_buildings_heat <- split(buildings_type_heat,f = buildings_type_heat$BuildingID_MeterID)

list_of_buildings_elect <- list_of_buildings_elect[sapply(list_of_buildings_elect,function(x) dim(x)[1])>0]
list_of_buildings_heat <- list_of_buildings_heat[sapply(list_of_buildings_heat,function(x) dim(x)[1])>0]
# length(list_of_buildings_heat)
# View(list_of_buildings_elect[1])


for(b in 1:length(list_of_buildings_elect)){
  #  buildings <- data.frame(buildings)
  buildings <- data.frame(list_of_buildings_elect[b])
  colnames(buildings) <- colnames(buildings_type_elect)
  
  #  buildings$Weekday <- as.character(buildings$Weekday)
  #  buildings$Holiday <- as.character(buildings$Holiday)
  #  buildings$Base_hour_Flag <- as.character(buildings$Base_hour_Flag)
  
  #buildings$month = factor(buildings$month)
  #m <- model.matrix( ~buildings$month , data = buildings )
  #buildings <- cbind(buildings,m)
  if(sum(buildings$Norm_Consumption >0)){
  buildings$Weekday <- as.numeric(buildings$Weekday)
  buildings$Holiday <- as.numeric(buildings$Holiday)
  buildings$Base_hour_Flag <- as.numeric(buildings$Base_hour_Flag)
  buildings$month <- factor(buildings$month)
  #buildings$month <- factor(buildings$month)
  
  smp_size <- floor(0.75 * nrow(buildings))
  
  #Set the seed to make your partition reproductible
  set.seed(123)
  train_ind <- sample(seq_len(nrow(buildings)), size = smp_size)
  
  #Split the data into training and testing
  train <- buildings[train_ind, ]
  test <- buildings[-train_ind, ]
  #View(buildings)
  y=train$Base_Hour_Class
  train$month <- factor(train$month)
  train$Weekday <- as.numeric(train$Weekday)
  train$Holiday <- as.numeric(train$Holiday)
  train$Base_hour_Flag <- as.numeric(train$Base_hour_Flag)
  
  
  glm_fit <- glm(y ~ Base_hour_Flag + Weekday + Holiday + TemperatureF
                 + Dew_PointF + Humidity 
                 + Sea_Level_PressureIn + month,
                 data = train, family=binomial(link="logit"))
  
  
  #summary(glm_fit)
  
  test.prob <- predict(glm_fit, test, type="response")
  pred <- rep("High",length(test.prob))
  
  
  #Set Cutoff Value to 0.5
  pred[test.prob >=0.5] <- "Low"
  
  colnames(test) <- colnames(buildings)
  
  test$predicted_Flag <- pred
  
  grouped_test = sqldf("SELECT day,month,Count(*) as count FROM test where predicted_Flag != Base_Hour_Class GROUP BY day,month ")
  grouped_test$outlier_day <- ifelse(grouped_test$count > 6,'True','False')
  
  grouped_test$count <- NULL
  
  test <- merge(grouped_test, test, by=c("month","day"),all.y = TRUE )
  
  test$outlier_day[is.na(test$outlier_day)] <- 'False'
  
  prediction_file <- rbind(prediction_file,test)
  
  #pred
  
  #Creating Confusion matrix
  confusion_matrix <- confusionMatrix(test$Base_Hour_Class, pred)
  
  confusion_matrix <- as.data.frame.matrix(confusion_matrix$table)
  
  model_Terms <- formula(glm_fit$terms)
  
  confusion_matrix$Model <- as.character(model_Terms[3]) 
  
  confusion_matrix$BuildingID_MeterID <- unique(train$BuildingID_MeterID)

  final_file<- rbind(final_file,confusion_matrix)
  
  ##ModelWise Plot & ROC Curves for Each Building  
 # plot(glm_fit)
  #prediction <- prediction(test.prob, test$Base_Hour_Class)
  #performance <- performance(prediction, measure = "tpr", x.measure = "fpr")
  #plot(performance, main="ROC curve", xlab="1-Specificity", ylab="Sensitivity")
  
  }
}


for(b_m in 1:length(list_of_buildings_heat)){
  #  buildings <- data.frame(buildings)
  buildings <- data.frame(list_of_buildings_heat[b_m])
  colnames(buildings) <- colnames(buildings_type_heat)
  
  #  buildings$Weekday <- as.character(buildings$Weekday)
  #  buildings$Holiday <- as.character(buildings$Holiday)
  #  buildings$Base_hour_Flag <- as.character(buildings$Base_hour_Flag)
  
  #buildings$month = factor(buildings$month)
  #m <- model.matrix( ~buildings$month , data = buildings )
  #buildings <- cbind(buildings,m)
  
  buildings$Weekday <- as.numeric(buildings$Weekday)
  buildings$Holiday <- as.numeric(buildings$Holiday)
  buildings$Base_hour_Flag <- as.numeric(buildings$Base_hour_Flag)
  buildings$month <- factor(buildings$month)
  #buildings$month <- factor(buildings$month)
  
  smp_size <- floor(0.75 * nrow(buildings))
  
  #Set the seed to make your partition reproductible
  set.seed(123)
  train_ind <- sample(seq_len(nrow(buildings)), size = smp_size)
  
  #Split the data into training and testing
  train <- buildings[train_ind, ]
  test <- buildings[-train_ind, ]
  #View(buildings)
  y=train$Base_Hour_Class
  train$month <- factor(train$month)
  train$Weekday <- as.numeric(train$Weekday)
  train$Holiday <- as.numeric(train$Holiday)
  train$Base_hour_Flag <- as.numeric(train$Base_hour_Flag)
  
  
  glm_fit <- glm(y ~ Base_hour_Flag + Weekday + Holiday + TemperatureF
                 + Dew_PointF + Humidity 
                 + Wind_SpeedMPH + month,
                 data = train, family=binomial(link="logit"))
  
  
  #summary(glm_fit)
  
  test.prob <- predict(glm_fit, test, type="response")
  pred <- rep("High",length(test.prob))
  
  
  #Set Cutoff Value to 0.5
  pred[test.prob >=0.5] <- "Low"
  
  colnames(test) <- colnames(buildings)
  
  test$predicted_Flag <- pred
  
  grouped_test = sqldf("SELECT day,month,Count(*) as count FROM test where predicted_Flag != Base_Hour_Class GROUP BY day,month ")
  grouped_test$outlier_day <- ifelse(grouped_test$count > 6,'True','False')
  
  grouped_test$count <- NULL
  
  test <- merge(grouped_test, test, by=c("month","day"),all.y = TRUE )
  
  test$outlier_day[is.na(test$outlier_day)] <- 'False'
  
  prediction_file <- rbind(prediction_file,test)
  
  #pred
  
  #Creating Confusion matrix
  confusion_matrix <- confusionMatrix(test$Base_Hour_Class, pred)
  
  confusion_matrix <- as.data.frame.matrix(confusion_matrix$table)
  
  model_Terms <- formula(glm_fit$terms)
  
  confusion_matrix$Model <- as.character(model_Terms[3]) 
  
  confusion_matrix$BuildingID_MeterID <- unique(train$BuildingID_MeterID)
  
  final_file<- rbind(final_file,confusion_matrix)
  
  ##ModelWise Plot & ROC Curves for Each Building  
  # plot(glm_fit)
  #prediction <- prediction(test.prob, test$Base_Hour_Class)
  #performance <- performance(prediction, measure = "tpr", x.measure = "fpr")
  #plot(performance, main="ROC curve", xlab="1-Specificity", ylab="Sensitivity")
  
}

View(final_file)
View(prediction_file)

write.csv(final_file,"confusionMatrix.csv")
write.csv(prediction_file,"Logistic_combined_Buildingwise_output_.csv")




