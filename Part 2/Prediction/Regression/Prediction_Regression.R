ab <- read.csv("final_sample_format_Part1.csv")
#View(ab)
final_file <- data.frame()
outlier_tag_file <- data.frame()
#View(ab)
ab$Base_hour_Flag <- as.character(ab$Base_hour_Flag)
ab$Base_hour_Flag[ab$Base_hour_Flag=='True'] <- 0
ab$Base_hour_Flag[ab$Base_hour_Flag=='False'] <- 1
ab$Base_hour_Flag <- as.numeric(ab$Base_hour_Flag)

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
ab$Base_Hour_Class <- NULL
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
  
  buildings$Weekday <- as.numeric(buildings$Weekday)
  buildings$Holiday <- as.numeric(buildings$Holiday)
  buildings$Base_hour_Flag <- as.numeric(buildings$Base_hour_Flag)
  #buildings$month <- factor(buildings$month)
  
  smp_size <- floor(0.75 * nrow(buildings))
  
  #Set the seed to make your partition reproductible
  set.seed(123)
  train_ind <- sample(seq_len(nrow(buildings)), size = smp_size)
  
  #Split the data into training and testing
  train <- buildings[train_ind, ]
  test <- buildings[-train_ind, ]
  
  #View(buildings)
  y=train$Norm_Consumption
  train$month <- factor(train$month)
  train$Weekday <- as.numeric(train$Weekday)
  train$Holiday <- as.numeric(train$Holiday)
  train$Base_hour_Flag <- as.numeric(train$Base_hour_Flag)
  lm_fit <- lm(y~ train$Base_hour_Flag +train$Weekday +train$Holiday    + train$month      
                      +train$TemperatureF +train$Dew_PointF ,data=train)
  #summary(lm_fit)
  selected_model <- formula(lm_fit$terms)
  pred_lm <- predict(lm_fit,newdata=train)
  acc <- accuracy(pred_lm,train$Norm_Consumption)
  acc_df <- as.data.frame(acc)
  acc_df$model <- as.character(selected_model[3])
  acc_df$BuildingID_meter <- unique(train$BuildingID_MeterID)
  
  train$predicted_value <- pred_lm
  residual_values <- train$Norm_Consumption - pred_lm
  train$residual_values <- residual_values
  std_dev_residuals<- sd(residual_values)
  #train$outlier <- "False" 
  train$outlier <- ifelse(train$residual_values>=2*std_dev_residuals,"True","False")
  train$residual_values <- NULL
  final_file<- rbind(final_file,acc_df)
  outlier_tag_file <- rbind(outlier_tag_file,train)
  
}
#View(list_of_buildings_heat[1])
for(b_m in 1:length(list_of_buildings_heat)){
  #  buildings <- data.frame(buildings)
  buildings <- data.frame(list_of_buildings_heat[b_m])
  colnames(buildings) <- colnames(buildings_type_heat)
 # outlier_tag_file <- buildings
  #  buildings$Weekday <- as.character(buildings$Weekday)
  #  buildings$Holiday <- as.character(buildings$Holiday)
  #  buildings$Base_hour_Flag <- as.character(buildings$Base_hour_Flag)
  
  #buildings$month = factor(buildings$month)
  #m <- model.matrix( ~buildings$month , data = buildings )
  #buildings <- cbind(buildings,m)
  
  buildings$Weekday <- as.numeric(buildings$Weekday)
  buildings$Holiday <- as.numeric(buildings$Holiday)
  buildings$Base_hour_Flag <- as.numeric(buildings$Base_hour_Flag)
  #buildings$month <- factor(buildings$month)
  
  smp_size <- floor(0.75 * nrow(buildings))
  
  #Set the seed to make your partition reproductible
  set.seed(123)
  train_ind <- sample(seq_len(nrow(buildings)), size = smp_size)
  
  #Split the data into training and testing
  train <- buildings[train_ind, ]
  test <- buildings[-train_ind, ]
  #View(buildings)
  y=train$Norm_Consumption
  train$month <- factor(train$month)
  train$Weekday <- as.numeric(train$Weekday)
  train$Holiday <- as.numeric(train$Holiday)
  train$Base_hour_Flag <- as.numeric(train$Base_hour_Flag)
  train$WindDirDegrees <- as.factor(train$WindDirDegrees)
  
  lm_fit <- lm(y~ train$month +train$Base_hour_Flag +train$Weekday                  
               +train$TemperatureF +train$Dew_PointF +train$WindDirDegrees ,data=train)
  
  #summary(lm_fit)
  selected_model <- formula(lm_fit$terms)
  pred_lm <- predict(lm_fit,newdata=train)
  acc <- accuracy(pred_lm,train$Norm_Consumption)
  acc_df <- as.data.frame(acc)
  acc_df$model <- as.character(selected_model[3])
  acc_df$BuildingID_meter <- unique(train$BuildingID_MeterID)
  
  train$predicted_value <- pred_lm
  residual_values <- train$Norm_Consumption - pred_lm
  train$residual_values <- residual_values
  std_dev_residuals<- sd(residual_values)
  #train$outlier <- "False" 
  train$outlier <- ifelse(train$residual_values>=2*std_dev_residuals,"True","False")
  train$residual_values <- NULL
  final_file<- rbind(final_file,acc_df)
  outlier_tag_file <- rbind(outlier_tag_file,train)
  
}
final_file$ME<-NULL
final_file$MPE<-NULL
final_file$algorithm <- "Linear Regression"
#View(outlier_tag_file)
write.csv(final_file,file = "Linear_regrssion_models.csv")
write.csv(outlier_tag_file,file = "Outlier_regrssion_models.csv")


