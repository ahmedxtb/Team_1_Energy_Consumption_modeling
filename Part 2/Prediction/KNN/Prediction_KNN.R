
library("rjson")
config_json<- fromJSON(file="config_KNN.json")
k_value <- as.integer(config_json$nearN)

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

  buildings <- data.frame(list_of_buildings_elect[b])
  colnames(buildings) <- colnames(buildings_type_elect)
  if(sum(buildings$Norm_Consumption)>0){
    buildings$Weekday <- as.character(buildings$Weekday)
    buildings$Holiday <- as.character(buildings$Holiday)
    buildings$Base_hour_Flag <- as.character(buildings$Base_hour_Flag)
    
    buildings$month = factor(buildings$month)
    m <- model.matrix( ~buildings$month , data = buildings )
    buildings <- cbind(buildings,m)
    
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
    
    #View(train)
  
    y=train$Norm_Consumption
    #train$month <- factor(train$month)
    train$Weekday <- as.numeric(train$Weekday)
    train$Holiday <- as.numeric(train$Holiday)
    train$Base_hour_Flag <- as.numeric(train$Base_hour_Flag)
  #View(train)
    
    normalize<-function(x)(return((x-min(x))/(max(x)-min(x))))
    #colnames(train)
    #Select the features
    
    train_features_knn <-  select(train,Norm_Consumption,Weekday,Base_hour_Flag,
                                  Holiday,TemperatureF,Dew_PointF,as.numeric(buildings$month5),
                                  as.numeric(buildings$month6),as.numeric(buildings$month7),
                                  as.numeric(buildings$month8))
    train_features_target_knn <- train_features_knn[,1]
    train_features_for_train_knn <- as.data.frame(lapply(train_features_knn[,-1],normalize))
    test_features_knn <- select(test,Norm_Consumption,Weekday,Base_hour_Flag,
                                Holiday,TemperatureF,Dew_PointF,as.numeric(buildings$month5),
                                as.numeric(buildings$month6),as.numeric(buildings$month7),
                                as.numeric(buildings$month8))
    test_features_target_knn <- test_features_knn[,1]
    test_features_for_test_knn <- as.data.frame(lapply(test_features_knn[,-1],normalize))
    #colnames(train_features_for_train_knn)
    #Model
    knnModel<- knn(train= train_features_for_train_knn, test= test_features_for_test_knn, 
                   cl=train_features_target_knn, k=k_value)
    
    knnModel <- as.numeric(as.character(knnModel))
  
    RMSE <- sqrt(mean((knnModel-test_features_target_knn)^2))
    MAE <- mean(abs(knnModel-test_features_target_knn))
    MAPE <- mean(abs((knnModel-test_features_target_knn)/knnModel))
    selected_model<- colnames(train_features_for_train_knn)
    selected_model <- toString(selected_model)
    selected_model <- gsub(",","+",selected_model)
    BuildingID_meter <- unique(train$BuildingID_MeterID)
    algorithm <- "KNN"
    acc_df_e <- data.frame(RMSE,MAE,MAPE,selected_model,BuildingID_meter,algorithm) 
    
    test$predicted_value <- knnModel
    test$residual_values <- knnModel-test$Norm_Consumption
    std_dev_residuals<- sd(test$residual_values)
    #test$outlier <- NULL 
    test$outlier <- ifelse(abs(test$residual_values)>=2*abs(std_dev_residuals),"True","False")
    test$residual_values <- NULL
    final_file<- rbind(final_file,acc_df_e)
    #outlier_tag_file <- rbind(outlier_tag_file,test)
  }
}
#View(test)


#View(list_of_buildings_heat[1])
for(b_m in 1:length(list_of_buildings_heat)){
  #  buildings <- data.frame(buildings)
  buildings <- data.frame(list_of_buildings_heat[b_m])
  colnames(buildings) <- colnames(buildings_type_heat)
 
  buildings$Weekday <- as.character(buildings$Weekday)
  buildings$Holiday <- as.character(buildings$Holiday)
  buildings$Base_hour_Flag <- as.character(buildings$Base_hour_Flag)
  
  buildings$month = factor(buildings$month)
  m <- model.matrix( ~buildings$month , data = buildings )
  buildings <- cbind(buildings,m)
  
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
  
  #View(train)
  
  y=train$Norm_Consumption
  #train$month <- factor(train$month)
  train$Weekday <- as.numeric(train$Weekday)
  train$Holiday <- as.numeric(train$Holiday)
  train$Base_hour_Flag <- as.numeric(train$Base_hour_Flag)
  #View(train)
  
  
  normalize<-function(x)(return((x-min(x))/(max(x)-min(x))))
  #colnames(train)
  #Select the features
  
  
  train_features_knn <-  select(train,Norm_Consumption,Weekday,Base_hour_Flag,
                                TemperatureF,Dew_PointF,as.numeric(buildings$month5),
                                as.numeric(buildings$month6),as.numeric(buildings$month7),
                                as.numeric(buildings$month8))
  train_features_target_knn <- train_features_knn[,1]
  train_features_for_train_knn <- as.data.frame(lapply(train_features_knn[,-1],normalize))
  test_features_knn <- select(test,Norm_Consumption,Weekday,Base_hour_Flag,
                              TemperatureF,Dew_PointF,as.numeric(buildings$month5),
                              as.numeric(buildings$month6),as.numeric(buildings$month7),
                              as.numeric(buildings$month8))
  test_features_target_knn <- test_features_knn[,1]
  test_features_for_test_knn <- as.data.frame(lapply(test_features_knn[,-1],normalize))
#  colnames(train_features_for_train_knn)
  #Model
  knnModel<- knn(train= train_features_for_train_knn, test= test_features_for_test_knn, 
                 cl=train_features_target_knn, k=k_value)
  
  knnModel <- as.numeric(as.character(knnModel))
  
  RMSE <- sqrt(mean((knnModel-test_features_target_knn)^2))
  MAE <- mean(abs(knnModel-test_features_target_knn))
  MAPE <- mean(abs((knnModel-test_features_target_knn)/knnModel))
  selected_model<- colnames(train_features_for_train_knn)
  selected_model <- toString(selected_model)
  selected_model <- gsub(",","+",selected_model)
  BuildingID_meter <- unique(train$BuildingID_MeterID)
  algorithm <- "KNN"
  acc_df <- data.frame(RMSE,MAE,MAPE,selected_model,BuildingID_meter,algorithm) 
  
  test$predicted_value <- knnModel
  test$residual_values <- knnModel-test$Norm_Consumption
  std_dev_residuals<- sd(test$residual_values)
  #test$outlier <- NULL 
  test$outlier <- ifelse(abs(test$residual_values)>=2*abs(std_dev_residuals),"True","False")
  test$residual_values <- NULL
  final_file<- rbind(final_file,acc_df)
  outlier_tag_file <- rbind(outlier_tag_file,test)
  
}

#View(final_file)
#final_file$algorithm <- "Linear Regression"
#View(outlier_tag_file)
write.csv(final_file,file = "KNN_regrssion_models.csv")
write.csv(outlier_tag_file,file = "Outlier_KNN_models.csv")
