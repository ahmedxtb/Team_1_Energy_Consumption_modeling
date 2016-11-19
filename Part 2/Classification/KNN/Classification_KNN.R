
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
#ab$Norm_Consumption <- NULL
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
    buildings$Norm_Consumption <- NULL
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
  
    y=train$Base_Hour_Class
    #train$month <- factor(train$month)
    train$Base_Hour_Class <- factor(train$Base_Hour_Class)
    train$Weekday <- as.numeric(train$Weekday)
    train$Holiday <- as.numeric(train$Holiday)
    train$Base_hour_Flag <- as.numeric(train$Base_hour_Flag)
  #View(train)
    
    normalize<-function(x)(return((x-min(x))/(max(x)-min(x))))
    #colnames(train)
    #Select the features
    
    train_features_knn <-  select(train,Base_Hour_Class,Weekday,Base_hour_Flag,
                                  Holiday,TemperatureF,Dew_PointF,as.numeric(buildings$month5),
                                  as.numeric(buildings$month6),as.numeric(buildings$month7),
                                  as.numeric(buildings$month8))
    train_features_target_knn <- train_features_knn[,1]
    train_features_for_train_knn <- as.data.frame(lapply(train_features_knn[,-1],normalize))
    test_features_knn <- select(test,Base_Hour_Class,Weekday,Base_hour_Flag,
                                Holiday,TemperatureF,Dew_PointF,as.numeric(buildings$month5),
                                as.numeric(buildings$month6),as.numeric(buildings$month7),
                                as.numeric(buildings$month8))
    test_features_target_knn <- test_features_knn[,1]
    test_features_for_test_knn <- as.data.frame(lapply(test_features_knn[,-1],normalize))
    #colnames(train_features_for_train_knn)
    #Model
    knnModel<- knn(train= train_features_for_train_knn, test= test_features_for_test_knn, 
                   cl=train_features_target_knn, k=k_value)
  
  
    cm <- table(test_features_target_knn,knnModel)
    confusion_matrix <- as.data.frame.matrix(cm)
    selected_model<- colnames(train_features_for_train_knn)
    selected_model <- toString(selected_model)
    selected_model <- gsub(",","+",selected_model)
    confusion_matrix$Model <- selected_model
    confusion_matrix$BuildingID_MeterID <- unique(train$BuildingID_MeterID)
    confusion_matrix$algorithm <- "KNN"
  
    final_file<- rbind(final_file,confusion_matrix)
    
    colnames(test) <- colnames(buildings)
    
    test$predicted_Flag <- knnModel
    #View(test)
    grouped_test = sqldf("SELECT day,month,Count(*) as count FROM test where predicted_Flag != Base_Hour_Class GROUP BY day,month ")
    grouped_test$outlier_day <- ifelse(grouped_test$count > 6,'True','False')
    grouped_test$count <- NULL
    test <- merge(grouped_test, test, by=c("month","day"),all.y = TRUE )
    
    test$outlier_day[is.na(test$outlier_day)] <- 'False'
    
    outlier_tag_file <- rbind(outlier_tag_file,test)
    #outlier_tag_file <- rbind(outlier_tag_file,test)
  }
}
#View(final_file)


#View(list_of_buildings_heat[1])

for(b in 1:length(list_of_buildings_heat)){
  
  buildings <- data.frame(list_of_buildings_heat[b])
  colnames(buildings) <- colnames(buildings_type_heat)
  if(sum(buildings$Norm_Consumption)>0){
    buildings$Weekday <- as.character(buildings$Weekday)
    buildings$Holiday <- as.character(buildings$Holiday)
    buildings$Base_hour_Flag <- as.character(buildings$Base_hour_Flag)
    buildings$Norm_Consumption <- NULL
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
    #View(test)
    #View(train)
    
    y=train$Base_Hour_Class
    #train$month <- factor(train$month)
    train$Base_Hour_Class <- factor(train$Base_Hour_Class)
    train$Weekday <- as.numeric(train$Weekday)
    train$Holiday <- as.numeric(train$Holiday)
    train$Base_hour_Flag <- as.numeric(train$Base_hour_Flag)
    #View(train)
    
    normalize<-function(x)(return((x-min(x))/(max(x)-min(x))))
    #colnames(train)
    #Select the features
    
    train_features_knn <-  select(train,Base_Hour_Class,Weekday,Base_hour_Flag,
                                  TemperatureF,Dew_PointF,as.numeric(buildings$month5),
                                  as.numeric(buildings$month6),as.numeric(buildings$month7),
                                  as.numeric(buildings$month8))
    train_features_target_knn <- train_features_knn[,1]
    train_features_for_train_knn <- as.data.frame(lapply(train_features_knn[,-1],normalize))
    test_features_knn <- select(test,Base_Hour_Class,Weekday,Base_hour_Flag,
                                TemperatureF,Dew_PointF,as.numeric(buildings$month5),
                                as.numeric(buildings$month6),as.numeric(buildings$month7),
                                as.numeric(buildings$month8))
    test_features_target_knn <- test_features_knn[,1]
    test_features_for_test_knn <- as.data.frame(lapply(test_features_knn[,-1],normalize))
    #colnames(train_features_for_train_knn)
    #Model
    knnModel<- knn(train= train_features_for_train_knn, test= test_features_for_test_knn, 
                   cl=train_features_target_knn, k=k_value)
    
    #View(cm)
    cm <- table(test_features_target_knn,knnModel)
    confusion_matrix <- as.data.frame.matrix(cm)
    selected_model<- colnames(train_features_for_train_knn)
    selected_model <- toString(selected_model)
    selected_model <- gsub(",","+",selected_model)
    confusion_matrix$Model <- selected_model
    confusion_matrix$BuildingID_MeterID <- unique(train$BuildingID_MeterID)
    confusion_matrix$algorithm <- "KNN"
    
    final_file<- rbind(final_file,confusion_matrix)
    
    colnames(test) <- colnames(buildings)
    
    test$predicted_Flag <- knnModel
    #View(test)
    grouped_test = sqldf("SELECT day,month,Count(*) as count FROM test where predicted_Flag != Base_Hour_Class GROUP BY day,month ")
    grouped_test$outlier_day <- ifelse(grouped_test$count > 6,'True','False')
    grouped_test$count <- NULL
    test <- merge(grouped_test, test, by=c("month","day"),all.y = TRUE )
    
    test$outlier_day[is.na(test$outlier_day)] <- 'False'
    
    outlier_tag_file <- rbind(outlier_tag_file,test)
  }
}

#View(final_file)
#final_file$algorithm <- "Linear Regression"
#View(outlier_tag_file)
write.csv(final_file,file = "KNN_Classification_models.csv")
write.csv(outlier_tag_file,file = "Classi_Outlier_KNN_models.csv")

