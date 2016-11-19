


install.packages("neuralnet")

library("dply")
library("neuralnet")
ab <- read.csv("final_sample_format_Part1.csv")

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
ab$Base_Hour_Class <- NULL
ab$VisibilityMPH <- NULL ## more than 50% data is negative
ab$Gust_SpeedMPH<-NULL    #601105  values = '-'
ab$PrecipitationIn<-NULL  #614116  values N/A
ab$Wind_Direction <- NULL # wind dir deg is numreical for wind_direction
ab$Events <- NULL         # 350626 values empty

ab$Conditions <- factor(ab$Conditions)
ab$Base_hour_Flag <- as.character(ab$Base_hour_Flag)
#ab$Weekday <- as.factor(ab$Weekday)
ab$hour <- as.factor(ab$hour)
#ab$Holiday <- as.factor(ab$Holiday)
ab$month <- as.factor(ab$month)
ab$Day.of.Week <- as.factor(ab$Day.of.Week)



ab$Base_hour_Flag[ab$Base_hour_Flag=='True']= 1
ab$Base_hour_Flag[ab$Base_hour_Flag=='False']= 0


#list_of_buildings <- split(ab,f = ab$BuildingID_MeterID)

#model1 <- data.frame(list_of_buildings[1])
model1 <- ab

#Converting categorical values into dummy values
m <- model.matrix( ~ model1$month + model1$hour, data = model1 )
model1 <- cbind(model1,m)

smp_size <- floor(0.75 * nrow(model1))

#Set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(model1)), size = smp_size)

#Split the data into training and testing
train <- model1[train_ind, ]
test <- model1[-train_ind, ]


predict_y=train$Norm_Consumption
classify_y=train$Base_hour_Flag

#Normalization Function
normalize<-function(x)(return((x-min(x))/(max(x)-min(x))))


#Conversion
train$Base_hour_Flag <- as.numeric(train$Base_hour_Flag)
train$Weekday <- as.numeric(train$Weekday)
train$Holiday <- as.numeric(train$Holiday)


#Prediction
neuralnet <- neuralnet(predict_y ~ train$Base_hour_Flag +
                          train$Weekday+train$Holiday
                        + train$TemperatureF + as.numeric(train$'model1$month5') +
                          + train$Dew_PointF + as.numeric(train$'model1$month6') + 
                          train$'model1$month7' + train$'model1$month8' + 
                          train$'model1$month9' + train$'model1$month10' + 
                          train$'model1$month11' + train$'model1$month12', 
                        data=train, hidden=0)

#Classification
neuralnet <- neuralnet(classify_y ~ train$Weekday+train$Holiday +
                         train$TemperatureF + as.numeric(train$'model1$month5') +
                         train$Dew_PointF + as.numeric(train$'model1$month6') + 
                         train$'model1$month7' + train$'model1$month8' + 
                         train$'model1$month9' + train$'model1$month10' + 
                         train$'model1$month11' + train$'model1$month12', 
                       data=train, hidden=c(7))



#Accuracy
res <- neuralnet$result.matrix

#Plot
plot(neuralnet)

#Predict
p <- predict(neuralnet,test)

table(test,p)

#accuracy (for classification)
mean(test$Base_hour_Flag==p)


#Prediction parameters
install.packages("hydroGOF")
library("hydroGOF")
RMSE <- sqrt(mean((p-test$Norm_Consumption)^2))
MAE <- mean(abs(p-test$Norm_Consumption))
MAPE <- mean(abs((p - test$Norm_Consumption)/p))

performance_eva  = data.frame()
performance_eva$RMSE <- RMSE
performance_eva$MAE <- MAE
performance_eva$MAPE <- MAPE
performance_eva$model <- ''
performance_eva$BuildingID_meter <- train$BuildingID_MeterID



