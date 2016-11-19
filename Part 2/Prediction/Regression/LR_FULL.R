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


ab$Weekday <- as.numeric(ab$Weekday)
ab$Holiday <- as.numeric(ab$Holiday)
ab$Base_hour_Flag <- as.numeric(ab$Base_hour_Flag)
#ab$month <- factor(ab$month)

smp_size <- floor(0.60 * nrow(ab))

#Set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(ab)), size = smp_size)

#Split the data into training and testing
train <- ab[train_ind, ]
test <- ab[-train_ind, ]

#View(ab)
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

train$predicted_value <- pred_lm
residual_values <- train$Norm_Consumption - pred_lm
train$residual_values <- residual_values
std_dev_residuals<- sd(residual_values)
#train$outlier <- "False" 
train$outlier <- ifelse(train$residual_values>=2*std_dev_residuals,"True","False")
train$residual_values <- NULL
final_file<- rbind(final_file,acc_df)
outlier_tag_file <- rbind(outlier_tag_file,train)

final_file$ME<-NULL
final_file$MPE<-NULL
final_file$algorithm <- "Linear Regression"
#View(outlier_tag_file)
write.csv(final_file,file = "Linear_regrssion_FULL.csv")
write.csv(outlier_tag_file,file = "Outlier_regression_FULL.csv")


