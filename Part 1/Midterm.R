install.packages("dplyr")
install.packages("stringr")
install.packages("reshape")
install.packages("lubridate")
install.packages("weatherData")
install.packages("sqldf")
install.packages("tcltk")
install.packages("dummy")
install.packages("zoo")
install.packages("tidyr")
install.packages("jsonlite")
install.packages("curl")
install.packages("httr")
install.packages("rvest")
install.packages("ggmap")
install.packages("xml2")
install.packages("XML")
install.packages("plyr")

library("plyr")
library("xml2")
library("XML")
library("ggmap")
library("httr")
library("rvest")
library("curl")
library("jsonlite")
library("dplyr")
library("reshape")
library("stringr")
library("lubridate")
library("weatherData")
library("tidyr")
library("tcltk")
library("dummy")
library("zoo")
library("sqldf")

#read input csv
df <- read.csv("Finland_masked.csv")

#Filtering the unwanted values
df <- df %>% filter (type == 'elect' | type == 'Dist_Heating')


#converting factor to Character
df$vac <- as.character(df$vac)

# Handling missing Building names as per class instructions
df <- transform(df, vac = ifelse(BuildingID == 81909, 'Building 27', vac))
df <- transform(df, vac = ifelse(BuildingID == 82254 | BuildingID == 83427 | BuildingID == 84681, 'Building 9', vac))

df$BuildingID_MeterID <- paste(df$BuildingID,df$meternumb, sep = "_")

View(df)

#write.csv(df,file = "df.csv")


#Converting Factor to character
df$type <- as.character(df$type)

#str(df)

#Finding the sum per hour per building_ID_meter_ID by aggregation
aggregatedData <- df %>% group_by(BuildingID_MeterID,date,hour,type) %>% mutate(Consumption = sum(Consumption)) %>% ungroup()

View(aggregatedData)

#converting to date
aggregatedData$date <- as.Date(as.character(aggregatedData$date), "%Y%m%d")


#Splitting the Date field and converting it into Data frame
a <- strsplit(as.character(aggregatedData$date), "-")
mat <- matrix(unlist(a), ncol=3, byrow=TRUE)
dfdate   <- as.data.frame(mat)

#View(dfdate)

#Setting the values of month,day, year and Day of the week
aggregatedData$year = dfdate$V1
aggregatedData$month = dfdate$V2
aggregatedData$day = dfdate$V3
aggregatedData$"Day of Week" = wday(as.Date(aggregatedData$date,'%m/%d/%Y')) - 1
aggregatedData$Weekday <- ifelse(aggregatedData$"Day of Week" == 0,0,ifelse(aggregatedData$"Day of Week" == 6,0,1))

#Setting Base_Hour_Flag
aggregatedData$Base_hour_Flag <- ifelse(aggregatedData$hour < 5,'True',ifelse(aggregatedData$hour> 21,'True','False'))

#View(aggregatedData)

#write.csv(aggregatedData,file = "aggregatedData.csv")

#Read Address csv
df1 <- read.csv("Finland_addresses_area.csv")


#Inner Join for Merging aggregated data with address
processedOp <- merge(aggregatedData, df1, by.x = "vac", by.y = "building")

#View(processedOp)
#write.csv(processedOp,file = "sample_processedOp.csv")

#Getting Weather data for given Addresses
url_api <- "http://api.wunderground.com/auto/wui/geo/GeoLookupXML/index.xml?query"
code_list = c()
i=1

for (e in df1$X..address){
# Finding Latitude & longitude for the address
  lat_lon <- geocode(e, source = "google",sensor = TRUE)
  cordinates <- paste(lat_lon$lat,lat_lon$lon,sep=",")
#Passing the cordinates to the url
  url_loc <- paste(url_api,cordinates,sep="=")
#Reading the XML
  data <- read_xml(url_loc)
#Finding the nearest airport
  data_airport <- xml_find_all(data,".//airport")
  data_air_codes <- xml_find_all(data_airport,".//icao")
  codes <- xml_contents(data_air_codes)
  code <- toString(codes[1])
  code_list[i] <- code
  i = i+1
}

#getting the country name to build the api parameters
country_of_building <- (geocode(e, source = "google",sensor = TRUE,output = "more"))$country


df1$airport_code = code_list

#View(df1)

#Preparing Sample df for Weather to get the Template
df3 <- getWeatherForDate("KBOS", start_date="2013-01-01",
                         end_date = "2013-01-01",
                         opt_detailed = TRUE,
                         opt_all_columns = TRUE)
df3$airport_code = "KBOS"

#View(df3)


weatherData <- select(df3,Time,TemperatureF,Dew_PointF,Humidity,Sea_Level_PressureIn,VisibilityMPH,
                      Wind_Direction,Wind_SpeedMPH,Gust_SpeedMPH,PrecipitationIn,
                      Events,Conditions,WindDirDegrees,airport_code)

#View(weatherData)

distinctDate <- distinct(aggregatedData)


#Get latest and earliest date
a = distinctDate[order(as.Date(distinctDate$date, format="%Y%m%d")),] %>% select(date)
startDate = format(as.Date(head(a$date,1), format="%Y%m%d"),"%Y-%m-%d")

endDate = format(as.Date(tail(a$date,1), format="%Y%m%d"),"%Y-%m-%d")
endDate <- as.Date(endDate)
startDate <- as.Date(startDate)


for (c_air in unique(code_list)){    
  d3 <- getWeatherForDate(c_air, start_date=startDate,
                          end_date = endDate,
                          opt_detailed = TRUE,
                          opt_all_columns = TRUE)
  d3$airport_code = c_air
  d3 <- select(d3,Time,TemperatureF,Dew_PointF,Humidity,Sea_Level_PressureIn,VisibilityMPH,
               Wind_Direction,Wind_SpeedMPH,Gust_SpeedMPH,PrecipitationIn,
               Events,Conditions,WindDirDegrees,airport_code)
  weatherData <- rbind(weatherData,d3)
}



#View(weatherData)
weatherData <- weatherData %>% filter(weatherData$airport_code != "KBOS")

#merge weatherdata with address
building_wise_weather_data <- merge(df1, weatherData, by=c("airport_code"),all.x = TRUE)

#View(building_wise_weather_data)

#get Date
building_wise_weather_data$date <- format(as.Date(substr(building_wise_weather_data$Time,1,10),format = "%Y-%m-%d"),"%Y%m%d")

#Get hour
building_wise_weather_data$hour <- as.numeric(substr(building_wise_weather_data$Time,12,13))

# group the merged data based on building,date & hour
grouped_building_weather = sqldf("SELECT * FROM building_wise_weather_data GROUP BY building,date,hour")


#converting to date
grouped_building_weather$date <- as.Date(as.character(grouped_building_weather$date), "%Y%m%d")


#View(grouped_building_weather)
View(processedOp)

#Combine Consumption & weather data with address respectively
processedOp <- merge(grouped_building_weather, processedOp, by.x = c("building","date","hour"), by.y = c("vac","date","hour"),all.y = TRUE)


#ab <- merge(grouped_building_weather, processedOp, by.x = c("building","date","hour"), by.y = c("vac","date","hour"),all.y = TRUE)



#drop Recurring Column
processedOp$X..address.x <- NULL
processedOp$airport_code <- NULL
processedOp$area_floor._m.sqr.x <- NULL


#Convert Category Calm into Numeric Value
processedOp$Wind_SpeedMPH[processedOp$Wind_SpeedMPH=="Calm"] <- 0 

#Convert character column to numeric
processedOp$Wind_SpeedMPH <- as.double(processedOp$Wind_SpeedMPH)
#unique(processedOp$Wind_SpeedMPH)

#Remove the empty & N/A
processedOp$Conditions[processedOp$Conditions==""] <- NA
processedOp$Humidity[processedOp$Humidity=="N/A"] <- NA

#Convert into integer
processedOp$Humidity <- as.integer(processedOp$Humidity)


#Remove the outliers using Boxplot
processedOp$TemperatureF[processedOp$TemperatureF %in% boxplot.stats(processedOp$TemperatureF)$out] <- NA
processedOp$Dew_PointF[processedOp$Dew_PointF %in% boxplot.stats(processedOp$Dew_PointF)$out] <- NA
processedOp$Humidity[processedOp$Humidity %in% boxplot.stats(processedOp$Humidity)$out] <- NA
processedOp$Sea_Level_PressureIn[processedOp$Sea_Level_PressureIn %in% boxplot.stats(processedOp$Sea_Level_PressureIn)$out] <- NA
processedOp$VisibilityMPH[processedOp$VisibilityMPH %in% boxplot.stats(processedOp$VisibilityMPH)$out] <- NA
processedOp$Wind_SpeedMPH[processedOp$Wind_SpeedMPH %in% boxplot.stats(processedOp$Wind_SpeedMPH)$out] <- NA


#Replacing NA values with linear interpolation
processedOp$TemperatureF <- na.approx(processedOp$TemperatureF,na.rm = FALSE) 
processedOp$Dew_PointF <- na.approx(processedOp$Dew_PointF,na.rm = FALSE) 
processedOp$Humidity <- na.approx(processedOp$Humidity,na.rm = FALSE) 
processedOp$Sea_Level_PressureIn <- na.approx(processedOp$Sea_Level_PressureIn,na.rm = FALSE)
processedOp$VisibilityMPH <- na.approx(processedOp$VisibilityMPH,na.rm = FALSE) 
processedOp$Wind_SpeedMPH <- na.approx(processedOp$Wind_SpeedMPH,na.rm = FALSE)


#Replacing NA values randomly
processedOp$WindDirDegrees[is.na(processedOp$WindDirDegrees)]<- sample(1:36,1)*10

#Replacing NA with last known non null value
processedOp$Conditions <-  na.locf(processedOp$Conditions,fromLast = TRUE,na.rm = FALSE)


#Calculating Normalized power consumption Value per floor area per metre square
processedOp$Norm_Consumption <- processedOp$Consumption / processedOp$area_floor._m.sqr.y



#Retrieving List of Holidays
url_time_api <- "http://www.timeanddate.com/calendar/custom.html"
time_api <- read_html(url_time_api)

country_form <- xml_find_all(time_api,".//form")
country <- xml_find_all(country_form,".//option")

#index_of_country

ct_id =1
index_of_country =0
for(cntry in xml_text(country)){
  if(cntry==country_of_building){
    index_of_country  = ct_id
    break
  }
  ct_id = ct_id+1
}
if(index_of_country!=0){
  option_index <- toString(country[index_of_country])
  country_code_for_Holiday = substr(option_index,16,17)  
}

yr = unique(processedOp$year)
params <- paste(paste("year",yr,sep="="),paste("country",country_code_for_Holiday,sep="=")
                ,paste("holm","1",sep="="),paste("hol","9",sep="="),paste("df","1",sep="="),sep="&")

#Building url to get Holiday Dynamically
url_time_api_for_country <- paste(url_time_api,params,sep = "?")

#getting List of Holidays
holiday_api <- GET(url_time_api_for_country,add_headers("Accept-Language"="en-US"))
holiday_html <- read_html(holiday_api$content)

holidays_table <- xml_find_all(holiday_html,".//table")
holidays_node <- xml_find_all(holidays_table,".//span")

#### for trim
#install.packages("base")
#library("base")
install.packages("gdata")
library("gdata")

holidays_string <- toString(xml_contents(holidays_node))
holidays_string_list <- trim(unlist(strsplit(holidays_string, ",")))
paste(holidays_string_list,yr,sep=",")
holi_c <- c()
h =1
for (dates_in_list in holidays_string_list){
  tryCatch(if(grep(pattern="[0-9]",x=dates_in_list)){
    holi_c[h] = dates_in_list 
    h=h+1
  },
  error = function(e){
    NaN;
  }
  )
}

holi_c <- paste(holi_c,yr,sep = " ")
holi_c <- format(as.Date(holi_c,format = "%d %b %Y"),"%Y-%m-%d")



#Setting a Column for Holiday by checking in List
processedOp$Holiday <- ifelse(as.character(processedOp$date) %in% as.character(holi_c),1,0)

#Calculating Base_hour_usage
base_hour_usage_aggregation = sqldf("SELECT BuildingID_MeterID,type,Weekday,month,Holiday,Avg(Norm_Consumption) as Base_hour_usage  FROM processedOp where hour in ('0','1','2','3','4','22','23') GROUP BY BuildingID_MeterID,type,Weekday,month,Holiday")

#View(base_hour_usage_aggregation)

#Merge with final df
processedOp <- merge(base_hour_usage_aggregation, processedOp, by=c("BuildingID_MeterID","type","Weekday","month","Holiday"),all.y = TRUE)

#Creating Base hour Class
processedOp$Base_Hour_Class <- ifelse(processedOp$Norm_Consumption > processedOp$Base_hour_usage,'High','Low')

#Drop TimeColumn
processedOp$Time <- NULL

#Rearranging Column
processedOp <- select(processedOp,building,X..address.y,BuildingID,meternumb,BuildingID_MeterID,type,date,year,month,day,`Day of Week`,hour,Base_hour_Flag,Weekday,Holiday,Consumption,area_floor._m.sqr.y,Norm_Consumption,Base_hour_usage,Base_Hour_Class,TemperatureF,Dew_PointF,Humidity,Sea_Level_PressureIn,VisibilityMPH,Wind_Direction,Wind_SpeedMPH,Gust_SpeedMPH,PrecipitationIn,Events,Conditions,WindDirDegrees)


View(processedOp)
write.csv(processedOp,file="final_sample_format_Part1.csv",row.names=FALSE)

