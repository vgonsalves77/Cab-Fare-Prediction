   rm(list = ls())
#################################################################Load Cab_Train and required packages and libraries########################################################################
#-->Set working directory
    setwd("C:/Users/vgonsalv/Desktop/Cab_TrainScience/Edwisor/project/project 2--Cab Fare Prediction/Project Cab Fare Prediction--Vijay Gonsalves")
#-->Confirm whether working directory is set or not
    getwd()
#-install required packages
    install.packages(c("gplots", "psych","ggplot2","gridExtra","corrgram","usdm","DMwR","randomForest","dplyr"))
    install.packages("lubridate")
    install.packages("caret")
    library("usdm")
    library("class")
    library("ggplot2")
    library("gridExtra")
    library("scales")
    library("psych")
    library("gplots")
    library("corrgram")
    library("rpart")
    library("DMwR")
    library("randomForest")
    library("dplyr")
    library("lubridate")
    library("caret")
#-->Load train_cab.csv file in Cab_Train object and test.csv into Cab_Test object.MArk 1st row as header and also mark Null and 0 value columns as NA
    Cab_Train = read.csv("train_cab.csv",header = T,na.strings = c("", " ", "NA","0"))
   
#################################################################Exploratory Cab_Train analysis########################################################################

#-->Seperating pickup_datetime variable into seperate variables after confirming that it does not have any NA values.
    #We get warning as 1 failed to parse as it has 43 in datetime which results in NA in month etc.We will delete that row first
    sum(is.na(Cab_Train$pickup_datetime))
    Cab_Train=Cab_Train[-which(Cab_Train$pickup_datetime==43),]
    Cab_Train = Cab_Train %>% 
      mutate(
        pickup_datetime = ymd_hms(pickup_datetime),
        month = month(pickup_datetime),
        year = year(pickup_datetime),
        day = day(pickup_datetime),
        dayOfWeek = wday(pickup_datetime),
        hour = hour(pickup_datetime),
        timeofday = as.factor(ifelse(hour >= 3 & hour < 9,
                                     "Morning", ifelse(hour >= 9 & hour < 14, "Mid-Day",
                                                       ifelse(hour >= 14 & hour < 18, "Evening", "Night"))))
      )
  
#-->Deleting pickup_datetime variable as it is of now use now
    Cab_Train$pickup_datetime = NULL   
#-->checking Cab_Train types and converting fare_amount to numeric and cols derived from pickup_datetime to factor
    str(Cab_Train)
    Cab_Train$fare_amount = as.numeric(as.character(Cab_Train$fare_amount))
    is.numeric(Cab_Train$fare_amount)
    Cab_Train$month=as.factor(Cab_Train$month)
    Cab_Train$year=as.factor(Cab_Train$year)
    Cab_Train$day=as.factor(Cab_Train$day)
    Cab_Train$dayOfWeek=as.factor(Cab_Train$dayOfWeek)
    Cab_Train$hour=as.factor(Cab_Train$hour)
#-->we can see that fare_amount has -ve values which is not logically possible and hence making them as NA    
    Cab_Train$fare_amount[which(Cab_Train$fare_amount <= 0)]="NA"
    Cab_Train$fare_amount = as.numeric(as.character(Cab_Train$fare_amount))
    is.numeric(Cab_Train$fare_amount)
#-->Pickup and dropoff location cant be the same so deleting such rows
    Cab_Train=Cab_Train[-which((Cab_Train$pickup_longitude==Cab_Train$dropoff_longitude) & (Cab_Train$pickup_latitude==Cab_Train$dropoff_latitude)),]
#-->removing rows with passenger_count<1 and>6 as logically not possible
    Cab_Train=Cab_Train[-which(Cab_Train$passenger_count > 6),]
    Cab_Train=Cab_Train[-which(Cab_Train$passenger_count < 1),]
#-->We have one value with passenger_count as 1.3 which is obviously wrong and hence we round it to nearest value
    table(Cab_Train$passenger_count)
    Cab_Train[, 6] = round(Cab_Train[,6])
    str(Cab_Train)
#-->Check if Latitudes range from -90 to 90 and     Longitudes range from -180 to 180.found oK except for pickup_latitude but it will be handled in outlier analysis
    summary(Cab_Train$pickup_latitude)
    summary(Cab_Train$dropoff_latitude)
    summary(Cab_Train$pickup_longitude)
    summary(Cab_Train$dropoff_longitude)
#################################################################Outlier Analysis########################################################################
    
#-->Create a numeric index having numeric cols from Cab_train
    numeric_index = sapply(Cab_Train, is.numeric)
    factor_index= sapply(Cab_Train, is.factor)
#-->create a Cab_Trainframe having only numeric Cab_Train and extract the column names having only numeric Cab_Train
    numeric_Cab_Train = Cab_Train[, numeric_index]
    factor_Cab_Train = Cab_Train[, factor_index]
    num_cnames = colnames(numeric_Cab_Train)
    cnames=colnames(numeric_Cab_Train)
    factor_cnames = colnames(factor_Cab_Train)
#-->Plot boxplot for all numeric values and assign names to boxplot as gn_column_name
   for (i in 1:length(num_cnames))
   {
     assign(
       paste0("gn", i),
       ggplot(aes_string(y = (num_cnames[i]), x = "fare_amount"), data =  subset(Cab_Train)) +
         stat_boxplot(geom = "errorbar", width = 0.5) +
         geom_boxplot(
           outlier.colour = "red",
           fill = "grey" ,
           outlier.shape = 18,
           outlier.size = 1,
           notch = FALSE
         ) +
         theme(legend.position = "bottom") +
         labs(y = num_cnames[i], x = "fare_amount") +
         ggtitle(paste("Box plot of fare_amount for", num_cnames[i]))
     )
   }
#-->See the plotted boxplot and for conclusions
   gridExtra::grid.arrange(gn1, ncol =1)
   gridExtra::grid.arrange( gn2, ncol =1)
   gridExtra::grid.arrange( gn3,  ncol =1)
   gridExtra::grid.arrange( gn4, ncol =1)
   gridExtra::grid.arrange( gn5,  ncol =1)
   gridExtra::grid.arrange( gn6,  ncol =1)
  
#-->As can be seen we have some outliers in all columns but the count is too high to remove.
   #So instead we will make it as NA.We dont do outlier analysis for passenger count as it will lead to loss of 4,5,6 values which are valid values
   for (i in cnames[-6]) {
     print(i)
     val = Cab_Train[, i][Cab_Train[, i] %in% boxplot.stats(Cab_Train[, i])$out]
     print(length(val))
     Cab_Train[,i][Cab_Train[,i]%in%val] = NA
   }
   
   Cab_Train$passenger_count=as.numeric(Cab_Train$passenger_count)
   numeric_Cab_Train = Cab_Train[, numeric_index]
   factor_Cab_Train = Cab_Train[, factor_index]
   num_cnames = colnames(numeric_Cab_Train)
   cnames=colnames(numeric_Cab_Train)
   factor_cnames = colnames(factor_Cab_Train)
#################################################################Missing value Analysis########################################################################
#-->From Below we see that missing values are less than 30% and hence we need to impute instead of deleting
      missing_val = data.frame(apply(Cab_Train, 2, function(x) {
     sum(is.na(x))
   }))
   
   missing_val$columns=row.names(missing_val)
   row.names(missing_val)=NULL
   names(missing_val)[1]="Missing_Percentage"
   missing_val$Missing_Percentage=(missing_val$Missing_Percentage/nrow(Cab_Train))*100

#-->Finding the best of mean,median  for numeric
     #Choosing a sample and saving its value
   sample_NA_Numeric = numeric_Cab_Train[50, ]
   #Putting values of sample equal to NA for required columns
   numeric_Cab_Train[50,] = NA
   # duplicating Cab_Train
   numeric_data_duplicate = numeric_Cab_Train
   #MEAN Method
   for(i in num_cnames)
     numeric_Cab_Train[, i][is.na(numeric_Cab_Train[, i])] = mean(numeric_Cab_Train[, i], na.rm = TRUE)
   
   sample_NA_mean = numeric_Cab_Train[50,]
   #MEDIAN Method
   numeric_Cab_Train = numeric_data_duplicate
   for(i in num_cnames)
     numeric_Cab_Train[, i][is.na(numeric_Cab_Train[, i])] = median(numeric_Cab_Train[, i], na.rm = TRUE)
   
   sample_NA_median = numeric_Cab_Train[50,]
   #Comparing different imputing methods
   sample_numeric = rbind(sample_NA_Numeric, sample_NA_mean, sample_NA_median)
   #Inserting a new blank row in "sample"
   sample_numeric[nrow(sample_numeric)+1, ]=NA
   #Changing row names
   row.names(sample_numeric) = c("sample_NA","sample_NA_mean","sample_NA_median","Best Method")
   #Finding the best method of imputation for each column
   for (d in (1:ncol(sample_numeric)))
   {
     if(abs(as.numeric(sample_numeric[1,d])-as.numeric(sample_numeric[2,d]))<abs(as.numeric(sample_numeric[1,d])-as.numeric(sample_numeric[3,d])))
     {
       sample_numeric[4,d] = "MEAN"
     } else {
       sample_numeric[4,d] = "MEDIAN"
     }
   }
   Cab_Train$pickup_longitude[is.na(Cab_Train$pickup_longitude)] = median(Cab_Train$pickup_longitude, na.rm = TRUE)
   Cab_Train$pickup_latitude[is.na(Cab_Train$pickup_latitude)] = mean(Cab_Train$pickup_latitude, na.rm = TRUE)
   Cab_Train$dropoff_longitude[is.na(Cab_Train$dropoff_longitude)] = median(Cab_Train$dropoff_longitude, na.rm = TRUE)
   Cab_Train$dropoff_latitude[is.na(Cab_Train$dropoff_latitude)] = median(Cab_Train$dropoff_latitude, na.rm = TRUE)
   Cab_Train$fare_amount[is.na(Cab_Train$fare_amount)] = mean(Cab_Train$fare_amount, na.rm = TRUE)
   Cab_Train$passenger_count[is.na(Cab_Train$passenger_count)] = median(Cab_Train$passenger_count, na.rm = TRUE)
   Cab_Train[, 6] = round(Cab_Train[,6])
   Cab_Train$passenger_count=as.factor(Cab_Train$passenger_count)
   
   rm(list = c("gn1","gn2","gn3","gn4","gn5","gn6","numeric_data_duplicate","sample_NA_mean","sample_NA_median","um_cnames","val","i","d"))
   rm(list =   c("sample_NA_Numeric","sample_numeric","sample"))

   numeric_index = sapply(Cab_Train, is.numeric)
   factor_index= sapply(Cab_Train, is.factor)
   numeric_Cab_Train = Cab_Train[, numeric_index]
   factor_Cab_Train = Cab_Train[, factor_index]
   num_cnames = colnames(numeric_Cab_Train)
   cnames=colnames(numeric_Cab_Train)
   factor_cnames = colnames(factor_Cab_Train)
   
   missing_val = data.frame(apply(Cab_Train, 2, function(x) {
     sum(is.na(x))
   }))
   

   
  
   
#################################################################Feature Selection########################################################################
#-->For numerical values.Although pickup_longitude and pickup_latitude do so some corelation its not strong enough 
   #and we need both cols and hence we dont delete any numeric variables.
      corrgram(numeric_Cab_Train, order = F,
            upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")
   cor(numeric_Cab_Train)
#-->For Categorical values we find that p value is >0.05 for day and dayofweek and hence we remove those variables
   aov_1_way=aov(fare_amount~passenger_count,data=Cab_Train)
   summary(aov_1_way)
   aov_1_way=aov(fare_amount~month,data=Cab_Train)
   summary(aov_1_way)
   aov_1_way=aov(fare_amount~year,data=Cab_Train)
   summary(aov_1_way)
   aov_1_way=aov(fare_amount~dayOfWeek,data=Cab_Train)
   summary(aov_1_way)
   aov_1_way=aov(fare_amount~day,data=Cab_Train)
   summary(aov_1_way)
   aov_1_way=aov(fare_amount~hour,data=Cab_Train)
   summary(aov_1_way)
   aov_1_way=aov(fare_amount~timeofday,data=Cab_Train)
   summary(aov_1_way)
   Cab_Train = subset(Cab_Train, select = -c(dayOfWeek, day))
   
#################################################################Feature Scaling########################################################################
#-->Both numerical variables are in same range and hence no need of feature scaling
   #################################################################Multicollinearity check########################################################################
   
   vif(Cab_Train[,-c(1,6:10)])
   vifcor(Cab_Train[,-c(1,6:10)], th = 0.9)
   #-->No Multicollinearity problem
   
#################################################################Model Creation and Validating########################################################################
   table(Cab_Train$passenger_count) 
   table(Cab_Train$month) 
   table(Cab_Train$year) 
   table(Cab_Train$hour) 
   table(Cab_Train$timeofday) 
#-->As can be seen above passenger_count is highly biased towards 1 hence we use stratified sampling
   #taking passenger count as reference variable so that all apssenger_counts are represented properly

   train_index = createDataPartition(Cab_Train$passenger_count, p = 0.8, list = FALSE)
   train_set = Cab_Train[train_index,]
   validation_set = Cab_Train[-train_index,]


#-->using decision tree algo
fit = rpart(c(fare_amount) ~ ., data =  train_set, method = "anova")
predictions_DT = predict(fit, validation_set[, -c(1)])
MAPE = function(y, yhat) {
  mean(abs((y - yhat) / y)*100)
}
MAPE(validation_set[, c(1)], predictions_DT)
#-->only fare_amount-->37.53% error

#---------------------------------------------------------------------------------------------------------------------------------------------
#-->using linear  regression

lm_model=lm(fare_amount~.,data = train_set)
summary(lm_model)
predictions_LR=predict(lm_model,validation_set[, -c(1)])
MAPE(validation_set[, c(1)], predictions_LR)

#-->only fare_amount-->39.23% error
#---------------------------------------------------------------------------------------------------------------------------------------------
#-->using KNN regression forest

predictions_KNN = knnregTrain(train_set[, -c(1,7:10)], validation_set[, -c(1,7:10)], train_set$fare_amount, k = 9)
MAPE(validation_set[, c(1)], predictions_KNN)
#-->only fare_amount-->22.74% error
#---------------------------------------------------------------------------------------------------------------------------------------------
#-->using random forest
RF_Model <- randomForest(fare_amount~.,data = train_set, importance = TRUE)
varImpPlot(RF_Model)
imp <- importance(RF_Model)
RF_Predictions <- predict(RF_Model,validation_set[, -c(1)])
MAPE(validation_set[, c(1)], RF_Predictions)
plot(RF_Predictions)
MAPE(validation_set[, c(1)], RF_Predictions)
MAPE(validation_set[, c(1)], predictions_LR)
MAPE(validation_set[, c(1)], predictions_DT)
#-->only fare_amount-->26.95 % 
#---------------------------------------------------------------------------------------------------------------------------------------------
#As can be seen above best accuracy is for KNN but it only uses continous variables and  hence we take next best i.e. Random Forest as our final model
#---------------------------------------------------------------------------------------------------------------------------------------------
results_R <- data.frame( pred_fare_amount = RF_Predictions,validation_set)
write.csv(results_R, file = 'Random_Forest_output_R_on_validation_data.csv', row.names = FALSE, quote=FALSE)
#Write results of Ranfom forest in csv
qplot(x = results_R[,2], y = results_R[,1], data = results_R, color = I("red"), geom = "point", xlab = "Test Data", ylab = "Predictions")

#################################################################Model Running on TEST########################################################################

CAB_TEST = read.csv("test.csv",header = T,na.strings = c("", " ", "NA","0"))
sum(is.na(CAB_TEST$pickup_datetime))

CAB_TEST = CAB_TEST %>% 
  mutate(
    pickup_datetime = ymd_hms(pickup_datetime),
    month = month(pickup_datetime),
    year = year(pickup_datetime),
    day = day(pickup_datetime),
    dayOfWeek = wday(pickup_datetime),
    hour = hour(pickup_datetime),
    timeofday = as.factor(ifelse(hour >= 3 & hour < 9,
                                 "Morning", ifelse(hour >= 9 & hour < 14, "Mid-Day",
                                                   ifelse(hour >= 14 & hour < 18, "Evening", "Night"))))
  )
CAB_TEST$pickup_datetime = NULL   
str(CAB_TEST)

CAB_TEST$month=as.factor(CAB_TEST$month)
CAB_TEST$year=as.factor(CAB_TEST$year)
CAB_TEST$day=as.factor(CAB_TEST$day)
CAB_TEST$dayOfWeek=as.factor(CAB_TEST$dayOfWeek)
CAB_TEST$hour=as.factor(CAB_TEST$hour)
CAB_TEST$passenger_count=as.factor(CAB_TEST$passenger_count)
CAB_TEST = subset(CAB_TEST, select = -c(dayOfWeek, day))

CAB_TEST$Test_Predictions = predict(RF_Model, CAB_TEST)

write.csv(CAB_TEST, file = 'pred_output_on_test_data(R).csv', row.names = FALSE, quote=FALSE)












