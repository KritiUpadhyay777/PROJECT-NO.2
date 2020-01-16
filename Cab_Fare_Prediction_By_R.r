
-----------------------------------------------------------------------------------------
############################ Cab Ride Fare Prediction in R  ######################################
-----------------------------------------------------------------------------------------
  
#Removing RAM 
  rm(list = ls())

#Setting working directory
  setwd("C://Users//akhil//Desktop//Kriti Data//EDWISOR_invoice//data science study material//Project//Project_2")

#Checking the working directory
  getwd()

#Loading Required Libraries
  x = c("ggplot2", "corrgram", "DMwR", "usdm", "caret", "randomForest", "e1071",
      "rpart","MASS","xgboost","stats")

#Loading Packages
  lapply(x, require, character.only = TRUE)
  rm(x)

# The details of data attributes in the dataset are as follows:
# pickup_datetime - timestamp value indicating when the cab ride started.
# pickup_longitude - float for longitude coordinate of where the cab ride started.
# pickup_latitude - float for latitude coordinate of where the cab ride started.
# dropoff_longitude - float for longitude coordinate of where the cab ride ended.
# dropoff_latitude - float for latitude coordinate of where the cab ride ended.
# passenger_count - an integer indicating the number of passengers in the cab ride.


#Loading datasets
  Cab_Train_Data = read.csv("train_cab.csv", header = T, na.strings = c(" ", "", "NA"))
  Cab_Test_Data = read.csv("test.csv")
  test_pickup_datetime = Cab_Test_Data["pickup_datetime"]
  

#Viewing Structure of both train and test data
  str( Cab_Train_Data)
  str( Cab_Test_Data)
  
#Viewing Summary of the both train and test data
  summary( Cab_Train_Data)
  summary( Cab_Test_Data)
  
#Viewing records of both train and test data
  head( Cab_Train_Data,5)
  head( Cab_Test_Data,5)
  
  
-----------------------------------------------------------------------------------------
############################ EXPLORATORY DATA ANALYSIS #################################
-----------------------------------------------------------------------------------------
        
#Changing the data types of variables
  Cab_Train_Data$fare_amount = as.numeric(as.character(Cab_Train_Data$fare_amount))
  Cab_Train_Data$passenger_count=round(Cab_Train_Data$passenger_count)

#Removing values which are not within desired range depending upon basic understanding of dataset.
  
  #1.Fare amount has a negative value, which doesn't make sense. A price amount cannot be negative and also cannot be 0. So we will remove these fields.
    #Rows with negative Fare amount
      Cab_Train_Data[which(Cab_Train_Data$fare_amount < 1 ),]
      
    #Count of rows having negative fare amount
      nrow(Cab_Train_Data[which(Cab_Train_Data$fare_amount < 1 ),])
    
    #Removing rows from data containing negative fare amount
      Cab_Train_Data= Cab_Train_Data[-which(Cab_Train_Data$fare_amount < 1 ),]

  #2.Passenger_count variable
    for (i in seq(4,11,by=1))
      {
      print(paste('passenger_count above ' ,i,nrow(Cab_Train_Data[which(Cab_Train_Data$passenger_count > i ),])))
    }
      
    #So 20 observations of passenger_count is consistenly above from 6,7,8,9,10,11 passenger_counts, checking them.
      Cab_Train_Data[which(Cab_Train_Data$passenger_count > 6 ),]
      
    #We need to see if there are any passenger_count==0
      Cab_Train_Data[which(Cab_Train_Data$passenger_count <1 ),]
      
    #Getting number of observation having passenger_count==0
      nrow(Cab_Train_Data[which(Cab_Train_Data$passenger_count <1 ),])

    #Removing 58 observations having passenger_count==0 and 20 observation which are above 6 because a cab cannot hold these number of passengers.
      Cab_Train_Data = Cab_Train_Data[-which(Cab_Train_Data$passenger_count < 1 ),]
      Cab_Train_Data = Cab_Train_Data[-which(Cab_Train_Data$passenger_count > 6),]


  #3.Latitudes range from -90 to 90.Longitudes range from -180 to 180.Removing which does not satisfy these ranges
    #Getting rows ofLatitudes range from -90 to 90.Longitudes range from -180 to 180.
      print(paste('pickup_longitude above 180=',nrow(Cab_Train_Data[which(Cab_Train_Data$pickup_longitude >180 ),])))
      print(paste('pickup_longitude above -180=',nrow(Cab_Train_Data[which(Cab_Train_Data$pickup_longitude < -180 ),])))
      print(paste('pickup_latitude above 90=',nrow(Cab_Train_Data[which(Cab_Train_Data$pickup_latitude > 90 ),])))
      print(paste('pickup_latitude above -90=',nrow(Cab_Train_Data[which(Cab_Train_Data$pickup_latitude < -90 ),])))
      print(paste('dropoff_longitude above 180=',nrow(Cab_Train_Data[which(Cab_Train_Data$dropoff_longitude > 180 ),])))
      print(paste('dropoff_longitude above -180=',nrow(Cab_Train_Data[which(Cab_Train_Data$dropoff_longitude < -180 ),])))
      print(paste('dropoff_latitude above -90=',nrow(Cab_Train_Data[which(Cab_Train_Data$dropoff_latitude < -90 ),])))
      print(paste('dropoff_latitude above 90=',nrow(Cab_Train_Data[which(Cab_Train_Data$dropoff_latitude > 90 ),])))
    
    #Removing pickup_latitude above 90
      Cab_Train_Data= Cab_Train_Data[-which(Cab_Train_Data$pickup_latitude > 90),]
    
    #Getting rows having longitude and latitude values which are equal to 0 
      nrow(Cab_Train_Data[which(Cab_Train_Data$pickup_longitude == 0 ),])
      nrow(Cab_Train_Data[which(Cab_Train_Data$pickup_latitude == 0 ),])
      nrow(Cab_Train_Data[which(Cab_Train_Data$dropoff_longitude == 0 ),])
      nrow(Cab_Train_Data[which(Cab_Train_Data$pickup_latitude == 0 ),])
      
    #Removing longitude and latitude values which are equal to 0 
      Cab_Train_Data = Cab_Train_Data[-which(Cab_Train_Data$pickup_longitude == 0),]
      Cab_Train_Data = Cab_Train_Data[-which(Cab_Train_Data$dropoff_longitude == 0),]

-------------------------------------------------------------------------------
########################## MISSING VALUE ANALYSIS #############################
-------------------------------------------------------------------------------
    
#Missing value detection in each variable of the dataset by using apply function that uses Sum(is.na()) function as an argument and will return total count of the missing values present in each variable of the dataset. 
  missing_val = data.frame(apply(Cab_Train_Data,2,function(x){sum(is.na(x))}))
  missing_val

#Converting rownames into column and renaming variable name  
  missing_val$Columns = row.names(missing_val)
  row.names(missing_val) = NULL
  names(missing_val)[1] =  "Missing_percentage"
  
#Calculating Missing_percentage
  missing_val$Missing_percentage = (missing_val$Missing_percentage/nrow(Cab_Train_Data)) * 100

#Sorting variable
  missing_val = missing_val[order(-missing_val$Missing_percentage),]

#Rearranging columns
  missing_val = missing_val[,c(2,1)]
  missing_val

#Getting unqiue passenger count for train and test data
  unique(Cab_Train_Data$passenger_count)
  unique(Cab_Test_Data$passenger_count)
  
#Converting datatype of passenger_count variable as factor for both test and train data
  Cab_Train_Data[,'passenger_count'] = factor(Cab_Train_Data[,'passenger_count'], labels=(1:6))
  Cab_Test_Data[,'passenger_count'] = factor(Cab_Test_Data[,'passenger_count'], labels=(1:6))

#Applying different methods for computing missing values
  #Mode Method
    #Cab_Train_Data$passenger_count[1000]
    #Cab_Train_Data$passenger_count[1000] = NA
    #getmode = function(v)
    #{
    #uniqv = unique(v)
    #uniqv[which.max(tabulate(match(v, uniqv)))]
    #}
    #getmode(Cab_Train_Data$passenger_count)
      #For Passenger_count variable:
      # Actual value = 1
      # Mode = 1
      # We can't use mode method because data is more biased towards passenger_count=1

   # Mean Method 
    #Cab_Train_Data$fare_amount[1000]
    #Cab_Train_Data$fare_amount[1000]= NA
    
    #mean(Cab_Train_Data$fare_amount, na.rm = T)
    
  # Median Method
    #Cab_Train_Data$fare_amount[1000]
    #Cab_Train_Data$fare_amount[1000]= NA
    
    #median(train$fare_amount, na.rm = T)
   
  # kNN Imputation
    #Cab_Train_Data$fare_amount[1000]
    #Cab_Train_Data$fare_amount[1000]=NA
    
    #Cab_Train_Data = knnImputation(Cab_Train_Data, k = 181)

      # For fare_amount variable:
      # Actual value = 18.1,
      # Mean = 15.117,
      # Median = 8.5,
      # KNN = 18.28

#Since the missing values computed by different methods but only KNN imputation method would be good choice as the value computed by it is more closer to the actual values.
  #Computing missing values using KNN imputation method 
  Cab_Train_Data = knnImputation(Cab_Train_Data, k = 181)
  
  #Check for missing values
  sum(is.na(Cab_Train_Data))
  
  #Getting structure and summary of the train data
  str(Cab_Train_Data)
  summary(Cab_Train_Data)


-----------------------------------------------------------------------------
######################## OUTLIER ANALYSIS ###################################
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------  
#################### USING OUTLIERS REPLACE BY NA  METHOD ##################
-----------------------------------------------------------------------------
    
#Selecting only numeric variables
  Numeric_index= sapply(Cab_Train_Data,is.numeric)
  Numeric_data=Cab_Train_Data[,Numeric_index]
  
#Variables names containing numeric data
  Cnames=colnames(Numeric_data)
  Cnames
  
#Performing Outlier Analysis only on Fare_amount because other variables will be required to perform feature engineering.
  # Boxplot for fare_amount
    pl1 = ggplot(Cab_Train_Data,aes(x = factor(passenger_count),y = fare_amount))
    pl1 + geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,outlier.size=1, notch=FALSE)+ylim(0,100)
  
  # Replace all outliers with NA and impute
  vals = Cab_Train_Data[,"fare_amount"] %in% boxplot.stats(Cab_Train_Data[,"fare_amount"])$out
  Cab_Train_Data[which(vals),"fare_amount"] = NA
  
  #lets check the NA's
  sum(is.na(Cab_Train_Data$fare_amount))
  
  #Imputing with KNN
  Cab_Train_Data = knnImputation(Cab_Train_Data,k=3)
  
  #Checking the missing values
  sum(is.na(Cab_Train_Data$fare_amount))
  str(Cab_Train_Data)
  

--------------------------------------------------------------------------------
######################## FEATURE ENGINEERING ###################################
--------------------------------------------------------------------------------
    
#1.Feature Engineering for timestamp variable
  #we will derive new features from pickup_datetime variable and new features will be year,month,day_of_week,hour
  #Convert pickup_datetime from factor to date time
    Cab_Train_Data$pickup_date = as.Date(as.character(Cab_Train_Data$pickup_datetime))
  #Monday = 1
    Cab_Train_Data$pickup_weekday = as.factor(format(Cab_Train_Data$pickup_date,"%u"))
    Cab_Train_Data$pickup_month = as.factor(format(Cab_Train_Data$pickup_date,"%m"))
    Cab_Train_Data$pickup_year = as.factor(format(Cab_Train_Data$pickup_date,"%Y"))
    pickup_time = strptime(Cab_Train_Data$pickup_datetime,"%Y-%m-%d %H:%M:%S")
    Cab_Train_Data$pickup_hour = as.factor(format(pickup_time,"%H"))
  
  #Adding similar features to test dataset
    Cab_Test_Data$pickup_date = as.Date(as.character(Cab_Test_Data$pickup_datetime))
  # Monday = 1
    Cab_Test_Data$pickup_weekday = as.factor(format(Cab_Test_Data$pickup_date,"%u"))
    Cab_Test_Data$pickup_month = as.factor(format( Cab_Test_Data$pickup_date,"%m"))
    Cab_Test_Data$pickup_year = as.factor(format( Cab_Test_Data$pickup_date,"%Y"))
    pickup_time = strptime( Cab_Test_Data$pickup_datetime,"%Y-%m-%d %H:%M:%S")
    Cab_Test_Data$pickup_hour = as.factor(format(pickup_time,"%H"))
    
  #Check for NA values
    missing_val = data.frame(apply(Cab_Train_Data,2,function(x){sum(is.na(x))}))
    missing_val
    
  #One NA was present in variable pickup_datetime which created NA in above new feature engineered variables.
  #Removing that 1 row of NA's
    Cab_Train_Data= na.omit(Cab_Train_Data) 
  
  #Displaying Cab_Train_Data 
    Cab_Train_Data
    
  #Removing the variables which were used to engineer new variables from both train and test dataset
    Cab_Train_Data = subset(Cab_Train_Data,select = -c(pickup_datetime,pickup_date))
    Cab_Test_Data= subset( Cab_Test_Data,select = -c(pickup_datetime,pickup_date))
  
  
#2.Calculate the distance travelled using longitude and latitude
  #function to convert degree into radian 
    deg_to_rad = function(deg)
      {
        (deg * pi) / 180
      }
    
  #Using haversine formula to calculate distance for both train and test data
    haversine = function(long1,lat1,long2,lat2)
      {
        #long1rad = deg_to_rad(long1)
        phi1 = deg_to_rad(lat1)
        #long2rad = deg_to_rad(long2)
        phi2 = deg_to_rad(lat2)
        delphi = deg_to_rad(lat2 - lat1)
        dellamda = deg_to_rad(long2 - long1)
    
        a = sin(delphi/2) * sin(delphi/2) + cos(phi1) * cos(phi2) * 
        sin(dellamda/2) * sin(dellamda/2)
    
        c = 2 * atan2(sqrt(a),sqrt(1-a))
        R = 6371e3
        #1000 is used to convert to meters
        R * c / 1000 
      }
  
    Cab_Train_Data$distance = haversine( Cab_Train_Data$pickup_longitude, Cab_Train_Data$pickup_latitude, Cab_Train_Data$dropoff_longitude, Cab_Train_Data$dropoff_latitude)
    Cab_Test_Data$distance = haversine(Cab_Test_Data$pickup_longitude,Cab_Test_Data$pickup_latitude,Cab_Test_Data$dropoff_longitude,Cab_Test_Data$dropoff_latitude)
  
#Removing the variables which were used to engineer new variables from both train and test dataset
    Cab_Train_Data = subset( Cab_Train_Data,select = -c(pickup_longitude,pickup_latitude,dropoff_longitude,dropoff_latitude))
    Cab_Test_Data = subset(Cab_Test_Data,select = -c(pickup_longitude,pickup_latitude,dropoff_longitude,dropoff_latitude))
  
#Getting summary and structure of the train data
  str( Cab_Train_Data)
  summary( Cab_Train_Data)
  
-----------------------------------------------------------------------------------------
########################## FEATURE SELECTION ######################################
-----------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------
############################ CORRELATION PLOT ######################################
-----------------------------------------------------------------------------------------
    
#Selecting only numeric variables   
  Numeric_index= sapply(Cab_Train_Data,is.numeric)
  Numeric_data=Cab_Train_Data[,Numeric_index]
  
#Variables names containing numeric data
  Cnames=colnames(Numeric_data)
  Cnames
  
#Correlation plot 
  corrgram(Cab_Train_Data[,Cnames],order=F,upper.panel=panel.pie,text.panel=panel.txt,main="CORRELATION PLOT")

-----------------------------------------------------------------------------------------
############################ ANOVA TEST  ###############################################
-----------------------------------------------------------------------------------------
    
#ANOVA for categorical variables with target numeric variable
  Anova_results = aov(fare_amount ~ passenger_count + pickup_hour + pickup_weekday + pickup_month + pickup_year,data = Cab_Train_Data)
  
#Summary of anova result
  summary(Anova_results)
  
#pickup_weekday has p value greater than 0.05 ,so rejecting this variable
  Cab_Train_Data = subset(Cab_Train_Data,select=-pickup_weekday)
  
#Also remove that variable from from test dataset
  Cab_Test_Data = subset(Cab_Test_Data,select=-pickup_weekday)
  
--------------------------------------------------------------------------------------
############################ FEATURE SCALING ########################################
-------------------------------------------------------------------------------------
  
#Normality check
   qqnorm(Cab_Train_Data$fare_amount)
   histogram(Cab_Train_Data$fare_amount)
   qqnorm(Cab_Train_Data$distance)
   histogram(Cab_Train_Data$distance)
  
#Normalisation ( distance variable is not uniformly distributed)
  Cab_Train_Data[,'distance'] = (Cab_Train_Data[,'distance'] - min(Cab_Train_Data[,'distance']))/
    (max(Cab_Train_Data[,'distance'] - min(Cab_Train_Data[,'distance'])))
  
#check for multicollinearity
  
  #vif(Cab_Train_Data[,2:6])
  #vifcor(Cab_Train_Data[,-1], th = 0.9)
  
------------------------------------------------------------------------------------------- 
############## SPLITTING TRAIN DATA INTO TRAIN AND VALIDATION SUBSET(TEST) ################
-------------------------------------------------------------------------------------------- 
  set.seed(1000)
  
#Splitting 80% of Cab-Train_Data in train_data and 20% in Validation Dataset(test_data)
  train_index = createDataPartition(Cab_Train_Data$fare_amount,p=0.80,list = FALSE)
  train_data = Cab_Train_Data[train_index,]
  test_data = Cab_Train_Data[-train_index,]
  

------------------------------------------------------------------------------------------- 
################################ LINEAR REGRESSION #######################################
-------------------------------------------------------------------------------------------- 
  
#Running Regression model
  lm_model = lm(fare_amount ~.,data=train_data)
  
#Summary of the model
  summary(lm_model)
  str(train_data)
  
#plotting regression model
  plot(lm_model$fitted.values,rstandard(lm_model),main = "Residual plot",
       xlab = "Predicted values of fare_amount",
       ylab = "standardized residuals")
  
#Predicting test_data using predict() method  
  lm_predictions = predict(lm_model,test_data[,2:6])
  
#plotting regression model on the basis of test_data
  qplot(x = test_data[,1], y = lm_predictions, data = test_data, color = I("blue"), geom = "point")

#Evaluation of the linear regression model on the basis of test_data 
  regr.eval(test_data[,1],lm_predictions)
  
# mae        mse        rmse       mape 
# 3.5120358 19.0085569  4.3598804  0.4544157 
  
#Error rate =0.45
#Accuracy=55%
  
------------------------------------------------------------------------------------------- 
################################ DECISION TREE #######################################
-------------------------------------------------------------------------------------------- 
 
#Running Decision tree model   
  Dt_model = rpart(fare_amount ~ ., data = train_data, method = "anova")

#Summary of the model  
  summary(Dt_model)
  
#Predicting test cases using predict() method 
  predictions_DT = predict(Dt_model, test_data[,2:6])
 
#plotting Decision tree model on the basis predicted test_data 
  qplot(x = test_data[,1], y = predictions_DT, data = test_data, color = I("blue"), geom = "point")
  
#Evaluation of Decision tree model on the basis of test_data  
  regr.eval(test_data[,1],predictions_DT)

# mae       mse       rmse      mape 
# 1.9674997 7.0442171 2.6540944 0.2317727   

#Error rate =0.23
#Accuracy=77%  

-------------------------------------------------------------------------------------- 
################################ RANDOM FOREST #######################################
--------------------------------------------------------------------------------------

#Running Random Forest model    
  rf_model = randomForest(fare_amount ~.,data=train_data)
  
#Summary of the model  
  summary(rf_model)
  
#Predicting test cases using predict() method 
  rf_predictions = predict(rf_model,test_data[,2:6])

#plotting Random Forest model on the basis predicted test_data  
  qplot(x = test_data[,1], y = rf_predictions, data = test_data, color = I("blue"), geom = "point")
 
#Evaluation of Random Forest model on the basis of test_data 
  regr.eval(test_data[,1],rf_predictions)
  
  
# mae       mse       rmse      mape 
# 1.8838831 6.2805142 2.5060954 0.2312715 
  
#Error rate =0.23
#Accuracy=77%   
  

-------------------------------------------------------------------------------------- 
################################ XGBOOST MODEL  #######################################
--------------------------------------------------------------------------------------
  
#Running xgboost model 
  train_data_matrix = as.matrix(sapply(train_data[-1],as.numeric))
  test_data_matrix = as.matrix(sapply(test_data[-1],as.numeric))
  xgboost_model =xgboost(data = train_data_matrix,label = train_data$fare_amount,nrounds = 15,verbose = FALSE)
  
#Summary of the model
  summary(xgboost_model)
  
#Predicting test cases using predict() method 
  xgb_predictions = predict(xgboost_model,test_data_matrix)
  
#plotting xgboost model on the basis predicted test_data 
  qplot(x = test_data[,1], y = xgb_predictions, data = test_data, color = I("blue"), geom = "point")
  
#Evaluation of Random Forest model on the basis of test_data 
  regr.eval(test_data[,1],xgb_predictions)
  
# mae       mse       rmse      mape 
# 1.6206727 5.2563892 2.2926817 0.1835422 
  
#Error rate =0.18
#Accuracy=82%   

  
----------------------------------------------------------------------------------------------------------------- 
################################ FARE_AMOUNT PREDICTION FOR NEW TEST DATA #######################################
-----------------------------------------------------------------------------------------------------------------

#Training model on whole training Dataset and saving model using xgboost model has it has more accuracy when compared to other models
  train_data_matrix1 = as.matrix(sapply(Cab_Train_Data[-1],as.numeric))
  test_data_matrix1 = as.matrix(sapply(Cab_Test_Data,as.numeric))
  
#Running xgboost model on enire train data 
  xgboost_model1 = xgboost(data = train_data_matrix1,label = Cab_Train_Data$fare_amount,nrounds = 15,verbose = FALSE)
  
#Saving the trained model
  saveRDS(xgboost_model1, "./Trained_Xgboost_model_using_R.rds")
  
#loading the saved model
  Final_Trained_model= readRDS("./Trained_Xgboost_model_using_R.rds")
  print(Final_Trained_model)
  
#Predicting fare_amount on test dataset
  xgb = predict(Final_Trained_model,test_data_matrix1)
  xgb_pred = data.frame(test_pickup_datetime,"predictions" = xgb)
  
#Writing the predicted fare_amount in disk in .csv format 
  write.csv(xgb_pred,"Cab_Fare_Prediction_By_R.csv",row.names = FALSE)
  

    
  