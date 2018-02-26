rm(list=ls())
setwd("/Users/Ola/Documents/R")
install.packages("zoo")
install.packages("xts")
install.packages("timeSeries")
install.packages("forecast")
install.packages("Metrics")
library(zoo)
library(xts)
library(timeSeries)
library(forecast)
library(Metrics)

df<-read.csv("usa.csv", header=TRUE)
str(df) #date is in factor format and we want it to be in date format 
df$date<-as.Date(df$date, format="%m/%d/%y")
df.zoo<-read.zoo(df, FUN=as.yearmon) #turn into "zoo" series - useful for time series analysis
df.xts<-as.xts(df.zoo) #convert to "xts" object for time series analysis 

#Q6.1-A

####FORMATTING DATA :/ boring zzzz ###########
df_CPI_monthly<-to.monthly(df.xts$CPI)
df_56month<-df_CPI_monthly[1:57, 1] #only working with the first 56-months of data
df_112month<-df_CPI_monthly[58:113, 1] #second half of data !!!! CHECK IT ! 
df56_orig_ts<-ts(df_56month)
df112_orig_ts<-ts(df_112month)
dfcpi_monthly <- as.data.frame(df_CPI_monthly)
dfcpi_monthly <- dfcpi_monthly[,-c(2,3,4), drop=FALSE]

##Some cool plots of data
plot.xts(df_56month, ylab="CPI", main="Monthly CPI, Months 1-56") #we can see here that our data is not stationary 
df_56_stat<-diff(df_56month, lag=1, differences=1, log=FALSE, na.pad=TRUE) #stationarize time-series with first difference-how do I get rid of NA in first row - stuck here?
df_56_stat<-df_56_stat[-1,]
df.ts<-ts(df_56_stat) #make into time-series object
plot.xts(df_56_stat, ylab="CPI", main="Monthly CPI, Months 1-56: First Difference")



#First thing is to examine the ACF of data difference to have an idea of what works
df_CPI_monthlyTs<-ts(df_CPI_monthly[,1])
differencedAllData <- diff(df_CPI_monthlyTs, lag=1, differences=1, log=FALSE, na.pad=TRUE)
plot(acf(differencedAllData), main="ACF of Differenced Data")
plot(pacf(differencedAllData), main="PACF of Differenced Data")
#plots suggest order 2 might work.
######## END OF Cool plots and BORING FORMATTING ########


################START Q1A PROPER ##################
mse_values<-numeric(11)
#####Checking for Best Order
for (i in 1:11) {
  predictions<-c() #Initialize predictions vector
  actual <- dfcpi_monthly[58:113, , drop=FALSE]  #validation data for MSE later
  df56data<-dfcpi_monthly[1:57, , drop=FALSE] 
  df.arfit<-arima(df56data, order=c(i,1,0))   #Fit an arima model "model 56" on 56 months and order i 
  pred1<-predict(df.arfit,n.ahead=1) #Predict month 57
  tempval<-as.data.frame(pred1$pred)
  predictions<-cbind(predictions,tempval[1,])
  for (j in 58:113){
    newdata <- dfcpi_monthly[1:j, , drop=FALSE] #update data to include latest month
    if ( j<113){
      #Apply fitted model 56, to latest data.
      fitter <- Arima(newdata, model=df.arfit) 
      #predict and store
      pred1<-predict(fitter, n.ahead=1)  
      tempval <- as.data.frame(pred1$pred)
      predictions<-cbind(predictions,tempval[1,])
    }
  }
  #compute MSE for order i and store
  mse_values[i]<- mse(actual, predictions)
}
#plot to see order with lowest MSE 
plot(c(1:11),mse_values, type="l", main = "Mean Squared Error for Orders 1 to 11", ylab="Mean Squared Value", xlab="Order")    #Order two is lowest with MSE of 0.07081703

###We may now proceed with order 2 since it has lowest MSE 
#But first lets do some predictions stuff with Order 2 
#.......Initialize predictions storage
predictionsfor2<-c()  
#Actual data for check MSE later
actual <- dfcpi_monthly[58:113, , drop=FALSE]  #test set
df56data<-dfcpi_monthly[1:57, , drop=FALSE] #train set
#Fit to Order 2 and create our "56 Model'
df.arfit1<-arima(df56data, order=c(2,1,0)) 
pred1<-predict(df.arfit1,n.ahead=1) #Predict month 57
tempval<-as.data.frame(pred1$pred)
predictionsfor2<-cbind(predictionsfor2,tempval[1,])
for (j in 58:113){
  #update data to include latest month j
  newdata1 <- dfcpi_monthly[1:j, , drop=FALSE] 
  if ( j<113){
    #Apply fitted 56 model data to latest data
    fitterar1 <- Arima(newdata1, model=df.arfit1)  
    #predict and store.
    pred1<-predict(fitterar1, n.ahead=1)
    tempval <- as.data.frame(pred1$pred)
    predictionsfor2<-cbind(predictionsfor2,tempval[1,])
  }
}
mse_valuesforar2<- mse(actual, predictionsfor2)  #0.070817

#Plot Order 2 values
cpidf<-as.data.frame(df_CPI_monthly)
cpidf<-cpidf[,1]
plot(cpidf, type="l",main = "Time Series of Original Data", ylab="CPI values", xlab="Month")
plot(cpidf, type="l",main = "Time Series of Original Data and AR order 2 prediction", ylab="CPI values", xlab="Month")
dfcpi_monthly <- as.data.frame(df_CPI_monthly)
dfcpi_monthly <- dfcpi_monthly[,-c(2,3,4), drop=FALSE]
lines(c(58:113), predictionsfor2, col="red") 

###Confirming the residuals of prediction for order 2 to show that our stuff makes sense
res2<-predictionsfor2-actual
plot(acf(res2), main="ACF Residual, Order 2")
plot(pacf(res2), main="PACF Residual, Order 2")
#### END OF 6.1a ###############

########### START OF 6.1B #################
#Q6.1-B
###Reading data and doing boring formatting again ######
#next, read-in the new data as extra column
df_ber<-read.csv("BER.csv", header=TRUE)
str(df_ber) #date is in factor format and we want it to be in date format 
df_ber$DATE<-as.Date(df_ber$DATE, format="%m/%d/%y")
df_ber<-na.omit(df_ber)
df_ber.zoo<-read.zoo(df_ber, FUN=as.yearmon) #turn into "zoo" series - useful for time series analysis
dfber_monthly <- aggregate(df_ber.zoo, as.yearmon, mean, na.rm=TRUE) #remove NA's
dfber_monthly <- as.data.frame(dfber_monthly)
dfbpp_monthly<- aggregate(df.xts$BPP, as.yearmon, mean)
dfbpp_monthly <- as.data.frame(dfbpp_monthly)
str(df_ber)
dfber_monthly <- dfber_monthly[67:179, , drop=FALSE]
dfcpi_monthly <- as.data.frame(df_CPI_monthly)
dfcpi_monthly <- dfcpi_monthly[,-c(2,3,4), drop=FALSE]
##End of formatting of data set. We can now do AR fittings #####

##6.1bi and 6.1ci####
#Extract Regressor data first
regressor <- cbind(dfbpp_monthly, dfber_monthly)
regressorfake1<-regressor[1,]
regressorfakeRest<-regressor[1:nrow(regressor)-1,]
regressorfake<-rbind(regressorfake1,regressorfakeRest)
regressorfake[1,]<-NA
regressor<-regressorfake
reg1 <- regressor[58:113, , drop=FALSE] #testing ext regression data
reg0 <- regressor[1:57, , drop=FALSE] #training ext regression data
predictions2<-c()  #Initialize predictions vector
predictions2MA<-c() #Initialize predictions for improvement vector as asked in 6.1c

actual <- dfcpi_monthly[58:113, , drop=FALSE] #validation/testing data for mse later
dfcpi_monthly_0 <- dfcpi_monthly[1:57, , drop=FALSE] 
#Fit on First 56 months data i.e. "df.ts"
tempfit_11<-arima(df56data, xreg=reg0, order=c(2,1,0)) #method="ML" #Fit order one on first 56 months i.e. "56 fit model"
tempfit_12 <- arima(df56data, xreg=reg0, order=c(2,1,0), seasonal=c(2,0,0)) #method="ML"
#Predict month 57 first
reg1val<-reg1val <- regressor[58, ,drop=FALSE]
pred1<-predict(tempfit_11, newxreg= reg1val,  n.ahead=1)
pred1MA<-predict(tempfit_12, newxreg= reg1val , n.ahead=1)
tempval<-as.data.frame(pred1$pred)
tempvalMA<-as.data.frame(pred1MA$pred)
predictions2<-cbind(predictions2,tempval[1,])
predictions2MA<-cbind(predictions2MA,tempvalMA[1,])
#create vector that fixes coefficients for model refitting #to solve code bug of tempfit_12 with Arima
k=length(tempfit_12$coef)
fixedvec=numeric(k)
for (item in 1:k){
  fixedvec[item]<-tempfit_12$coef[[item]]
}
#Now do for loop for last 56 months 
for (j in 58:113){
  #update external regression training data
  reg0<- regressor[1:j, , drop=FALSE]  
  if ( j<113){
    
    #get latest data
    dfcpi_monthly_0 <- dfcpi_monthly[1:j, , drop=FALSE] 
    
    #get next month's external regressor value for later prediction
    reg1val <- regressor[j+1, ,drop=FALSE]  
    
    #Apply fitted 56 model to latest data
    #newData<-cbind(dfcpi_monthly_0,reg0)  
    Artempfit_11 <- Arima(dfcpi_monthly_0, xreg=reg0, model=tempfit_11) #using "56 model" on newer data 
    Artempfit_12 <-arima(dfcpi_monthly_0, xreg=reg0, order=c(2,1,0), seasonal=c(2,0,0), fixed = fixedvec) #applied fixed vector coefficients.
    
    #predict next month values based on refit and this month's external regressor
    pred1<-predict(Artempfit_11, newxreg= reg1val, n.ahead=1)
    pred1MA<-predict(Artempfit_12, newxreg= reg1val, n.ahead=1)
   
    #get predicted values
    tempval <- as.data.frame(pred1$pred)
    tempvalMA <-as.data.frame(pred1MA$pred)
    
    #store predictions
    predictions2<-cbind(predictions2,tempval[1,])
    predictions2MA<-cbind(predictions2MA,tempvalMA[1,])
  }
}

#Evaluate Mean Squared Error
mse_values2<- mse(actual, predictions2) #mse of standard monthly average  BER + BPP method  0.0893617
mse_values2betterone<-mse(actual, predictions2MA) #mse of monthly average BER + BPP method but with MA or seasonality tweaks 0.088348

plot(cpidf, type="l", main="CPI series plot showing original and AR(2) predictions with external regressors (Monthly Average)", ylab="CPI index", xlab="Time (Months)")
lines(c(58:113), predictions2, col="red") 

plot(cpidf, type="l", main="CPI Series Plots of Original and AR(2) Predictions with External Regressors (Monthly Average) & Seasonal AR Term", ylab="CPI index", xlab="Time (Months)")
lines(c(58:113), predictions2MA, col="red") 
##### END OF 6.1bi and 6.1ci ########

#####################################
#Q6.1bii and 6.1cii: Using the last day of month and not the average bpp and ber
#First Format data to extract last day values
df_ber.zoo<-read.zoo(df_ber, FUN=as.yearmon) #turn into "zoo" series - useful for time series analysis
df_ber.xts<-as.xts(df_ber.zoo)
colnames(df_ber.xts)<-c("T10YIE")
df_ber_last<-to.monthly(df_ber.xts$T10YIE)
df_ber_last <- df_ber_last[,-c(1,2,3)]

df_bpp_last <- to.monthly(df.xts$BPP)
df_bpp_last <- df_bpp_last[,-c(1,2,3)]
#End of formatting we can now do AR Regressive model - last day 


##2nd create the external regressor data for last day
regressor_last <- cbind(df_bpp_last, df_ber_last)
regressor_last <- regressor_last[67:179, drop=FALSE]

regressorfake2<-regressor_last[1,]
regressorfakeRest2<-regressor_last[1:nrow(regressor_last)-1,]
regressorfake2<-rbind(regressorfake2,regressorfakeRest2)
regressorfake2[1,]<-NA
regressor_last<-regressorfake2

##Now we can do our prediction for loops
predictions3<-c()  #initialize vector storage for predictions 
predictions3MA<-c() #stores predictions for part c question on 'improvement'
actual <- dfcpi_monthly[58:113, , drop=FALSE] #testing data for MSE computing.
dfcpi_monthly_0 <- dfcpi_monthly[1:57, , drop=FALSE]  #training data for our "56 model". 57 data points because we difference to stationarize. 

#Create Fit data on First 56 months.
reg0_last<- regressor_last[1:57, , drop=FALSE]
tempfit_11_last<-arima(dfcpi_monthly_0, xreg=reg0_last, order=c(2,1,0)) #method="ML" #Fit order one on first 56 months i.e. "56 fit model"
tempfit_12_last <- arima(dfcpi_monthly_0, xreg=reg0_last, order=c(2,1,0), seasonal=c(1,0,0)) #method="ML"
#Predict month 57 first
reg1val<- regressor_last[58, ,drop=FALSE]
pred2<-predict(tempfit_11_last, newxreg= reg1val,  n.ahead=1)
pred2MA<-predict(tempfit_12_last, newxreg= reg1val , n.ahead=1)
tempval<-as.data.frame(pred2$pred)
tempvalMA<-as.data.frame(pred2MA$pred)
predictions3<-cbind(predictions3,tempval[1,])
predictions3MA<-cbind(predictions3MA,tempvalMA[1,])
#create vector that fixes coefficients for model refitting #to solve code bug of tempfit_12 with Arima
k=length(tempfit_12_last$coef)
fixedvec=numeric(k)
for (item in 1:k){
  fixedvec[item]<-tempfit_12_last$coef[[item]]
}

for (j in 58:113){
 
  if ( j<113){
    ###Now do
    #get latest data
    dfcpi_monthly_0 <- dfcpi_monthly[1:j, , drop=FALSE] #update data to include more recent data of latest jth month
    reg0_last<- regressor_last[1:j, , drop=FALSE] #update external regressor data to include latest month values
    
    #get next month regressor value for prediction later
    reg1val <- regressor_last[j+1, ,drop=FALSE] 
    
    #Apply fitted "56" model to latest data
    Artempfit_11_last <- Arima(dfcpi_monthly_0, xreg =reg0_last,  model=tempfit_11_last) #use "56 model" on newer data 
    Artempfit_12_last <-arima(dfcpi_monthly_0, xreg=reg0_last, order=c(2,1,0), seasonal=c(1,0,0), fixed = fixedvec) #applied fixed vector coefficients.
    
    #Predict next values 
    pred2<-predict(Artempfit_11_last, newxreg= reg1val , n.ahead=1)
    pred2MA<-predict(Artempfit_12_last, newxreg= reg1val , n.ahead=1) #seasonality version

    #Get Predictions
    tempval <- as.data.frame(pred2$pred)
    tempvalnew<-as.data.frame(pred2MA$pred) #seasonality version
    
    #Store Predictions
    predictions3<-cbind(predictions3,tempval[1,])
    predictions3MA<-cbind(predictions3MA,tempvalnew[1,]) #seasonality version
    
  }
}

#Evaluate MSE and plot
mse_values3<- mse(actual, predictions3) #0.10742134
mse_values3_betterone<-mse(actual,predictions3MA)  #0.1572134
plot(cpidf, type="l", main="Original Series and AR(2) predictions with 'last day' External Regressors", ylab="CPI index", xlab="Time (Months)")
lines(c(58:113), predictions3, col="red") 

plot(cpidf, type="l", main="CPI data showing original and Modified AR predictions 'with last day' ", ylab="CPI index", xlab="Time (Months)")
lines(c(58:113), predictions3MA, col="red") 
#end 


########################END OF 6.1##################################


########################START OF 6.2###############################
#Q6.2
df_2<-read.csv("CO2Data.csv", header=TRUE) #I transformed data in Excel before importing
str(df_2) #type is data-frame and date(s) is int format; need to make ts
df_2.ts<-ts(df_2$CO2) #just make CO2 time-seires 
#Q6.2A
df_2$CO2[which(df_2$CO2==-99.99)]=NA #remove the outliers to smooth it
plot.ts(as.matrix(df_2$CO2), main="Linear Model", ylab="CO2", xlab="Time (Months)") 
df_2$Date.1<-df_2$Date.1-1958 #subtract to get only the fraction in the column

lmfit <- tslm(df_2$CO2 ~ df_2$Date.1, data=df_2.ts)
lines(fitted(lmfit), col="blue") #fit data to linear model
#Coefficients:
#(Intercept)  df_2$Date.1  
#306.835        1.524  

plot(residuals(lmfit), type="p", main="Residual Error-Linear", ylab="Residuals", xlab="Time (Months)")

#6.2B
quadfit<- tslm(df_2$CO2 ~ df_2$Date.1 + I(df_2$Date.1^2), data=df_2.ts)
plot.ts(as.matrix(df_2$CO2), main="Quadratic Model", ylab="CO2", xlab="Time (Months)") 
lines(fitted(quadfit), col="green")
plot(residuals(quadfit), type="p", main="Residual Error-Quad Function", ylab="Residuals", xlab="Time (Months)")
tslm(formula = df_2$CO2 ~ df_2$Date.1 + I(df_2$Date.1^2), data = df_2.ts)

#Coefficients:
#(Intercept)       df_2$Date.1  I(df_2$Date.1^2)  
#314.24134           0.78341           0.01252  

#6.2C - Fit 2 is better

#6.2D 
residual<-cbind(df_2$Mn, as.matrix(residuals(quadfit)))
monthavg<-matrix(0,12,1)
for (i in 1:12){
  monthavg[i,1]<-mean(residual[which(residual[,1]==i),2], na.rm=TRUE)
}
plot(monthavg, type="b", pch=21, ylab="Periodic Signal, Pi", xlab="Time (Months)", main="Periodic Components of F2(t)")

#6.2E
f2 <- as.matrix(fitted(quadfit))
f2 <- as.vector(f2) + as.vector(rep(monthavg, 718/12))
plot(f2, type="l",main="F2(t)+Pi Plot", ylab="F2(t)+Pi Plot", xlab="Time (Months)")
lines(fitted(quadfit), col="red")
#variation is increasing - the world is fucked and so are the polar bears. fuck climate change and republicans denying it.
