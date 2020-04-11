## CA4

### 
#Global Options
rm(list = ls())
options(scipen = 999)
options(stringsAsFactors = FALSE)

## Packages Required ###
library(plyr)
#Loading required libraries
library(lubridate)
library(dplyr)
library(stats)
library(forecast)
library(sjstats)
library(MLmetrics)
library(plotly)

path = "E:/Crime"  ### Directory Path 
setwd(path)  ## To Set Path ##

temp = list.files(pattern = "*.csv$", recursive = TRUE) ##  To get list of files into R from the directory ##

myfiles = lapply(temp, read.csv)  ## To load all the files into R ###

df <- ldply(myfiles, data.frame)  ## converting list to dataframe and binding multiple list into single dataframe

df$Month <-  as.Date(as.yearmon(df$Month, "%Y-%m")) # To convert character to date format ##

final_data <- data.frame(table(df$Month))   ## Aggregate number of Crimes by month ###
names(final_data) <- c("Month","CrimeNumber") ## change column names ###
final_data$Month <-  as.Date(final_data$Month, "%Y-%m-%d") ## To Date


## plot data ###
p1 <- ggplot() + geom_line(aes(y = CrimeNumber, x = Month),
                 data = final_data)
p1 + labs(title = "Crime by Year-Month", x = "Year-month", y = "Number of Crimes")


##Sub-setting Train Data ###
Train_Sub1 <- final_data[1:30,] 
	
#Sub-setting the Test data 
Test_Sub <- final_data[31:nrow(final_data),c("Month", "CrimeNumber")]

#Reordering the data by Date
Train_Sub1 <- Train_Sub1[order(Train_Sub1$Month),] 

# changing  to time series ##
Train_ts <- ts(Train_Sub1$CrimeNumber, frequency = 12)

## Model building using auto arima ###
model <- auto.arima(Train_ts)

## foracast for test data ###
forecast <- as.numeric(forecast(model,h = nrow(Test_Sub))$mean)

### comapre actual and forecast ###
Actual_forecasts <- data.frame(Date = Test_Sub$Month, Actual = Test_Sub$CrimeNumber, Forecast = round(forecast,0))	

### Evaluating foracated Values ###								
Error_metrics <- MAPE(Actual_forecasts$Forecast, Actual_forecasts$Actual)		

#### Mape value is around "0.1153481" so accuracy is about 88%

## plot for actual and forecast ####
plot_ly(Actual_forecasts, x = ~Date, y = ~Actual, name = 'Actual', type = 'scatter', mode = 'lines+markers') %>%
	add_trace(y = ~Forecast, name = 'forecast', line = list(color = 'rgb(237,125,49)'), marker = list(color = 'rgb(237,125,49)'), mode = 'lines+markers')
					