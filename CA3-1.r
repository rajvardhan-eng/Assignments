## CA3

### 
#Global Options
rm(list = ls())
options(scipen = 999)
options(stringsAsFactors = FALSE)

## Packages Required ###
library(plyr)

path = "E:/Crime"  ### Directory Path 
setwd(path)  ## To Set Path ##

temp = list.files(pattern = "*.csv$", recursive = TRUE) ##  To get list of files into R from the directory ##

myfiles = lapply(temp, read.csv)  ## To load all the files into R ###

df <- ldply(myfiles, data.frame)  ## converting list to dataframe and binding multiple list into single dataframe

#Q1  examine the structure of your datasets 
str(df)

#Q2 Type of response variables under scrutiny 
## Most of the variable here are categorical with 1 level values 

#Q3 Number of groups being studied 
## Variables considered : crime type and location

#Q4 How these enable you to answer your research question 
#Q5 Assumptions you are making about your data 
## Assumptions ##
## To check of their is significant difference between Crime Type and Location

#Ho: Crime Type and Location are independent.

#Ha: Crime Type and Location are not independent.

## As both the varaibles considered are categorical we use chi square test to check the significance level between the variables
Result = chisq.test(table(df$Crime.type,df$Location)) 

## Interpreting Results ###
if((Result$p.value) < 0.05)
{
print("Crime type dependes on location") 
}else{
print("Crime type and location are independent")
}

