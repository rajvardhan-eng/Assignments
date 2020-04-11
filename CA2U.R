library(dplyr)
library(tidyverse)
library(plyr)

#####Loading the data
NIPostcodes <- read.csv("E:/NIPostcodes.csv",na.strings=c("",".","NA"))
########Question1
####Total number of rows of NIPostcodes
nrow(NIPostcodes)
##943033
####First 10 rows
head(NIPostcodes,n=10)

#####Column Names for the NIPostcodes
colnames(NIPostcodes) <- c('Organisation_Name' ,
                           'Sub_building_Name' ,
                           'Building_Name' ,
                           'Number' ,
                           'Primary_Thorfare' ,
                           'Alt_Thorfare' ,
                           'Secondary_Thorfare' ,
                           'Locality' ,
                           'Townland' ,
                           'Town' ,
                           'County' ,
                           'Postcode' ,
                           'xcoordinates' ,
                           'ycoordinates' ,
                           'Primary_Key' 
                           )


colnames(NIPostcodes)[apply(NIPostcodes, 2, anyNA)]
colnames(NIPostcodes)[apply(NIPostcodes, 2, anyNA)]
# [1] "Organisation_Name"  "Sub_building_Name"  "Building_Name"     
# [4] "Number"             "Primary_Thorfare"   "Alt_Thorfare"      
# [7] "Secondary_Thorfare" "Locality"           "Town"              
# [10] "Postcode"          
 sum(is.na(NIPostcodes$Organisation_Name))
# [1] 890536
 sum(is.na(NIPostcodes$Sub_building_Name))
# 884112
 sum(is.na(NIPostcodes$Building_Name))
# 895541
 sum(is.na(NIPostcodes$Number))
# 28754
 sum(is.na(NIPostcodes$Primary_Thorfare))
#470
 sum(is.na(NIPostcodes$Alt_Thorfare))
#921787
 sum(is.na(NIPostcodes$Secondary_Thorfare))
#938399
 sum(is.na(NIPostcodes$Locality))
#856788
 sum(is.na(NIPostcodes$Town))
#19872
 sum(is.na(NIPostcodes$Postcode))
#8900
#####out of 15 variables 6 have NAs more than 50% in their fields. To clean the data we should 
####remove these columns 

#d:Show the total number and mean missing values for each column in the
#postcode data frame.
sapply(NIPostcodes, function(x)sum(is.na(x)))
sapply(NIPostcodes, function(x)mean(is.na(x)))


#e) Modify the County attribute to be a categorising factor.
NIPostcodes$County ->as.factor(NIPostcodes$County)

#f) Move the primary key identifier to the start of the dataset.

NIPostcodes <- NIPostcodes %>% select('Primary_Key',
                                      'Organisation_Name' ,
                                      'Sub_building_Name' ,
                                      'Building_Name' ,
                                      'Number' ,
                                      'Primary_Thorfare' ,
                                      'Alt_Thorfare' ,
                                      'Secondary_Thorfare' ,
                                      'Locality' ,
                                      'Townland' ,
                                      'Town' ,
                                      'County' ,
                                      'Postcode' ,
                                      'xcoordinates' ,
                                      'ycoordinates' 
                                       )

#g) Create a new dataset called Limavady_data. Store within it only information
#that has locality, townland and town containing the name Limavady. Store this
#information in an external csv file called Limavady.

Limavady_data <- NIPostcodes %>% select('Locality','Townland','Town') %>% 
  filter(Town=="LIMAVADY")

write.csv(Limavady_data,file ="C:/Users/Downloads/Limavady.csv")

#h) Save the modified NIPostcode dataset in a csv file called
#CleanNIPostcodeData.

write.csv(NIPostcodes,file ="E:/CleanNIPostcodeData.csv")

######NI CRIME DATASET######
# SECTION 2
# (a) Using R, amalgamate all of the crime data from each csv file into one dataset.
# Save this dataset into a csv file called AllNICrimeData. Count and show the number
# of rows in the AllNICrimeData dataset.

setwd("E:/NI Crime Data/NI Crime Data")
year = list.files(pattern="20*")
a<- data.frame()
for(j in 1:length(year)){
temp2 = list.files(path = paste0("E:/NI Crime Data/NI Crime Data/",year[j],"/"),pattern="*.csv")
temp=paste0("E:/NI Crime Data/NI Crime Data/",year[j],"/",temp2)
for (i in 1:length(temp)) assign(paste0("crime",i), read.csv(temp[i]))
a<-rbind(a,crime1)}

AllNICrimeData <- a
#number of rows in AllNICrimeData 
nrow(AllNICrimeData)
###Writing CSV

write.csv(AllNICrimeData,file ="E:/AllNICrimeData.csv")

# (b) Modify the structure of the newly created AllNICrimeData csv file and remove
# the following attributes: CrimeID, Reported by, Falls within, LSOA code, LSOA name,
# last outcome and context. Show the structure of the modified file.
AllNICrimeData<-read.csv("E:/AllNICrimeData.csv")
AllNICrimeData <- AllNICrimeData %>% select(-c("Crime.ID","Reported.by", "Falls.within","LSOA.code" ,"LSOA.name","Last.outcome.category","Context"  ))

head(AllNICrimeData)
#(c) Factorise the Crime type attribute. Show the modified structure.
AllNICrimeData <- AllNICrimeData %>% rename("Crime_Type"="Crime.type")

AllNICrimeData$Crime_Type <- as.factor(AllNICrimeData$Crime_Type)
head(AllNICrimeData)

# (d) Modify the AllNICrimeData dataset so that the Location attribute contains only a
# street name. For example, the attribute value "On or near Westrock Square" should
# be modified to only contain "Westrock Square". Modify the resultant empty location
# attributes with a suitable identifier.

AllNICrimeData$Location <- substr(AllNICrimeData$Location,12,100)
AllNICrimeData$Location[AllNICrimeData$Location==""]<-NA

# (e) Choose 1000 random samples of crime data from the AllNICrimeData dataset
# where the location attribute contains location information. This means that the
# location information should NOT contain an NA identifier. Store this data in a data
# frame called random_crime_sample. Then create a function called find_a_postcode
# that takes as an input each location attribute from random_crime_sample and finds
# a suitable postcode value from the postcode dataset.
Crime_without_na <- AllNICrimeData %>% na.omit(AllNICrimeData$Location)
random_crime_sample <- Crime_without_na[sample(nrow(Crime_without_na), 1000), ]
random_crime_sample<- random_crime_sample %>% select(-c('X'))
random_crime_sample$Location1 <- toupper(random_crime_sample$Location)
NIPostcodes1 <- NIPostcodes %>% select(c('Primary_Thorfare','Postcode'))

random_crime_sample_PC <- random_crime_sample
NIPostcodes_new<-NIPostcodes
random_crime_sample_PC$POST_CODE <- NA
for(i in 1:nrow(random_crime_sample)){
  if(!is.na(match(random_crime_sample$Location1[i],NIPostcodes_new$Primary_Thorfare)) &&
     is.na(NIPostcodes_new$Postcode[which(random_crime_sample$Location1[i]==NIPostcodes_new$Primary_Thorfare)])){
    df <- data.frame(Var1="NOTFOUND",Freq=999)  
  }else if(!is.na(match(random_crime_sample$Location1[i],NIPostcodes_new$Primary_Thorfare)) &&
           !is.na(NIPostcodes_new$Postcode[which(random_crime_sample$Location1[i]==NIPostcodes_new$Primary_Thorfare)])){
    
    df <- data.frame(table(NIPostcodes_new$Postcode[which(random_crime_sample$Location1[i]==NIPostcodes_new$Primary_Thorfare)]))}else{
      df <- data.frame(Var1="NOTFOUND",Freq=999)
      
    }
  df1 <- df %>% subset(Freq==max(Freq))
  
  random_crime_sample_PC$POST_CODE[i] <- as.character(df1$Var1)

}

#random_crime_sample_PC$POST_CODE[random_crime_sample_PC$POST_CODE=="NOTFOUND"] <- NA


######random_crime_sample_PC contains post code information for the crime data locations.
head(random_crime_sample_PC)
nrow(random_crime_sample_PC)
##1000


# (f) Append the data output from your find_a_postcode function to the
# random_crime_sample dataset. Show the modified structure. Save the modified
# random crime sample data frame as random_crime_sample.csv.
write.csv(random_crime_sample_PC,file ="E:/random_crime_sample.csv")


##############G. Extract this data into a new data frame called updated_random_sample. Then
# create another data frame called chart_data using the updated_random_sample
# data frame. Sort the chart_data dat frame by postcode where the postcode
# contains "BT1" and then by crime type. Show the summary statistics for the crime
# type from this chart_data data frame.
updated_random_sample <- random_crime_sample_PC %>% select(-c('Location1'))

class(updated_random_sample$POST_CODE)
####Sorting on the basis of post codes and crime type
chart_data <- arrange(updated_random_sample, POST_CODE, Crime_Type)
summary(chart_data)


# (h) Create a bar plot of the crime type from the chart_data data frame. Show a
# suitable main title for the bar chart, and suitable x and y-axis labels. Make sure all
# labels on the x-axis can be read. Show the bar plot in your CA document.
barplot(table(chart_data),
        main="Age Count of 10 Students",
        xlab="Crime_Type",
        ylab="Count",
        border="red",
        col="blue",
        density=10
)

count <-1
chart_data1 <- chart_data %>% group_by(Crime_Type) %>%
  dplyr::summarize(count = n() )

library(ggplot2)
# Basic barplot For count of Crime Types
p<-ggplot(data=chart_data1, aes(x=Crime_Type, y=count)) +
  geom_bar(stat="identity")
print(p)

