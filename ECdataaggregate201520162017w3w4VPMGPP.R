####Aggregates the EC data convert the datetime and plots daily GPP 
#### Rename the column names


####The year data of 2016 and 2017 were not on right order
## dfway316: way3 2016 30 min EC data
## dfway316
setwd("C:/Users/rbmahbub/Documents/RProjects/FluxdataR/30minECdata")

##Loading the required libraries
library(dplyr)
library(tidyr)
library(chron)
library(lubridate)
library(datetime)
library(ggplot2)
library(scales)


###Way3 2016
## Reading the files
dfway316<-read.csv("way3ushrcnorth2016.csv")

#renaming the columns for better coding
colnames(dfway316)[colnames(dfway316) == "Way.3..2016"]<- "Date_DMY"
colnames(dfway316)[colnames(dfway316) == "GPP..umol.CO2..m2.s"]<- "GPP"
colnames(dfway316)[colnames(dfway316) == "Reco..umol.CO2..m2.s"]<- "Reco"
colnames(dfway316)[colnames(dfway316) == "LE_gf..W.m2"]<- "Lh"
colnames(dfway316)[colnames(dfway316) == "H_gf..W.m2"]<- "Sh"

## Inspecting the date column
typeof(dfway316$Date_DMY)


#Splitting Date and Time (The date and time was together)
dfway316<- dfway316%>%
  separate(Date_DMY, c("Date", "Time"), " ")

#converting the date to date format
dfway316$Date<-as.Date(dfway316$Date,
                 format = "%m/%d/%Y")

#Converting date to year, month and day columns
dfway316<-dfway316 %>%
  dplyr::mutate(year = lubridate::year(Date), 
                month = lubridate::month(Date), 
                day = lubridate::day(Date))

#Checking the Date and time columns
typeof(dfway316$Date)
typeof(dfway316$Time)

# Converting the Time column to time format
dfway316$Time<-as.time(dfway316$Time)

#Separating The Time column into hour and minute columns
dfway316<- dfway316%>%
  separate(Time, c("hour", "minute"), ":")

#Checking the class differences
class(dfway316$year)
class(dfway316$hour)

##The hour and minute was in character format and was converted to numeric format as like the date formats
dfway316$hour<-as.numeric(dfway316$hour)
dfway316$minute<-as.numeric(dfway316$minute)

# There was no second column we created a second column with zero values
# Creating a column of zero elements
dfway316$second <- rep(0,nrow(dfway316))
typeof(dfway316$second)
class(dfway316$second)

View(dfway316)
dfway316$year<-dfway316$year+1


#Our task is to take the various columns of date and time information and convert these to a vector of type *datetime*. Let's go step by step, we will start with a new character vector of date
date.char<-paste(dfway316$year,dfway316$month,dfway316$day,sep = '-')
#do the same for time
time.char<-paste(dfway316$hour,dfway316$minute,dfway316$second,sep = ':')
#combine the two

dfway316$datetime<-as.POSIXct(paste(date.char,time.char,sep = ' '))

#Adding date as a separate column which would be useful later
dfway316$date<-as.Date(date.char)
head(dfway316$date)
View(dfway316)

## Daily mean
dfway316.daily <- aggregate(GPP ~ date, dfway316, mean)
head(dfway316.daily)
View(dfway316.daily)

## Converting umol/m2/s to gC/m2/day
dfway316.daily$GPP<- (dfway316.daily$GPP)* 1e-6*86400*12.011
class(dfway316.daily$GPP)


## Plotting Time vs GPP
pdfway316_daily<-ggplot(dfway316.daily,aes(x=date,y=GPP))+
  geom_point(size = 5)+ ylab(bquote('GPP ( '*g~ 'C'~ m^-2~day^-1*')')) +
  xlab("Date")+
  scale_x_date(breaks = date_breaks('30 day'),
               labels = date_format("%m-%d-%y"))+
  theme(text = element_text(size = 50))     

pdfway316_daily

##Save the figure
ggsave(pdfway316_daily, filename = "way3_2016_daily.png", width = 18, height = 12)



##Write the daily data in csv formats
write.csv(dfway316.daily, file ="way3ushrcnorth2016daily.csv", row.names=FALSE)

##########################WAY32017################################################
###Way3 2017
## Reading the files
dfway317<-read.csv("way3ushrcnorth2017.csv")

## See the column names
head(dfway317)

#renaming the columns
colnames(dfway317)[colnames(dfway317) == "Way.3..2017"]<- "Date_DMY"
colnames(dfway317)[colnames(dfway317) == "GPP..umol.CO2..m2.s"]<- "GPP"
colnames(dfway317)[colnames(dfway317) == "Reco..umol.CO2..m2.s"]<- "Reco"
colnames(dfway317)[colnames(dfway317) == "LE_gf..W.m2"]<- "Lh"
colnames(dfway317)[colnames(dfway317) == "H_gf..W.m2"]<- "Sh"

## Inspecting the date column
typeof(dfway317$Date_DMY)


#Splitting Date and Time (The date and time was together)
dfway317<- dfway317%>%
  separate(Date_DMY, c("Date", "Time"), " ")
View(dfway317)

#converting the date to date format
dfway317$Date<-as.Date(dfway317$Date,
                       format = "%m/%d/%Y")

#Converting date to year, month and day columns
dfway317<-dfway317 %>%
  dplyr::mutate(year = lubridate::year(Date), 
                month = lubridate::month(Date), 
                day = lubridate::day(Date))

#Checking the Date and time columns
typeof(dfway317$Date)
typeof(dfway317$Time)

# Converting the Time column to time format
dfway317$Time<-as.time(dfway317$Time)

#Separating The Time column into hour and minute columns
dfway317<- dfway317%>%
  separate(Time, c("hour", "minute"), ":")

#Checking the class differences
class(dfway317$year)
class(dfway317$hour)

##The hour and minute was in character format and was converted to numeric format as like the date formats
dfway317$hour<-as.numeric(dfway317$hour)
dfway317$minute<-as.numeric(dfway317$minute)

# There was no second column we created a second column with zero values
# Creating a column of zero elements
dfway317$second <- rep(0,nrow(dfway317))
typeof(dfway317$second)
class(dfway317$second)

dfway317$year<-dfway317$year+2
View(dfway317)

#Our task is to take the various columns of date and time information and convert these to a vector of type *datetime*. Let's go step by step, we will start with a new character vector of date
date.char<-paste(dfway317$year,dfway317$month,dfway317$day,sep = '-')
#do the same for time
time.char<-paste(dfway317$hour,dfway317$minute,dfway317$second,sep = ':')
#combine the two

dfway317$datetime<-as.POSIXct(paste(date.char,time.char,sep = ' '))

#Adding date as a separate column which would be useful later
dfway317$date<-as.Date(date.char)
head(dfway317$date)

## Daily mean
dfway317.daily <- aggregate(GPP ~ date, dfway317, mean)
head(dfway317.daily)
View(dfway317.daily)

## Converting umol/m2/s to gC/m2/day
dfway317.daily$GPP<- (dfway317.daily$GPP)* 1e-6*86400*12.011
class(dfway317.daily$GPP)


## Plotting Time vs GPP
pdfway317_daily<-ggplot(dfway317.daily,aes(x=date,y=GPP))+
  geom_point(size = 5)+ ylab(bquote('GPP ( '*g~ 'C'~ m^-2~day^-1*')')) +
  xlab("Date")+
  scale_x_date(breaks = date_breaks('30 day'),
               labels = date_format("%m-%d-%y"))+
  theme(text = element_text(size = 50))     

pdfway317_daily

##Save the figure
ggsave(pdfway317_daily, filename = "way3_2017_daily.png", width = 18, height = 12)

##Write the daily data in csv formats
write.csv(dfway317.daily, file ="way3ushrcnorth2017daily.csv", row.names=FALSE)

##########################WAY42015################################################
###Way4 2015
## Reading the files
dfway415<-read.csv("way4ushrasouth2015.csv")

## See the column names
head(dfway415)

#renaming the columns
colnames(dfway415)[colnames(dfway415) == "Way.4..2015"]<- "Date_DMY"
colnames(dfway415)[colnames(dfway415) == "GPP..umol.CO2..m2.s"]<- "GPP"
colnames(dfway415)[colnames(dfway415) == "Reco..umol.CO2..m2.s"]<- "Reco"
colnames(dfway415)[colnames(dfway415) == "LE_gf..W.m2"]<- "Lh"
colnames(dfway415)[colnames(dfway415) == "H_gf..W.m2"]<- "Sh"

## Inspecting the date column
typeof(dfway415$Date_DMY)


#Splitting Date and Time (The date and time was together)
dfway415<- dfway415%>%
  separate(Date_DMY, c("Date", "Time"), " ")
View(dfway415)

#converting the date to date format
dfway415$Date<-as.Date(dfway415$Date,
                       format = "%m/%d/%Y")

#Converting date to year, month and day columns
dfway415<-dfway415 %>%
  dplyr::mutate(year = lubridate::year(Date), 
                month = lubridate::month(Date), 
                day = lubridate::day(Date))

#Checking the Date and time columns
typeof(dfway415$Date)
typeof(dfway415$Time)

# Converting the Time column to time format
dfway415$Time<-as.time(dfway415$Time)

#Separating The Time column into hour and minute columns
dfway415<- dfway415%>%
  separate(Time, c("hour", "minute"), ":")

#Checking the class differences
class(dfway415$year)
class(dfway415$hour)

##The hour and minute was in character format and was converted to numeric format as like the date formats
dfway415$hour<-as.numeric(dfway415$hour)
dfway415$minute<-as.numeric(dfway415$minute)

# There was no second column we created a second column with zero values
# Creating a column of zero elements
dfway415$second <- rep(0,nrow(dfway415))
typeof(dfway415$second)
class(dfway415$second)

#Our task is to take the various columns of date and time information and convert these to a vector of type *datetime*. Let's go step by step, we will start with a new character vector of date
date.char<-paste(dfway415$year,dfway415$month,dfway415$day,sep = '-')
#do the same for time
time.char<-paste(dfway415$hour,dfway415$minute,dfway415$second,sep = ':')
#combine the two

dfway415$datetime<-as.POSIXct(paste(date.char,time.char,sep = ' '))

#Adding date as a separate column which would be useful later
dfway415$date<-as.Date(date.char)
head(dfway415$date)

## Daily mean
dfway415.daily <- aggregate(GPP ~ date, dfway415, mean)
head(dfway415.daily)
View(dfway415.daily)

## Converting umol/m2/s to gC/m2/day
dfway415.daily$GPP<- (dfway415.daily$GPP)* 1e-6*86400*12.011
class(dfway415.daily$GPP)


## Plotting Time vs GPP
pdfway415_daily<-ggplot(dfway415.daily,aes(x=date,y=GPP))+
  geom_point(size = 5)+ ylab(bquote('GPP ( '*g~ 'C'~ m^-2~day^-1*')')) +
  xlab("Date")+
  scale_x_date(breaks = date_breaks('30 day'),
               labels = date_format("%m-%d-%y"))+
  theme(text = element_text(size = 50))     

pdfway415_daily

##Save the figure
ggsave(pdfway415_daily, filename = "way4_2015_daily.png", width = 18, height = 12)


##Write the daily data in csv formats
write.csv(dfway415.daily, file ="way4ushrasouth2015daily.csv", row.names=FALSE)





##########################WAY42016################################################
###Way4 2016
## Reading the files
dfway416<-read.csv("way4ushrasouth2016.csv")

## See the column names
head(dfway416)

#renaming the columns
colnames(dfway416)[colnames(dfway416) == "Way.4..2016"]<- "Date_DMY"
colnames(dfway416)[colnames(dfway416) == "GPP..umol.CO2..m2.s"]<- "GPP"
colnames(dfway416)[colnames(dfway416) == "Reco..umol.CO2..m2.s"]<- "Reco"
colnames(dfway416)[colnames(dfway416) == "LE_gf..W.m2"]<- "Lh"
colnames(dfway416)[colnames(dfway416) == "H_gf..W.m2"]<- "Sh"

## Inspecting the date column
typeof(dfway416$Date_DMY)


#Splitting Date and Time (The date and time was together)
dfway416<- dfway416%>%
  separate(Date_DMY, c("Date", "Time"), " ")
View(dfway416)

#converting the date to date format
dfway416$Date<-as.Date(dfway416$Date,
                       format = "%m/%d/%Y")

#Converting date to year, month and day columns
dfway416<-dfway416 %>%
  dplyr::mutate(year = lubridate::year(Date), 
                month = lubridate::month(Date), 
                day = lubridate::day(Date))

#Checking the Date and time columns
typeof(dfway416$Date)
typeof(dfway416$Time)

# Converting the Time column to time format
dfway416$Time<-as.time(dfway416$Time)

#Separating The Time column into hour and minute columns
dfway416<- dfway416%>%
  separate(Time, c("hour", "minute"), ":")

#Checking the class differences
class(dfway416$year)
class(dfway416$hour)

##The hour and minute was in character format and was converted to numeric format as like the date formats
dfway416$hour<-as.numeric(dfway416$hour)
dfway416$minute<-as.numeric(dfway416$minute)

# There was no second column we created a second column with zero values
# Creating a column of zero elements
dfway416$second <- rep(0,nrow(dfway416))
typeof(dfway416$second)
class(dfway416$second)

View(dfway416)
dfway416$year<-dfway416$year+1

#Our task is to take the various columns of date and time information and convert these to a vector of type *datetime*. Let's go step by step, we will start with a new character vector of date
date.char<-paste(dfway416$year,dfway416$month,dfway416$day,sep = '-')
#do the same for time
time.char<-paste(dfway416$hour,dfway416$minute,dfway416$second,sep = ':')
#combine the two

dfway416$datetime<-as.POSIXct(paste(date.char,time.char,sep = ' '))

#Adding date as a separate column which would be useful later
dfway416$date<-as.Date(date.char)
head(dfway416$date)

## Daily mean
dfway416.daily <- aggregate(GPP ~ date, dfway416, mean)
head(dfway416.daily)
View(dfway416.daily)

## Converting umol/m2/s to gC/m2/day
dfway416.daily$GPP<- (dfway416.daily$GPP)* 1e-6*86400*12.011
class(dfway416.daily$GPP)


## Plotting Time vs GPP
## Plotting Time vs GPP
pdfway416_daily<-ggplot(dfway416.daily,aes(x=date,y=GPP))+
  geom_point(size = 5)+ ylab(bquote('GPP ( '*g~ 'C'~ m^-2~day^-1*')')) +
  xlab("Date")+
  scale_x_date(breaks = date_breaks('30 day'),
               labels = date_format("%m-%d-%y"))+
  theme(text = element_text(size = 50))     

pdfway416_daily

##Save the figure
ggsave(pdfway416_daily, filename = "way4_2016_daily.png", width = 18, height = 12)

##Write the daily data in csv formats
write.csv(dfway416.daily, file ="way4ushrasouth2016daily.csv", row.names=FALSE)


##########################WAY42017################################################
###Way4 2017
## Reading the files
dfway417<-read.csv("way4ushrasouth2017.csv")

## See the column names
head(dfway417)

#renaming the columns
colnames(dfway417)[colnames(dfway417) == "Way.4..2017"]<- "Date_DMY"
colnames(dfway417)[colnames(dfway417) == "GPP..umol.CO2..m2.s"]<- "GPP"
colnames(dfway417)[colnames(dfway417) == "Reco..umol.CO2..m2.s"]<- "Reco"
colnames(dfway417)[colnames(dfway417) == "LE_gf..W.m2"]<- "Lh"
colnames(dfway417)[colnames(dfway417) == "H_gf..W.m2"]<- "Sh"

## Inspecting the date column
typeof(dfway417$Date_DMY)


#Splitting Date and Time (The date and time was together)
dfway417<- dfway417%>%
  separate(Date_DMY, c("Date", "Time"), " ")
View(dfway417)

#converting the date to date format
dfway417$Date<-as.Date(dfway417$Date,
                       format = "%m/%d/%Y")

#Converting date to year, month and day columns
dfway417<-dfway417 %>%
  dplyr::mutate(year = lubridate::year(Date), 
                month = lubridate::month(Date), 
                day = lubridate::day(Date))

#Checking the Date and time columns
typeof(dfway417$Date)
typeof(dfway417$Time)

# Converting the Time column to time format
dfway417$Time<-as.time(dfway417$Time)

#Separating The Time column into hour and minute columns
dfway417<- dfway417%>%
  separate(Time, c("hour", "minute"), ":")

#Checking the class differences
class(dfway417$year)
class(dfway417$hour)

##The hour and minute was in character format and was converted to numeric format as like the date formats
dfway417$hour<-as.numeric(dfway417$hour)
dfway417$minute<-as.numeric(dfway417$minute)

# There was no second column we created a second column with zero values
# Creating a column of zero elements
dfway417$second <- rep(0,nrow(dfway417))
typeof(dfway417$second)
class(dfway417$second)

View(dfway417)
dfway417$year<-dfway417$year+2

#Our task is to take the various columns of date and time information and convert these to a vector of type *datetime*. Let's go step by step, we will start with a new character vector of date
date.char<-paste(dfway417$year,dfway417$month,dfway417$day,sep = '-')
#do the same for time
time.char<-paste(dfway417$hour,dfway417$minute,dfway417$second,sep = ':')
#combine the two

dfway417$datetime<-as.POSIXct(paste(date.char,time.char,sep = ' '))

#Adding date as a separate column which would be useful later
dfway417$date<-as.Date(date.char)
head(dfway417$date)

## Daily mean
dfway417.daily <- aggregate(GPP ~ date, dfway417, mean)
head(dfway417.daily)
View(dfway417.daily)

## Converting umol/m2/s to gC/m2/day
dfway417.daily$GPP<- (dfway417.daily$GPP)* 1e-6*86400*12.011
class(dfway417.daily$GPP)


## Plotting Time vs GPP
pdfway417_daily<-ggplot(dfway417.daily,aes(x=date,y=GPP))+
  geom_point(size = 5)+ ylab(bquote('GPP ( '*g~ 'C'~ m^-2~day^-1*')')) +
  xlab("Date")+
  scale_x_date(breaks = date_breaks('30 day'),
               labels = date_format("%m-%d-%y"))+
  theme(text = element_text(size = 50))     

pdfway417_daily

##Save the figure
ggsave(pdfway417_daily, filename = "way4_2017_daily.png", width = 18, height = 12)


pdfway417_daily
##Write the daily data in csv formats
write.csv(dfway417.daily, file ="way4ushrasouth2017daily.csv", row.names=FALSE)


figure <- ggarrange(pdfway417_daily, pdfway417_daily, pdfway417_daily, pdfway417_daily, pdfway417_daily, pdfway417_daily,
                    labels = c("A", "B", "C", "D", "E", "F"),
                    ncol = 3, nrow = 2)
ggsave(figure, filename = "6years.png", width = 48, height = 32)

