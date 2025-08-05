### Aggregating 30 minute data to daily timescale
### Source code: https://github.com/OptPhotLab/EnvDataSciNotebooks/blob/d7d5317d8d6cb604795418dbfc41dc434dff95ab/R-Markdown-solutions/R6-times-and-timeseries.Rmd
### micro mol to g C formula: https://github.com/bgctw/REddyProc/blob/master/vignettes/useCase.md
### Date: 6/9/2021

getwd()
setwd("C:/Users/rbmahbub/Documents/RProjects/FluxdataR/30minECdata")

install.packages("datetime")

##Loading the required libraries
library(dplyr)
library(tidyr)
library(chron)
library(lubridate)
library(datetime)
library(ggplot2)
library(scales) 

## Reading the files
df<-read.csv("way3ushrcnorth2015.csv")

## See the column names
head(df)

#renaming the columns
colnames(df)[colnames(df) == "Way.3..2015"]<- "Date_DMY"
colnames(df)[colnames(df) == "GPP..umol.CO2..m2.s"]<- "GPP"
colnames(df)[colnames(df) == "Reco..umol.CO2..m2.s"]<- "Reco"
colnames(df)[colnames(df) == "LE_gf..W.m2"]<- "Lh"
colnames(df)[colnames(df) == "H_gf..W.m2"]<- "Sh"

## Inspecting the date column
typeof(df$Date_DMY)


#Splitting Date and Time (The date and time was together)
df<- df%>%
  separate(Date_DMY, c("Date", "Time"), " ")
View(df)

#converting the date to date format
df$Date<-as.Date(df$Date,
              format = "%m/%d/%Y")

View(df)
#Converting date to year, month and day columns
df<-df %>%
  dplyr::mutate(year = lubridate::year(Date), 
                month = lubridate::month(Date), 
                day = lubridate::day(Date))
View(df)
#Checking the Date and time columns
typeof(df$Date)
typeof(df$Time)

# Converting the Time column to time format
df$Time<-as.time(df$Time)

#Separating The Time column into hour and minute columns
df<- df%>%
  separate(Time, c("hour", "minute"), ":")

#Checking the class differences
class(df$year)
class(df$hour)

##The hour and minute was in character format and was converted to numeric format as like the date formats
df$hour<-as.numeric(df$hour)
df$minute<-as.numeric(df$minute)

# There was no second column we created a second column with zero values
# Creating a column of zero elements
df$second <- rep(0,nrow(df))
typeof(df$second)
class(df$second)

View(df)

#Our task is to take the various columns of date and time information and convert these to a vector of type *datetime*. Let's go step by step, we will start with a new character vector of date
date.char<-paste(df$year,df$month,df$day,sep = '-')
#do the same for time
time.char<-paste(df$hour,df$minute,df$second,sep = ':')
#combine the two

df$datetime<-as.POSIXct(paste(date.char,time.char,sep = ' '))

#Adding date as a separate column which would be useful later
df$date<-as.Date(date.char)
head(df$date)

## Daily mean
df.daily <- aggregate(GPP ~ date, df, mean)
head(df.daily)
View(df.daily)

## Converting umol/m2/s to gC/m2/day
df.daily$GPP<- (df.daily$GPP)* 1e-6*86400*12.011
class(df.daily$GPP)


## Plotting Time vs GPP
pdfway315_daily<-ggplot(df.daily,aes(x=date,y=GPP))+
  geom_point(size = 5)+ ylab(bquote('GPP ( '*g~ 'C'~ m^-2~day^-1*')')) +
  xlab("Date")+
  scale_x_date(breaks = date_breaks('30 day'),
               labels = date_format("%m-%d-%y"))+
  theme(text = element_text(size = 50))     

pdfway315_daily

##Save the figure
ggsave(pdfway315_daily, filename = "way3_2015_daily.png", width = 18, height = 12)

##Write the daily data in csv formats
write.csv(df.daily, file ="way3ushrcnorth2015daily.csv", row.names=FALSE)
