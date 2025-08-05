## Runkle 2015 2016 2017 ameriflux growing season data
getwd()

#WAY4
data1<- read.csv("Runkle_Ameriflux_ECdata_201520162017/US-HRA_HH_201504121730_201508171100.csv")
data2<- read.csv("Runkle_Ameriflux_ECdata_201520162017/US-HRA_HH_201604252300_201609110730.csv")
data3<- read.csv("Runkle_Ameriflux_ECdata_201520162017/US-HRA_HH_201704080800_201708261230.csv")

data<-rbind(data1, data2, data3)

View(data)

##VPD (kPa)
## TA (degree celsius)
esat <- function(TA){  
  esatval <- 0.6108* (exp(17.27 * TA/(237.3 + TA)))
  return(esatval)
}

RHtoVPD <- function(RH, TA){
  esatval <- esat(TA)
  e <- (RH/100) * (esatval)/1000
  VPD <- (esatval - e)
  return(VPD)
}

data<-data %>% 
  mutate(VPD = RHtoVPD(data$RH_1_1_1, data$TA_1_1_1))

View(data)

data_no_NA<-data.dropna

data_no_NA <- c(data$TA_1_1_1, data$VPD)
View(data_no_NA)
