getwd()
B<- 1+5
B
ls()

library(tidyverse)
library(here)
###########################################################
#create some datasets, first a vector of 1-100 and 101-200
Data1 <- c(1:100)
Data2 <- c(101:200)
#Plot the data
plot(Data1, Data2, col="red")
Data3 <- rnorm(100, mean = 53, sd=34)
Data4 <- rnorm(100, mean = 64, sd=14)
#plot
plot(Data3, Data4, col="blue")
df <- data.frame(Data1, Data2)
plot(df, col="green")
head(df)
tail(df)
df %>% 
  select(column1)
LondonDataOSK<- read.csv("D:/gis/week2ward-profiles-excel-version.csv", 
                         +                          header = TRUE, 
                         +                          sep = ",",  
                         +                          encoding = "latin1")
LondonData <- read_csv("https://files.datapress.com/london/dataset/ward-profiles-and-atlas/2015-09-24T14:21:24/ward-profiles-excel-version.csv", 
                       locale = locale(encoding = "latin1"))
class(LondonData)
class(LondonDataOSK)
LondonData %>% 
  summarise_all(class)