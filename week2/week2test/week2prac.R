install.packages("here")
install.packages("tidyverse")
install.packages("janitor")
install.packages("plotly")
install.packages("maptools")
install.packages(c("classInt", "tmap"))
install.packages(c("RColorBrewer", "sp", "rgeos", 
                   "tmaptools", "sf", "downloader", "rgdal", 
                   "geojsonio"))
library(plotly)
library(janitor)
library(here)
library(tidyverse)

LondonDataOSK<- read.csv("D:/gis/week2/ward-profiles-excel-version.csv", 
                         header = TRUE, 
                         sep = ",",  
                         encoding = "latin1")
LondonData <- read_csv("https://files.datapress.com/london/dataset/ward-profiles-and-atlas/2015-09-24T14:21:24/ward-profiles-excel-version.csv",
                       locale = locale(encoding = "latin1"),
                       na = "n/a")
class(LondonData)
class(LondonDataOSK)

Datatypelist <- LondonData %>%
  select(1:10)%>%
  summarise_all(class) %>%
  pivot_longer(everything(), 
               names_to="All_variables", 
               values_to="Variable_class")


LondonData <- edit(LondonData)
LondonData%>%
  colnames()%>%
  # just look at the head, top5
  head()
LondonBoroughs<-LondonData[626:658,]
Femalelifeexp<- LondonData %>% 
  filter(`Female life expectancy -2009-13`>90)
LondonBoroughs<- LondonData %>% 
  filter(str_detect(`New code`, "^E09"))
LondonBoroughs$`New code`
LondonBoroughs$`Ward name`
LondonBoroughs<-LondonBoroughs %>%
  distinct()
#select columns 1,19,20 and 21
LondonBoroughs_manualcols<-LondonBoroughs[,c(1,19,20,21)]
LondonBoroughs_dplyrcols<-LondonBoroughs %>%
  dplyr::select(c(1,19,20,21))
LondonBoroughs_contains<-LondonBoroughs %>% 
  dplyr::select(contains("expectancy"), 
                contains("obese - 2011/12 to 2013/14"),
                contains("Ward name")) 

LondonBoroughs <- LondonBoroughs %>%
  dplyr::rename(Borough=`Ward name`)%>%
  clean_names()
LondonBoroughs <- LondonBoroughs %>%
  clean_names()

LondonBoroughs %>%
  dplyr::select(female_life_expectancy2009_13)


Life_expectancy <- LondonBoroughs %>% 
  #new column with average of male and female life expectancy
  mutate(averagelifeexpectancy= (female_life_expectancy2009_13 +
                                   male_life_expectancy2009_13)/2) %>%

mutate(normalisedlifeepectancy= averagelifeexpectancy /
         mean(averagelifeexpectancy))%>%

  #select only columns we want
  dplyr::select(new_code,
                borough,
                averagelifeexpectancy, 
                normalisedlifeepectancy)%>%
  #arrange in descending order
  #ascending is the default and would be
  #arrange(normalisedlifeepectancy)
  arrange(desc(normalisedlifeepectancy))
slice_head(Life_expectancy, n=5)
slice_tail(Life_expectancy,n=5)
  
Life_expectancy2 <- Life_expectancy %>%
  mutate(UKcompare = case_when(averagelifeexpectancy>81.16 ~ "above UK average",
                               TRUE ~ "below UK average"))
Life_expectancy2

Life_expectancy2_group <- Life_expectancy2 %>%
  mutate(UKdiff = averagelifeexpectancy-81.16) %>%
  group_by(UKcompare)%>%
  summarise(range=max(UKdiff)-min(UKdiff), count=n(), Average=mean(UKdiff))
Life_expectancy2_group 
Life_expectancy3 <- Life_expectancy %>%
  mutate(UKdiff = averagelifeexpectancy-81.16)%>%
  mutate(across(where(is.numeric),round,3))%>%
  mutate(across(UKdiff, round,0))

Life_expectancy3 <- Life_expectancy %>%
  mutate(UKdiff = averagelifeexpectancy-81.16)%>%
  mutate(across(where(is.numeric), round, 3))%>%
  mutate(across(UKdiff, round, 0))%>%
  mutate(UKcompare = case_when(averagelifeexpectancy >= 81 ~ 
                                 str_c("equal or above UK average by",
                                       UKdiff, 
                                       "years", 
                                       sep=" "), 
                               TRUE ~ str_c("below UK average by",
                                            UKdiff,
                                            "years",
                                            sep=" ")))%>%
  group_by(UKcompare)%>%
  summarise(count=n())

plot_ly(LondonBoroughs, 
        #data for x axis
        x = ~male_life_expectancy2009_13, 
        #data for y axis
        y = ~percent_children_in_reception_year_who_are_obese2011_12to2013_14, 
        #attribute to display when hovering 
        text = ~borough, 
        type = "scatter", 
        mode = "markers")
plot(LondonBoroughs$male_life_expectancy2009_13,
     LondonBoroughs$percent_children_in_reception_year_who_are_obese_2011_12to2013_14)