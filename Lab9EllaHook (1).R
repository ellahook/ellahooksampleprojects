######################
#Project: Lab 9 
# Purpose: INTRO TO R & RSTUDIO
# Author: Ella Hook
# Edit Date: March 18 2024
# Data: music data
###################
# Install the tidyverse, dplyr, and janitor packages using menu options tools ---> Install packages 
#load tidyverse, dplyr, and janitor packages 
library(package="tidyverse")
library(package="dplyr")
library(package="janitor")
#read in data package
music.data <- read.csv(file.choose(),header=T)
#look at data
music.data
#see how R read the data and how data types were assigned 
summary(music.data)
# take a "glimpse" at the data 
glimpse(music.data)
# clean data using tidyverse and its piping function
musicdata.clean <- music.data %>%
rename(Gender=Sex, Country=Country.Music, Hours=Hours.per.day.listening.to.Music, Platform= Favorite.Music.Streaming.Platform) %>%
mutate(Gender=as.factor(x=Gender),Platform=as.factor(x=Platform)) %>%
mutate(Gender=recode(.x=Gender, "1"="Male", "2"="Female")) %>%
mutate(Platform= na_if(Platform,"Not Sure")) %>%
select(Gender, Country, Hours, Platform, Concert) 
# look at summary of cleaned data 
summary(musicdata.clean)
glimpse(musicdata.clean)
# I am now going to use Base R to do data cleaning on the variable Concert.
#Base-R is the basic software which contains the R programming language.
# Base R is different than Tidy Verse in the fact that Base-R refers to all the 
#functionality that comes built into the R programming language.
#The tidyverse is a collection of packages that add onto R, and you must install this package. 
#Using Base R to change Concert to be a factor instead of a character
musicdata.clean$Concert <-as.factor(musicdata.clean$Concert)
#Using Base R to set the value “not sure” as Null
musicdata.clean$Concert <-recode(musicdata.clean$Concert, "Not Sure"= NULL, "y"="yes", "n"= "no")
# look at summary of cleaned data (redone)
summary(musicdata.clean)
glimpse(musicdata.clean)
#saved reduced data frame as a csv
write.csv(musicdata.clean,"MDcleanedHook.csv", row.names
          = FALSE)

