######################
#Project: Lab 10 
# Purpose: Running Univariate and Bivariate Analyses in R
# Author: Ella Hook
# Edit Date: March 28 2024
# Data: lyrics data
###################
#load necessary packages#
library(dplyr)
library(tidyverse)
# set working directory using session tab
#open data set 
lyrics_data <- read.csv(file.choose(),header=T)
#look at data
lyrics_data
#see how R read the data and how data types were assigned 
summary(lyrics_data)
# take a "glimpse" at the data 
glimpse(lyrics_data)
# clean data using tidyverse and its piping function
lyrics_data.clean <- lyrics_data %>%
  mutate(Gender=as.factor(x=Gender),Condition=as.factor(x=Condition),Year=as.factor(x=Year),LyricsOnly=as.factor(x=LyricsOnly)) %>%
  mutate(Gender=recode(.x=Gender, "1"="Male", "2"="Female")) %>%
  mutate(Year=recode(.x=Year, "1"="Freshman", "2"="Sophmore", "3"="Junior", "4"="Senior")) %>%
  mutate(LyricsOnly=recode(.x=LyricsOnly, "1"="Heard Lyrics With or Without Music", "2"="Heard No Lyrics")) %>%
  mutate(Condition=recode(.x=Condition, "1"="Lose Yourself” by Eminem (complete song)", "2"="Instrumental-only version of “Lose Yourself” by Eminem", "3"="Audio recording of the lyrics from “Lose Yourself” by Eminem", "4"="Nothing")) %>%
  mutate(Pieces=as.numeric(x=Pieces)) %>%
  #variable selection 
  select(Condition, Pieces, Gender, Year, LyricsOnly)
#Create a new variable called Percent that reports the percent of pieces placed correctly
lyrics_data.clean$Percent <- (lyrics_data.clean$Pieces / 25) * 100
#check if variable is numeric
if (is.numeric(lyrics_data$percent)) {print("Percent variable is numeric.")} else {print("Percent variable is not numeric.")}
#install dscr
install.packages("descr")
#read into library
library(descr)
#run frequency table 
freq(x= lyrics_data.clean$Pieces, plot = FALSE)
freq(x= lyrics_data.clean$Percent, plot = FALSE)
# Run bivariate table for Pieces and Percent
table(lyrics_data.clean$Pieces, lyrics_data.clean$Percent) 
# Run mean, median, variance, sd, and range for Pieces and Percent
#Run for Pieces 
mean(lyrics_data.clean$Pieces, na.rm = TRUE)
median(lyrics_data.clean$Pieces, na.rm = TRUE)
var(lyrics_data.clean$Pieces, na.rm = TRUE)
sd(lyrics_data.clean$Pieces, na.rm = TRUE)
range(lyrics_data.clean$Pieces, na.rm = TRUE)
#For Percent
mean(lyrics_data.clean$Percent, na.rm = TRUE)
median(lyrics_data.clean$Percent, na.rm = TRUE)
var(lyrics_data.clean$Percent, na.rm = TRUE)
sd(lyrics_data.clean$Percent, na.rm = TRUE)
range(lyrics_data.clean$Percent, na.rm = TRUE)
#Run freq distributions for Condition, Gender, Year, LyricsOnly
freq(x= lyrics_data.clean$Condition, plot = FALSE)
freq(x= lyrics_data.clean$Gender, plot = FALSE)
freq(x= lyrics_data.clean$Year, plot = FALSE)
freq(x= lyrics_data.clean$LyricsOnly, plot = FALSE)
#Run means and Std. Dev. by for Pieces by Condition. Then run ANOVA and Interpret
#mean
lyrics_data.clean %>%
group_by(Condition)%>%
summarise(Pieces=mean(Pieces)) 
#standard deviation 
lyrics_data.clean %>%
group_by(Condition)%>%
summarise(Pieces=sd(Pieces)) 
# Run ANOVA test
Condition_Pieces_aov <- aov(Pieces ~ Condition, data = lyrics_data.clean)
# Summary of the analysis
summary(Condition_Pieces_aov)
# Run bivariate table for Pieces and Percent
table(lyrics_data.clean$Pieces, lyrics_data.clean$Percent) 
#Run means and Std. Dev. by for Pieces by Condition. Then run T-test and Interpret
#mean
lyrics_data.clean %>%
  group_by(LyricsOnly)%>%
  summarise(Pieces=mean(Pieces)) 
#standard deviation 
lyrics_data.clean %>%
  group_by(LyricsOnly)%>%
  summarise(Pieces=sd(Pieces)) 
# Run T-test
Condition_Pieces_ttest <- t.test(formula = lyrics_data.clean$Pieces ~
        
         lyrics_data.clean$LyricsOnly)
Condition_Pieces_ttest
#Run crosstab for Gender by Condition
source("http://pcwww.liv.ac.uk/~william/R/crosstab.r")
crosstab(lyrics_data.clean, row.vars = "Gender", col.vars = "Condition", type = "c")
#Run crosstab for Year by Condition
source("http://pcwww.liv.ac.uk/~william/R/crosstab.r")
crosstab(lyrics_data.clean, row.vars = "Year", col.vars = "Condition", type = "c")
#Save the File
write.csv(lyrics_data.clean,"lyricdata.csv", row.names = FALSE)


