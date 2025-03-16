###########################################
# Project: Week 11 Lab- DATA CLEANING & REGRESSIONS IN R
# Purpose: Cleaning + Regressions in R
# Author: Ella Hook
# Edit date: March 3rd, 2024
# Data: Michigan Survey
###########################################
# CLEAN DATA -------------------------------------------------------

# set working directory so that R knows which folder your file is in

# read in dataset and save in object 'Michigan.Survey'
Michigan.Survey <- read.csv('/Users/ellahook/Downloads/Lab 11 Assignment Data (1).csv', header=TRUE)

# install 'car' package to get ready to recode variables
# USE THIS COMMAND FOR THE FIRST TIME: 
install.packages('car') # run this only once
# then load the package using library()
library(car)

# Define a new variable in “Michigan.Survey” called “gender”, and use recode() to
#change the values of CD1 (gender) and store it in your new variable. Change 1s to
# male and 2s to female
Michigan.Survey$Gender <- recode(Michigan.Survey$CD1, "1='Male'; 2='Female'")
# Using table(), check that CD1 and gender successfully recoded
table(Michigan.Survey$Gender)
table(Michigan.Survey$CD1)
#Using class(), check the data type of CD1 and gender
class(Michigan.Survey$CD1)
class(Michigan.Survey$Gender)
##How did recoding CD1 to gender change the data type, and why?
##recoding CD1 to gender changed the data type from integer to character as I recoded the initial numeric data points to descrptive ones (charcter),I changed 1 to male and 2 to female in the dataset for the variable Gender.
#Use prop.table() to find the proportion of respondents by gender
# use table() to get counts
table(Michigan.Survey$Gender)
# use prop.table() to get proportions
table1<- table(Michigan.Survey$Gender) 
prop1<- prop.table(table1)
prop1
#Use summary() to find summary statistics for age of respondents
summary(Michigan.Survey$age)
#Write 2-3 sentences summarizing what you see in part (a) and (b)
##The proportions of respondents by gender, are approximately 46.63% female respondents and 53.37% male respondents. Additionally the age distribution of the respondents showed a  minimum age of 18, median age of 57, mean age of 54.22, and a maximum age of 94.
#Run a linear regression to predict someone’s beliefs about the race-wage gap based on
#someone’s age and gender. The regression should have inclusion6 as the dependent variable,
#and age and gender as the two independent variables, and you should store it in a new object
#called “lm”. Run summary() on your linear regression
lm<- lm(inclusion6 ~ age + Gender, data = Michigan.Survey)
summary(lm)
##nterpret your linear regression results in one paragraph. Include:
#Is the model statistically significant overall, and what does this tell us? Are the
#variables statistically significant? How do gender and age affect someone’s
#beliefs about what explains the race-wage gap?
##The linear regression model is statistically significant overall, as indicated by a highly significant 
#F-statistic (p-value: 7.136e-09), suggesting that the model explains a significant portion of the variability
#in the responses to Diversity - Race Wage Gap (inclusion6). 
#Both age and gender are statistically significant predictors of respondents' beliefs. 
#Specifically, for every one-unit increase in age, the belief that job skills predominantly explain the 
#race-wage gap decreases by approximately 0.011 units, holding other variables constant. Similarly, being male is 
# associated with a decrease in the belief that job skills explain the race-wage gap compared to being female. This suggests that as individuals age, they may 
# attribute the wage gap less to job skills and more to discrimination. Additionally, males may be less likely than females to attribute the wage gap to job skills. 
# These findings provide insights into how demographic factors like age and gender influence perceptions regarding the causes of the race-wage gap, with implications for understanding attitudes towards equity and 
#discrimination in the labor market.

