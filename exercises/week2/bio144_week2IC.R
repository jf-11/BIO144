########################################################################

# BIO144
# WEEK 2 IC

########################################################################
# EXERCISE 1
########################################################################

rm(list=ls()) # clean session
library(tidyverse)

########################################################################

df <- read.delim("~/Desktop/BIO144 Übungen/datasets_master/bodyfat.txt")
str(df)

########################################################################

#Make a graph that allows you to see the frequency distribution of the response variable (bodyfat).
#That is the distribution of values of body fat of the individual people in the study.
#Hint: use the ggplot function and the geom_histogram function. Do you think the values of bodyfat are normally distributed?

df %>% 
  ggplot(aes(bodyfat)) +
  geom_histogram(bins=30,fill='lightblue',color='black')

# Its quite normally distributed, but there are a couple of rather high values.
# The mean and median are very similar.
mean(df$bodyfat)
median(df$bodyfat)

#Use the filter function from the dplyr package to show the individuals with bodyfat greater than 35.
#How many individuals have body fat greater than 35?

bodyfat_over_35 <- filter(df,bodyfat > 35)
bodyfat_over_35

#4 individuals have a bodyfat greater than 35.

#When we identify a potentially problematic value in the response variable, 
#we might want to look at that particular row. There are a few ways to do this in R / RStudio. 
#However you find easiest, find the bmi of the individual with the highest value of bodyfat and type it in below:

highest_bodyfat <- df %>% 
  filter(bodyfat == max(bodyfat))
bmi_of_highest_bodyfat <- highest_bodyfat['bmi']
bmi_of_highest_bodyfat

#We have so far focused on looking at the distribution of the response variable (bodyfat in this dataset). 
#We should also, however, look at the distribution of the other variables, for example to check if there are 
#any rather unusal / extreme values, and whether a transformation might be in order.
#Look at the distrbution of all the other variables. Which of the ones listed below seem to have at least one 
#rather extreme / unusual value?
  
#Hint: Use your current knowledge of R to make all the histograms; 
#probably this means copy and paste the code for one, and change the variable name. 
#There is a quicker way, that you'll see in the explanation text you will get when you answer the question.

ggplot(gather(df, key=variable, value=value), aes(x=value)) + geom_histogram(bins=20) + facet_wrap(~variable, scales="free")
?gather()
gather(df,key=variable,value=value)

#We've seen that several of the variables contain rather extreme / unusual values. 
#An important question now, is whether it is the same few individuals (rows) that have 
#the extreme values for all these variables, or whether its different individuals.

extremes <- slice(df,-c(1:252)) #generate empty data frame

for (i in colnames(df)) {
  print(filter(df,df[i]==max(df[i])))
}

#The dataset contains a variable called bmi. This is body mass index. We can calculate this ourselves, 
#from two other variables in the dataset.
#Find the formula for calculating bmi, and add your own calculation of bmi to the dataset.

# Formula: gewicht/grösse^2

df <- mutate(df,bmiself = (gewicht)/((hoehe/100)^2))

#Make a copy of the data that doesn't include the individuals in the correct answer to the previous question.
# 31, 39, 42, 86 

df_copy <- slice(df,-c(31,39,42,86))
df_coppy <- filter(df, !(Nr %in% c(31,39,42,86)))

#Looking at relationships among variables

library(GGally) # needed to use ggpairs(), same in base R as pairs(), generates pair plots
ggpairs(df_copy,columns = c('bodyfat','weight','height','bmi')) # you can specify if you want all colums and arrange them
pairs(df_copy)

########################################################################
# EXERCISE 2
########################################################################


