########################################################################

# BIO144

########################################################################
# WEEK 2 BC
########################################################################

rm(list=ls()) # clean session
library(dplyr)
library(ggplot2)

####### Look at the dataset #######

# names()
# head()
# dim()
# str()
# tbl_df()
# glimpse()

####### 3.1.2 summary() function #######

compensation <- read.csv("~/Desktop/BIO144 Übungen/datasets_master/compensation.csv")
summary(compensation)
View(compensation)

####### 3.2 dplyr #######

# select() for selecting cols
# slice() for selecting rows
# filter() conditional selecting
# arrange() for sorting rows
# mutate() for creating new variables in the data frame

# !! FIRST ARGUMENT FOR ALL DPLYR FUNCTIONS IS THE DATA FRAME !!

####### 3.3.1 select() #######

select(compensation,Fruit)
# ALL DPLYR FUNCTIONS CAN ONLY DO ONE THNG!
select(compensation,-Root) # take all cols except one
select(compensation,c(Root,Fruit)) # take specific

####### 3.3.2 slice() #######

slice(compensation,2:10) # from 2 to 10
slice(compensation,c(2,3,10)) # uncontinious rows

####### 3.3.3 filter() #######

with(compensation, Fruit > 80) # look at data frame, then do whats next and stop looking
filter(compensation, Fruit > 80) # | or   & and
filter(compensation, Fruit > 80 | Fruit < 20)

####### 3.4.1 mutate() #######

compensation <- mutate(compensation,logFruit = log(Fruit))
head(compensation)

####### 3.5.1 arrange() #######

arrange(compensation,Fruit) # sorts fruit by ascending values

####### 3.6 combining functions #######

select(filter(compensation, Fruit > 80), Root) # root of those fruit is higher than 80

####### magrittr #######

# %>% pipe method, ctrl + shift + m

compensation %>%
  filter(Fruit > 80)
    select(Root)
    
####### 3.7 group_by() and summarise() #######
    
summarise(
  group_by(compensation,Grazing),
  meanFruit = mean(Fruit))
    
compensation %>%
  group_by(Grazing) %>%
  summarise(meanFruit=mean(Fruit))

compensation %>% 
  group_by(Grazing) %>% 
  summarise(
    meanFruit = mean(Fruit),
    sdFruit = sd(Fruit))

####### Appendix 3A #######
# base R vs dplyr

# subset()
# order() 
# transform()
compensation[1:5,c(1:2)] # rows and cols
compensation[1:5,c('Root','Fruit')]
compensation[compensation$Fruit>80,]
compensation$logFruitt <- log(compensation$Fruit)

####### Appendix 3B #######

# join() merge datasets together
compensation_mean_centered <- compensation %>% 
  group_by(Grazing) %>% 
  mutate(Fruit_minus_mean = Fruit - mean(Fruit))

library(broom)
compensation_lms <- compensation %>% 
  group_by(Grazing) %>% 
  do(tidy(lm(Fruit ~ Root, data = .)))
compensation_lms

####### 4 basic visualizations #######

library(ggthemes)

ggplot(data=compensation, aes(Root,Fruit,color=Grazing,shape=Grazing)) + geom_point(size=2) +
  labs(x='Root Biomass',y='Fruit Production')

ggplot(compensation,aes(x=Grazing,y=Fruit)) + geom_boxplot() + geom_point(alpha=0.5,color='lightblue')

ggplot(compensation,aes(Fruit)) + geom_histogram(binwidth = 15)

ggplot(compensation,aes(Fruit)) + geom_histogram(binwidth = 15) + facet_wrap(~Grazing)


    
########################################################################
# WEEK 2 IC
########################################################################





########################################################################