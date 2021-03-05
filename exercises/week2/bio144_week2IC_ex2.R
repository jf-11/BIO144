########################################################################

# BIO144
# WEEK 2 IC

########################################################################
# EXERCISE 2
#A particularly interesting and important question one can delve into 
#with this dataset is if there is a relationship between health care 
#expenditure and child mortality.
########################################################################
rm(list=ls()) # clean session
library(tidyverse)
########################################################################

##### importing data #####

financing_healthcare <- read.csv("~/Desktop/BIO144 Übungen/datasets_master/financing_healthcare.csv")

##### Q1 how many countries are in dataset? #####

glimpse(financing_healthcare$country)
summarise(financing_healthcare, countries=length(unique(country)))

##### Q2 how many years are in dataset? #####

summarise(financing_healthcare, years=length(unique(year)))

##### Q3 How many rows have a non "NA" value 
#for both of the two variables we're most interested in: 
#health_exp_total and child_mort? #####

new_df_non_na <- financing_healthcare %>% 
  filter((!is.na(health_exp_total))&(!is.na(child_mort)))
View(new_df_non_na)
nrow(new_df_non_na)


#Make a dataset:
#containing only observations from 2013 (hint, use the filter function in the dplyr package)
#that contains only these variables: year, country, continent, health_exp_total, child_mort, 
#life_expectancy (hint: use the select function in the dplyr package)
#contains no missing values (hint: use the drop_na() function in the tidyr package)
#Your new dataset should have 178 rows and 6 variables.

library(tidyr)

shortened_data <- financing_healthcare %>% 
  filter(year==2013) %>% 
  select(c('year','country','continent','health_exp_total','child_mort','life_expectancy')) %>% 
  drop_na()
str(shortened_data)

##### Q4 we want to know the average and standard deviation 
#of the child mortality in each continent #####

average_child_mort <- shortened_data %>% 
  group_by(continent) %>% 
  summarise(average = mean(child_mort))

sd_child_mort <- shortened_data %>% 
  group_by(continent) %>% 
  summarise(sd = sd(child_mort))

average_child_mort
sd_child_mort

#Now make a box and whisker plot of child mortality (y-axis / response variable) 
#for each continent (x-axis / explanatory variable).
#Hint: work with the dataset containing the 178 observations. 
#Do not use the averages you just calculated.
#Hint: Use ggplot and geom_boxplot.

# Box-Plot

shortened_data %>% 
  ggplot() + 
  aes(x=continent,y=child_mort,color=continent) + 
  geom_boxplot() +
  labs(title='Boxplot of child mortality',y='Continent',x='Child Mortality')

##### Q5 Looking at your boxplot, which of these are reasonably correct? #####

# Visualization
#For a little practice with graphing, make some bivariate scatterplots of child 
#mortality on the y-axis and total health care expenditure on the x-axis.
#In one visualisation, assign colours to the points corresponding to their continent. 
#(I.e. map the colour aesthetic to the levels in the continent variable.) 
#Hint: use ggplot, because it will automatically make a nice key, showing which continents are which colour.
#In another, make a separate graph (i.e. facet) for each continent. 
#Hint: use ggplot because then its real easy! 
#Hint: the ggplot default is to make the axes limits the same on all facets, 
#but we have quite different data ranges in different continents. 
#Using the help for facet_wrap, see if you can work out how to make the axes 
#limits free to vary among the facets.

p1 <- shortened_data %>% 
  ggplot() +
  aes(x=health_exp_total,y=child_mort,color=continent) +
  geom_point()

p2 <- shortened_data %>% 
  ggplot() +
  aes(x=health_exp_total,y=child_mort,color=continent) +
  geom_point() +
  facet_wrap(vars(continent),scales='free')

p1
p2

##### Q6 When you made the axes limits free to vary 
#among the facets, you get a better view of the relationship 
#between child mortality and healthcare spending. 
#Which of these do you think is most appropriate, given what you see? #####

