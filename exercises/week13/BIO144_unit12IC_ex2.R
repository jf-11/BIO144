###########################################################################
###########################################################################
###########################################################################
###########################################################################

# BIO144

###########################################################################
# UNIT 12 IC EXERCISE 2
###########################################################################

rm(list=ls())
library(ggplot2)
library(dplyr)
library(tidyr)
library(MASS)
library(readr)
library(MuMIn)
library(ggthemes)
library(latex2exp)
library(ggfortify)
library(GGally)
library(gridExtra)
library(coefplot)
library(venn)
library(ggiraph)
library(ggiraphExtra)
library(broom)
library(boot)
library(janitor)
library(lme4)
library(simex)
c_theme_sky <- theme(legend.position="right",
                     text=element_text(size=12,
                                       family="serif")) +
  theme(panel.background = element_rect(fill = "aliceblue")) +
  theme(axis.line = element_line(colour = "black"))

###########################################################################

# WORKFLOW --> PLOT --> MODEL --> CHECK ASSUMPTIONS --> INTERPRET --> PLOT

###########################################################################

# A POSTERIORI variable selection is suitable when the aim is to develop a 
# predictive model, but is not appropriate when one aims for explanation.
# Only A PRIORI variable selection is appropriate when one aims for 
# explanation.

# MODEL SELECTION BIO144 --> EVERYTHING ABOUT MODEL (TRANSFORMATIONS, 
# DISTRIBUTION ETC.)
# MODEL SELECTION SOMESTIMES --> ONLY SELECTION OF VARIABLES

###########################################################################

# little chance of a causal link here, even though the correlation is very strong.
# "correlation does not imply causation"
#  ("correlation implies an unknown causative structure") from a book

# research has to be scientifically valid and practical
# interpretation is important 
# LOOK BEYOUND EFFECT SIZES AND P-VALUES AND MAKE INFORMED JUDGEMENTS ABOUT
# WHAT YOU SEE!

###########################################################################

# OVERDISPERSION describes the observation that variation is higher than 
# would be expected.

# DEVIANCE is a measure of error; lower deviance means better fit to data.
# saturated model, proposed model, null model 
# NullDeviance=2(ll(SaturatedModel)−ll(NullModel))
# ResidualDeviance=2(ll(SaturatedModel)−ll(ProposedModel))

# TRANSFORMED OR GLM WITH LINK FUNCTION
# LINK FUNCTION --> log(E(y)) = ...
# TRANSFORMED ----> E(y_log) = ...
# log von E von y VS. E von logarithmiertem y

###########################################################################

# Count data 
# uses logit as link function
# # response can take two values --> 0 or 1...

###########################################################################
###########################################################################
###########################################################################

# QUESTION / TASK
# Visually explore the data. Is variation within individuals large 
#(or not) compared to variation among individuals?
#Before doing the next step, make a table with the columns: Response 
#variable, fixed effect, random effect, R model formulae, r function used 
#to make model, effect size (i.e. difference between female and male reaction 
#times) and 95% confidence interval of this effect size, number of observations, 
#number of degrees of freedom for error.

###########################################################################
#Workflow:
##1. Label your skript (title, name, date)
##2. Come up with your question and write down the null-hypothesis.
##3. Import your data
##4. Check wether your data has been imported correctly.
##5. Plot the data.
##6. Plot the data for a statistical model.
##7. Check the assumptions of that model par(mfrow), plot(m1).
##8. Look at the summary.
##9. Make a nice graph for communication.
##10. Write down method and result section.
###########################################################################

# DATA 
data <- read.csv("~/Desktop/My reaction time (2021) - Form responses 1 (1).csv")

###########################################################################

# tidying data according to the instruction on the webpage
names(data) <- c("Timestamp", "ID", "Gender", "Pref_Reaction_time_1",
               "Verbal_memory_score", "Number_memory_score",
               "Visual_memory_score",
               "Weight_kgs", "Handed", "Nonpref_Reaction_time_ave",  
               "Pref_Reaction_time_2", "Pref_Reaction_time_3",  
               "Pref_Reaction_time_4", "Pref_Reaction_time_5",
               "Pref_Reaction_time_ave", "Random_number")
dd_filtered <- data %>%
  
  filter(Pref_Reaction_time_ave > 50,
         
         Pref_Reaction_time_ave < 500)

head(dd_filtered)

###########################################################################
###########################################################################

# CHECKBOX1
# Now make a scatterplot with the (mean) preferred reaction time on the x 
# axis and the non-preferred reaction time (average) on the y-axis 
# (colour these points blue).

plot <- ggplot(dd_filtered) + aes(Pref_Reaction_time_ave,Nonpref_Reaction_time_ave) +
  geom_point(color='blue',alpha=0.5) +
  c_theme_sky
plot

# Add to the same plot the points where the x-axis shows the preferred 
# reaction time 1 (only from the first measurement), using the non-preferred 
# reaction time on the y axis (as before). Color these added points in red.

final_plot <- plot + 
  geom_point(aes(Pref_Reaction_time_1,Nonpref_Reaction_time_ave),color='red',alpha=0.5)
final_plot

# What does the plot show and why?
# all answered correctly
# also the plot is correctly made

# Explanation
# Using only one measurement leads to more measurement error than when using 
# the average of multiple measurements. Therefore, a hypothetical slope is less 
# steep (thus attenuated) with respect to the slope where averages were taken 
# (i.e., where less error is present).

# USING ONLY ONE MEASUREMENT --> MORE MEASUREMENT ERROR THAN AVERAGE
# SLOPE IS LESS STEEP --> UNDERESTIMATED

###########################################################################

# Fit a linear regression model of Nonpref_Reaction_time_ave (y) against 
# Pref_Reaction_time_ave (x). For later use, please add x=TRUE as option 
# to the lm function.

m1 <- lm(Nonpref_Reaction_time_ave~Pref_Reaction_time_ave,dd_filtered,x=TRUE)

###########################################################################

# What is the slope for the preferred reaction time (two decimal digits)?
summary(m1) # 0.77

###########################################################################

# What is the intercept of the regression?
summary(m1) # 67.34

###########################################################################

# the data has been measured with error, look at variation between the 
# different values for the reaction times. this affects the slope. (look
# at the graph above). ATTENTION also mean contains measurement error.

# TIME FOR AN ERROR MODELLING STEP

# we need to estimate error variance with a simple classic error model:
# w = x + u where u ~ N(0,sigma^2)

# we have repeated measurement here.

# we get error variance according to the webpage:
temp <- dplyr::select(dd_filtered,
               
               Pref_Reaction_time_1,
               
               Pref_Reaction_time_2,
               
               Pref_Reaction_time_3,
               
               Pref_Reaction_time_4,
               
               Pref_Reaction_time_5)

error_var <- sum((temp - rowMeans(temp))^2) / (nrow(temp)*4) / 5

###########################################################################

# Now use the simex procedure for error modelling! To this end, install 
# the simex package in R and plug the naive regression output and the estimted 
# error variance into the simex function. You might check slide 18 from lecture 12.1.

set.seed(123)
r.simex <- simex(m1,
                 SIMEXvariable = 'Pref_Reaction_time_ave',
                 measurement.error=sqrt(error_var),
                 lambda=seq(0.1,2,0.1),
                 B=50) # that is, the error increases from +0.1 to +2.0 times the error variance, 
# and at each error level the simulation is repeated B=50 times.
summary(r.simex)$coef

###########################################################################

# QUESTIONS

# What is the error-corrected slope (two digits after decimal point)?
# 0.85
# 43.03

# Explanation
# As expected, the slope estimate is now corrected upwards!
#Interpretation: without error correction, the slope is attenuated (=underestimated in magnitued).
# NOW WE HAVE AN UPWARDS CORRECTED SLOPE

###########################################################################

# Finally, plot the simex results by using the plot() function, e.g.,

plot(r.simex)

# You should see the expected trend when the error increases and the extrapolation to no error.

###########################################################################



