###########################################################################

# BIO144 BC UNIT 10

###########################################################################
###########################################################################

rm(list=ls())
library(ggplot2)
library(dplyr)
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

c_theme_bisq <- theme(legend.position="right",
                      text=element_text(size=12,
                                        family="serif")) +
  theme(panel.background = element_rect(fill = "blanchedalmond")) +
  theme(axis.line = element_line(colour = "black"))
c_theme_withe <- theme(legend.position="right",
                       text=element_text(size=12,
                                         family="serif")) +
  theme(panel.background = element_rect(fill = "white")) +
  theme(axis.line = element_line(colour = "black"))
c_theme_sky <- theme(legend.position="right",
                     text=element_text(size=12,
                                       family="serif")) +
  theme(panel.background = element_rect(fill = "aliceblue")) +
  theme(axis.line = element_line(colour = "black"))

###########################################################################

# CHAPTER 7 GETTING STARTED WITH R
# GETTING STARTED WITH GLM'S

# until now --> continous variables, positive and negative, fractions,
# normally distributed residuals, constant mean-variance relationship
# now --> not normal, counts, binary, integers --> GENERALIZED LM
# counts --> violate normality assumption, no constant mean-variance relationship
# binomial data --> probability of an event occurring depends on expl. variable.
# also violate normality and no constant var-mean relationship

# FAMILY --> probability distribution assumed to describe the response
# also called error structure. ex: poisson, binomial

# LINEAR PREDICTOR --> ex: an equation that describes how different predictors
# affect the expected value of the response.

# LINK FUNCTION --> describes the mathematical relationship between the expected
# value of the response and the linear predictor.

# general vs generalized linear model
# lm() and glm()

# POISSON GLM'S
# do bigger mums make more babies?
soay <- read.csv("~/Desktop/BIO144 Übungen/datasets_master/SoaySheepFitness.csv")
head(soay)
ggplot(soay) + aes(body.size,fitness) +
  geom_point() +
  geom_smooth(method='lm', se=FALSE) +
  geom_smooth(span=1,color='red', se=FALSE) +
  labs(x='Body mass [kg]', y='Lifetime fitness') +
  c_theme_sky
# red --> statistically more flexible (local regression)
# span controls how flexible the line is.
# both indicate a strong relationship between fitness and body size. 

# DOING IT THE WRONG WAY
wrong.model <- lm(fitness ~.,data=soay)
autoplot(wrong.model) + c_theme_sky
# residuals vs fitted --> clear pattern in the relationship, the U shape
# shows that the straight line model fails to account for the curvature...
# qq looks also bad. scale location --> positive relationship between absoulute size
# of the residuals and the fitted values. --> positive mean, variance relationship...
# large predicted fitness values are associated with more variation in the residuals.
# leverage looks ok. 

# POISSON DISTRIBUTION
# normal --> fractions, negative, symetric --> count data is not
# variance increases with mean...
# unbounded count data --> no upper limit that is known but they are all bounded at 0

# GLM
# general lm --> normality, glm --> other dist
# poisson, binomial, gamma (positive continous variables)
# --> wide range of response variables
# linear predictor --> y = intercept + slope * explanatory
# summary() shows estimates of intercept and slop in linear predictor
# called LINEAR bc we add up the components
# link function --> with wrong model we can get negative values and this
# make no sense in this case here --> need link function to correct this
# --
# WE MODEL A MATHEMATICAL TRANSFORMATION OF THE PREDICTION
# --
# the function that does this transformation is called link function
# for example --> natural log (always for standard glm with pois)
# linear predictor does not describe fitness directly, it relates the natural
# log of predicted fitness to bodysize. 
# then use exp() to get predicted value --> pred = exp(intercept+....)
# implies a exponential (not linear) relationship between variables...
# LINEAR MODEL MEANS NOT LINEAR RELATIONSHIP
# LINK FUNCTION --> ESTIMATE PARAMETERS OF A LINEAR PREDICTOR THAT BEHAVES
# CORRECTLY AND IT DOES SO BY MOVING US FROM THE RESPONSE SCALE TO THE
# TO THE SCALE OF THE LINEAR PREDICTOR.
# NATURAL LOG SCALE DEFINED BY THE LINK FUNCTION.
# TO DO EFFECTICE STATISTICS WE NEED TO BE UNBOUNDED. LINK FUNCTION DOES
# THIS. PREDICTOR AND RESPONSE CAN TAKE ANY VALUES THEY LIKE. 
# LOG IS THE RIGHT ONE HERE BECAUSE TAKES VALUES FROM POSITIVE NUMBERS 
# TO WHOLE NUMBER LINE. 

# fitting the right model:
soy.glm <- glm(fitness ~ body.size, data=soay,family = poisson(link = 'log'))
# log link function is canocial function for family poisson

autoplot(soy.glm) + c_theme_sky
# residuals vs fitted suggests that systemic part is now pretty good.
# no clear pattern, slight upward trend in the end
# normal qq id ok, much better that before..
# scale location slight positive relationship between the size of the
# residuals and the fitted values but not much going on
# leverage is fine

# R uses the standardized deviance residuals, specially transformed
# version of raw residuals. makes them normally distributed if right
# family is used in glm. 
# the plots are checking if poission is appropriate and scale location
# is checking if mean-variance relationship is ok. R adapts here.

anova(soy.glm)
# deviance closely related to likelihood
# LIKELIHOOD --> MEASURE OF HOW PROBABLE THE DATA WOULD BE IF REALLY
# PRODUCED BY THAT MODE!
# TOTAL DEVIANCE --> 85.081
# DEVIANCE EXPLAINED BY BODY SIZE --> 37.041

# to get p-values:
anova(soy.glm, test='Chisq')
# means not doing a chi square test!
# test statistic is X^2 value of 37.041 with 1 df

# P-VALUE --> LIKELIHOOD RATIO TEST! 
# DEVIANCE HAS DROPPED FROM 85 TO 48 WITHOUT BODYMASS EXPLANATORY VARIABLE!
# --> LOTS OF VARIATION EXPLAINED!

# THERE IS A SIGNIFICANT EFFEXT OF BODY MASS ON FITNESS!!!

summary(soy.glm)
# residuals are scaled, large numbers means outliers.
#  intercept and slope with standard error and z and p value
# NULL DEVIANCE --> measure of all the variation
# RESIDUAL DEVIANCE --> measure of what is left after fitting model
# big difference in those mean more variation explained...
# Fischer Scoring iterations --> R to find best model

# WHAT DO COEFFICIENTS TELL US?
# intercept is negative --> not what we are expecting...
# LINK FUNCTION --> GLM predicts the log of fitness!
# fitness = exp(-2.422 + 0.541 * 5) for 5kg sheep

# MAKE FINAL GRAPH GLM:
min.size <- min(soay$body.size)
max.size <- max(soay$body.size)
# now use expand grid --> use the same names!!
new.x <- expand.grid(body.size = seq(min.size,max.size,length=1000))
# now we can use predict() to predict the values for the new x:
# prediction are on the scale of the link function (same scale as linear predictor)
# predictions are log of predicted values
new.y <- predict(soy.glm,newdata=new.x,se.fit=TRUE) #se.fit --> std error bc option ci not available here
new.y <- data.frame(new.y)
# housekeeping to bring together
addThese <- data.frame(new.x,new.y)
addThese <- rename(addThese,fitness=fit)
# add confidence intervall
addThese <- mutate(addThese,
                   lwr = fitness - 1.96 * se.fit,
                   upr = fitness + 1.96 * se.fit)
# exponentiate the fitness and CI to get back to response scale
addThese <- mutate(addThese,
                   fitness = exp(fitness),
                   lwr = exp(lwr),
                   upr = exp(upr))
# plot
ggplot(soay) + aes(body.size,fitness) +
  geom_point(alpha=0.5) +
  geom_smooth(data=addThese,
              aes(ymin=lwr,ymax=upr), stat='identity',color='red',fill='bisque',alpha=0.5) +
  c_theme_sky

# CASES POISSON GLM IS NOT GOOD FOR COUNTS:
# OVERDISPERSION --> extra variation
# variance of poisson increases with the mean... 
# when something measure in the nature --> some variability that maybe cannot 
# accounted by the possion model...
# detect overdispersion
summary(soy.glm)
# look at residual deviance and degrees of freedom, they should be equal...
# dispersion parameter --> residuals deviance/residual df
# when greater than 2 --> worrying

# FIX IT
# change family to quasi
# switch to negative binomial family
#anova(test='F') needs to be used in quasi family

glm.nb() # for negative binomial
# link function also natural log

# DONT FIX OVERDISPERSION CAUSED BY NON-INDEPENDENCE
# --> use mixed model
# ZERO INFLATION --> SPECIFIC SOURCE OF OVERDISPERSION
# too many zeros to the number we expect
# use bar chart to detect it...
# 1) MIXED MODEL
# model with 2 distributions
# 2) HURDLE MODEL
# binary part for all zeros, possion part for non-zero
# asks wether runner makes hurdle (zero or not)

# SOMETIMES TRANSFORMING WORKS, SOMETIMES NOT.

# SUMMARY
# very specific assumption on the nature of the variability in data

###########################################################################

# CHAPTER 8 NEW STATISTICS WITH R BY HECTOR
# MAXIMUM LIKELIHOOD
# --> given data, specified model, maximum likelihood estimation finds the 
# values of the parameters of the statistiscal model that are most likely
# to reproduce the observed data
# maximum likelihood in simple linear regression is normal least squares
# --> interatice least squares for GLM
# maximum likelihood are interative and approximate
# --> calculations done with log-likelihood

# DEVIANCE --> generalized SS (sum of squares), comparing complex with 
# simplified model, DEVIANCE = 2 * difference log-likelihood of complex and simple model
# DEVIANCE follows chi square distribution df = params in model

# BOX-COX TRANSFORM
# --> uses maximum likelihood, tries out many transformations and asses which one
# produces the best fit to the data...
# raising data to a power (lambda) and vary it symetrically (lambda = 2 
# means squared data), lambda = 1 means untransformed
# lambda = 0 is natural log transformation
# boxbox() from MASS --> plots max likelihood with max likelihood based CI
# 95% CI produced by dropping 1.92 log-likelihood units from maximum
# 1.92 --> chi squared critical value with 1df at alpha = 0.05

# GLM
# linear predictor, variance function, a link function
# linar predictor in that after ~
# variance function models variation in the data --> normal least squares 
# uses normal distribution to model the residual variation
# glm is not restricted to normal distribution
# link function --> transform the predictions by the linear predictor
# 'identity' --> no transformation

# anova()
# maximum likelihood analysis of deviance (ANODEV)

# sometimes normal least squares fails to capture a 'curvature' 
# because of the assumption of equal variability around the regression line
# sometimes variance increases with the mean
# poisson for example is variance function
# link function models the mean

###########################################################################

# CHAPTER 9 NEW STATISTICS WITH R BY HECTOR
# 9.4 count data
# log link ensures values larger than 0

###########################################################################