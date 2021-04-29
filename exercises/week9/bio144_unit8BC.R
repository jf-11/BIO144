###########################################################################

# BIO144 BC UNIT 8

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

# A POSTERIORI variable selection is suitable when the aim is to develop a 
# predictive model, but is not appropriate when one aims for explanation.
# Only A PRIORI variable selection is appropriate when one aims for 
# explanation.

# MODEL SELECTION BIO144 --> EVERYTHING ABOUT MODEL (TRANSFORMATIONS, 
# DISTRIBUTION ETC.)
# MODEL SELECTION SOMESTIMES --> ONLY SELECTION OF VARIABLES

###########################################################################

# in this data it is clear we are not seeking to EXPLAIN, all we do is 
# trying to measure easy things and PREDICT something that is more
# difficult --> bodyfat
# we havent done a a priori selection, we do a posterior selection

###########################################################################

# import data
dd <- read.delim("~/Desktop/BIO144 Übungen/datasets_master/bodyfat.txt")

# tell R to fail if NAs
options(na.action="na.fail")

###########################################################################

# only keeping desired variables
dd <- dplyr::select(dd, bodyfat, age, weight, height, bmi, neck, chest, 
                    abdomen, hip, thigh, knee, ankle, biceps)

###########################################################################

# start with full model
m1 <- lm(bodyfat ~ .,data = dd) # . means all other variables

# AIC m1
AICc(m1)

dropterm(m1, sorted = TRUE) # in MASS package, take smodel and then fits
# new model with one of the terms removed and saves it. On the left we 
# see which variable is missing and on top we see the full model. Does not
# return corrected (AICc) AIC.

# remoove knee bc least explanation
dt1 <- update(m1, . ~ . -knee)
AICc(dt1) # reduced by 2, so better model bc AIC is lower

# look at dropterm again
dropterm(dt1, sorted = TRUE)

dt2 <- update(dt1, . ~ . -age)
AICc(dt2)

dropterm(dt2, sorted = TRUE)

dt3 <- update(dt2, . ~ . -biceps)
AICc(dt3)

dropterm(dt3, sorted = TRUE)

dt4 <- update(dt3, . ~ . -chest)
AICc(dt4)

dropterm(dt4, sorted = TRUE)

dt5 <- update(dt4, . ~ . -ankle)
AICc(dt5)

dropterm(dt5, sorted = TRUE)

dt6 <- update(dt5, . ~ . -height)
AICc(dt6)

dropterm(dt6, sorted = TRUE)

dt7 <- update(dt6, . ~ . -bmi)
AICc(dt7)

dropterm(dt7, sorted = TRUE)

dt8 <- update(dt7, . ~ . -hip)
AICc(dt8) # AIC has gone up

dropterm(dt8, sorted = TRUE)

dt9 <- update(dt8, . ~ . -thigh)
AICc(dt9) # again up

dropterm(dt9, sorted = TRUE)

dt10 <- update(dt9, . ~ . -neck)
AICc(dt10) # up again

dropterm(dt10, sorted = TRUE)

dt11 <- update(dt10, . ~ . -weight)
AICc(dt11) # huge up

dropterm(dt11, sorted = TRUE)

dt12 <- update(dt11, . ~ . -abdomen)
AICc(dt12) # big big increase

dropterm(dt12, sorted = TRUE)

###########################################################################

# compare mdoels
mods <- list(m1=m1,dt1=dt1,dt2=dt2,dt3=dt3,dt4=dt4,dt5=dt5,dt6=dt6,dt7=dt7,
             dt8=dt8,dt9=dt9,dt10=dt10,dt11=dt11,dt12=dt12)
model.sel(mods) # delta is difference in AIC to best AIC

###########################################################################

# let computer do that for us
fit1 <- m1 # full model (most complex mdoel)
fit2 <- lm(bodyfat ~ 1, data = dd) # only intercept (easiest model)

step_forward <- stepAIC(fit2,direction='forward', 
                        scope=list(upper=fit1, lower=fit2))

step_backward <- stepAIC(m1,direction='backward', 
                         scope=list(upper=fit1, lower=fit2))

step_both <- stepAIC(m1,direction='both', 
                     scope=list(upper=fit1, lower=fit2))

mods <- list(step_forward=step_forward, step_backward=step_backward,
             stept_both=step_both)
model.sel(mods)

###########################################################################

s1 <- dredge(m1)
model.sel(get.models(s1, subset=delta < 1))

best_model_dredge <- get.models(s1, subset=delta == 0)
model.sel(best_model_dredge)

mods <- list(m1=m1,dt10=dt10, sf = step_forward, sba= step_backward, 
             sbo = step_both, best_dredge = best_model_dredge[[1]])

model.sel(mods)
###########################################################################

# look how good the model predictions are
p1 <- data.frame(dt10=predict(dt10),
                 dt11=predict(dt11))

dd1 <- cbind(dd,p1)


plot1 <- ggplot(dd1) +
  aes(dt10, bodyfat) +
  geom_point() +
  c_theme_darksky

plot2 <- ggplot(dd1) +
  aes(dt11, bodyfat) +
  geom_point() +
  c_theme_darksky

grid.arrange(plot1,plot2,nrow=1)

# the second plot is a less good model...
# such a plot is really useful!! 
# bigger spread means less good prediction. 

###########################################################################

# The most famous thing in statistics that scientists haven't heard about
# (Video)



###########################################################################