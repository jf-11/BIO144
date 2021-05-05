###########################################################################
###########################################################################
###########################################################################
###########################################################################

# BIO144

###########################################################################
# UNIT 9 IC EXERCISE 3
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
###########################################################################
###########################################################################
###########################################################################

# Question --> 

###########################################################################

# DATA 
link <- "https://raw.githubusercontent.com/opetchey/BIO144/master/3_datasets/bodyfat.txt"
bodyfat <- readr::read_delim(link, delim="\t", escape_double = FALSE, trim_ws = TRUE)

###########################################################################

# RESCALING VARIABLES

#first we have to adapt the dataset:
bodyfat <- bodyfat %>% 
  mutate(scaled_abdomen=scale(abdomen), 
         scaled_weight=scale(weight),
         scaled_height=scale(height),
         scaled_ankle=scale(ankle),
         scaled_wrist=scale(wrist))

#fit the model
linear_model <- lm(bodyfat~scaled_abdomen+scaled_weight+scaled_height+scaled_ankle+scaled_wrist,
                   data=bodyfat)

###########################################################################

# REBUILD THE PLOT
tidy(linear_model)
head(augment(linear_model))
glance(linear_model)

# coeff plot
tidy_data <- tidy(linear_model, conf.int = TRUE)
ggplot(tidy_data, aes(estimate, term, color = term)) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high))+
  geom_vline(xintercept = 0,color='red',alpha=0.2)+
  c_theme_sky

coefplot(linear_model)
###########################################################################

# from solution
tidy_data %>%
  filter(term != "(Intercept)") %>%
  ggplot(aes(x = estimate, y = term)) + 
  geom_vline(xintercept = 0, col = "grey") +
  geom_point() +
  geom_errorbarh(aes(xmin = estimate - 2 * std.error,
                     xmax = estimate + 2 * std.error),
                 height = 0.2)

###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################