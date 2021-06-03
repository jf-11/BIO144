########################################################################
# R CHEATSHEET!
########################################################################
# tidy with tydr

# TIDY DATA --> 
# 1. every column is a variable
# 2. every row is an observation
# 3. every cell is a single value

library(tidyr)
### PIVOT LONGER ###
new <- class_RTs %>% 
  pivot_longer(cols = c('Pref_Reaction_time','Nonpref_Reaction_time_ave'),
               names_to = 'Hand', values_to = 'times')

### PIVOT WIDER ###
neww <- new %>% 
  pivot_wider(names_from = 'Hand', values_from = 'times')

### SEPARATE ###
# more thant one value in a cell
#separate(variable, into = c('one','two'), sep = '/', convert = TRUE)

### UNITE ###
# contrary of separate
# unite(new_name, firstthingtounite, secondthingtounite, sep ='', remove = FALSE)
########################################################################
# tidy output
library(broom)
tidy(linear_model)
head(augment(linear_model))
glance(linear_model)
# coeff plot
coefplot(linear_model)
# or
tidy_data <- tidy(linear_model, conf.int = TRUE)
ggplot(tidy_data, aes(estimate, term, color = term)) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high))+
  geom_vline(xintercept = 0)+
  c_theme_sky
# seperate lms
mtcars %>% group_by(am) %>% do(tidy(lm(mpg ~ wt, .)))
########################################################################
# tell R to fail if NAs
options(na.action="na.fail")
# omit NAs
na.omit(dd)
########################################################################
# relative importance
library('relaimpo')
?calc.relimp()
########################################################################
# AIC automated selection
fit1 <- m1 # full model (most complex mdoel)
fit2 <- lm(bodyfat ~ 1, data = dd) # only intercept (easiest model)

step_forward <- stepAIC(fit2,direction='forward', 
                        scope=list(upper=fit1, lower=fit2))
model.sel(mods) # compare models, mods is a list of models 
# delta is difference in AIC to best AIC
# get model from dredge function
final_model <- get.models(s1, subset = delta==0)[[1]]
########################################################################
# histogram with desnsity
resids <- rnorm(100, 0, 1)
ggplot(as.data.frame(resids)) + aes(resids) + geom_histogram(aes(y=..density..),bins=30) +
  stat_function(fun = dnorm,args = list(mean=1,sd=1),aes(color='red'),geom= 'area', alpha=0.5,fill='bisque') +
  scale_colour_manual(name='Legend',values = c('red'),breaks = c('red'),labels=c('normal dist')) +
  labs(title='test') +
  annotate(geom='text', x = -0.2, y = 0.6,label='cool',color='blue',
           family="serif")
 # scale_colour_discrete(name = "Legend", labels = c("Post treatment", "Pre treatment"))
# p1 + p2 + plot_annotation(tag_levels = 'A') + c_theme_sky
########################################################################
# QQ-plot
library(extrafont)
font_import(pattern = "lmroman*")
loadfonts()
library(latex2exp)
ggplot(as.data.frame(resids), aes(sample=resids)) + stat_qq() + stat_qq_line(aes(color='red')) +
  theme(legend.position = "none") +
  theme(text = element_text(size=12, family="LM Roman 10")) +
  labs(x=TeX('$\\alpha$'),title='Latex Theme')
########################################################################
milk_data %>% #gather is depreacted
  gather(key=variable, value=value, kcal.per.g, mass, neocortex.perc) %>%
  ggplot() +
  geom_histogram(aes(x = value), bins=6) +
  facet_wrap(~variable, scales = "free", nrow = 3)
########################################################################
# stack function
ggplot(stack(df), aes(x = ind, y = values)) +
  geom_boxplot()
########################################################################
qqfunc <- function(model, num.reps) {
  N <- length(resid(model))
  sigma <- summary(model)$sigma
  x <- rnorm(N, 0, sigma)
  xx <- qqnorm(x, plot.it=F)
  xx$y <- xx$y[order(xx$x)]
  xx$x <- xx$x[order(xx$x)]
  plot(xx$x, scale(xx$y), pch=19, col="#00000011", type="l",
       xlab="Theoretical quantiles",
       ylab="Standardised residuals")
  ##QQ-line(x)
  for(i in 2:num.reps) {
    x <- rnorm(N, 0, sigma)
    xx <- qqnorm(x, plot.it=F)
    xx$y <- xx$y[order(xx$x)]
    xx$x <- xx$x[order(xx$x)]
    points(xx$x, scale(xx$y), pch=19, col="#00000011", type="l")
  }
  xx <- qqnorm(m1$residuals, plot.it=F)
  points(xx$x, scale(xx$y), col="red", pch=19)
}
########################################################################
# rounding
options(pillar.sigfig = 4)
########################################################################
# expand grid, predict and graph...
# in bio144_week7_BC.R
new_data <- expand.grid(
  rugged=seq(from=min(data$rugged), to=max(data$rugged), length.out = 166),
  cont_africa1= unique(data$cont_africa1))
head(new_data)

rgdppc_2000_log <- predict(lin.mod, newdata = new_data, interval = 'confidence')
head(rgdppc_2000_log)

plotting_data <- cbind(new_data, rgdppc_2000_log)
head(plotting_data)
########################################################################
# shapiro wilk normality test
#shapiro.test()
# null hyp0 --> normally dist. --> large p-value --> cannot reject
########################################################################
# area
ggplot(data.frame(x = c(-2, 2)), aes(x)) +
  stat_function(fun = dnorm,args = list(mean=0,sd=sqrt(1))) + 
  stat_function(fun = dnorm, 
                xlim = c(-2,-1),
                geom = "area",fill='blue',alpha=0.2) +
  stat_function(fun = dnorm, 
                xlim = c(1,2),
                geom = "area",fill='red',alpha=0.2)
# geom_function(fun = function(x) beta0.estimated + beta1.estimated*x,color ='red')
########################################################################
# ci manually
#geom_smooth(data=plotting_data,
            #aes(x=rugged,y=rgdppc_2000_log,ymin=lwr, ymax=upr), 
            #stat = 'identity')
# segment
#  geom_segment(aes(x = x, y = 0, xend = x, yend = y),color='red')
########################################################################
#venn diagram
venn(3,snames='A,B,C',linetype='dashed', ggplot=TRUE,zcolor='red,blue,green')
########################################################################
# second axis
ggplot(mpg, aes(displ, hwy)) + 
  geom_point() + 
  scale_y_continuous(
    "mpg (US)", 
    sec.axis = sec_axis(trans = ~ . + 20, name = "sec.axis")
  )
########################################################################
# area
x<-seq(-2,2, 0.01)
y<-dnorm(x,0,1)
xddf <- data.frame(x=x,y=y)
qplot(x,y,data=xddf,geom="line") +
  geom_ribbon(data=subset(xddf ,x>-2 & x< -1),aes(ymax=y,ymin=0),
              fill="red",alpha=0.5)
########################################################################
# relevel function and some dplyr
growth <- 
  mutate(growth,supplement=relevel(supplement, ref='control'))

means <- growth %>% 
  group_by(diet,supplement) %>% 
  summarise(meanGrow = mean(gain),
            seGrow = sd(gain)/sqrt(n()))
# n() does counts rows in each group bc has to divide by n
########################################################################
# errorbar
ggplot(means,aes(supplement,meanGrow,color=diet, group=diet)) +
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin=meanGrow - seGrow,ymax = meanGrow + seGrow),width=0.1)
########################################################################
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
              aes(ymin=lwr,ymax=upr), stat='identity',color='red') +
  c_theme_sky
# plot a binomial model
new_data <- expand.grid(logdose=seq(min(data$logdose), max(data$logdose),
                                    length=100),
                        product= unique(data$product))
p1 <- predict(m2,newdata = new_data, se.fit = TRUE, type='response')
n1 <- cbind(new_data,p1)
# plot 
ggplot(data,aes(logdose,proportion_success,color=product)) +
  geom_point() + c_theme_sky + 
  geom_smooth(data = n1, aes(logdose,fit, ymin=fit-se.fit,ymax=fit+se.fit, fill=product), stat = 'identity')
########################################################################
# make table with xtabs:
tt <- xtabs(Count ~ Prey + Sex, data = data.eagles)
# make barchart
ggplot(data.eagles, aes(x=Sex, y=Count, fill = Prey)) +
  geom_col(position="dodge") + c_theme_sky
########################################################################
# boxcox (transformation needed or not?)
library(MASS)
boxcox(linear.model)
########################################################################
# use jitter
ggplot(data=data, aes(height, sqrt_Area, color = herbivores)) +
  geom_point(position=position_jitterdodge(jitter.width = 0.1,
                                           dodge.width = 1)) +
  geom_point(data=means, mapping=aes(y=mean), size=5, shape=3,
             position = position_dodge(width=1))
########################################################################
# order function (keep an eye on the commas to specify cols and rows)
rugged_ranking <- data[,c(3,4)]
rugged_ranking <- rugged_ranking[order(rugged_ranking$rugged,decreasing=TRUE),]
rugged_ranking <- cbind(rugged_ranking,rank =1:nrow(rugged_ranking))
rugged_ranking[rugged_ranking$country=='Switzerland',]
########################################################################
# matrices
# multiplication -> %*%
# inverse -> solve()
# transpose -> t()
# det unequal to 0 --> regular, invertible, rows and col are lin indep vecs
diag(nrow = 3, ncol = 3)
diag(c(2, 1), nrow = 2, ncol = 2)
# m * m^-1 = id
m <- matrix(c(0, 2, 1, 0), nrow = 2, ncol = 2, byrow = TRUE)
solve(m) %*% m == diag(nrow = nrow(m), ncol = ncol(m))
eigen(m) # get eigenvals and vecs
dim(m) # get dimensions
det(m)
########################################################################
# custom theme
c_theme_bisq <- theme(legend.position="right",
                      text=element_text(size=12,
                                        family="serif")) +
  theme(panel.background = element_rect(fill = "bisque1")) +
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
########################################################################