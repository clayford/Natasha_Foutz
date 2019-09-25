# Bayesian modeling with R
# Clay Ford (clayford@virginia.edu)
# UVa Library StatLab


# R scripts and comments --------------------------------------------------

# This is an R script. It contains R code. It is a text file than can be viewed
# and edited in any text editor. 

# To start a new R script in RStudio: File...New File...R script
# When you save an R script in Rstudio, it will end with '.R'

# Everything preceded with a hashtag is a comment. Comments are ignored by R but
# can be useful to you. They are typically used to explain the what and why of
# the R code.


# functions and assignment ------------------------------------------------

# R uses functions to do stuff. For example, the c() function combine values
# into a vector (like a column of numbers in a spreadsheet). Separate values
# with commas:

# Put your cursor on the following line and click Ctrl + Enter (Windows) or 
# Cmd + Enter (Mac) to submit or "run" the code
c(1, 3, 56, 7, 12)

# The c() function combined the values into a single object. In R we often want
# to save objects. We do that with the assignment operator: <-
# For example, save our vector as "x". Run this line of code:
x <- c(1, 3, 56, 7, 12)

# We see the "x" object in our Environment. If we enter "x" in the console, or
# run "x" from the script, we see our vector of numbers.
x


# Basic functions and data frames -----------------------------------------

# We can work with "x" using other R functions, such as mean(), median() or
# length()
mean(x)
median(x)
length(x)

# A vector is a one-dimensional data structure. Vectors can only have one type
# of data, such as numeric or text.

# We often need data structures that contain multiple types of data. In R we
# call these "data frames". These are two-dimensional data structures. Think of
# them as a collection of vectors stacked column-wise next to one another.

# Create three vectors containing 5 elements
gender <- c("m", "m", "f", "m", "f")
age <- c(19, 25, 22, 20, 22)
score <- c(5, 4, 5, 5, 7)

# Combine into a data frame called "DF" using the data.frame function:
DF <- data.frame(gender, age, score)
DF

# We can extract columns from a data frame using the dollar sign, like so:
DF$age
mean(DF$age)
median(DF$age)

# We don't usually build data frames from scratch in R when it comes to data
# analysis. Instead we import data. We can import Excel spreadsheets, CSV files,
# and many others. Today we will import CSV files. Most import functions begin
# with the word "read", such as read.csv() or read_excel(). 

# Saving and restoring R objects ------------------------------------------

# If we close RStudio, DF and x will be deleted from memory. If we want to save
# them, we can either save a collection of objects, or save individual objects.

# save a collection of objects; use extension .Rdata or .Rda
save(x, DF, file = "workspace.Rdata")

# save an individual object; use extension .Rds
saveRDS(DF, "DF.Rds")

# remove objects using rm()
rm(x, DF)

# restore multiple objects using .Rdata file
load("workspace.Rdata")

# restore individual object using .Rds
my_dataframe <- readRDS("DF.Rds")

# This can be very handy after running a Bayesian model that takes a long time
# to complete.

# Let's tidy up
rm(x, DF, my_dataframe)


# load packages -----------------------------------------------------------

# R packages contain functions. 

# rstanarm: functions for doing Bayesian modeling
# ggeffects: functions for visualizing models
# ggplot2: functions for creating plots

library(rstanarm)
library(ggeffects)
library(ggplot2)


# linear regression -------------------------------------------------------

# get data
ps <- read.csv("patient_satisfaction.csv")

# ps = patient satisfaction score (dependent variable)
# age = age of patient
# illness = severity of illness
# anxiety = anxiety level

# explore data
names(ps)
str(ps)
summary(ps)
pairs(ps)
hist(ps$ps)

# - fit a simple additive model with default priors;
# - Model patient satisfaction as a weighted sum of age, illness and anxiety;
# - "family = gaussian" essentially implies we think our dependent variable is
#   approximately symmetric
mod1 <- stan_glm(ps ~ age + illness + anxiety, 
                 data = ps, 
                 family = gaussian)

# In the console we see output on the sampling procedure. The model is not fit
# with a closed-form math formula or numeric approximation but rather with a
# sophisticated sampling engine. If something goes wrong with the sampling, you
# should get a warning message in red saying not to trust the results.

# which priors were used? Notice the scale is adjusted to accomodate the range
# of the data. These are "weakly informative". They rule out extreme values,
# which helps with the sampling.
prior_summary(mod1)

# check convergence. Did the sampling "settle down" around a specific region.
# These plots look good.
plot(mod1, plotfun = "trace")

# visualize posterior distributions. This is the objective of Basyesian modeling.
plot(mod1, plotfun = "dens")

# credibility intervals of coefficients
posterior_interval(mod1)

# model summary; summary statistics of the posterior distributions;
# We want all Rhat < 1.1 (assessment of convergence)
# We want all n_eff > 1000 (n_eff = effective sample size)
summary(mod1)

# Some interpretation:

# - for every one year increase in age, expected patient satisfaction decreases
#   by about 1.1

# - for every one unit increase in the anxiety score, expected patient
#   satisfaction decreases by about 13.2

# - it's not clear what effect illness has on patient satisfaction

# - the intercept is the expected patient satisfaction for someone age 0, with
#   illness = 0 and anxiety = 0. Not useful.

# - sigma is the estimate of the standard deviation of the normal (gaussian)
#   distribution from which we assume the errors are "drawn".


# The model summary is summarizing 4000 samples; use the as.data.frame()
# function to create an object that contains the samples.
mod1_df <- as.data.frame(mod1)
summary(mod1_df)

# We can work with this object to answer questions such as...

# what is the probability the effect of anxiety is greater than 0
mean(mod1_df$anxiety > 0)

# what is the probability the effect of age is between -0.1 and -0.5
mean(mod1_df$age > -1.0 & mod1_df$age < -0.5)

# what is the probability the effect of illness is less than 0
mean(mod1_df$illness < 0)


# Is this a good model? Assess model fit with posterior predictive check. The
# dark line is the observed patient satisfaction data represented as a smooth
# distribution. The lighter lines are simulated patient satisfaction scores from
# our model. This looks like a good model!
pp_check(mod1)


# visualize model using ggeffects package
# predicted mean patient satisfaction
# ppd = posterior predictive distribution 
plot(ggpredict(mod1, ppd = FALSE)) # default

# predicted patient satisfaction for a give patient
# notice the larger uncertainty intervals
plot(ggpredict(mod1, ppd = TRUE))


# fit a model with custom prior distributions
# autoscale = F means do not rescale. Use the priors exactly as specified
mod2 <- stan_glm(ps ~ age + illness + anxiety, 
                 data = ps, 
                 family = gaussian,
                 prior_intercept = normal(location = 100, scale = 50, autoscale = F), 
                 prior = normal(location = c(0, 10, 10), scale = c(5, 10, 10), autoscale = F), 
                 prior_aux = NULL) # flat, uniform prior

# evaluate model
prior_summary(mod2)
plot(mod2, plotfun = "trace")
plot(mod2, plotfun = "dens")
posterior_interval(mod2)
posterior_interval(mod1) # compare to mod1 that used default priors
summary(mod2)
pp_check(mod2)

# Not much difference in the results.

# fit a model with interactions
# perhaps we hypothesize the effect of anxiety depends on age
# anxiety:age means "allow anxiety and age to interact"
mod3 <- stan_glm(ps ~ age + illness + anxiety + anxiety:age, 
                 data = ps, 
                 family = gaussian)

# check convergence
plot(mod3, plotfun = "trace")

# visualize posterior distributions
plot(mod3, plotfun = "dens")

# model summary
summary(mod3)

# assess model fit with posterior predictive check
pp_check(mod3)

# visualize the interaction; there does not appear to be any interaction
plot(ggpredict(mod3, terms = c("anxiety", "age"))) 

# visualize the interaction at ages = 30, 40, 50
plot(ggpredict(mod3, terms = c("anxiety", "age [30,40,50]"))) 

# visualize the interaction at anxiety = 1.25, 2, 2.75
plot(ggpredict(mod3, terms = c("age", "anxiety [1.25,2,2.75]"))) 

# compare models; lower waic is better
waic(mod1)
waic(mod3)
compare_models(waic(mod1), waic(mod3))

# The model with no interaction seems to be as good as the more complicated
# model with the interaction.

# Using model to make predictions

# Find expected patient satisfaction for someone age = 35, illness = 50, and
# anxiety = 2 for mod1. Use the posterior predictive distribution to draw
# samples.
pp <- posterior_predict(mod1, newdata = data.frame(age = 35, 
                                                   illness = 50, 
                                                   anxiety = 2), 
                        draws = 1000)
dim(pp)
mean(pp)
summary(pp)
quantile(pp, probs = c(0.025, 0.975))



# logistic regression -----------------------------------------------------

# get data
ipo <- read.csv("ipo.csv")

# vcf = presence or absence of venture capital funding (dependent variable)
# cmpy_value = est face value of company from prospectus
# shares = num shares offered
# buyout = presence/absence of leveraged buyout

# explore data
names(ipo)
str(ipo)
summary(ipo)

table(ipo$vcf)
table(ipo$buyout)


boxplot(cmpy_value ~ vcf, data = ipo)
boxplot(shares ~ vcf, data = ipo)
plot(shares ~ cmpy_value, data = ipo)

# dist'n of numeric predictors are quite skewed
hist(ipo$cmpy_value)
hist(ipo$shares)

# log base 10 transform make more symmetric?
hist(log10(ipo$cmpy_value))
hist(log10(ipo$shares))

# add log transformed values to data
ipo$cmpy_value_log <- log10(ipo$cmpy_value)
ipo$shares_log <- log10(ipo$shares)

# convert indicator variables to "factor"
ipo$vcf <- factor(ipo$vcf, labels = c("no", "yes"))
ipo$buyout <- factor(ipo$buyout, labels = c("no", "yes"))

# fit a simple additive model with default priors;
# "family = binomial" because our dependent variable is binary
# model probability of venture capital funding
mod4 <- stan_glm(vcf ~ cmpy_value_log + shares_log + buyout, 
                 data = ipo, 
                 family = binomial)

# prior summary
prior_summary(mod4)

# check convergence
plot(mod4, plotfun = "trace")

# visualize posterior distributions
plot(mod4, plotfun = "dens")

# assess model fit with posterior predictive check
pp_check(mod4)

# model summary
summary(mod4)

# visualize model 
plot(ggpredict(mod4, ppd = FALSE)) # default

# take advice from console output
plot(ggpredict(mod4, terms="cmpy_value_log [all]")) 
plot(ggpredict(mod4, terms="shares_log [all]")) 

# make the plot prettier; convert x axis back to original scale
# need to load ggplot2 package
plot(ggpredict(mod4, terms="cmpy_value_log [all]")) +
  scale_x_continuous("Company value", breaks = 6:8, 
                     labels = scales::dollar(10^(6:8)))
  
# fit model with custom priors
mod5 <- stan_glm(vcf ~ cmpy_value_log + shares_log + buyout, 
                 data = ipo, 
                 family = binomial,
                 prior = normal(location = c(2, -1, 1), 
                                scale = c(1, 1, 1), 
                                autoscale = F))

# evaluate model
prior_summary(mod5)
plot(mod5, plotfun = "trace")
plot(mod5, plotfun = "dens")
pp_check(mod5)
summary(mod5)
posterior_interval(mod5)
posterior_interval(mod4) # compare to first model




