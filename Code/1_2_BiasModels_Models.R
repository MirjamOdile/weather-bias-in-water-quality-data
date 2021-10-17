### ----------------------------------
###                                                                          
### Weather Bias In Water Quality Data                           
### Logistic regression models                                                                    
###
### Author: Mirjam Nanko
### Email: m.nanko@exeter.ac.uk
### Date: April 16, 2021
###
### ----------------------------------
###
### References used for assumption testing:
### 
### Garson, G. D. (2016). Logistic Regression: Binomial and Multinomial,
### 2016 Edition. Statistical Associates Publishing.
### 
### Hosmer, D. W., Lemeshow, S., & Sturdivant, R. X. (2013). Applied logistic
### regression (Third edition). Wiley.
### https://doi.org/10.1002/9781118548387
###
### King, G., & Zeng, L. (2001). Logistic Regression in Rare Events Data. 
### Political Analysis, 9(2), 137–163. 
### https://doi.org/10.1093/oxfordjournals.pan.a004868
###
### McFadden, D. (1979). Quantitative Methods for Analysing Travel Behaviour of 
### Individuals. In D. A. Hensher & P. R. Stopher (Eds.), Behavioural travel 
### modelling (pp. 279–318). Croom Helm.
###
### Menard, S. (2010). Logistic Regression: From Introductory to Advanced 
### Concepts and Applications. SAGE Publications, Inc. 
### https://doi.org/10.4135/9781483348964
###
### ----------------------------------


library(tidyverse); theme_set(theme_bw()) # data handling + plotting
library(lubridate) # dates
library(stargazer) # summary statistics and model export
library(psych) # pairs plot (descriptive statistics)
library(DescTools) # OddsRatio(); PseudoR2(); DurbinWatsonTest(); VIF()
library(ResourceSelection) # Hosmer-Lemeshow test
library(ROCR) # ROC
dx <- LogisticDx::dx # logistic regression diagnostics
library(glarma) # generalised linear autoregressive moving average models
library(sjPlot) # effects plots


# Specify root as path to 'Data Cleaned' folder containing the lake data folders 
# 'Manual' and 'WeatherStation'
root <- '/home/mirjam/GitHub/weather-bias-in-water-quality-data/Data/Data Cleaned/'

# Set the working directory

setwd(root)
getwd()
list.files()

##########################
###  Data preparation  ###
##########################

samples <- read.csv('Manual/SAMPLES_2016-2019_cleaned.csv',
                    stringsAsFactors = FALSE) %>%
           mutate(Datetime = as_date(Datetime),
                  Date = as_date(Date),
                  Time = hms::as_hms(Time)) %>% 
           group_by(Date, Location) %>%
           summarise(Starttime = hms::as_hms(min(Time)),
                     Endtime = hms::as_hms(max(Time))) %>%
           drop_na() %>% 
           subset(Date >= '2016-07-26') %>% 
           as_tibble()


weather <- read.csv('WeatherStation/MLRCWeather_2016-2019_cleaned.csv',
                    stringsAsFactors = FALSE) %>%
           mutate(Date = as_date(Date))


##########################
###  Data aggregation  ###
##########################

# Aggregate the weather data by date
data <-  weather %>% group_by(Date) %>%
  summarise(airtemp.mean = mean(Air.Temperature, na.rm = TRUE),
            windspeed.mean = mean(Wind.Speed, na.rm = TRUE),
            rain.mean = mean(Interval.Rain, na.rm = TRUE)
            ) %>%
# Add sampling and temporal information
  mutate(year = year(Date),
         quarter = factor(quarter(Date), levels = c('1', '2', '3', '4')),
         month = month(Date, label = TRUE),
         week = week(Date),
         day = factor(weekdays(Date, abbreviate = TRUE), 
                      levels = c('Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat', 'Sun')),
         wday = factor(ifelse(day == 'Sat' | day == 'Sun', 'no', 'yes')),
         wday.numeric = ifelse(day == 'Sat' | day == 'Sun', 0, 1),
         sampling.season = factor(case_when(
           month %in% c('Nov', 'Dec', 'Jan', 'Feb', 'Mar') ~ 'no',
           month %in% c('Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct') ~ 'yes'),
           levels = c('no', 'yes')),
         schedule =  factor(case_when(
           month %in% c('Apr', 'Oct') ~ 'monthly',
           month %in% c( 'May', 'Sep') ~ 'biweekly',
           month %in% c('Jun', 'Jul', 'Aug') ~ 'weekly'),
           levels = c('monthly', 'biweekly', 'weekly')),
         sample =  as.factor(if_else(Date %in% samples$Date, 1, 0)),
         sample_labelled =  factor(if_else(Date %in% samples$Date, 'yes', 'no')),
         sample.numeric = as.numeric(as.character(sample))
         )

# Add lag variables
for (i in c(1,2,3,4,5,6,7,14)){
  data[[paste0('lag', i)]] <- as.numeric(lag(data$sample, n = i, default = '0'))
}

###########################################
###        Subset relevant months       ###
###########################################

# Standard procedure is for profiles to be taken weekly in June - August, 
# biweekly in May and September, and once in April and October.

table(data$sampling.season, data$sample_labelled)
data <- subset(data, data$sampling.season == "yes")
# 20 sampling days dropped (8.3% of total)

# Add unique identifier for each observation
data$id <- as.numeric(rownames(data))

#####################################
###     Anomalous observations    ###
#####################################

# Due to external reasons, no sampling took place in April and May 2017 as
# shown in the table and plot below:

cbind(
  table(data$month[data$year == 2016], data$sample.numeric[data$year == 2016]),
  table(data$month[data$year == 2017], data$sample.numeric[data$year == 2017]),
  table(data$month[data$year == 2018], data$sample.numeric[data$year == 2018]),
  table(data$month[data$year == 2019], data$sample.numeric[data$year == 2019]))

data$standard.sampling <- factor(ifelse(data$Date < as.Date("2018-04-01") | 
                                    data$Date >= as.Date("2018-06-01"), 
                                  'standard sampling', 'no sampling'))

ggplot(data = data, aes(Date, sample,color = standard.sampling)) +
  geom_jitter() + 
  scale_color_manual(values = c("Orangered", "Slategray"), 
                     name = "Did sampling take place?")

# The following code can be used to exclude observations from those months from 
# the data in order to compare the model with an without them:

# data <- subset(data, standard.sampling == 'standard sampling')

# >> Exclusion these observations does not change the results in any meaningful 
# way. Therefore, the data is kept in the model.

###########################################
###             Rare event?             ###
###########################################

# See King & Zeng (2001).

sum(data$sample.numeric)/nrow(data) # 29.86%

# >> Not rare events data.


###########################################
###            Descriptives             ###
###########################################


descriptives <- data %>% select(sample.numeric, airtemp.mean, windspeed.mean,
                               rain.mean, wday.numeric) %>% data.frame()
colnames(descriptives) <- c('Sample (0 = No, 1 = Yes)', 
                            'Mean air temperature (°C)', 
                            'Mean wind speed (m/s)', 
                            'Mean rainfall (mm/15min)', 
                            'Working day (0 = No, 1 = Yes)')
summary(descriptives)

##########################
###  Investigate data  ###
##########################


# Pairs plot of the explantory and predictor variables

data[,c('sample', 'airtemp.mean', 'windspeed.mean', 'rain.mean', 'wday')] %>%

pairs.panels(scale = TRUE,
             ellipses = TRUE,
             hist.col = "#00AFBB",
             smooth = TRUE,
             smoother = TRUE,
             lm = FALSE)

table(data$wday, data$sample_labelled)

# >> Assumption: Zero cell count ✓ 
# (Menard 2010, Chapter 7, p. 2)

###################################
###  Logit Model 1: Base Model  ###
###################################

summary(logit1 <- glm(sample ~ airtemp.mean + windspeed.mean + rain.mean + wday,
                      family = binomial(logit), 
                      data = data))

OddsRatio(logit1)

#---------------------------------------#
#  Summary measures of goodness of fit  #
#---------------------------------------#

# Pseudo R-squared measures
# See Menard (2010, Chapter 3, p. 8 f.))
PseudoR2(logit1, which = "all") 
# McFadden's Pseudo R2 (likelihood ratio R2): 0.249
# >> Excellent fit (McFadden, 1979, p. 306)

# Hosmer-Lemeshow test for goodness of fit for logistic regression models
# (See Hosmer et al., 2013, p. 157 ff.)
print(hl1 <- hoslem.test(logit1$y, fitted(logit1), g = 10))
cbind(round(hl1$expected), hl1$observed)
par(mfrow=c(1,1))
plot(hl1$expected[,2]/(sum(hl1$expected[,1], hl1$expected[,2])),
     hl1$observed[,2]/(sum(hl1$observed[,1], hl1$observed[,2])),
     main = 'Logit1: observed vs. expected probabilty',
     xlab = 'Observed proportion in data',
     ylab = 'Expected proportion from logistic model')
abline(0,1)
# >> Model not a good fit for the data

# Four diagnostic plots to describe discrimination of the logit1 model with
# an area under the ROC of 0.82, n = 740 
# (See Hosmer et al., 2013, p. 176 ff.)
par(mfrow=c(2,2))
# Plot of jittered outcome versus estimated probabilities
plot(logit1$fitted.values, jitter(data$sample.numeric, amount = 0.01),
      xlab = "Estimated probability", ylab = "Density", main = "")
# Histogram of the estimated probabilities of a sample being taken for 
# observations where no sample was taken
hist(logit1$fitted.values[logit1$y == 0], 50, xlim = c(0,1),
     xlab = "Estimated probability", 
     ylab = "Jittered outcome",
     main = "Outcome = 0")
# ROC Curve 
pred <- prediction(predict(logit1, type = "response"), 
                   as.numeric(as.character(data$sample)))
perf <- performance(pred,"tpr","fpr")
auc_ROCR1 <- performance(pred, measure = "auc")
auc_ROCR1@y.values[[1]]
# >> excellent discrimination (Hosmer et al., 2013, p. 177)
plot(perf,colorize=TRUE)
# Histogram of the estimated probabilities of a sample being taken for 
# observations where a sample was taken 
hist(logit1$fitted.values[logit1$y == 1], 50, xlim = c(0,1),
     xlab = "Estimated probability", ylab = "Density", main = "Outcome = 1")

#---------------------#
#  Multicollinearity  #
#---------------------#
# Generalized variance-inflation factors
VIF(logit1)
# >> All < 5
# >> No multicollinearity

#--------------------------#
#  Linearity in the logit  #
#--------------------------#
par(mfrow=c(3,1))
plot(logit1$linear.predictors ~ data$airtemp.mean)
plot(logit1$linear.predictors ~ data$windspeed.mean)
plot(logit1$linear.predictors ~ data$rain.mean)

# Box-Tidwell Test (Garson, 2016, p. 276 f.)
# Regression with an added interaction term of the IV and the ln of the IV:
summary(glm(sample ~ airtemp.mean + airtemp.mean:log(airtemp.mean+1.7) +
              windspeed.mean + rain.mean + wday,
            family = binomial(logit), data = data))
summary(glm(sample ~ airtemp.mean +  windspeed.mean +
              windspeed.mean:log(windspeed.mean) + rain.mean + wday,
            family = binomial(logit), data = data))
summary(glm(sample ~ airtemp.mean + windspeed.mean + rain.mean +
              rain.mean:log(rain.mean+1e-20) + wday,
            family = binomial(logit), data = data))
# >> No significant interaction effects
# >> No evidence of a non-linear association
# >> Model is linear in the logit for each continuous variable

#--------------------#
#  Auto-correlation  #
#--------------------#
# Auto-Correlation Function Estimation & Durbin Watson Test
par(mfrow=c(1,2))
acf(residuals(logit1, type = 'response'), lag = 30,
    main = "Auto-Correlation Function Estimation")
pacf(residuals(logit1, type = 'response'), lag = 30,
     main = "Partial Auto-Correlation Function Estimation")
DurbinWatsonTest(logit1)
# >> p < 0.05: Errors not independent
# >> (Temporal) auto-correlation present

#----------------------------#
#  Influential observations  #
#----------------------------#
# (See Hosmer et al., 2013, p. 186 ff.)
diagnostics1 <- cbind(data, pihat = logit1$fitted.values) 
diagnostics1 <- merge(diagnostics1, dx(logit1)[,c(-1,-6, -8, -9)], 
                      by = c("airtemp.mean","windspeed.mean", "rain.mean"))

par(mfrow=c(2,2))
plot(diagnostics1$pihat, diagnostics1$h,
     xlab = "Estimated probability", ylab = "Leverage")
plot(diagnostics1$pihat, diagnostics1$dChisq,
     xlab = "Estimated probability", ylab = "Change in Pearson chi-square")
plot(diagnostics1$pihat, diagnostics1$dDev,
     xlab = "Estimated probability", ylab = "Change in deviance")
plot(diagnostics1$pihat, diagnostics1$dBhat,
     xlab = "Estimated probability", ylab = "Cook's distance")

print(outliers1 <- diagnostics1[diagnostics1$h > 0.03 | 
                                diagnostics1$dChisq > 50 |
                                diagnostics1$dDev > 6 |
                                diagnostics1$dBhat > 0.2,] %>% 
  select(id, sample.numeric, airtemp.mean, windspeed.mean, rain.mean, 
         wday.numeric, pihat, h, dChisq, dDev, dBhat))

# Repeat analysis on dataset without outliers
# data.no.outliers1 <- data[!data$id %in% outliers1$id,]
# table(data.no.outliers1$wday, data.no.outliers1$sample_labelled)
# 
# summary(logit1.no.outliers <- glm(sample ~ airtemp.mean + windspeed.mean +
#                                   rain.mean + wday,
#                                   family = binomial(logit),
#                                   data = data.no.outliers1))

# >> Six of the twelve influential cases are the six observations where a
# sample was taken on the weekend (wday.numeric = 0, sample.numeric = 1).
# Excluding this leads to a zero cell count problem, i.e. no samples
# are taken on any of the weekend days in the data. This leads to a
# very high estimated standard error for the workingday variable.
# >> Importantly, the variables of interest (airtemp, windspeed, rain) remain
# significant with the same direction of the effect as in the logit1 model.
# >> Seeing that the outliers appear to be a valid observations and exclusion
# of them does not change the interpretation of the model in any meaningful
# way, the observations will be kept in the analysis.


#########################################################
###  Logit Model 2: Lagged Endogenous Variable Model  ###
#########################################################

# In the second model, the significant lag variables (see partial auto-correlationF plot for 
# the logit1 model) will be included into the model to control for temporal 
# auto-correlation (see Menard (2010, Chapter 13, p.8 ff) on Lagged Endogenous 
# Variable Models in Logistic Regression).

summary(logit2 <- glm(sample ~ airtemp.mean + windspeed.mean + rain.mean + 
                        wday + lag1 + lag5 + lag7 + lag14,
                      family = binomial(logit), 
                      data = data))

OddsRatio(logit2)

#---------------------------------------#
#  Summary measures of goodness of fit  #
#---------------------------------------#

# Pseudo R-squared measures
# See Menard (2010, Chapter 3, p. 8 f.))
PseudoR2(logit2, which = "all") 
# McFadden's Pseudo R2 (likelihood ratio R2): 0.332
# >> Excellent fit (McFadden, 1979, p. 306)

# Hosmer-Lemeshow test for goodness of fit for logistic regression models
# (See Hosmer et al., 2013, p. 157 ff.)
print(hl2 <- hoslem.test(logit2$y, fitted(logit2), g = 10))
cbind(round(hl2$expected), hl2$observed)
par(mfrow=c(1,1))
plot(hl2$expected[,2]/(sum(hl2$expected[,1], hl2$expected[,2])),
     hl2$observed[,2]/(sum(hl2$observed[,1], hl2$observed[,2])),
     main = 'Logit2: observed vs. expected probabilty',
     xlab = 'Observed proportion in data',
     ylab = 'Expected proportion from logistic model')
abline(0,1)
# >> Model a good fit for the data


# Four diagnostic plots to describe discrimination of the logit1 model with
# an area under the ROC of 0.87, n = 740 
# (See Hosmer et al., 2013, p. 176 ff.)
par(mfrow=c(2,2))
# Plot of jittered outcome versus estimated probabilities
plot(logit2$fitted.values, jitter(data$sample.numeric, amount = 0.01),
      xlab = "Estimated probability", ylab = "Density", main = "")
# Histogram of the estimated probabilities of a sample being taken for 
# observations where no sample was taken
hist(logit2$fitted.values[logit2$y == 0], 50, xlim = c(0,1),
     xlab = "Estimated probability", 
     ylab = "Jittered outcome",
     main = "Outcome = 0")
# ROC Curve 
pred <- prediction(predict(logit2, type = "response"), 
                   as.numeric(as.character(data$sample)))
perf <- performance(pred,"tpr","fpr")
auc_ROCR2 <- performance(pred, measure = "auc")
auc_ROCR2@y.values[[1]]# >> excellent discrimination (H&L, p. 177)
plot(perf,colorize=TRUE)
# Histogram of the estimated probabilities of a sample being taken for 
# observations where no sample was taken
hist(logit2$fitted.values[logit2$y == 1], 50, xlim = c(0,1),
     xlab = "Estimated probability", ylab = "Density", main = "Outcome = 1")

#---------------------#
#  Multicollinearity  #
#---------------------#
# Generalized variance-inflation factors
VIF(logit2)
# >> All < 5
# >> No multicollinearity

#--------------------------#
#  Linearity in the logit  #
#--------------------------#
par(mfrow=c(3,1))
plot(logit2$linear.predictors ~ data$airtemp.mean)
plot(logit2$linear.predictors ~ data$windspeed.mean)
plot(logit2$linear.predictors ~ data$rain.mean)

# Box-Tidwell Test (Garson, 2016, p. 276 f.)
# Regression with an added interaction term of the IV and the ln of the IV:
summary(glm(sample ~ airtemp.mean + airtemp.mean:log(airtemp.mean+1.7) +
              windspeed.mean + rain.mean + wday + lag1 + lag5 + lag7 + lag14,
            family = binomial(logit), data = data))
summary(glm(sample ~ airtemp.mean +  windspeed.mean +
              windspeed.mean:log(windspeed.mean) + rain.mean + wday + lag1 +
              lag5 + lag7 + lag14,
            family = binomial(logit), data = data))
summary(glm(sample ~ airtemp.mean + windspeed.mean + rain.mean +
              rain.mean:log(rain.mean+1e-20) + wday + lag1 + lag5 + lag7 + lag14,
            family = binomial(logit), data = data))
# >> No significant interaction effects
# >> No evidence of a non-linear association
# >> Model is linear in the logit for each continuous variable

#--------------------#
#  Auto-correlation  #
#--------------------#
# Auto-Correlation Function Estimation & Durbin Watson Test
par(mfrow=c(1,2))
acf(residuals(logit2, type = 'response'), lag = 30,
    main = "Auto-Correlation Function Estimation")
pacf(residuals(logit2, type = 'response'), lag = 30,
     main = "Partial Auto-Correlation Function Estimation")
DurbinWatsonTest(logit2)
# >> p > 0.05: Errors independent
# >> No auto-correlation present

#----------------------------#
#  Influential observations  #
#----------------------------#
# (See Hosmer et al., 2013, p. 186 ff.)
diagnostics2 <- cbind(data, pihat = logit2$fitted.values) 
diagnostics2 <- merge(diagnostics2, dx(logit2)[,c(-1,-6, -8, -9)], 
                      by = c("airtemp.mean","windspeed.mean", "rain.mean"))

par(mfrow=c(2,2))
plot(diagnostics2$pihat, diagnostics2$h,
     xlab = "Estimated probability", ylab = "Leverage")
plot(diagnostics2$pihat, diagnostics2$dChisq, cex = sqrt(diagnostics2$dBhat)*5,
     xlab = "Estimated probability", ylab = "Change in Pearson chi-square")
plot(diagnostics2$pihat, diagnostics2$dDev, cex = sqrt(diagnostics2$dBhat)*5,
     xlab = "Estimated probability", ylab = "Change in deviance")
plot(diagnostics2$pihat, diagnostics2$dBhat,
     xlab = "Estimated probability", ylab = "Cook's distance")

print(outliers2 <- diagnostics2[diagnostics2$h > 0.05 |
                                diagnostics2$dChisq > 50 |
                                diagnostics2$dDev > 5 | 
                                diagnostics2$dBhat > 0.2,] %>% 
  select(id, sample.numeric, airtemp.mean, windspeed.mean, rain.mean, 
         wday.numeric, pihat, h, dChisq, dDev, dBhat))

influences2 <- data.frame(influence.measures(logit2)$infmat)
head(influences2)
head(diagnostics2)

head(cooks.distance(logit2))
# Repeat analysis on dataset without outliers
# data.no.outliers2 <- data[!data$id %in% outliers2$id,]
# table(data.no.outliers2$wday, data.no.outliers2$sample_labelled)
# 
# summary(logit2.no.outliers <- glm(sample ~ airtemp.mean + windspeed.mean +
#                                   rain.mean + wday + lag1 + lag5 + lag7 + lag14,
#                                   family = binomial(logit), 
#                                   data = data.no.outliers2))

# >> No change in significance and direction of effect compared to the logit2
# model.
# >> Seeing that the outliers appear to be a valid observations and exclusion of
# them does not change the interpretation of the model in any meaningful way,
# the observations will be kept in the analysis.


###############################
###    Model comparison     ###
###############################

anova(logit1, logit2,  test = 'Chisq')
# >> Model logit2 statistically significantly better model.


############################################
###           GLARMA Models              ###
############################################

#####################
###  Data set up  ###
#####################

y = as.matrix(cbind(data$sample.numeric,
                    ifelse(data$sample.numeric == 0, 1, 0)))

X = as.matrix(cbind(rep(1,nrow(data)), 
                    data[, c('airtemp.mean', 'windspeed.mean', 
                             'rain.mean', 'wday.numeric')]))

X2 = as.matrix(cbind(rep(1,nrow(data)), 
                     data[, c('airtemp.mean', 'windspeed.mean', 
                             'rain.mean', 'wday.numeric',
                              'lag1', 'lag5', 'lag7', 'lag14')]))


######################################
### Comparing GLARMA models to glm ###
######################################

# The following section will show how the glarma models can be compared to the 
# standard glm models. It will also recreate a glarma version of the logit1 and
# logit2 models to have models from the same object type to compare to the final
# new models.

# See definition of residuals here: 
# https://rdrr.io/cran/binomTools/man/Residuals.html

# The glarma models can estimate either identity, score or pearson residuals:
summary(mod_id <- glarma(y, X, type = 'Bin', residuals = 'Identity'))
summary(mod_sc <- glarma(y, X, type = 'Bin', residuals = 'Score'))
summary(mod_ps <- glarma(y, X, type = 'Bin', residuals = 'Pearson')) # Default!

# The glarma models' identity residuals are equal to the response residuals, 
# also called the raw residuals: y_i - \hat{y}_i
head(mod_id$residuals)
head(residuals(logit1, 'response'))
head(logit1$y - logit1$fitted.values)
# They can also be backtransformed from the pearson residuals (which is 
# important as some of the glarma models don't converge when using identity
# residuals):
head(mod_ps$residuals * sqrt((mod_ps$fitted.values*(1-mod_ps$fitted.values))))

# The glarma models' score residuals are equal to the working residuals:
head(mod_sc$residuals)
head(residuals(logit1, 'working'))
head(logit1$residuals)

# The glarma models' pearson residuals are equal to the pearson residuals:
head(mod_ps$residuals)
head(residuals(logit1, 'pearson'))


##########################################
###  Null model (for model comparison) ###
##########################################

summary(null_model <- glm(sample ~ 1,
                          family = binomial(logit), data = data))


######################################
###  Base model (equal to logit1)  ###
######################################

summary(mod1 <- glarma(y, X, type = 'Bin'))

#--------------------#
#  Diagnostic plots  #
#--------------------#
par(mfrow=c(3,2))
plot(mod1)

#-------------------#
#  Goodness of fit  #
#-------------------#

# McFaddens pseudo R-squared manually
(logLik(null_model)[1] - mod1$logLik[1]) / (logLik(null_model)[1])
# Likelihood ratio test 
pchisq(2*(mod1$logLik[1] - logLik(null_model)[1]),
       df = 2, 
       lower.tail=FALSE)
# >> Highly statistically significant 
# >> Model significantly better than the null model

# Note: The Residual deviance reported by the glarma package model summary seems
# to be incorrect. 
# Residual deviance for logistic regression can be calculated with the 
# formula: - 2 * LogLikelihood(Proposed model)
-2*mod1$logLik
# which is the same as in the logit1 model
logit1$deviance
# While the rest of the model is exactly equal to the values from a 
# standard glm logistic regression, the reported residual deviance seems to be 
# wrong.

#--------------------#
#  Auto-correlation  #
#--------------------#
# Auto-Correlation Function Estimation (with identity residuals)
par(mfrow=c(1,2))
acf(mod1$residuals * sqrt((mod1$fitted.values*(1-mod1$fitted.values))), 
    lag = 30,
    main = "Auto-Correlation Function Estimation")
pacf(mod1$residuals * sqrt((mod1$fitted.values*(1-mod1$fitted.values))),
     lag = 30,
     main = "Partial Auto-Correlation Function Estimation")
# >> (Temporal) auto-correlation present


############################################################
###  Lagged endogenous variable model (equal to logit2)  ###
############################################################

summary(mod2 <- glarma(y, X2, type = 'Bin'))

#--------------------#
#  Diagnostic plots  #
#--------------------#
par(mfrow=c(3,2))
plot(mod2)

#-------------------#
#  Goodness of fit  #
#-------------------#

# McFaddens pseudo R-squared manually
(logLik(null_model)[1] - mod2$logLik[1]) / (logLik(null_model)[1])
# Likelihood ratio test 
pchisq(2*(mod2$logLik[1] - logLik(null_model)[1]),
       df = 2, 
       lower.tail=FALSE)
pchisq(2*(mod2$logLik[1] - mod1$logLik[1]),
       df = 2, 
       lower.tail=FALSE)
# >> Highly statistically significant 
# >> Model significantly better than the null model and logit1

#--------------------#
#  Auto-correlation  #
#--------------------#
# Auto-Correlation Function Estimation (with identity residuals)
par(mfrow=c(1,2))
acf(mod2$residuals * sqrt((mod2$fitted.values*(1-mod2$fitted.values))), 
    lag = 30,
    main = "Auto-Correlation Function Estimation")
pacf(mod2$residuals * sqrt((mod2$fitted.values*(1-mod2$fitted.values))),
     lag = 30,
     main = "Partial Auto-Correlation Function Estimation")
# >> No auto-correlation present


####################################
###  Autoregressive model ar(p)  ###
####################################

summary(mod_ar <- glarma(y, X, type = 'Bin', 
                           phiLags = c(1,5,7,14)))
mod_ar$aic # AIC: 643.20

#--------------------#
#  Diagnostic plots  #
#--------------------#
par(mfrow=c(3,2))
plot(mod_ar)

#-------------------#
#  Goodness of fit  #
#-------------------#

# McFaddens pseudo R-squared manually
(logLik(null_model)[1] - mod_ar$logLik[1]) / (logLik(null_model)[1])
# Likelihood ratio test 
pchisq(2*(mod_ar$logLik[1] - logLik(null_model)[1]),
       df = 2, 
       lower.tail=FALSE)
pchisq(2*(mod_ar$logLik[1] - mod1$logLik[1]),
       df = 2, 
       lower.tail=FALSE)
# >> Highly statistically significant 
# >> Model significantly better than the null model and logit1

#--------------------#
#  Auto-correlation  #
#--------------------#
# Auto-Correlation Function Estimation (with identity residuals)
par(mfrow=c(1,2))
acf(mod_ar$residuals * sqrt((mod_ar$fitted.values*(1-mod_ar$fitted.values))), 
    lag = 30,
    main = "Auto-Correlation Function Estimation")
pacf(mod_ar$residuals * sqrt((mod_ar$fitted.values*(1-mod_ar$fitted.values))),
     lag = 30,
     main = "Partial Auto-Correlation Function Estimation")
# >> Slight auto-correlation present


####################################
###  Moving-average model ar(p)  ###
####################################

summary(mod_ma <- glarma(y, X, type = 'Bin', 
                           thetaLags = c(1,5,7,14)))
mod_ma$aic # AIC: 651.29

#--------------------#
#  Diagnostic plots  #
#--------------------#
par(mfrow=c(3,2))
plot(mod_ma)

#-------------------#
#  Goodness of fit  #
#-------------------#

# McFaddens pseudo R-squared manually
(logLik(null_model)[1] - mod_ma$logLik[1]) / (logLik(null_model)[1])
# Likelihood ratio test 
pchisq(2*(mod_ma$logLik[1] - logLik(null_model)[1]),
       df = 2, 
       lower.tail=FALSE)
pchisq(2*(mod_ma$logLik[1] - mod1$logLik[1]),
       df = 2, 
       lower.tail=FALSE)
# >> Highly statistically significant 
# >> Model significantly better than the null model and logit1

#--------------------#
#  Auto-correlation  #
#--------------------#
# Auto-Correlation Function Estimation (with identity residuals)
par(mfrow=c(1,2))
acf(mod_ma$residuals * sqrt((mod_ma$fitted.values*(1-mod_ma$fitted.values))), 
    lag = 30,
    main = "Auto-Correlation Function Estimation")
pacf(mod_ma$residuals * sqrt((mod_ma$fitted.values*(1-mod_ma$fitted.values))),
     lag = 30,
     main = "Partial Auto-Correlation Function Estimation")
# >> Some auto-correlation present


#######################################################
###  Autoregressive moving-average model arma(p,q)  ###
#######################################################

# summary(mod_arma <- glarma(y, X, type = 'Bin', 
#                            phiLags = c(1,5,7), thetaLags = c(14)))
# mod_arma$aic # AIC: 639.52 >> slight auto-correlation
summary(mod_arma <- glarma(y, X, type = 'Bin', 
                           phiLags = c(1,5), thetaLags = c(7,14)))
mod_arma$aic # AIC: 640.14 >> no auto-correlation
# summary(mod_arma <- glarma(y, X, type = 'Bin',
#                            phiLags = c(1), thetaLags = c(5, 7,14)))
# mod_arma$aic # AIC: 643.29 >> no auto-correlation

#--------------------#
#  Diagnostic plots  #
#--------------------#
par(mfrow=c(1,1))
plot(mod_arma, which = 1)
par(mfrow=c(3,2))
plot(mod_arma)

#--------------------#
#  Auto-correlation  #
#--------------------#
# Auto-Correlation Function Estimation (with identity residuals)
par(mfrow=c(1,2))
acf(mod_arma$residuals * sqrt((mod_arma$fitted.values*(1-mod_arma$fitted.values))),
    lag = 30,
    main = "Auto-Correlation Function Estimation")
pacf(mod_arma$residuals * sqrt((mod_arma$fitted.values*(1-mod_arma$fitted.values))),
     lag = 30,
     main = "Partial Auto-Correlation Function Estimation")

#-------------------#
#  Goodness of fit  #
#-------------------#

# McFaddens pseudo R-squared manually
(logLik(null_model)[1] - mod_arma$logLik[1]) / (logLik(null_model)[1])
# Likelihood ratio test 
pchisq(2*(mod_arma$logLik[1] - logLik(null_model)[1]),
       df = 2,
       lower.tail=FALSE)
pchisq(2*(mod_arma$logLik[1] - mod1$logLik[1]),
       df = 2,
       lower.tail=FALSE)
# >> Highly statistically significant 
# >> Model significantly better than the null model and logit1

# Hosmer-Lemeshow test for goodness of fit for logistic regression models
# (See Hosmer et al., 2013, p. 157 ff.)
print(hl3 <- hoslem.test(logit1$y, fitted(mod_arma), g = 10))
cbind(round(hl3$expected), hl3$observed)
par(mfrow=c(1,1))
plot(hl3$expected[,2]/(sum(hl3$expected[,1], hl3$expected[,2])),
     hl3$observed[,2]/(sum(hl3$observed[,1], hl3$observed[,2])),
     main = 'Mod_arma: observed vs. expected probabilty',
     xlab = 'Observed proportion in data',
     ylab = 'Expected proportion from logistic model')
abline(0,1)
# >> Model a good fit for the data

###############################
###    Model comparison     ###
###############################

print(c(mod1$aic, mod2$aic, mod_ar$aic, mod_ma$aic, mod_arma$aic))

# >> Lagged endogenous variable model is the best model.
# >> Autoregressive moving-average model confirms and validates the findings.


###########################################
###   Export results: effects plots     ###
###########################################

# Pred: discrete predictors are held constant at their reference level
# Eff: discrete predictors are held constant at their proportions


### WEATHER ###
plot_model(logit2, type = "eff", terms = "airtemp.mean")
plot_model(logit2, type = "eff", terms = "windspeed.mean")
plot_model(logit2, type = "eff", terms = "rain.mean")


### WEATHER  + WEEKEND ###
plot_model(logit2, type = "eff", terms = c("airtemp.mean", 'wday'))
plot_model(logit2, type = "eff", terms = c("windspeed.mean", 'wday'))
plot_model(logit2, type = "eff", terms = c("rain.mean", 'wday'))


### WEATHER COMBINED ###
# Summary statistics for plotting
round(summary(data$windspeed.mean),2)
round(summary(data$rain.mean),2)
round(summary(data$airtemp.mean),2)
# Temperature + rain
plot_model(logit2, type = "eff", 
           terms = c("airtemp.mean", 'rain.mean [0, 0.02304, 0.45417]'))
# Temperature + wind
plot_model(logit2, type = "eff", 
           terms = c("airtemp.mean", 'windspeed.mean [0.32, 1.22, 6.17]'))
# Rain and wind
plot_model(logit2, type = "eff", 
           terms = c("rain.mean", 'windspeed.mean [0.32, 1.22, 6.17]'))
# Rain and temperature
plot_model(logit2, type = "eff", 
           terms = c("rain.mean", "airtemp.mean [-1.64, 17.55, 30.01]"))
# Wind and temperature
plot_model(logit2, type = "eff", 
           terms = c("windspeed.mean", "airtemp.mean [-1.64, 17.55, 30.01]"))


### REGRESSION MODEL ODDS RATIOS ###
plot_model(logit2, type = "std", vline.color = "gray", show.values = TRUE)


stargazer(logit1, logit2, logit2, type = 'latex',
          digits = 2,
          align = TRUE,
          column.labels = c('Base Model', 'LDV Model', 'GLARMA Model'),
          dep.var.labels = 'Sample',
          single.row = TRUE,
          nobs = FALSE,
          star.char = c('.', '*', '**', '***'),
          star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
          covariate.labels = c('Air Temperature', 'Wind Speed', 'Rain', 
                               'Weekday', '1 day', '5 days', '7 days', '14 days'), 
          notes.label = 'Signif. codes:',
          # notes = '0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1',
          notes = '1 ‘ ’ 0.1 ‘.’ 0.05 ‘*’ 0.01 ‘**’ 0.001 ‘***’ 0',
          notes.append = FALSE
          )


###########################################
###   Combined Effects Plot             ###
###########################################

library(gridExtra)

### Standard model ###
p11 <- plot_model(logit1, type = "eff", terms = "airtemp.mean")
p21 <- plot_model(logit1, type = "eff", terms = "windspeed.mean [0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5, 5.5, 6, 6.5, 7]")
p31 <- plot_model(logit1, type = "eff", terms = "rain.mean [0, 0.025, 0.05, 0.075, 0.1, 0.125, 0.15, 0.175, 0.2, 0.225, 0.25, 0.275, 0.3, 0.325, 0.35, 0.375, 0.4, 0.425, 0.45, 0.475, 0.5, 0.525, 0.55, 0.575, 0.6]")
p41 <- plot_model(logit1, type = "eff", terms = "wday")
grid.arrange(p11, p21, p31, p41)

### LDV Model ###
p12 <- plot_model(logit2, type = "eff", terms = "airtemp.mean")
p22 <- plot_model(logit2, type = "eff", terms = "windspeed.mean [0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5, 5.5, 6, 6.5, 7]")
p32 <- plot_model(logit2, type = "eff", terms = "rain.mean [0, 0.025, 0.05, 0.075, 0.1, 0.125, 0.15, 0.175, 0.2, 0.225, 0.25, 0.275, 0.3, 0.325, 0.35, 0.375, 0.4, 0.425, 0.45, 0.475, 0.5, 0.525, 0.55, 0.575, 0.6]")
p42 <- plot_model(logit2, type = "eff", terms = "wday")
grid.arrange(p12, p22, p32, p42)

# Load msm for deltamethod function.
library(msm)

# Effects plot for GLARMA model.
# get coefficients and covariance.
beta_arma <- summary(mod_arma)$coefficients1$Estimate
cov_arma <- mod_arma$cov[1:5,1:5]
t.crit <- qt(0.975, length(mod_arma$residuals) - 2)

# Get the means of each predictor.
airtemp.mean <- mean(data$airtemp.mean)
windspeed.mean <- mean(data$windspeed.mean)
rain.mean <- mean(data$rain.mean)
wday.mean <- mean(data$wday.numeric)

### Air temperature ###
# Points to evaluate (extract from effects plot)
airtemp.x <- p11$data$x
# get the raw (untransformed) prediction
airtemp.y_raw <- beta_arma[1] + airtemp.x*beta_arma[2] + windspeed.mean*beta_arma[3] + rain.mean*beta_arma[4] + wday.mean*beta_arma[5]
# transform with logistic function.
airtemp.y <- exp(airtemp.y_raw)/(1+exp(airtemp.y_raw))

# set an empty vector for the standard errors and evaluate for each point.
airtemp.se = numeric(length(airtemp.x))
for (i in 1:length(airtemp.x)) {
  x = airtemp.x[i]
  airtemp.se[i] <- deltamethod(~ exp(x1 + x2*x + x3*windspeed.mean + x4*rain.mean + x5*wday.mean)/
                                 (1 + exp(x1 + x2*x + x3*windspeed.mean + x4*rain.mean + x5*wday.mean)), 
                               beta_arma, cov_arma)
}

# create confidence intervals.
airtemp.y.lo <- airtemp.y - t.crit*airtemp.se; airtemp.y.lo[airtemp.y.lo<0] = 0
airtemp.y.hi <- airtemp.y + t.crit*airtemp.se

# collect everything in a tibble (dataframe), including the other models.
airtemp.data <- tibble(airtemp.x, 
                       p11$data$predicted, p11$data$conf.low, p11$data$conf.high,
                       p12$data$predicted, p12$data$conf.low, p12$data$conf.high,
                       airtemp.y, airtemp.y.lo, airtemp.y.hi)

# make the plot.
airtemp_plot <- airtemp.data %>% ggplot(aes(airtemp.x)) +
  geom_ribbon(aes(ymin=p11$data$conf.low, ymax=p11$data$conf.high), fill='goldenrod', alpha=0.1) + 
  geom_line(aes(y=p11$data$predicted, colour='Standard', linetype='Standard', size='Standard')) +
  geom_ribbon(aes(ymin=p12$data$conf.low, ymax=p12$data$conf.high), fill='darkorchid', alpha=0.1) + 
  geom_line(aes(y=p12$data$predicted, colour='LDV', linetype='LDV', size='LDV')) +
  geom_ribbon(aes(ymin=airtemp.y.lo, ymax=airtemp.y.hi), fill='limegreen', alpha=0.1) + 
  geom_line(aes(y=airtemp.y, colour='GLARMA', linetype='GLARMA', size='GLARMA')) +
  xlab('Mean Air Temperature (°C)') +
  ylab('E(Y|x)') + 
  scale_colour_manual(name = element_blank(), 
                      breaks = c('Standard', 'LDV', 'GLARMA'),
                      values =c('Standard'='goldenrod', 'LDV'='darkorchid', 'GLARMA'='limegreen')) +
  scale_linetype_manual(name = element_blank(), 
                      breaks = c('Standard', 'LDV', 'GLARMA'),
                      values =c('Standard'='dotted', 'LDV'='dashed', 'GLARMA'='solid')) +
  scale_size_manual(name = element_blank(), 
                        breaks = c('Standard', 'LDV', 'GLARMA'),
                        values =c('Standard'=0.8, 'LDV'=0.6, 'GLARMA'=0.5)) +
  theme(panel.grid=element_blank(), 
        legend.background = element_rect(fill="transparent"),
        legend.position='top',
        legend.text=element_text(size=11))

### Wind speed ###
# Points to evaluate (extract from effects plot)
windspeed.x <- p21$data$x
# get the raw (untransformed) prediction
windspeed.y_raw <- beta_arma[1] + airtemp.mean*beta_arma[2] + windspeed.x*beta_arma[3] + rain.mean*beta_arma[4] + wday.mean*beta_arma[5]
# transform with logistic function.
windspeed.y <- exp(windspeed.y_raw)/(1+exp(windspeed.y_raw))

# set an empty vector for the standard errors and evaluate for each point.
windspeed.se = numeric(length(windspeed.x))
for (i in 1:length(windspeed.x)) {
  x = windspeed.x[i]
  windspeed.se[i] <- deltamethod(~ exp(x1 + x2*airtemp.mean + x3*x + x4*rain.mean + x5*wday.mean)/
                                   (1 + exp(x1 + x2*airtemp.mean + x3*x + x4*rain.mean + x5*wday.mean)), 
                                 beta_arma, cov_arma)
}

# create confidence intervals.
windspeed.y.lo <- windspeed.y - t.crit*windspeed.se; windspeed.y.lo[windspeed.y.lo<0] = 0
windspeed.y.hi <- windspeed.y + t.crit*windspeed.se

# collect everything in a tibble (dataframe), including the other models.
windspeed.data <- tibble(windspeed.x, 
                         p21$data$predicted, p21$data$conf.low, p21$data$conf.high,
                         p22$data$predicted, p22$data$conf.low, p22$data$conf.high,
                         windspeed.y, windspeed.y.lo, windspeed.y.hi)

# make the plot.
windspeed_plot <- windspeed.data %>% ggplot(aes(windspeed.x)) +
  geom_ribbon(aes(ymin=p21$data$conf.low, ymax=p21$data$conf.high), fill='goldenrod', alpha=0.1) + 
  geom_line(aes(y=p21$data$predicted), colour='goldenrod', linetype='dotted', size=0.8) +
  geom_ribbon(aes(ymin=p22$data$conf.low, ymax=p22$data$conf.high), fill='darkorchid', alpha=0.1) + 
  geom_line(aes(y=p22$data$predicted), colour='darkorchid', linetype='dashed', size=0.6) +
  geom_ribbon(aes(ymin=windspeed.y.lo, ymax=windspeed.y.hi), fill='limegreen', alpha=0.1) + 
  geom_line(aes(y=windspeed.y), colour='limegreen', linetype='solid') +
  xlab('Mean Wind Speed (m/s)') +
  ylab('E(Y|x)') +
  ylim(0, 0.6) +
  theme(panel.grid=element_blank(), legend.position="none")

### Rain intensity ###
# Points to evaluate (extract from effects plot)
rain.x <- p31$data$x
# get the raw (untransformed) prediction
rain.y_raw <- beta_arma[1] + airtemp.mean*beta_arma[2] + windspeed.mean*beta_arma[3] + rain.x*beta_arma[4] + wday.mean*beta_arma[5]
# transform with logistic function.
rain.y <- exp(rain.y_raw)/(1+exp(rain.y_raw))

# set an empty vector for the standard errors and evaluate for each point.
rain.se = numeric(length(rain.x))
for (i in 1:length(rain.x)) {
  x = rain.x[i]
  rain.se[i] <- deltamethod(~ exp(x1 + x2*airtemp.mean + x3*windspeed.mean + x4*x + x5*wday.mean)/
                              (1 + exp(x1 + x2*airtemp.mean + x3*windspeed.mean + x4*x + x5*wday.mean)), 
                            beta_arma, cov_arma)
}

# create confidence intervals.
rain.y.lo <- rain.y - t.crit*rain.se; rain.y.lo[rain.y.lo<0] = 0
rain.y.hi <- rain.y + t.crit*rain.se

# collect everything in a tibble (dataframe), including the other models.
rain.data <- tibble(rain.x, 
                    p31$data$predicted, p31$data$conf.low, p31$data$conf.high,
                    p32$data$predicted, p32$data$conf.low, p32$data$conf.high,
                    rain.y, rain.y.lo, rain.y.hi)

# make the plot.
rain_plot <- rain.data %>% ggplot(aes(rain.x)) +
  geom_ribbon(aes(ymin=p31$data$conf.low, ymax=p31$data$conf.high), fill='goldenrod', alpha=0.1) + 
  geom_line(aes(y=p31$data$predicted), colour='goldenrod', linetype='dotted', size=0.8) +
  geom_ribbon(aes(ymin=p32$data$conf.low, ymax=p32$data$conf.high), fill='darkorchid', alpha=0.1) + 
  geom_line(aes(y=p32$data$predicted), colour='darkorchid', linetype='dashed', size=0.6) +
  geom_ribbon(aes(ymin=rain.y.lo, ymax=rain.y.hi), fill='limegreen', alpha=0.1) + 
  geom_line(aes(y=rain.y), colour='limegreen', linetype='solid') +
  xlab('Mean Rainfall Intensity (mm/15min)') +
  ylab('E(Y|x)') +
  ylim(0, 0.6) +
  theme(panel.grid=element_blank(), legend.position="none")

# put all effect plots in a grid.
grid.arrange(airtemp_plot, windspeed_plot, rain_plot, heights=c(39, 31, 31))
