### ----------------------------------
###                                                                          
### Weather Bias In Water Quality Data                           
### Bias Estimation, automatic subset, all variables.
###
### Author: Mirjam Nanko
### Email: m.nanko@exeter.ac.uk
### Date: April 27, 2021
###
### ----------------------------------


library(tidyverse); theme_set(theme_bw()) # data handling + plotting
library(lubridate) # dates
library(gridExtra) # arrange ggplots on a grid
library(fritools) # weighted variance


# Specify root as path to 'Data Cleaned' folder containing the lake data folders 
# 'Manual' and 'WeatherStation'
root <- '/home/mirjam/GitHub/weather-bias-in-water-quality-data/Data/Data Cleaned/'
root <- '/home/mikkel/OneDrive/Projects/WeatherBiasArticle/weather-bias-in-water-quality-data/Data/Data Cleaned/'

# Set the working directory

setwd(root)
getwd()
list.files()

##########################
###  Data preparation  ###
##########################

#---------------------#
#  Automatic samples  #
#---------------------#

# Read in the automatic sampling data.
automatic <- read.csv('GreatPond/GP_buoy_2016-2019_temp_do.csv',
                    stringsAsFactors = FALSE) %>%
           mutate(Datetime = as_datetime(Datetime),
                  Date = as_date(Date),
                  Time = hms::as_hms(Time),
                  Month = month(Date))

length(unique(automatic$Date))
# >> Measurements from 430 automatic sampling days were loaded.

# Extract date and compute PAR.
automatic$Date <- as.Date(automatic$Datetime)
automatic$Year <- year(automatic$Datetime)
automatic$Time <- hms::as_hms(automatic$Datetime)
automatic$PAR <- (automatic$Surface.PAR - automatic$Underwater.PAR)/automatic$Surface.PAR

#automatic <- drop_na(automatic) # Drop missing temperature measurements
nrow(automatic) 
length(unique(automatic$Date))
# >> After removing all missing temperature measurements, 351306 valid 
# measurements from 422 out of the 430 automatic sampling days remain.

#------------------#
#  Manual samples  #
#------------------#

# Read in the manual sampling data.
# Create a depth variable maytching the automatic measurements. 
# Specify weights to weigh samples depending on their proximity to the
# specified depths.
manual <- read.csv('GreatPond/GP_manual_2016-2019_temp_do.csv',
                    stringsAsFactors = FALSE) %>%
           mutate(Datetime = as_datetime(Datetime),
                  Date = as_date(Date),
                  Time = hms::as_hms(Time),
                  Year = year(Date),
                  Month = month(Date),
                  Depth.continuous = Depth,
                  Depth = as.numeric(case_when(
                                Depth >= 0  & Depth < 2  ~ "1",
                                Depth >= 2  & Depth < 4  ~ "3",
                                Depth >= 4  & Depth < 6  ~ "5",
                                Depth >= 6  & Depth < 8  ~ "7",
                                Depth >= 8  & Depth < 10 ~ "9",
                                Depth >= 10 & Depth < 12 ~ "11", 
                                Depth >= 12 & Depth < 14 ~ "13", 
                                Depth >= 14 & Depth < 16 ~ "15", 
                                Depth >= 16 & Depth < 18 ~ "17", 
                                Depth >= 18 & Depth < 20 ~ "19" )),
                  Distance = abs(Depth - Depth.continuous),
                  Weights = 1-Distance)

# Remove samples above the water surface
manual <- manual[manual$Depth.continuous>=0,]

nrow(manual)
length(unique(manual$Date))
# >> 19099 temperature measurements from 73 manual sampling days were loaded.

# Subset manual samples to the de facto automatic sampling period:
for (i in 2016:2019){
  manual$match[manual$Year == i] <- ifelse(
    manual$Date[manual$Year == i] >= min(automatic$Date[automatic$Year == i]) &
    manual$Date[manual$Year == i] <= max(automatic$Date[automatic$Year == i]), 
    'yes', 'no')
}
manual <- subset(manual, manual$match == 'yes')

nrow(manual)
length(unique(manual$Date))
# >> After subsetting the data to the de facto sampling season, 14260 valid 
# measurements from 51 out of the 73 manual sampling days remain.

############################################################
###  Comparing the total automatic data with a subsample ###
###  corresponding to the manual sampling times.         ###
############################################################

# Mark each manual sampling series.
manual$Series = 0
s = 1
manual$Series[1] = s

# If two measurements are more than an hour apart, increment the series counter.
for (i in 2:nrow(manual)){
  if (as.numeric(manual$Datetime[i] - manual$Datetime[i-1], units='hours') > 1){
    s = s + 1
  }
  manual$Series[i] = s
}

# Summarise the series to get start and stop times.
manual_times <- manual %>%
  group_by(Series) %>%
  summarize(start = min(Datetime) - dminutes(0),
            stop = max(Datetime) + dminutes(0))

# Subset the automatic measurements to coincide with the manual ones.
# First manual series
automatic_subset = filter(automatic, Datetime >= manual_times[1,]$start, Datetime <=  manual_times[1,]$stop)
# Iterate through the remaining manual series.
for (i in 2:nrow(manual_times)){
  automatic_subset <- rbind(automatic_subset, filter(automatic, 
                                                     Datetime >= manual_times[i,]$start, 
                                                     Datetime <=  manual_times[i,]$stop))
}

#automatic_remaining <- setdiff(automatic, automatic_subset)

# Get the variable names.
test_vars <- c(colnames(automatic)[5:20])

# Initialise empty vectors to holds the results.
diff <- vector('numeric', length(test_vars))
CI_lo <-  vector('numeric', length(test_vars))
CI_hi <-  vector('numeric', length(test_vars))
t <- vector('numeric', length(test_vars))
df <- vector('numeric', length(test_vars))
p <- vector('numeric', length(test_vars))

# Do all the tests.
for (i in 1:length(test_vars)){
  mu <- mean(automatic[[test_vars[i]]], na.rm=T)
  test <- t.test(automatic_subset[[test_vars[i]]], mu=mu)
  diff[i] <- test$estimate[1]-mu
  CI_lo[i] <- test$conf.int[1]-mu
  CI_hi[i] <- test$conf.int[2]-mu
  t[i] <- test$statistic
  df[i] <- test$parameter
  p[i] <- test$p.value
}

# Plug the tests into a dataframe.
all_tests <- tibble(test_vars, diff, CI_lo, CI_hi, t, df, p)

# Export the dataframe to latex.
library(knitr)
kable(all_tests, digits=3,
      col.names=c('Variable', 'mu_diff', 'CI (low)', 'CI (high)', 't', 'df', 'p'))
kable(all_tests, format='latex', booktabs=T, digits=3,
      col.names=c('Variable', 'mu_diff', 'CI (low)', 'CI (high)', 't', 'df', 'p'))
