### ----------------------------------
###                                                                          
### Weather Bias In Water Quality Data                           
### Logistic regression models                                                                    
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
root <- '/home/mikkel/OneDrive/Projects/WeatherBiasArticle/Data/Data Cleaned'

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

# Gather automatic temperature measurements into long format
automatic <- gather(automatic[c(21, 11:20)],  "Depth", "Temp", 2:11)
automatic$Depth <- as.numeric(str_extract(automatic$Depth, "\\d+"))
automatic$Date <- as.Date(automatic$Datetime)
automatic$Year <- year(automatic$Datetime)
automatic$Time <- hms::as_hms(automatic$Datetime)
automatic <- drop_na(automatic) # Drop missing temperature measurements
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
  summarize(start = min(Datetime),
            stop = max(Datetime))

# Subset the automatic measurements to coincide with the manual ones.
# First manual series
automatic_subset = filter(automatic, Datetime >= manual_times[1,]$start, Datetime <=  manual_times[1,]$stop)
# Iterate through the remaining manual series.
for (i in 2:nrow(manual_times)){
  automatic_subset <- rbind(automatic_subset, filter(automatic, 
                                                     Datetime >= manual_times[i,]$start, 
                                                     Datetime <=  manual_times[i,]$stop))
}

#automatic_subset <- automatic %>%
#  filter(Date %in% unique(manual$Date))

# Plot it.
hist1 <- ggplot(automatic_subset, aes(Temp)) + 
  geom_histogram(aes(y=..density..), fill='orangered', alpha=0.5, bins=50) + 
  geom_density(color='orangered', size=1, bw=1) +
  geom_histogram(data=automatic, aes(y=..density..), fill='steelblue1', alpha=0.5, bins=50) +
  geom_density(data=automatic, color='steelblue1', size=1, bw=1) +
  scale_y_continuous(breaks=c(0.1, 0.2)) +
  ggtitle('All measurement depths') +
  theme(plot.title = element_text(size = 10)) +
  labs(x = element_blank(),
       y = 'Density')

hist2 <- ggplot(filter(automatic_subset, Depth<=8), aes(Temp)) + 
  geom_histogram(aes(y=..density..), fill='orangered', alpha=0.5, bins=50) + 
  geom_density(color='orangered', size=1, bw=1) +
  geom_histogram(data=filter(automatic, Depth<=7), aes(y=..density..), fill='steelblue1', alpha=0.5, bins=50) +
  geom_density(data=filter(automatic, Depth<=7), color='steelblue1', size=1, bw=1) +
  scale_y_continuous(breaks=c(0.1, 0.2)) +
  ggtitle('Measurement Depths 0-8m') +
  theme(plot.title = element_text(size = 10)) +
  labs(x = element_blank(),
       y = 'Density')

hist3 <- ggplot(filter(automatic_subset, Depth>8),aes(Temp)) + 
  geom_histogram(aes(y=..density..), fill='orangered', alpha=0.5, bins=50) + 
  geom_density(color='orangered', size=1, bw=1) +
  geom_histogram(data=filter(automatic, Depth>7), aes(y=..density..), fill='steelblue1', alpha=0.5, bins=50) +
  geom_density(data=filter(automatic, Depth>7), color='steelblue1', size=1, bw=1) +
  ggtitle('Measurement Depths 8-20m') +
  theme(plot.title = element_text(size = 10)) +
  labs(x = 'Temperature (°C)',
       y = 'Density')

grid.arrange(hist1, hist2, hist3)

### Compare the subset with the full automatic dataset.

# All depths
t.test(automatic_subset$Temp, automatic$Temp)
# >> Statistically significant difference in means.

# 1-7m
t.test(automatic_subset$Temp[automatic_subset$Depth <= 7], automatic$Temp[automatic$Depth <= 7])
# >> Statistically significant difference in means.

# 9-19m
t.test(automatic_subset$Temp[automatic_subset$Depth > 7], automatic$Temp[automatic$Depth > 7])
# >> Statistically significant difference in means.

# Do tests for all depths
depths = unique(automatic_subset$Depth)
all_tests <- vector('list', length(depths))
for (i in 1:length(depths)){
  all_tests[[i]] <- t.test(automatic_subset$Temp[automatic_subset$Depth == depths[i]], automatic$Temp[automatic$Depth == depths[i]])
  print(depths[i])
  print(all_tests[[i]])
}

# Using only daytime tempwratures to compare.
automatic_day <- automatic %>% filter(Time >= hms::as_hms('08:00:00'), 
                                      Time <= hms::as_hms('16:00:00'))

# All depths
t.test(automatic_subset$Temp, automatic_day$Temp)
# >> Statistically significant difference in means.

# 1-7m
t.test(automatic_subset$Temp[automatic_subset$Depth <= 7], automatic_day$Temp[automatic_day$Depth <= 7])
# >> Statistically significant difference in means.

# 9-19m
t.test(automatic_subset$Temp[automatic_subset$Depth > 7], automatic_day$Temp[automatic_day$Depth > 7])
# >> Statistically significant difference in means.

# Do tests for all depths
depths = unique(automatic_subset$Depth)
all_tests <- vector('list', length(depths))
for (i in 1:length(depths)){
  all_tests[[i]] <- t.test(automatic_subset$Temp[automatic_subset$Depth == depths[i]], automatic_day$Temp[automatic_day$Depth == depths[i]])
  print(depths[i])
  print(all_tests[[i]])
}

# Get the means per depth.for all the datasets.
# All automatic samples
automatic_means <- automatic %>% group_by(Depth) %>%
  summarize(mean=mean(Temp))

# Daytime automatic samples.
automatic_day_means <- automatic_day %>% group_by(Depth) %>%
  summarize(mean=mean(Temp))

# Automatic samples during manual samples.
automatic_subsets_means <- automatic_subset %>% group_by(Depth) %>%
  summarize(mean=mean(Temp))

# Manual samples.
manual_means <- manual %>% group_by(Depth) %>%
  summarize(mean=mean(Temp))

# Plug all the means together in a dataframe.
comparison <- bind_rows('automatic'=automatic_means, 
                        'automatic (day)'=automatic_day_means, 
                        'automatic (subset)'=automatic_subsets_means, 
                        'manual'=manual_means, 
                        .id='dataset')

# Plot that dataframe
comparison %>% ggplot(aes(Depth, mean, color=dataset)) +
  geom_point() +
  ylab('Mean Temperature') +
  scale_x_reverse() +
  coord_flip() +
  scale_color_brewer(palette="Paired") +
  theme(legend.title=element_blank(), legend.position="top")

