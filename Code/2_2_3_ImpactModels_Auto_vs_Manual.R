### ----------------------------------
###                                                                          
### Weather Bias In Water Quality Data                           
### Bias Estimation, automatic vs. manual.                                                              
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
###  Paired t-test: do the automatic and manual samples  ###
###          measure the same on the same days?          ###
############################################################

#------------------------------------------------#
#  Match the automatic and manual sampling days  #
#------------------------------------------------#

matched.automatic <- subset(automatic, Date %in% manual$Date)
length(unique(matched.automatic$Date)) # 46 matches
matched.manual <- subset(manual, Date %in% automatic$Date)
length(unique(matched.manual$Date)) # 46 matches
# >> On 46 days, both automatic and manual sampling took place.

# Temperature by Depth (violin + jitter)
v1 <- ggplot(matched.automatic, aes(as.factor(Depth), Temp)) + 
  geom_jitter(aes(color = month(Datetime, label = T)),
              width = 0.2, shape = 4) +
  geom_violin(fill = 'grey80', alpha = 0.5, adjust = 2) +
  scale_y_continuous(limits = c(5, 28), breaks = c(0, 5, 10, 15, 20, 25)) +
  labs(x = 'Depth (m)', y = 'Temperature (°C)', color = 'Month')
v2 <- ggplot(na.omit(matched.manual), aes(as.factor(Depth), Temp)) + 
  geom_jitter(aes(color = month(Datetime, label = T)),
              width = 0.2, shape = 4) +
  geom_violin(fill = 'grey80', alpha = 0.5, adjust = 2) +
  scale_y_continuous(limits = c(5, 28), breaks = c(0, 5, 10, 15, 20, 25)) +
  labs(x = 'Depth (m)', y = 'Temperature (°C)', color = 'Month')
grid.arrange(v1, v2)
# >> Similar distribtions at shallow depth (1-7m)
# >> Sensor lag at the deeper depths (9-19m)

#---------------------------------------------------------------------#
#  Aggregate and merge the matched automatic and manual measurements  #
#---------------------------------------------------------------------#

# Aggregate the matched automatic measurements by date and depth.
matched.automatic.agg <- matched.automatic %>% group_by(Date, Depth) %>%
  summarise(Temp.automatic.mean = mean(Temp, na.rm = T),
            Temp.automatic.var = var(Temp, na.rm = T))
dim(matched.automatic.agg)

# Aggregate the matched manual measurements by date and depth.
# Calculate the weighted mean and variance (weighted by proximity to the 
# specified depths, i.e. the closer the higher weighted).
matched.manual.agg <- matched.manual %>% group_by(Date, Depth) %>%
  summarise(Temp.manual.mean = mean(Temp),
            Temp.manual.var = var(Temp), 
            Temp.manual.mean.weighted = weighted.mean(Temp, Weights),
            Temp.manual.var.weighted = weighted_variance(Temp, Weights))
dim(matched.manual.agg)

# Merge the matched and aggregated data
paired <- merge(matched.automatic.agg, matched.manual.agg,
                        by = c('Date', 'Depth'))
paired %>% group_by(Depth) %>% 
  summarise(n = n()) %>% summarise(c = sum(n))

dim(paired)
rm(matched.automatic.agg, matched.manual.agg)

#-----------------------------------------------------------------#
#  Plot the  automatic vs manual daily mean temperatures by depth #
#-----------------------------------------------------------------#

# Automatic vs. manual daily mean temperature by depth (scatter)
ggplot(paired, aes(Temp.manual.mean.weighted, 
                           Temp.automatic.mean, 
                           color = as.factor(Depth))) + 
  geom_point(size=1) + 
  geom_errorbar(aes(ymin = Temp.automatic.mean - sqrt(Temp.automatic.var),
                    ymax = Temp.automatic.mean + sqrt(Temp.automatic.var)), 
                    alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, color = 'midnightblue') +
  labs(x = 'Manual: weighted mean temperature (°C)', 
       y = 'Automatic: daily mean temperature (°C)', color = 'Depth (m)') +
  scale_colour_viridis_d(option = 'plasma', end = 0.8, direction = -1)
# >> Low variance (y-axis error bars) shows that daily aggregation of the 
# automatic measurements is reasonable.
# >> Confirms bias at deeper depths (9-19m).


# Difference automatic vs. manual daily mean temperature by depth (violin)
ggplot(paired, aes(as.factor(Depth), 
                           Temp.manual.mean.weighted-Temp.automatic.mean, 
                           color = as.factor(Depth),
                           fill = as.factor(Depth))) +
  geom_violin(size = .7) +
  geom_point(shape = 1) +
  scale_color_viridis_d(option = 'plasma', end = 0.8, direction = -1) +
  scale_fill_viridis_d(option = 'plasma', end = 0.8, direction = -1,
                       alpha = 0.4) +
  labs(x = 'Depth (m)', color = 'Depth (m)', fill = 'Depth (m)',
       y = 'Difference manual vs. automatic daily mean temperature (°C)')
# >> Confirms bias at deeper depths (9-19m).

# Rotated and annotated version
textpos <- -4.25
arrowpos <- -3.75

ggplot(paired, aes(reorder(as.factor(Depth), desc(as.factor(Depth))), 
                   Temp.manual.mean.weighted-Temp.automatic.mean, 
                   color = as.factor(Depth),
                   fill = as.factor(Depth))) +
  geom_hline(yintercept=0, linetype='dotted') +
  geom_violin(size = .7) +
  geom_point(shape = 'o', size=2) +
  scale_color_viridis_d(option = 'plasma', end = 0.8, direction = -1) +
  scale_fill_viridis_d(option = 'plasma', end = 0.8, direction = -1,
                       alpha = 0.4) +
  labs(x = 'Depth (m)', color = 'Depth (m)', fill = 'Depth (m)',
       y = 'Manual Temperature Bias (°C)') + 
  ylim(-5,5) +
  annotate("text", x = 8.5, y = textpos, angle=90, label = "Epilimnion") +
  geom_segment(aes(x = 8.5, y = arrowpos, xend = 10, yend = arrowpos),
               arrow = arrow(length = unit(0.5, "cm"), angle=45), 
               size=0.5, color='gray20', lineend = 'round', linejoin = 'bevel') +
  geom_segment(aes(x = 8.5, y = arrowpos, xend = 7, yend = arrowpos),
               arrow = arrow(length = unit(0.5, "cm"), angle=45), 
               size=0.5, color='gray20', lineend = 'round', linejoin = 'bevel') +
  annotate("text", x = 3.5, y = textpos, angle=90, label = "Thermocline and Hypolimnion") +
  geom_segment(aes(x = 3.55, y = arrowpos, xend = 6, yend = arrowpos),
               arrow = arrow(length = unit(0.5, "cm"), angle=45), 
               size=0.5, color='gray20', lineend = 'round', linejoin = 'bevel') +
  geom_segment(aes(x = 3.55, y = arrowpos, xend = 1, yend = arrowpos),
               arrow = arrow(length = unit(0.5, "cm"), angle=45), 
               size=0.5, color='gray20', lineend = 'round', linejoin = 'bevel') +
  coord_flip() +
  theme(legend.position='none',
        axis.text=element_text(size=12),
        axis.title=element_text(size=14))

# Histograms
hist1 <- ggplot(paired) + 
  geom_histogram(aes(x=Temp.manual.mean.weighted, y=..density..), fill='orangered', alpha=0.5) + 
  geom_density(aes(x=Temp.manual.mean.weighted), color='orangered', size=1, bw=1) +
  geom_histogram(aes(x=Temp.automatic.mean, y=..density..), fill='steelblue1', alpha=0.5) +
  geom_density(aes(x=Temp.automatic.mean), color='steelblue1', size=1, bw=1) +
  scale_y_continuous(breaks=c(0.0, 0.1, 0.2)) +
  ggtitle('All measurement depths') +
  theme(plot.title = element_text(size = 10)) +
  labs(x = element_blank(),
       y = 'Density')

hist2 <- ggplot(filter(paired, Depth<=7)) + 
  geom_histogram(aes(x=Temp.manual.mean.weighted, y=..density..), fill='orangered', alpha=0.5) + 
  geom_density(aes(x=Temp.manual.mean.weighted), color='orangered', size=1, bw=1) +
  geom_histogram(aes(x=Temp.automatic.mean, y=..density..), fill='steelblue1', alpha=0.5) +
  geom_density(aes(x=Temp.automatic.mean), color='steelblue1', size=1, bw=1) +
  scale_y_continuous(breaks=c(0.0, 0.1, 0.2)) +
  ggtitle('Measurement Depths 1-7m') +
  theme(plot.title = element_text(size = 10)) +
  labs(x = element_blank(),
       y = 'Density')

hist3 <- ggplot(filter(paired, Depth>7)) + 
  geom_histogram(aes(x=Temp.manual.mean.weighted, y=..density..), fill='orangered', alpha=0.5) + 
  geom_density(aes(x=Temp.manual.mean.weighted), color='orangered', size=1, bw=1) +
  geom_histogram(aes(x=Temp.automatic.mean, y=..density..), fill='steelblue1', alpha=0.5) +
  geom_density(aes(x=Temp.automatic.mean), color='steelblue1', size=1, bw=1) +
  ggtitle('Measurement Depths 9-19m') +
  theme(plot.title = element_text(size = 10)) +
  labs(x = 'Temperature (°C)',
       y = 'Density')

grid.arrange(hist1, hist2, hist3)

#--------------------------#
#  Run the paired t-tests  #
#--------------------------#

# All depths
t.test(paired$Temp.manual.mean.weighted, 
       paired$Temp.automatic.mean, paired = T)
# >> Statistically significant difference in means.

# 1-7m
t.test(paired$Temp.manual.mean.weighted[paired$Depth<=7], 
       paired$Temp.automatic.mean[paired$Depth<=7], paired = T)
# >> No statistically significant difference in means.

# 9-19m
t.test(paired$Temp.manual.mean.weighted[paired$Depth>7], 
       paired$Temp.automatic.mean[paired$Depth>7],
       paired = T)
# >> Statistically significant difference in means.

# >> We find a statistically significant difference in means at the deeper 
# depths 9-19m, which we theorise is due to the rapid temperature changes in 
# the thermocline in combination with sensor lag.

# Bayesian estimation for two groups
library(BayesFactor)
library(bayestestR)

# All depths
fit_all <- ttestBF(paired$Temp.manual.mean.weighted, 
                   paired$Temp.automatic.mean, 
                   paired = T, posterior=T, iterations=10000)
summary(fit_all)
hdi(fit_all)
p_rope(fit_all)
plot(fit_all)

# Lower depths
fit_lo <- ttestBF(paired$Temp.manual.mean.weighted[paired$Depth<=7], 
                  paired$Temp.automatic.mean[paired$Depth<=7], 
                  paired = T, posterior=T, iterations=10000)
summary(fit_lo)
hdi(fit_lo)
p_rope(fit_lo)
plot(fit_lo)

# Higher depths
fit_hi <- ttestBF(paired$Temp.manual.mean.weighted[paired$Depth>7], 
                  paired$Temp.automatic.mean[paired$Depth>7], 
                  paired = T, posterior=T, iterations=10000)
summary(fit_hi)
hdi(fit_hi)
p_rope(fit_hi)
plot(fit_hi)

# Jensen-Shannon divergence
library(philentropy)

bins = seq(4, 30, 1)
hist_manual_all = hist(paired$Temp.manual.mean.weighted, breaks=bins, plot=F)
hist_automatic_all = hist(paired$Temp.automatic.mean, breaks=bins, plot=F)
JSD(rbind(hist_manual_all$density, hist_automatic_all$density))

hist_manual_lo = hist(paired$Temp.manual.mean.weighted[paired$Depth<=7], breaks=bins, plot=F)
hist_automatic_lo = hist(paired$Temp.automatic.mean[paired$Depth<=7], breaks=bins, plot=F)
JSD(rbind(hist_manual_lo$density, hist_automatic_lo$density))

hist_manual_hi = hist(paired$Temp.manual.mean.weighted[paired$Depth>7], breaks=bins, plot=F)
hist_automatic_hi = hist(paired$Temp.automatic.mean[paired$Depth>7], breaks=bins, plot=F)
JSD(rbind(hist_manual_hi$density, hist_automatic_hi$density))


##################################################################
###  Two-sample t-test: are the means of the population of the ###
###          automatic and manual samples the same?            ###
##################################################################

# Histogram plots of the two-sample data
hist1 <- ggplot(manual, aes(Temp)) + 
  geom_histogram(aes(y=..density..), fill='orangered', alpha=0.5) + 
  geom_density(color='orangered', size=1, bw=1) +
  geom_histogram(data=automatic, aes(y=..density..), fill='steelblue1', alpha=0.5) +
  geom_density(data=automatic, color='steelblue1', size=1, bw=1) +
  scale_y_continuous(breaks=c(0.1, 0.2)) +
  ggtitle('All measurement depths') +
  theme(plot.title = element_text(size = 10)) +
  labs(x = element_blank(),
       y = 'Density')

hist2 <- ggplot(filter(manual, Depth<=8), aes(Temp)) + 
  geom_histogram(aes(y=..density..), fill='orangered', alpha=0.5) + 
  geom_density(color='orangered', size=1, bw=1) +
  geom_histogram(data=filter(automatic, Depth<=7), aes(y=..density..), fill='steelblue1', alpha=0.5) +
  geom_density(data=filter(automatic, Depth<=7), color='steelblue1', size=1, bw=1) +
  scale_y_continuous(breaks=c(0.1, 0.2)) +
  ggtitle('Measurement Depths 0-8m') +
  theme(plot.title = element_text(size = 10)) +
  labs(x = element_blank(),
       y = 'Density')

hist3 <- ggplot(filter(manual, Depth>8),aes(Temp)) + 
  geom_histogram(aes(y=..density..), fill='orangered', alpha=0.5) + 
  geom_density(color='orangered', size=1, bw=1) +
  geom_histogram(data=filter(automatic, Depth>7), aes(y=..density..), fill='steelblue1', alpha=0.5) +
  geom_density(data=filter(automatic, Depth>7), color='steelblue1', size=1, bw=1) +
  ggtitle('Measurement Depths 8-20m') +
  theme(plot.title = element_text(size = 10)) +
  labs(x = 'Temperature (°C)',
       y = 'Density')


grid.arrange(hist1, hist2, hist3)

#-------------------#
#  Run the t-tests  #
#-------------------#

# All depths
t.test(manual$Temp, automatic$Temp)
       # alternative = 'greater')
# >> Statistically significant difference in means.

# 1-7m
t.test(manual$Temp[manual$Depth <= 7], automatic$Temp[automatic$Depth <= 7])
       # alternative = 'greater')
# >> Statistically significant difference in means.

# 9-19m
t.test(manual$Temp[manual$Depth > 7], automatic$Temp[automatic$Depth > 7])
       # alternative = 'greater')
# >> Statistically significant difference in means.

# Bayesian estimation for two groups
fit_all <- ttestBF(manual$Temp, automatic$Temp,
                   posterior=T, iterations=10000)
summary(fit_all)
hdi(fit_all)
p_rope(fit_all)
plot(fit_all)

# 1-7m
fit_lo <- ttestBF(manual$Temp[manual$Depth <= 7], 
                  automatic$Temp[automatic$Depth <= 7],
                  posterior=T, iterations=10000)
summary(fit_lo)
hdi(fit_lo)
p_rope(fit_lo)
plot(fit_lo)

# 9-19m
fit_hi <- ttestBF(manual$Temp[manual$Depth > 7], 
                  automatic$Temp[automatic$Depth > 7], 
                  posterior=T, iterations=10000)
summary(fit_hi)
hdi(fit_hi)
p_rope(fit_hi)
plot(fit_hi)

# Jensen-Shannon divergence
hist_manual_all = hist(manual$Temp, breaks=bins, plot=F)
hist_automatic_all = hist(automatic$Temp, breaks=bins, plot=F)
JSD(rbind(hist_manual_all$density, hist_automatic_all$density))

hist_automatic_lo = hist(automatic$Temp[automatic$Depth<=7], breaks=bins, plot=F)
hist_manual_lo = hist(manual$Temp[manual$Depth<=7], breaks=bins, plot=F)
JSD(rbind(hist_manual_lo$density, hist_automatic_lo$density))

hist_automatic_hi = hist(automatic$Temp[automatic$Depth>7], breaks=bins, plot=F)
hist_manual_hi = hist(manual$Temp[manual$Depth>7], breaks=bins, plot=F)
JSD(rbind(hist_manual_hi$density, hist_automatic_hi$density))

##################################################################
###  Two-sample t-test: Bias-corrected data                    ###
##################################################################

# Find the differences for each depth.
differences <- paired %>% group_by(Depth) %>%
  summarise(Diff = t.test(Temp.manual.mean.weighted, Temp.automatic.mean, paired = T)$estimate[[1]],
            CI_lo = t.test(Temp.manual.mean.weighted, Temp.automatic.mean, paired = T)$conf.int[1],
            CI_hi = t.test(Temp.manual.mean.weighted,  Temp.automatic.mean, paired = T)$conf.int[2],
            t = t.test(Temp.manual.mean.weighted,  Temp.automatic.mean, paired = T)$statistic[[1]],
            df = t.test(Temp.manual.mean.weighted,  Temp.automatic.mean, paired = T)$parameter[[1]],
            p = t.test(Temp.manual.mean.weighted, Temp.automatic.mean, paired = T)$p.value[[1]])

#library(stargazer)
#stargazer(as.data.frame(differences), summary=F, rownames=F)

# Correct using the computed differences.
manual <- manual %>%
  rowwise() %>%
  mutate(Temp_corrected = Temp-differences$Diff[differences$Depth==Depth])

# Plot the corrected manual measurements.
hist1 <- ggplot(manual, aes(Temp_corrected)) + 
  geom_histogram(aes(y=..density..), fill='orangered', alpha=0.5) + 
  geom_density(color='orangered', size=1, bw=1) +
  geom_histogram(data=automatic, aes(x=Temp, y=..density..), fill='steelblue1', alpha=0.5) +
  geom_density(data=automatic, aes(x=Temp), color='steelblue1', size=1, bw=1) +
  scale_y_continuous(breaks=c(0.0, 0.1, 0.2)) +
  ggtitle('All measurement depths') +
  theme(plot.title = element_text(size = 10)) +
  labs(x = element_blank(),
       y = 'Density')

hist2 <- ggplot(filter(manual, Depth<=8), aes(Temp_corrected)) + 
  geom_histogram(aes(y=..density..), fill='orangered', alpha=0.5) + 
  geom_density(color='orangered', size=1, bw=1) +
  geom_histogram(data=filter(automatic, Depth<=7), aes(x=Temp, y=..density..), fill='steelblue1', alpha=0.5) +
  geom_density(data=filter(automatic, Depth<=7), aes(x=Temp), color='steelblue1', size=1, bw=1) +
  scale_y_continuous(breaks=c(0.0, 0.1, 0.2)) +
  ggtitle('Measurement Depths 0-8m') +
  theme(plot.title = element_text(size = 10)) +
  labs(x = element_blank(),
       y = 'Density')

hist3 <- ggplot(filter(manual, Depth>8), aes(Temp_corrected)) + 
  geom_histogram(aes(y=..density..), fill='orangered', alpha=0.5) + 
  geom_density(color='orangered', size=1, bw=1) +
  geom_histogram(data=filter(automatic, Depth>7), aes(x=Temp, y=..density..), fill='steelblue1', alpha=0.5) +
  geom_density(data=filter(automatic, Depth>7), aes(x=Temp), color='steelblue1', size=1, bw=1) +
  ggtitle('Measurement Depths 8-20m') +
  theme(plot.title = element_text(size = 10)) +
  labs(x = 'Temperature (°C)',
       y = 'Density')

grid.arrange(hist1, hist2, hist3)

#-------------------#
#  Run the t-tests  #
#-------------------#

# All depths
t.test(manual$Temp_corrected, automatic$Temp)
# >> Statistically significant difference in means.

# 1-7m
t.test(manual$Temp_corrected[manual$Depth <= 7], automatic$Temp[automatic$Depth <= 7])
# >> Statistically significant difference in means.

# 9-19m
t.test(manual$Temp_corrected[manual$Depth > 7], automatic$Temp[automatic$Depth > 7])
# >> Statistically significant difference in means.

# Bayesian estimation for two groups
fit_all <- ttestBF(manual$Temp_corrected, automatic$Temp, 
                   posterior=T, iterations=10000)
summary(fit_all)
hdi(fit_all)
p_rope(fit_all)
plot(fit_all)

# 1-7m
fit_lo <- ttestBF(manual$Temp_corrected[manual$Depth <= 7], 
                  automatic$Temp[automatic$Depth <= 7], 
                  posterior=T, iterations=10000)
summary(fit_lo)
hdi(fit_lo)
p_rope(fit_all)
plot(fit_lo)

# 9-19m
fit_hi <- ttestBF(manual$Temp_corrected[manual$Depth > 7], 
                  automatic$Temp[automatic$Depth > 7], 
                  posterior=T, iterations=10000)
summary(fit_hi)
hdi(fit_hi)
p_rope(fit_hi)
plot(fit_hi)

# Jensen-Shannon divergence
hist_manual_all = hist(manual$Temp_corrected, breaks=bins, plot=F)
hist_automatic_all = hist(automatic$Temp, breaks=bins, plot=F)
JSD(rbind(hist_manual_all$density, hist_automatic_all$density))

hist_automatic_lo = hist(automatic$Temp[automatic$Depth<=7], breaks=bins, plot=F)
hist_manual_lo = hist(manual$Temp_corrected[manual$Depth<=7], breaks=bins, plot=F)
JSD(rbind(hist_manual_lo$density, hist_automatic_lo$density))

hist_manual_hi = hist(manual$Temp_corrected[manual$Depth>7], breaks=bins, plot=F)
hist_automatic_hi = hist(automatic$Temp[automatic$Depth>7], breaks=bins, plot=F)
JSD(rbind(hist_manual_hi$density, hist_automatic_hi$density))

# ##########################
# # Group Proportion Plot  #
# ##########################
# 
# desc <- cbind(
#   automatic %>% group_by(Depth) %>% 
#   summarise(automatic_n = n()) %>% 
#   mutate(Automatic = automatic_n/sum(automatic_n)),
#   manual %>% group_by(Depth) %>% 
#   summarise(manual_n = n()) %>% 
#   mutate(Manual = manual_n/sum(manual_n)),
#   paired %>% group_by(Depth) %>% 
#   summarise(paired_n = n()) %>% 
#   mutate(Paired = paired_n/sum(paired_n)))[,c(1,3,6,9)]
# 
# desc <- gather(desc, key = "Data", value = "Prop", 2:4)
# 
# ggplot(desc, aes(as.factor(Depth), Prop*100, 
#                  color = Data, fill = Data)) + 
#   geom_bar(stat = 'identity', position = "dodge", density=c(5,10,20) , angle=c(0,45,90)) +
#   scale_color_viridis_d(option = 'plasma', end = 0.8, direction = -1) +
#   scale_fill_viridis_d(option = 'plasma', end = 0.8, direction = -1,
#                        alpha = 0.7) +
#   scale_y_continuous(breaks = c(0,5,10)) +
#   labs(x = 'Depth (m)', color = 'Data', fill = 'Data',
#        y = 'Proportion (%)')

