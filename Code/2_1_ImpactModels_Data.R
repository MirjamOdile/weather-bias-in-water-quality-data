### ----------------------------------
###                                                                          
### Weather Bias In Water Quality Data                           
### Data Cleaning and Aggregation                                                       
###
### Author: Mirjam Nanko
### Email: m.nanko@exeter.ac.uk
### Date: April 22, 2021
###
### ----------------------------------

# Load packages
library(tidyverse)
library(lubridate)
library(naniar)


# Specify root as path to 'Data' folder containing the lake data folder 
# 'Great Pond Data'
# Note: trailing slash required to join path, i.e. "path/to/folder/Data/"
root <- '/home/mirjam/GitHub/weather-bias-in-water-quality-data/Data/'

# Create new folders to save aggregated data
dir.create(file.path(root, 'Data Cleaned'))
dir.create(file.path(paste(root, 'Data Cleaned', sep=""), 'GreatPond'))


################################################################################
###                           MANUAL SAMPLING DATA                           ###
################################################################################


# Set the working directory

setwd(paste(root, 'Great Pond Data/Colby Data', sep=""))
getwd()
list.files()


#--------------------#
#  Data preparation  #
#--------------------#


manual1_16 <- read.csv("GPDEP1_2016_InSitu.csv", skip=2, 
                     header=FALSE, stringsAsFactors = FALSE)[c(2,4,5,6,15)]
manual2_16 <- read.csv("GPDEP2_2016_InSitu.csv", skip=2,
                     header=FALSE, stringsAsFactors = FALSE)[c(2,4,5,6,15)]
manual1_17 <- read.csv("GPDEP1_2017_InSitu.csv", skip=2,
                     header=FALSE, stringsAsFactors = FALSE)[c(2,4,5,6,15)]
manual2_17 <- read.csv("GPDEP2_2017_InSitu.csv", skip=2,
                     header=FALSE, stringsAsFactors = FALSE)[c(2,4,5,6,15)]
manual1_18 <- read.csv("GPDEP1_2018_InSitu.csv", skip=2,
                     header=FALSE, stringsAsFactors = FALSE)[c(2,4,5,6,15)]
manual2_18 <- read.csv("GPDEP2_2018_InSitu.csv", skip=2,
                     header=FALSE, stringsAsFactors = FALSE)[c(2,4,5,6,15)]
manual1_19 <- read.csv("GPDEP1_2019_InSitu.csv", skip=2,
                     header=FALSE, stringsAsFactors = FALSE)[c(2,4,5,6,15)]
manual2_19 <- read.csv("GPDEP2_2019_InSitu.csv", skip=2,
                     header=FALSE, stringsAsFactors = FALSE)[c(2,4,5,6,15)]

manual <- rbind(manual1_16, manual2_16, manual1_17, manual2_17, 
              manual1_18, manual2_18, manual1_19, manual2_19)

names(manual) <- c("Timestamp", "Temp", "RDO", "RDO.Sat", "Depth")

# Check result
head(manual)

# Drop individual datasets
rm(manual1_16, manual2_16, manual1_17, manual2_17, 
   manual1_18, manual2_18, manual1_19, manual2_19)

# Extract date-time
manual$Datetime <- as_datetime(manual$Timestamp, format ="%Y-%m-%d %H:%M:%S")
manual$Datetime[is.na(manual$Datetime)] =
  as_datetime(manual$Timestamp[is.na(manual$Datetime)],
              format = "%d/%m/%Y %H:%M:%S")
manual$Datetime[is.na(manual$Datetime)] =
  as_datetime(manual$Timestamp[is.na(manual$Datetime)],
              format = "%d/%m/%Y %H:%M")
manual$Datetime[is.na(manual$Datetime)] =
  as_datetime(manual$Timestamp[is.na(manual$Datetime)],
              format = "%m/%d/%Y %H:%M:%S")
manual$Datetime[is.na(manual$Datetime)] =
  as_datetime(manual$Timestamp[is.na(manual$Datetime)],
              format = "%m/%d/%Y %H:%M")
summary(manual$Datetime)

# Extract date
manual$Date <- date(manual$Datetime)

# Extract time
manual$Time <- hms::as_hms(manual$Datetime)

# Inspect manual
head(manual)
summary(manual)
plot(manual$Date, hour(manual$Time))

# Sort the manual sampling data (seeing that the data was saved in 2 csv files 
# per year, not all observations are in the correct order)
manual <- manual[order(manual$Datetime),]
# Reset the index
rownames(manual) <- NULL
# Create an identifier
manual$id <- as.numeric(rownames(manual))

# Save the cleaned data
write.csv(manual, 
          paste(root, 'Data Cleaned/GreatPond/GP_manual_2016-2019_temp_do.csv', 
                sep=""),
          row.names=FALSE)

# Drop data
rm(manual)



################################################################################
###                                 BUOY DATA                                ###
################################################################################


setwd(paste(root, 'Great Pond Data/Goldie', sep=""))
getwd()
list.files()


#--------------------#
#  Data preparation  #
#--------------------#


# buoy.var iable.names <- names(read.csv("GoldieClean2016.csv", skip=0, 
                       # header=TRUE, stringsAsFactors = FALSE))

goldie2016 <- read.csv("GoldieClean2016.csv", skip=2, 
                       header=FALSE, stringsAsFactors = FALSE)
goldie2017 <- read.csv("GoldieClean2017.csv", skip=2, 
                       header=FALSE, stringsAsFactors = FALSE)
goldie2018 <- read.csv("GoldieClean2018.csv", skip=2, 
                       header=FALSE, stringsAsFactors = FALSE)
goldie2019 <- read.csv("GoldieClean2019.csv", skip=2, 
                       header=FALSE, stringsAsFactors = FALSE)

buoy <- rbind(goldie2016, goldie2017, goldie2018, goldie2019)

names(buoy) <- c("id", "Date.Time", "Underwater.PAR", "Surface.PAR",
                 "Chlorophyll.2m", "Chlorophyll.6m", 
                 "DO.2m", "DO.6m", "DO.9.6m", "DO.16m", 
                 "T.1m", "T.3m", "T.5m", "T.7m", "T.9m", "T.11m", "T.13m",
                 "T.15m", "T.17m", "T.19m")

# Check result
head(buoy)

# Drop individual datasets
rm(goldie2016, goldie2017, goldie2018, goldie2019)

# Extract date-time
buoy$Datetime <- as_datetime(buoy$Date.Time, format ="%Y-%m-%d %H:%M:%S")
summary(buoy$Datetime)

# Extract date
buoy$Date <- date(buoy$Datetime)

# Extract time
buoy$Time <- hms::as_hms(buoy$Datetime)

# Extract time
buoy$Year <- year(buoy$Datetime)

# Inspect buoy data
head(buoy)
summary(buoy)
length(unique(buoy$Date)) # 448 days


#------------------------------------------------------#
#  Investigate data & remove unrealistic measurements  #
#------------------------------------------------------#

# Mark obviously wrong values as NAs
buoy <- replace_with_na_at(buoy, 
                           .vars = c("DO.2m", "DO.6m", "DO.9.6m", "DO.16m"),
                           condition = ~.x > 9000)
buoy <- replace_with_na_at(buoy, 
                           .vars = c("T.1m", "T.3m", "T.5m", "T.7m", "T.9m", 
                                     "T.11m", "T.13m", "T.15m", "T.17m", "T.19m",
                                     "Chlorophyll.2m", "Chlorophyll.6m"),
                           condition = ~.x <= -100)

# Some days exhibit unrealistic measurements, especially at the beginning and
# end of the sampling season when the buoy is put into and out of operation.
# These days are removed from the dataset as all the measurements on theses days
# are potentially biased. 

outlier.dates <- as.Date(c("2016-04-15", "2016-04-16",                # 2016
                           "2017-06-14", "2017-07-12", "2017-07-20",  # 2017
                           "2018-08-07", "2018-08-08", "2018-08-09",  # 2018
                           "2019-06-03", "2019-06-04", "2019-06-05")) # 2019

outlier <- ifelse(buoy$Date %in% outlier.dates, 'red', 'black')

# Plot the outliers
par(mfrow=c(2,5))
for(depth in 11:20){
  name = names(buoy[,as.numeric(depth):as.numeric(depth+1)])[1]
  plot(buoy$Datetime, buoy[,depth], 
       col = alpha(outlier, 0.7),
       ylim = c(5,70),
       xlab = "Year", ylab = name)
}

# Remove the outliers
buoy <- subset(buoy, ! Date %in% outlier.dates)

# Check the remaining data
outlier.dates <- as.Date(c("2018-10-16", "2018-10-17", "2018-10-18",
                           "2018-10-19", "2018-10-20", "2018-10-21", 
                           "2018-10-22"))
outlier <- ifelse(buoy$Date %in% outlier.dates, 'red', 'black')
par(mfrow=c(2,5))
for(depth in 11:20){
  name = names(buoy[,as.numeric(depth):as.numeric(depth+1)])[1]
  plot(buoy$Datetime, buoy[,depth], 
       col = alpha(outlier, 0.7),
       ylim = c(5,30),
       xlab = "Year", ylab = name)
}

# >> Some suspicious observations at the end of the sampling season 2018 remain.

# Check the individual years for a more detailed look:
# 2016: looks good
par(mfrow=c(2,5))
for(depth in 11:20){
  name = names(buoy[,as.numeric(depth):as.numeric(depth+1)])[1]
  plot(buoy$Date[buoy$Year == 2016], 
       buoy[,depth][buoy$Year == 2016],
       xlim = as.Date(c("2016-04-01","2016-10-31")), ylim = c(5,30),
       xlab = "Month (2016)", ylab = name)
}
# 2017: looks good
par(mfrow=c(2,5))
for(depth in 11:20){
  name = names(buoy[,as.numeric(depth):as.numeric(depth+1)])[1]
  plot(buoy$Date[buoy$Year == 2017], 
       buoy[,depth][buoy$Year == 2017], 
       xlim = as.Date(c("2017-04-01","2017-10-31")), ylim = c(5,30),
       xlab = "Month (2017)", ylab = name)
}
# 2018: some unrealistic measurements at the end of the season that 
# will be removed
outlier <- ifelse(buoy$Date[buoy$Year == 2018] %in% outlier.dates, 
                  'red', 'black')
par(mfrow=c(2,5))
for(depth in 11:20){
  name = names(buoy[,as.numeric(depth):as.numeric(depth+1)])[1]
  plot(buoy$Date[buoy$Year == 2018], 
       buoy[,depth][buoy$Year == 2018], 
       col = alpha(outlier, 0.7),
       xlim = as.Date(c("2018-04-01","2018-10-31")), ylim = c(5,30),
       xlab = "Month (2018)", ylab = name)
}
# 2019: looks good
par(mfrow=c(2,5))
for(depth in 11:20){
  name = names(buoy[,as.numeric(depth):as.numeric(depth+1)])[1]
  plot(buoy$Date[buoy$Year == 2019], 
       buoy[,depth][buoy$Year == 2019],
       xlim = as.Date(c("2019-04-01","2019-10-31")), ylim = c(5,30),
       xlab = "Month (2019)", ylab = name)
}

# Remove the outliers
buoy <- subset(buoy, ! Date %in% outlier.dates)

# Check the remaining data
par(mfrow=c(2,5))
for(depth in 11:20){
  name = names(buoy[,as.numeric(depth):as.numeric(depth+1)])[1]
  plot(buoy$Datetime, buoy[,depth], 
       ylim = c(5,30),
       xlab = "Year", ylab = name)
}
par(mfrow=c(2,5))
for(depth in 11:20){
  name = names(buoy[,as.numeric(depth):as.numeric(depth+1)])[1]
  plot(buoy$Date[buoy$Year == 2018], 
       buoy[,depth][buoy$Year == 2018], 
       xlim = as.Date(c("2018-04-01","2018-10-31")), ylim = c(5,30),
       xlab = "Month (2018)", ylab = name)
}

# Looks good. No more unrealistic measurements.
length(unique(buoy$Date))
# 18 out of 448 manual sampling days dropped (~4%)


# Save the cleaned data
write.csv(buoy,
          paste(root, 'Data Cleaned/GreatPond/GP_buoy_2016-2019_temp_do.csv', 
                sep=""),
          row.names=FALSE)

# Drop data
rm(buoy)
