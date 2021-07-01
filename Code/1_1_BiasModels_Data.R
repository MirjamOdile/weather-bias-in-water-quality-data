### ----------------------------------
###                                                                          
### Weather Bias In Water Quality Data                           
### Data Cleaning and Aggregation                                                                 
###
### Author: Mirjam Nanko
### Email: m.nanko@exeter.ac.uk
### Date: April 1, 2021
###
### ----------------------------------

# Load packages
# install.packages('lubridate')
library(lubridate)

# Specify root as path to 'Data' folder containing the lake data folders 
# 'Great Pond Data' and 'Other lakes' 
# Note: trailing slash required to join path, i.e. "path/to/folder/Data/"
root <- '/home/mirjam/GitHub/weather-bias-in-water-quality-data/Data/'

# Create new folders to save aggregated data
dir.create(file.path(root, 'Data Cleaned'))
dir.create(file.path(paste(root, 'Data Cleaned', sep=""), 'Manual'))
dir.create(file.path(paste(root, 'Data Cleaned', sep=""), 'WeatherStation'))


################################################################################
###                           MANUAL SAMPLING DATA                           ###
################################################################################

###--------------------------------------------------------------------------###
###                              'Great Pond'                                ###
###--------------------------------------------------------------------------###


# Set the working directory

setwd(paste(root, 'Great Pond Data/Colby Data', sep=""))
getwd()
list.files()


#--------------------#
#  Data preparation  #
#--------------------#


data1_16 <- read.csv("GPDEP1_2016_InSitu.csv", skip=2, 
                     header=FALSE, stringsAsFactors = FALSE )[2]
data2_16 <- read.csv("GPDEP2_2016_InSitu.csv", skip=2,
                     header=FALSE, stringsAsFactors = FALSE )[2]
data1_17 <- read.csv("GPDEP1_2017_InSitu.csv", skip=2,
                     header=FALSE, stringsAsFactors = FALSE )[2]
data2_17 <- read.csv("GPDEP2_2017_InSitu.csv", skip=2,
                     header=FALSE, stringsAsFactors = FALSE )[2]
data1_18 <- read.csv("GPDEP1_2018_InSitu.csv", skip=2,
                     header=FALSE, stringsAsFactors = FALSE )[2]
data2_18 <- read.csv("GPDEP2_2018_InSitu.csv", skip=2,
                     header=FALSE, stringsAsFactors = FALSE )[2]
data1_19 <- read.csv("GPDEP1_2019_InSitu.csv", skip=2,
                     header=FALSE, stringsAsFactors = FALSE )[2]
data2_19 <- read.csv("GPDEP2_2019_InSitu.csv", skip=2,
                     header=FALSE, stringsAsFactors = FALSE )[2]

data <- rbind(data1_16, data2_16, data1_17, data2_17, 
              data1_18, data2_18, data1_19, data2_19)


names(data) <- "Timestamp"

# Check result
head(data)

# Drop individual datasets
rm(data1_16, data2_16, data1_17, data2_17, 
   data1_18, data2_18, data1_19, data2_19)


# Extract date-time
data$Datetime <- as_datetime(data$Timestamp, format ="%Y-%m-%d %H:%M:%S")
data$Datetime[is.na(data$Datetime)] = as_datetime(data$Timestamp[is.na(data$Datetime)], 
                                                  format = "%d/%m/%Y %H:%M:%S")
data$Datetime[is.na(data$Datetime)] = as_datetime(data$Timestamp[is.na(data$Datetime)], 
                                                  format = "%d/%m/%Y %H:%M")
data$Datetime[is.na(data$Datetime)] = as_datetime(data$Timestamp[is.na(data$Datetime)], 
                                                  format = "%m/%d/%Y %H:%M:%S")
data$Datetime[is.na(data$Datetime)] = as_datetime(data$Timestamp[is.na(data$Datetime)], 
                                                  format = "%m/%d/%Y %H:%M")
summary(data$Datetime)


# Extract date
data$Date <- date(data$Datetime)

# Extract time
data$Time <- hms::as_hms(data$Datetime)

# Set Location
data$Location <-'Great Pond'

# Inspect data
head(data)
summary(data)
plot(data$Date, hour(data$Time))

# Save the cleaned data
write.csv(data, 
          paste(root, 'Data Cleaned/Manual/GPDEP_2016-2019_cleaned.csv', sep=""),
          row.names=FALSE)

# Drop data
rm(data)



###--------------------------------------------------------------------------###
###                               'Long Pond'                                ###
###--------------------------------------------------------------------------###


# Set the working directory
setwd(paste(root, 'Other lakes/Long Pond/', sep=""))
getwd()
list.files()


#--------------------#
#  Data preparation  #
#--------------------#


data1_16 <- read.csv("LPDEP1_2016_InSitu.csv", skip=2, 
                     header=FALSE, stringsAsFactors = FALSE )[2]
data2_16 <- read.csv("LPDEP2_2016_InSitu.csv", skip=2,
                     header=FALSE, stringsAsFactors = FALSE )[2]
data1_17 <- read.csv("LPDEP1_2017_InSitu.csv", skip=2,
                     header=FALSE, stringsAsFactors = FALSE )[2]
data2_17 <- read.csv("LPDEP2_2017_InSitu.csv", skip=2,
                     header=FALSE, stringsAsFactors = FALSE )[2]
data1_18 <- read.csv("LPDEP1_2018_InSitu.csv", skip=2,
                     header=FALSE, stringsAsFactors = FALSE )[2]
data2_18 <- read.csv("LPDEP2_2018_InSitu.csv", skip=2,
                     header=FALSE, stringsAsFactors = FALSE )[2]
data1_19 <- read.csv("LPDEP1_2019_InSitu.csv", skip=2,
                     header=FALSE, stringsAsFactors = FALSE )[2]
data2_19 <- read.csv("LPDEP2_2019_InSitu.csv", skip=2,
                     header=FALSE, stringsAsFactors = FALSE )[2]

data <- rbind(data1_16, data2_16, data1_17, data2_17, 
              data1_18, data2_18, data1_19, data2_19)


names(data) <- "Timestamp"

# Check result
head(data)

# Drop individual datasets
rm(data1_16, data2_16, data1_17, data2_17, 
   data1_18, data2_18, data1_19, data2_19)


# Extract date-time
data$Datetime <- as_datetime(data$Timestamp, format ="%Y-%m-%d %H:%M:%S")
data$Datetime[is.na(data$Datetime)] = as_datetime(data$Timestamp[is.na(data$Datetime)], 
                                                  format = "%d/%m/%Y %H:%M:%S")
data$Datetime[is.na(data$Datetime)] = as_datetime(data$Timestamp[is.na(data$Datetime)], 
                                                  format = "%d/%m/%Y %H:%M")
data$Datetime[is.na(data$Datetime)] = as_datetime(data$Timestamp[is.na(data$Datetime)], 
                                                  format = "%m/%d/%Y %H:%M:%S")
data$Datetime[is.na(data$Datetime)] = as_datetime(data$Timestamp[is.na(data$Datetime)], 
                                                  format = "%m/%d/%Y %H:%M")
summary(data$Datetime)


# Extract date
data$Date <- date(data$Datetime)

# Extract time
data$Time <- hms::as_hms(data$Datetime)

# Set Location
data$Location <-'Long Pond'

# Inspect data
head(data)
summary(data)
plot(data$Date, hour(data$Time))


# Save the cleaned data
write.csv(data,
          paste(root, 'Data Cleaned/Manual/LPDEP_2016-2019_cleaned.csv', sep=""),
          row.names=FALSE)

# Drop data
rm(data)



###--------------------------------------------------------------------------###
###                            'Messalonskee'                                ###
###--------------------------------------------------------------------------###


# Set the working directory
setwd(paste(root, 'Other lakes/Messalonskee/', sep=""))
getwd()
list.files()


#--------------------#
#  Data preparation  #
#--------------------#


data1_16 <- read.csv("MESSDEP1_2016_InSitu.csv", skip=2, 
                     header=FALSE, stringsAsFactors = FALSE )[2]
data2_16 <- read.csv("MESSDEP2_2016_InSitu.csv", skip=2,
                     header=FALSE, stringsAsFactors = FALSE )[2]
data1_17 <- read.csv("MESSDEP1_2017_InSitu.csv", skip=2,
                     header=FALSE, stringsAsFactors = FALSE )[2]
data2_17 <- read.csv("MESSDEP2_2017_InSitu.csv", skip=2,
                     header=FALSE, stringsAsFactors = FALSE )[2]
data1_18 <- read.csv("MESSDEP1_2018_InSitu.csv", skip=2,
                     header=FALSE, stringsAsFactors = FALSE )[2]
data2_18 <- read.csv("MESSDEP2_2018_InSitu.csv", skip=2,
                     header=FALSE, stringsAsFactors = FALSE )[2]
data1_19 <- read.csv("MESSDEP1_2019_InSitu.csv", skip=2,
                     header=FALSE, stringsAsFactors = FALSE )[2]
data2_19 <- read.csv("MESSDEP2_2019_InSitu.csv", skip=2,
                     header=FALSE, stringsAsFactors = FALSE )[2]

data <- rbind(data1_16, data2_16, data1_17, data2_17, 
              data1_18, data2_18, data1_19, data2_19)


names(data) <- "Timestamp"

# Check result
head(data)

# Drop individual datasets
rm(data1_16, data2_16, data1_17, data2_17, 
   data1_18, data2_18, data1_19, data2_19)


# Extract date-time
data$Datetime <- as_datetime(data$Timestamp, format ="%Y-%m-%d %H:%M:%S")
data$Datetime[is.na(data$Datetime)] = as_datetime(data$Timestamp[is.na(data$Datetime)], 
                                                  format = "%d/%m/%Y %H:%M:%S")
data$Datetime[is.na(data$Datetime)] = as_datetime(data$Timestamp[is.na(data$Datetime)], 
                                                  format = "%d/%m/%Y %H:%M")
data$Datetime[is.na(data$Datetime)] = as_datetime(data$Timestamp[is.na(data$Datetime)], 
                                                  format = "%m/%d/%Y %H:%M:%S")
data$Datetime[is.na(data$Datetime)] = as_datetime(data$Timestamp[is.na(data$Datetime)], 
                                                  format = "%m/%d/%Y %H:%M")
summary(data$Datetime)


# Extract date
data$Date <- date(data$Datetime)

# Extract time
data$Time <- hms::as_hms(data$Datetime)

# Set Location
data$Location <-'Messalonskee'

# Inspect data
head(data)
summary(data)
plot(data$Date, hour(data$Time))


# Save the cleaned data
write.csv(data, 
          paste(root, 'Data Cleaned/Manual/MESSDEP_2016-2019_cleaned.csv', sep=""),
          row.names=FALSE)

# Drop data
rm(data)



###--------------------------------------------------------------------------###
###                              'East Pond'                                 ###
###--------------------------------------------------------------------------###

# Set the working directory

setwd(paste(root, 'Other lakes/East Pond/', sep=""))
getwd()
list.files()


#--------------------#
#  Data preparation  #
#--------------------#


data1_16 <- read.csv("EPDEP1_2016_InSitu.csv", skip=2, 
                     header=FALSE, stringsAsFactors = FALSE )[2]
data1_17 <- read.csv("EPDEP1_2017_InSitu.csv", skip=2,
                     header=FALSE, stringsAsFactors = FALSE )[2]
data1_18 <- read.csv("EPDEP1_2018_InSitu.csv", skip=2,
                     header=FALSE, stringsAsFactors = FALSE )[2]
data1_19 <- read.csv("EPDEP1_2019_InSitu.csv", skip=2,
                     header=FALSE, stringsAsFactors = FALSE )[2]


data <- rbind(data1_16, data1_17, data1_18, data1_19)


names(data) <- "Timestamp"

# Check result
head(data)

# Drop individual datasets
rm(data1_16, data1_17, data1_18, data1_19)


# Extract date-time
data$Datetime <- as_datetime(data$Timestamp, format ="%Y-%m-%d %H:%M:%S")
data$Datetime[is.na(data$Datetime)] = as_datetime(data$Timestamp[is.na(data$Datetime)], 
                                                  format = "%d/%m/%Y %H:%M:%S")
data$Datetime[is.na(data$Datetime)] = as_datetime(data$Timestamp[is.na(data$Datetime)], 
                                                  format = "%d/%m/%Y %H:%M")
data$Datetime[is.na(data$Datetime)] = as_datetime(data$Timestamp[is.na(data$Datetime)], 
                                                  format = "%m/%d/%Y %H:%M:%S")
data$Datetime[is.na(data$Datetime)] = as_datetime(data$Timestamp[is.na(data$Datetime)], 
                                                  format = "%m/%d/%Y %H:%M")
summary(data$Datetime)


# Extract date
data$Date <- date(data$Datetime)

# Extract time
data$Time <- hms::as_hms(data$Datetime)

# Set Location
data$Location <-'East Pond'

# Inspect data
head(data)
summary(data)
plot(data$Date, hour(data$Time))


# Save the cleaned data
write.csv(data,
          paste(root, 'Data Cleaned/Manual/EPDEP_2016-2019_cleaned.csv', sep=""),
          row.names=FALSE)

# Drop data
rm(data)



###--------------------------------------------------------------------------###
###                                'McGrath'                                 ###
###--------------------------------------------------------------------------###

# Set the working directory

setwd(paste(root, 'Other lakes/McGrath/', sep=""))
getwd()
list.files()


#--------------------#
#  Data preparation  #
#--------------------#


data1_16 <- read.csv("MPDEP1_2016_InSitu.csv", skip=2, 
                     header=FALSE, stringsAsFactors = FALSE )[2]
data1_17 <- read.csv("MPDEP1_2017_InSitu.csv", skip=2,
                     header=FALSE, stringsAsFactors = FALSE )[2]
data1_18 <- read.csv("MPDEP1_2018_InSitu.csv", skip=2,
                     header=FALSE, stringsAsFactors = FALSE )[2]
data1_19 <- read.csv("MPDEP1_2019_InSitu.csv", skip=2,
                     header=FALSE, stringsAsFactors = FALSE )[2]


data <- rbind(data1_16, data1_17, data1_18, data1_19)


names(data) <- "Timestamp"

# Check result
head(data)

# Drop individual datasets
rm(data1_16, data1_17, data1_18, data1_19)


# Extract date-time
data$Datetime <- as_datetime(data$Timestamp, format ="%Y-%m-%d %H:%M:%S")
data$Datetime[is.na(data$Datetime)] = as_datetime(data$Timestamp[is.na(data$Datetime)], 
                                                  format = "%d/%m/%Y %H:%M:%S")
data$Datetime[is.na(data$Datetime)] = as_datetime(data$Timestamp[is.na(data$Datetime)], 
                                                  format = "%d/%m/%Y %H:%M")
data$Datetime[is.na(data$Datetime)] = as_datetime(data$Timestamp[is.na(data$Datetime)], 
                                                  format = "%m/%d/%Y %H:%M:%S")
data$Datetime[is.na(data$Datetime)] = as_datetime(data$Timestamp[is.na(data$Datetime)], 
                                                  format = "%m/%d/%Y %H:%M")
summary(data$Datetime)


# Extract date
data$Date <- date(data$Datetime)

# Extract time
data$Time <- hms::as_hms(data$Datetime)

# Set Location
data$Location <-'McGrath'

# Inspect data
head(data)
summary(data)
plot(data$Date, hour(data$Time))


# Save the cleaned data
write.csv(data,
          paste(root, 'Data Cleaned/Manual/MPDEP_2016-2019_cleaned.csv', sep=""),
          row.names=FALSE)

# Drop data
rm(data)



###--------------------------------------------------------------------------###
###                             'North Pond'                                 ###
###--------------------------------------------------------------------------###

# Set the working directory

setwd(paste(root, 'Other lakes/North Pond/', sep=""))
getwd()
list.files()


#--------------------#
#  Data preparation  #
#--------------------#


data1_16 <- read.csv("NPDEP1_2016_InSitu.csv", skip=2, 
                     header=FALSE, stringsAsFactors = FALSE )[2]
data1_17 <- read.csv("NPDEP1_2017_InSitu.csv", skip=2,
                     header=FALSE, stringsAsFactors = FALSE )[2]
data1_18 <- read.csv("NPDEP1_2018_InSitu.csv", skip=2,
                     header=FALSE, stringsAsFactors = FALSE )[2]
data1_19 <- read.csv("NPDEP1_2019_InSitu.csv", skip=2,
                     header=FALSE, stringsAsFactors = FALSE )[2]


data <- rbind(data1_16, data1_17, data1_18, data1_19)


names(data) <- "Timestamp"

# Check result
head(data)

# Drop individual datasets
rm(data1_16, data1_17, data1_18, data1_19)


# Extract date-time
data$Datetime <- as_datetime(data$Timestamp, format ="%Y-%m-%d %H:%M:%S")
data$Datetime[is.na(data$Datetime)] = as_datetime(data$Timestamp[is.na(data$Datetime)], 
                                                  format = "%d/%m/%Y %H:%M:%S")
data$Datetime[is.na(data$Datetime)] = as_datetime(data$Timestamp[is.na(data$Datetime)], 
                                                  format = "%d/%m/%Y %H:%M")
data$Datetime[is.na(data$Datetime)] = as_datetime(data$Timestamp[is.na(data$Datetime)], 
                                                  format = "%m/%d/%Y %H:%M:%S")
data$Datetime[is.na(data$Datetime)] = as_datetime(data$Timestamp[is.na(data$Datetime)], 
                                                  format = "%m/%d/%Y %H:%M")
summary(data$Datetime)


# Extract date
data$Date <- date(data$Datetime)

# Extract time
data$Time <- hms::as_hms(data$Datetime)

# Set Location
data$Location <-'North Pond'

# Inspect data
head(data)
summary(data)
plot(data$Date, hour(data$Time))


# Save the cleaned data
write.csv(data,
          paste(root, 'Data Cleaned/Manual/NPDEP_2016-2019_cleaned.csv', sep=""),
          row.names=FALSE)

# Drop data
rm(data)



###--------------------------------------------------------------------------###
###                             'Salmon Lake'                                ###
###--------------------------------------------------------------------------###

# Set the working directory

setwd(paste(root, 'Other lakes/Salmon/', sep=""))
getwd()
list.files()


#--------------------#
#  Data preparation  #
#--------------------#


data1_16 <- read.csv("SPDEP1_2016_InSitu.csv", skip=2, 
                     header=FALSE, stringsAsFactors = FALSE )[2]
data1_17 <- read.csv("SPDEP1_2017_InSitu.csv", skip=2,
                     header=FALSE, stringsAsFactors = FALSE )[2]
data1_18 <- read.csv("SPDEP1_2018_InSitu.csv", skip=2,
                     header=FALSE, stringsAsFactors = FALSE )[2]
data1_19 <- read.csv("SPDEP1_2019_InSitu.csv", skip=2,
                     header=FALSE, stringsAsFactors = FALSE )[2]


data <- rbind(data1_16, data1_17, data1_18, data1_19)


names(data) <- "Timestamp"

# Check result
head(data)

# Drop individual datasets
rm(data1_16, data1_17, data1_18, data1_19)


# Extract date-time
data$Datetime <- as_datetime(data$Timestamp, format ="%Y-%m-%d %H:%M:%S")
data$Datetime[is.na(data$Datetime)] = as_datetime(data$Timestamp[is.na(data$Datetime)], 
                                                  format = "%d/%m/%Y %H:%M:%S")
data$Datetime[is.na(data$Datetime)] = as_datetime(data$Timestamp[is.na(data$Datetime)], 
                                                  format = "%d/%m/%Y %H:%M")
data$Datetime[is.na(data$Datetime)] = as_datetime(data$Timestamp[is.na(data$Datetime)], 
                                                  format = "%m/%d/%Y %H:%M:%S")
data$Datetime[is.na(data$Datetime)] = as_datetime(data$Timestamp[is.na(data$Datetime)], 
                                                  format = "%m/%d/%Y %H:%M")
summary(data$Datetime)


# Extract date
data$Date <- date(data$Datetime)

# Extract time
data$Time <- hms::as_hms(data$Datetime)

# Set Location
data$Location <-'Salmon Lake'

# Inspect data
head(data)
summary(data)
plot(data$Date, hour(data$Time))


# Save the cleaned data
write.csv(data,
          paste(root, 'Data Cleaned/Manual/SPDEP_2016-2019_cleaned.csv', sep=""),
          row.names=FALSE)

# Drop data
rm(data)



###--------------------------------------------------------------------------###
###                      Merge Manual Sampling Data                          ###
###--------------------------------------------------------------------------###


# Set the working directory

setwd(paste(root, 'Data Cleaned/Manual/', sep=""))
getwd()
list.files()


#--------------------#
#  Data preparation  #
#--------------------#


data1 <- read.csv("EPDEP_2016-2019_cleaned.csv", stringsAsFactors = FALSE )
data2 <- read.csv("GPDEP_2016-2019_cleaned.csv", stringsAsFactors = FALSE )
data3 <- read.csv("LPDEP_2016-2019_cleaned.csv", stringsAsFactors = FALSE )
data4 <- read.csv("MESSDEP_2016-2019_cleaned.csv", stringsAsFactors = FALSE )
data5 <- read.csv("MPDEP_2016-2019_cleaned.csv", stringsAsFactors = FALSE )
data6 <- read.csv("NPDEP_2016-2019_cleaned.csv", stringsAsFactors = FALSE )
data7 <- read.csv("SPDEP_2016-2019_cleaned.csv", stringsAsFactors = FALSE )

data <- rbind(data1, data2, data3, data4, data5, data6, data7)
rm(data1, data2, data3, data4, data5, data6, data7)

# Save the cleaned data
write.csv(data,
          paste(root, 'Data Cleaned/Manual/SAMPLES_2016-2019_cleaned.csv', sep=""),
          row.names=FALSE)



################################################################################
###                          WEATHER STATION DATA                            ###
################################################################################

# Set the working directory

setwd(paste(root, 'Great Pond Data/MLRCWeather/', sep=""))
getwd()
list.files()


#--------------------#
#  Data preparation  #
#--------------------#


# Load column header and units
header <- colnames(read.csv('MLRCWeather2016.csv', 
                            header=TRUE)[1:16])
units <- read.csv('MLRCWeather2016.csv', 
                            header=TRUE)[1:16][1,]

units[14] <- 'W/m2'

# Load weather data
weather.2016 <- read.csv('MLRCWeather2016.csv', header=FALSE, 
                         skip=2, stringsAsFactors = FALSE, 
                         na.strings = c("999.9", "-99999.99", "-100000"))[1:16]
names(weather.2016) <- header

weather.2017 <- read.csv('MLRCWeather2017.csv', header=FALSE, 
                         skip=2, stringsAsFactors = FALSE, 
                         na.strings = c("999.9", "-99999.99", "-100000"))
names(weather.2017) <- header

weather.2018 <- read.csv('MLRCWeather2018.csv', header=FALSE, 
                         skip=2, stringsAsFactors = FALSE,
                         na.strings = c("999.9", "-99999.99", "-100000"))
names(weather.2018) <- header

weather.2019 <- read.csv('MLRCWeather2019.csv', header=FALSE, 
                         skip=2, stringsAsFactors = FALSE,
                         na.strings = c("999.9", "-99999.99", "-100000"))
names(weather.2019) <- header


# Merge weather data
weather <- rbind(weather.2016, weather.2017, weather.2018, weather.2019)

# Drop individual datasets
rm(header, weather.2016, weather.2017, weather.2018, weather.2019)

# Check result
str(weather)
summary(weather)

# Recode date
weather$Date = as_date(weather$Time, format = "%m/%d/%Y %I:%M")
weather$Date[is.na(weather$Date)] = as_date(weather$Time[is.na(weather$Date)], 
                                            format = "%m/%d/%Y")
summary(weather$Date)


#---------------------------#
#    Remove outlier data    #
#---------------------------#


# Mark problematic dates
weather$outlier.dates <- as.factor(ifelse(weather$Date >= '2016-07-01' & 
                                          weather$Date < '2016-07-26', 
                                          1, 0))
# Set plotting to 2 by 2
par(mfrow=c(2,4))
# Air temperature
plot(weather$Date, weather$Air.Temperature, col = weather$outlier.dates)
# Bulb temperature
plot(weather$Date, weather$Wet.Bulb.Temperature, col = weather$outlier.dates)
# Humidity
plot(weather$Date, weather$Relative.Humidity, col = weather$outlier.dates)
# Wind speed
plot(weather$Date, weather$Wind.Speed, col = weather$outlier.dates)
# Max wind speed
plot(weather$Date, weather$Max.Wind.Sp, col = weather$outlier.dates)
# Rain
plot(weather$Date, weather$Interval.Rain, col = weather$outlier.dates)
# Rain intensity
plot(weather$Date, weather$Rain.Intensity, col = weather$outlier.dates)
# Solar radiation
plot(weather$Date, weather$Solar.Rad, col = weather$outlier.dates)


# Drop problematic dates
weather <- subset(weather, weather$outlier.dates == 0, select = -c(outlier.dates))


# Check for outliers in the remaining data

# Air temperature
plot(weather$Date, weather$Air.Temperature)
# Bulb temperature
plot(weather$Date, weather$Wet.Bulb.Temperature)
# Humidity
plot(weather$Date, weather$Relative.Humidity)
# Wind speed
plot(weather$Date, weather$Wind.Speed)
# Max wind speed
plot(weather$Date, weather$Max.Wind.Sp)
# Rain
plot(weather$Date, weather$Interval.Rain)
# Rain intensity
plot(weather$Date, weather$Rain.Intensity)
# Solar radiation
plot(weather$Date, weather$Solar.Rad)


# Check for outliers by season

weather$Quarter <- quarter(weather$Date, with_year = FALSE)
weather$Year <- year(weather$Date)

# Air temperature
boxplot(weather$Air.Temperature ~ weather$Quarter)
# Bulb temperature
boxplot(weather$Wet.Bulb.Temperature ~ weather$Quarter)
# Humidity
boxplot(weather$Relative.Humidity ~ weather$Quarter)
# Wind speed
boxplot(weather$Wind.Speed ~ weather$Quarter)
# Max wind speed
boxplot(weather$Max.Wind.Sp ~ weather$Quarter)
# Rain
boxplot(weather$Interval.Rain ~ weather$Quarter)
# Rain intensity
boxplot(weather$Rain.Intensity ~ weather$Quarter)
# Solar radiation
boxplot(weather$Solar.Rad ~ weather$Quarter)


# Air temperature
boxplot(weather$Air.Temperature ~ weather$Year)
# Bulb temperature
boxplot(weather$Wet.Bulb.Temperature ~ weather$Year)
# Humidity
boxplot(weather$Relative.Humidity ~ weather$Year)
# Wind speed
boxplot(weather$Wind.Speed ~ weather$Year)
# Max wind speed
boxplot(weather$Max.Wind.Sp ~ weather$Year)
# Rain
boxplot(weather$Interval.Rain ~ weather$Year)
# Rain intensity
boxplot(weather$Rain.Intensity ~ weather$Year)
# Solar radiation
boxplot(weather$Solar.Rad ~ weather$Year)


# Descriptives
summary(weather[, c("Air.Temperature", "Wet.Bulb.Temperature", 
                    "Relative.Humidity", "Wind.Speed", "Max.Wind.Sp",
                    "Relative.Humidity", "Interval.Rain", "Rain.Intensity")])


# Save the cleaned data
write.csv(weather,
          paste(root, 'Data Cleaned/WeatherStation/MLRCWeather_2016-2019_cleaned.csv', sep=""),
          row.names=FALSE)
