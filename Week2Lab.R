# Week 2 Lab

# load packages

library(statsr)
library(dplyr)
library(ggplot2)

# load data

data(nycflights)

dim(nycflights)

names(nycflights)

str(nycflights)

# Question 1

# Create a new data frame that includes flights headed to SFO in February,
# and save this data frame as sfo_feb_flights. 
# How many flights meet these criteria?

sfo_feb_flights<-nycflights %>%
  filter(dest=="SFO", month==2)

nrow(sfo_feb_flights)

# Question 2

# Make a histogram and calculate appropriate summary statistics for 
# arrival delays of sfo_feb_flights. Which of the following is false?

# The distribution is right skewed.

# The distribution is unimodal.

# No flight is delayed more than 2 hours.

# More than 50% of flights arrive on time or earlier than scheduled.

# The distribution has several extreme values on the right side.

hist(sfo_feb_flights$arr_delay)

summary(sfo_feb_flights$arr_delay)

# answer - no flight is delayed more than 2 hrs


# Question 3

# Calculate the median and interquartile range for arr_delays of flights
# in the sfo_feb_flights data frame, grouped by carrier. Which carrier has
# the highest IQR of arrival delays?

sfo_feb_flights %>%
  group_by(carrier) %>%
  summarise(arr_IQR=IQR(arr_delay))


# Question 4

# Considering the data from all the NYC airports, which month has the highest
# average departure delay?

nycflights %>%
  group_by(month) %>%
  summarise(avg_delay=mean(dep_delay))

# Question 5

# Which month has the highest median departure delay from an NYC airport?

nycflights %>%
  group_by(month) %>%
  summarise(med_delay=median(dep_delay))
  

# Question 6

# Is the mean or the median a more reliable measure for deciding which month(s)
# to avoid flying if you really dislike delayed flights, and why?


# Mean would be more reliable as the distribution of delays is symmetric.

# Median would be more reliable as the distribution of delays is skewed.

# Median would be more reliable as the distribution of delays is symmetric.

# Mean would be more reliable as it gives us the true average.

# Both give us useful information.

hist(nycflights$dep_delay)

# Answer: median would be more reliable as the distribution of delays is skewed

# Question 7

# If you were selecting an airport simply based on on time departure percentage,
# which NYC airport would you choose to fly out of?

nycflights <- nycflights %>%
  mutate(dep_type = ifelse(dep_delay < 5, "on time", "delayed"))

nycflights %>%
  group_by(origin) %>%
  summarise(otdeppct=sum(dep_type=="on time")/n()) %>%
  arrange(desc(otdeppct))

# Question 8

# Mutate the data frame so that it includes a new variable that contains the 
# average speed, avg_speed traveled by the plane for each journey (in mph). 
# What is the tail number of the plane with the fastest avg_speed? 
# Hint: Average speed can be calculated as distance divided by number of hours
# of travel, and note that air_time is given in minutes. If you just want to show
# the avg_speed and tailnum and none of the other variables, use the select 
# function at the end of your pipe to select just these two variables with 
# select(avg_speed, tailnum). You can google this tail number to find out 
# more about the aircraft.

nycflights<- nycflights %>%
  mutate(avg_speed=(distance/(air_time/60))) %>%
  group_by(tailnum)

nycflightsspeed<-nycflights %>%
  select(tailnum, avg_speed) %>%
  arrange(desc(avg_speed))

head(nycflightsspeed)
 

# Question 9

# Make a scatterplot of avg_speed vs. distance. Which of the following is true
# about the relationship between average speed and distance.

# The distribution of distances are uniform over 0 to 5000 miles.

# As distance increases the average speed of flights decreases.

# There are no outliers.

# The relationship is linear.

# There is an overall positive association between distance and average speed.

plot(nycflights$distance, nycflights$avg_speed)

# Answer: there is an overall positive association between distance and average speed


# Question 10

# Suppose you define a flight to be "on time" if it gets to the destination
# on time or earlier than expected, regardless of any departure delays. 
# Mutate the data frame to create a new variable called arr_type with levels 
# "on time" and "delayed" based on this definition. Also mutate to create a 
# new variable called dep_type with levels "on time" and "delayed" depending 
# on the flight was delayed for fewer than 5 minutes or 5 minutes or more, 
# respectively. In other words, if arr_delay is 0 minutes or fewer, arr_type 
# is "on time". If dep_delay is less than 5 minutes, dep_type is "on time". 
# Then, determine the on time arrival percentage based on whether the flight 
# departed on time or not. What fraction of flights that were "delayed" 
# departing arrive "on time"? (Enter the answer in decimal point, like 0.xx)

nycflnew<-nycflights %>%
  mutate(arr_type = ifelse(arr_delay <= 0, "on time", "delayed"))

nycflnew %>%
  group_by(dep_type) %>%
  summarise(rate = sum(arr_type == "on time") / n())

j<- nycflnew %>%
  filter(dep_type=="delayed")

z<- j %>%
  filter(arr_type=="on time")

nrow(z)/nrow(j)
