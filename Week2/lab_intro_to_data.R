# R Script with the solution to Week2 Lab

# Loading the packages
library(ggplot2)
library(dplyr)
library(statsr)

# The Bureau of Transportation Statistics (http://www.rita.dot.gov/bts/about/) 
# (BTS) is a statistical agency that is a part of the Research and Innovative 
# Technology Administration (RITA). As its name implies, BTS collects and makes 
# available transportation data, such as the flights data we will be working with 
# in this lab.

# Loading the dataset nycflights, which contains informations about the flights
# in New York City
data("nycflights")

# View the names of columns (variables)
names(nycflights)

# Accordingly to the metadata of this dataset, it's presented below the characteristics
# of each variable

# `year`, `month`, `day`: Date of departure
# `dep_time`, `arr_time`: Departure and arrival times, local timezone.
# `dep_delay`, `arr_delay`: Departure and arrival delays, in minutes. 
# Negative times represent early departures/arrivals.
# `carrier`: Two letter carrier abbreviation.
# `9E`:           Endeavor Air Inc.
# `AA`:      American Airlines Inc.
# `AS`:        Alaska Airlines Inc.
# `B6`:             JetBlue Airways
# `DL`:        Delta Air Lines Inc.
# `EV`:    ExpressJet Airlines Inc.
# `F9`:      Frontier Airlines Inc.
# `FL`: AirTran Airways Corporation
# `HA`:      Hawaiian Airlines Inc.
# `MQ`:                   Envoy Air
# `OO`:       SkyWest Airlines Inc.
# `UA`:       United Air Lines Inc.
# `US`:             US Airways Inc.
# `VX`:              Virgin America
# `WN`:      Southwest Airlines Co.
# `YV`:          Mesa Airlines Inc.
# `tailnum`: Plane tail number
# `flight`: Flight number
# `origin`, `dest`: Airport codes for origin and destination. 
# (Google can help you with what code stands for which airport.)
# `air_time`: Amount of time spent in the air, in minutes.
# `distance`: Distance flown, in miles.
# `hour`, `minute`: Time of departure broken in to hour and minutes.

# View some more information about the data
str(nycflights)

# View the distribution of departure delays of all flights
pdf("hist_dep_delay.pdf")
ggplot(data = nycflights, aes(x = dep_delay)) +
  geom_histogram()
dev.off()

# As we see, the histogram shows that it has a lot of data near zero.
# To help us see it better, we can define the binwidth our histogram
# should have.
pdf("hist_dep_delay2.pdf")
ggplot(data = nycflights, aes(x = dep_delay)) +
  geom_histogram(binwidth = 15)
dev.off()
# Looking at the new histogram, we can have a clear insight, i.e., most of the
# departures are on time (dep_delay = 0), some of them are early (dep_delay < 0)
# and some of them are late (dep_delay > 0).

# Question 1: Create a new data frame that includes flights headed to SFO in 
# February, and save  this data frame as `sfo_feb_flights`. How many flights 
# meet these criteria? 
sfo_feb_flights <- nycflights %>%
  filter(dest == "SFO" & month == 2)
length(sfo_feb_flights$dep_delay)
# Answer: 68 flights headed to SFO in February.

# Question 2: Make a histogram and calculate appropriate summary statistics 
# for arrival delays of `sfo_feb_flights`. Which of the following is false? 
pdf("hist_sfo_feb.pdf")
ggplot(sfo_feb_flights, aes(x = dep_delay)) +
  geom_histogram()
dev.off()
# Answer: No flight is delayed for more than 2 hours.
# We see through the histogram that there is a flight with 200 minutes of delay,
# 200 minutes = 2 hours and 40 minutes.

# Question 3: Calculate the median and interquartile range for `arr_delay`s of 
# flights in the  `sfo_feb_flights` data frame, grouped by carrier. Which carrier
# has the hights IQR of arrival delays?
sfo_feb_flights %>%
  group_by(carrier) %>%
  summarise(median_sfo = median(arr_delay), iqr_sfo = IQR(arr_delay), n = n())
# Answer: The carrier with the highest IQR is DL and UA.

# Question 4: Which month has the highest average departure delay from an NYC airport?
nycflights %>%
  group_by(month) %>%
  summarise(avg_dep_delay = mean(dep_delay)) %>%
  arrange(desc(avg_dep_delay))
# Answer: Month 7 (July). avg_dep_delay = 20.8

# Question 5: Which month has the highest median departure delay from an NYC airport?
nycflights %>%
  group_by(month) %>%
  summarise(median_dep_delay = median(dep_delay)) %>%
  arrange(desc(median_dep_delay))
# Answer: Month 12 (December). median_dep_delay = 1

# Question 6: Is the mean or the median a more reliable measure for deciding 
# which month(s) to avoid flying if you really dislike delayed flights, and why?
# Answer: Median would be more reliable because the distribution of dep_delay
# is right skewed, as shown in the previous histograms. 

# Question 7: If you were selecting an airport simply based on on time departure
# percentage, which NYC airport would you choose to fly out of? 

# Defining the flights as "on time" or "delayed" accordingly to what was proposed,
# such as "on time" we call the flights that are less than 5 minutes late, and
# "delayed" we call the flights that delayed for 5 minutes or more.
nycflights <- nycflights %>%
  mutate(dep_type = ifelse(dep_delay < 5, "on time", "delayed"))

# Summarizing the data with the percentage of departure time
nycflights %>%
  group_by(origin) %>%
  summarise(ot_dep_rate = sum(dep_type == "on time") / n()) %>%
  arrange(desc(ot_dep_rate))
# Answer: I would chose the LGA airport based on on time departure percentage.

# Question 8: Mutate the data frame so that it includes a new variable that contains
# the average speed, `avg_speed` traveled by the plane for each flight (in mph). 
# What is the tail number of the plane with the fastest `avg_speed`? Hint: Average
# speed can be calculated as distance divided by number of hours of travel, and 
# note that `air_time` is given in minutes. If you just want to show the `avg_speed` 
# and `tailnum` and none of the other variables, use the select function at the end 
# of your pipe to select just these two variables with `select(avg_speed, tailnum)`. 
# You can Google this tail number to find out more about the aircraft.

# Adding a new column named avg_speed
nycflights <- nycflights %>%
  mutate(avg_speed = (distance / (air_time/60))) 

# searching for the tailnum with the avg_speed ordered
nycflights %>%
  arrange(desc(avg_speed)) %>%
  select(avg_speed, tailnum)
# Answer: N666DN is the tailnumber of the plane that has the fatest avg_speed.

# Question 9: Make a scatterplot of `avg_speed` vs. `distance`. Which of the 
# following is true  about the relationship between average speed and distance.
pdf("scatterplot_avgspeed.pdf")
ggplot(nycflights, aes(x = avg_speed, y = distance)) +
  geom_point()
dev.off()
# Answer: There is an overall postive association between distance and average speed.

# Question 10: Suppose you define a flight to be "on time" if it gets to the 
# destination ontime or earlier than expected, regardless of any departure delays. 
# Mutate the data frame to create a new variable called `arr_type` with levels 
# "on time" and "delayed" based on this definition. Then, determine the on time 
# arrival percentage  based on whether the flight departed on time or not. 
# What proportion of flights  that were "delayed" departing arrive "on time"? 

# Adding a new column with the arrival type
nycflights <- nycflights %>%
  mutate(arr_type = ifelse(arr_delay <= 0, "on_time", "delayed"))

# Grouping and summarizing the data based on arrival and departure time
nycflights %>%
  group_by(dep_type) %>%
  summarise(percent_ontime = sum(arr_type == "on_time") / n())
# Answer: The proportion of flights that were delayed departing and arrived 
# on time was 0.183.