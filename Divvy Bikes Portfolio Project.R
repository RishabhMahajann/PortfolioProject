### Divvy_Exercise_Full_Year_Analysis ###
## Key question: "In what ways do members and casual riders use Divvy bikes differently?"##

#Installing required packages and setting up libraries
library(tidyverse)
library(dplyr)
library(skimr)
library(ggplot2)
library(lubridate)
library(janitor)
library(readr)

#=====================
# STEP 1: Importing Data
#=====================

q2_2019 <- read_csv("portfolio and capstone/Capstone/Capstone 1/Divvy_Trips_2019_Q2.csv")
View(q2_2019)
q3_2019 <- read_csv("portfolio and capstone/Capstone/Capstone 1/Divvy_Trips_2019_Q3.csv")
View(q3_2019)
q4_2019 <- read_csv("portfolio and capstone/Capstone/Capstone 1/Divvy_Trips_2019_Q4.csv")
View(q4_2019)
q1_2020 <- read_csv("portfolio and capstone/Capstone/Capstone 1/Divvy_Trips_2020_Q1.csv")
View(q1_2020)
colnames(q1_2020)


#====================================================
# STEP 2: WRANGLE DATA AND COMBINE INTO A SINGLE FILE
#====================================================

colnames(q2_2019)
# Rename columns  to make them consistent with q1_2020 (as this will be the supposed going-forward table design for Divvy)
q2_2019 <- rename(q2_2019, 
          ride_id = "01 - Rental Details Rental ID",
          rideable_type = "01 - Rental Details Bike ID",
          started_at = "01 - Rental Details Local Start Time",
          ended_at = "01 - Rental Details Local End Time",
          start_station_name = "03 - Rental Start Station Name",
          start_station_id = "03 - Rental Start Station ID",
          end_station_name = "02 - Rental End Station Name",
          end_station_id = "02 - Rental End Station ID",
          member_casual = "User Type")

colnames(q3_2019)
q3_2019 <- rename(q3_2019,
                  ride_id = "trip_id",
                  rideable_type = "bikeid",
                  started_at = "start_time",
                  ended_at = "end_time",
                  start_station_name ="from_station_name",
                  start_station_id = "from_station_id",
                  end_station_name ="to_station_name",
                  end_station_id = "to_station_id",
                  member_casual= "usertype")

colnames(q4_2019)
q4_2019<- rename(q4_2019,
                 ride_id ="trip_id",
                 rideable_type = "bikeid",
                 started_at = "start_time",
                 ended_at = "end_time",
                 start_station_name = "from_station_name",
                 start_station_id = "from_station_id",
                 end_station_name = "to_station_name",
                 member_casual = "usertype")

#Inspecting the dataframes
str(q1_2020)
str(q4_2019)
str(q3_2019)
str(q2_2019)

q4_2019 <- mutate(q4_2019, ride_id = as.character(ride_id), rideable_type = as.character(rideable_type) )
q3_2019 <- mutate(q3_2019, ride_id = as.character(ride_id), rideable_type = as.character(rideable_type))
q2_2019 <- mutate(q2_2019, ride_id = as.character(ride_id), rideable_type = as.character(rideable_type))

#Combining data
all_trips <- bind_rows(q2_2019,q3_2019,q4_2019,q1_2020)
#Verifying the data combined
all_trips
colnames(all_trips)

# Remove lat, long, birthyear, and gender fields as this data was dropped beginning in 2020
all_trips <- all_trips %>%
select(-c(start_lat, start_lng, end_lat, end_lng, birthyear, gender, 
          "01 - Rental Details Duration In Seconds Uncapped", 
          "05 - Member Details Member Birthday Year", "Member Gender", "tripduration"))

#======================================================
# STEP 3: CLEAN UP AND ADD DATA TO PREPARE FOR ANALYSIS
#======================================================
# Inspect the new table that has been created

str(all_trips)
dim(all_trips)
nrow(all_trips)
head(all_trips)
tail(all_trips)
summary(all_trips)

# There are a few problems we will need to fix:
# (1) In the "member_casual" column, there are two names for members ("member" and "Subscriber") and two names for casual riders ("Customer" and "casual"). We will need to consolidate that from four to two labels.
# (2) The data can only be aggregated at the ride-level, which is too granular. We will want to add some additional columns of data -- such as day, month, year -- that provide additional opportunities to aggregate the data.
# (3) We will want to add a calculated field for length of ride since the 2020Q1 data did not have the "tripduration" column. We will add "ride_length" to the entire dataframe for consistency.
# (4) There are some rides where tripduration shows up as negative, including several hundred rides where Divvy took bikes out of circulation for Quality Control reasons. We will want to delete these rides.


# In the "member_casual" column, replace "Subscriber" with "member" and "Customer" with "casual"
# Before 2020, Divvy used different labels for these two types of riders ... we will want to make our dataframe consistent with their current nomenclature
# N.B.: "Level" is a special property of a column that is retained even if a subset does not contain any values from a specific level
# Begin by seeing how many observations fall under each usertype
table(all_trips$member_casual)

unique(all_trips$member_casual)
table(all_trips$member_casual)
all_trips <- all_trips %>% mutate(member_casual = recode(member_casual, 
                                            "Subscriber" = "member",
                                            "Customer" = "casual"))

unique(all_trips$member_casual)

# Check to make sure the proper number of observations were reassigned
table(all_trips$member_casual)

# Add columns that list the date, month, day, and year of each ride
# This will allow us to aggregate ride data for each month, day, or year before completing these operations we could only aggregate at the ride level
all_trips$date <- as.Date(all_trips$started_at)
all_trips$date
all_trips$month <- format(as.Date(all_trips$date),"%m")
all_trips$month
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$day
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$year
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")
all_trips$day_of_week
all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at)

all_trips$ride_length

# Inspect the structure of the columns
str(all_trips)
is.factor(all_trips$ride_length)
all_trips$ride_length <- as.numeric(all_trips$ride_length)
is.numeric(all_trips$ride_length)

# The dataframe includes a few hundred entries when bikes were taken out of docks and checked for quality by Divvy or ride_length was negative
# We will create a new version of the dataframe since data is being removed

all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length<0),]
all_trips_v2

#=====================================
# STEP 4: CONDUCT ANALYSIS
#=====================================
##Descriptive Analysis on Ride Length
mean(all_trips_v2$ride_length)
max(all_trips_v2$ride_length)
min(all_trips_v2$ride_length)

summary(all_trips_v2$ride_length)

# Compare members and casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)

# See the average ride time by each day for members vs casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

# Notice that the days of the week are out of order. Let's fix that.
all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

# Now, let's run the average ride time by each day for members vs casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

#analyze ridership data by type and weekday
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n(),
            average_duration = mean(ride_length)) %>% 
  arrange(member_casual,weekday)


#Lets visualize now

all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual,weekday) %>% 
  summarise(number_of_rides = n(),
            average_length = mean(ride_length)) %>% 
  arrange(member_casual, weekday) %>% 
  ggplot(mapping = aes(x= weekday, y= number_of_rides, fill= member_casual))+
  geom_col(position = "dodge")

all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual,weekday) %>% 
  summarise(number_of_rides = n(),
            average_length = mean(ride_length)) %>% 
  arrange(member_casual, weekday) %>% 
  ggplot(mapping = aes(x= weekday, y= number_of_rides, fill= member_casual))+
  geom_col(position = "dodge")

all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual,weekday) %>% 
  summarise(number_of_rides = n(),
            average_length = mean(ride_length)) %>% 
  arrange(member_casual, weekday) %>% 
  ggplot(mapping = aes(x= weekday, y= average_length, fill= member_casual))+
  geom_col(position = "dodge") +
  labs(title = "Average Length Covered vs Weekday", subtitle = "Casual/Members Variation")

#=================================================
# STEP 5: EXPORT SUMMARY FILE FOR FURTHER ANALYSIS
#=================================================
write.csv(count, "done_divvy_rides.csv")
