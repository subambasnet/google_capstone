# Cyclistic Bike Analysis

# Load packages
library(tidyverse)
library(lubridate)
library(ggplot2)

# Set to working directory
setwd("/Users/hoangnguyen/Documents/Projects/Cyclistic_Project")

# Import bike trip data (April 2022 to March 2023)
apr2022_trip <- read_csv('202204-divvy-tripdata.csv')
may2022_trip <- read_csv('202205-divvy-tripdata.csv')
jun2022_trip <- read_csv('202206-divvy-tripdata.csv')
jul2022_trip <- read_csv('202207-divvy-tripdata.csv')
aug2022_trip <- read_csv('202208-divvy-tripdata.csv')
sep2022_trip <- read_csv('202209-divvy-publictripdata.csv')
oct2022_trip <- read_csv('202210-divvy-tripdata.csv')
nov2022_trip <- read_csv('202211-divvy-tripdata.csv')
dec2022_trip <- read_csv('202212-divvy-tripdata.csv')
jan2023_trip <- read_csv('202301-divvy-tripdata.csv')
feb2023_trip <- read_csv('202302-divvy-tripdata.csv')
mar2023_trip <- read_csv('202303-divvy-tripdata.csv')

# Checking column names before merging data
colnames(apr2022_trip)
colnames(may2022_trip)
colnames(jun2022_trip)
colnames(jul2022_trip)
colnames(aug2022_trip)
colnames(sep2022_trip)
colnames(oct2022_trip)
colnames(nov2022_trip)
colnames(dec2022_trip)
colnames(jan2023_trip)
colnames(feb2023_trip)
colnames(mar2023_trip)

## PROCESS
# Merge the data
trip_data = rbind(apr2022_trip, may2022_trip, jun2022_trip, jul2022_trip, aug2022_trip, sep2022_trip, oct2022_trip, nov2022_trip, dec2022_trip, jan2023_trip, feb2023_trip, mar2023_trip)

head(trip_data)
str(trip_data)
View(trip_data)
nrow(trip_data)


# Create date, month, day, year, and day_of_week 
trip_data$date <- as.Date(trip_data$started_at) 
trip_data$month <- format(as.Date(trip_data$date), "%m")
trip_data$day <- format(as.Date(trip_data$date), "%d")
trip_data$year <- format(as.Date(trip_data$date), "%Y")
trip_data$day_of_week <- format(as.Date(trip_data$date), "%A")

# Remove unnecessary columns
trip_data <- trip_data %>%
  select(-c(start_lat,start_lng,end_lat,end_lng))


# Count the total of missing values in the dataset
sum(is.na(trip_data))

# Remove all the missing values
trip_data_clean <- na.omit(trip_data)

# Count number of duplicate rows
nrow(trip_data_clean[duplicated(trip_data_clean), ])

# Create a ride_length column in minutes
trip_data_clean$ride_length <- round(difftime(trip_data_clean$ended_at,trip_data_clean$started_at,units='mins'),2)

# Change the the data type of ride_length to numeric for calculations
trip_data_clean$ride_length <- as.numeric(as.character(trip_data_clean$ride_length))

# Count number of negative values in ride_length
nrow(trip_data_clean[trip_data_clean$ride_length<0,])

# Order the day of week column
trip_data_clean$day_of_week <- ordered(trip_data_clean$day_of_week, 
                                    levels=c("Sunday", "Monday", "Tuesday", "Wednesday", 
                                             "Thursday", "Friday", "Saturday"))

# Remove rows with negative and zero values from ride_length
trip_data_clean <- trip_data_clean[!(trip_data_clean$ride_length<0 | trip_data_clean$ride_length==0),]

## ANALYZE

# Perform descriptive analysis
summary(trip_data_clean$ride_length)


# Compute the minimum, mean, maximum and difference (max - min) of ride length for member and causal riders
trip_data_clean %>% 
  group_by(member_casual) %>% 
  summarise(lower_ride_length = min(ride_length), average_ride_length = mean(ride_length),
            upper_ride_length = max(ride_length), difference_ride_length = max(ride_length) - min(ride_length))

View(trip_data_clean)



## TOTAL NUMBER OF RIDES

# Count each member type (member vs. casual)
trip_data_clean %>%
  group_by(member_casual) %>% 
  summarise(total_count = n())

# Count number of each bike type
trip_data_clean %>% 
  group_by(rideable_type) %>% 
  summarise(total_count = n())

# Count the number for each type of bike for each user type
trip_data_clean %>% 
  group_by(member_casual, rideable_type) %>% 
  summarise(total_count = n())

# Count number of rides by month and year
trip_data_clean %>% 
  group_by(month,year) %>% 
  summarise(total_count = n()) %>% 
  arrange(year)

# Count number of rides by weekday
trip_data_clean %>% 
  group_by(member_casual, day_of_week) %>% 
  summarise(total_count = n()) 

## AVERAGE RIDE LENGTH

# Avg ride length by bike type
trip_data_clean %>% 
  group_by(rideable_type) %>% 
  summarise(avg_ride_length = mean(ride_length))

# Avg ride length by member type
trip_data_clean %>% 
  group_by(member_casual) %>% 
  summarise(avg_ride_length = mean(ride_length))

# Avg ride length by member type and bike type
trip_data_clean %>% 
  group_by(member_casual, rideable_type) %>% 
  summarise(avg_ride_length = mean(ride_length))

# Avg ride length by member type and day of week
trip_data_clean %>% 
  group_by(member_casual, day_of_week) %>% 
  summarise(avg_ride_length = mean(ride_length)) 

# Avg ride length by member type and year
trip_data_clean %>% 
  group_by(member_casual,year) %>% 
  summarise(avg_ride_length = mean(ride_length))


## VISUALIZE 
options(scipen=999)

member_perc <- trip_data_clean %>%
  group_by(member_casual) %>%
  summarise(count = n()) %>%
  mutate(perc=count/sum(count)) %>% 
  ungroup() 

# Create a pie chart that shows the distribution of member types  
ggplot(member_perc, aes(x="", y=perc, fill=member_casual)) +
  geom_bar(stat="identity", width=1,color="white") +
  coord_polar("y", start=0) +
  geom_text(aes(label = scales::percent(perc)),
  position = position_stack(vjust = 0.5)) +
  theme_void()
  
  
# Total rides based on member types and bike types
trip_data_clean %>%
  group_by(member_casual, rideable_type) %>% 
  summarise(total_count = n()) %>% 
  ggplot(aes(x=rideable_type, y = total_count, fill=member_casual)) +
  geom_bar(position="dodge", stat="identity",color="black",width=0.7) +
  labs(title = "Total Number of Rides by Bike and Member Types", x='Type of Bikes', y='Number of Rides')+
  theme_classic()

# Total rides based on member types and day of week
trip_data_clean %>% 
  group_by(member_casual, day_of_week) %>% 
  summarise(total_count = n()) %>% 
  ggplot(aes(x=day_of_week, y=total_count, fill=member_casual)) +
  geom_bar(position="dodge", stat='identity',color="black",width=0.7) +
  labs(title = "Total Number of Rides by Member Types and Day of Week",x='Day of Week',y='Number of Rides')+
  theme_classic()


# Total rides based on member types and months
trip_data_clean %>%
  group_by(member_casual, month) %>%
  summarise(total_count = n()) %>% 
  ggplot(aes(x=month, y = total_count, fill=member_casual)) +
  geom_bar(position="dodge", stat="identity",color="black",width=0.7)+
  labs(title = 'Total Number of Rides by Member Types and Months',x='Month',y='Number of Rides')+
  theme_classic()


# Average ride length by member types
trip_data_clean %>% 
  group_by(member_casual) %>% 
  summarise(avg_ride_length = mean(ride_length)) %>% 
  ggplot(aes(x=member_casual, y=avg_ride_length, fill=member_casual)) +
  geom_bar(position="dodge", stat="identity",color="black",width=0.5) +
  labs(title = 'Average Ride Duration Spent by Member Types',x='Type of Members',y='Average Ride Duration (mins)')+
  theme_classic()
  
# Average ride length by member and ride types
trip_data_clean %>% 
  group_by(member_casual,rideable_type) %>% 
  summarise(avg_ride_length = mean(ride_length)) %>% 
  ggplot(aes(x=rideable_type, y=avg_ride_length, fill=rideable_type))+
  geom_bar(position="dodge", stat="identity",color="black",width=0.5)+
  facet_wrap(~member_casual)+
  labs(title = 'Average Ride Duration Spent by Member and Bike Types', x='Type of Bikes', y='Average Ride Duration (mins)')+
  theme_bw()




