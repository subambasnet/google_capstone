# Load libraries
library(tidyverse)  # For calculations
library(lubridate)  # For handling dates
library(hms)        # For handling time
library(data.table) # For exporting data frames

# Specify the months
months <- c("11", "12", "01", "02", "03", "04", "05", "06", "07", "08", "09", "10")

# Initialize an empty list to store data frames
monthly_dfs <- list()

# Specify the directory where the CSV files are located
directory_path <- "E:/Portfolio_projects/Cyclistic"

# Loop through the months and read the corresponding CSV files
for (month in months) {
  # Adjust file naming for November and December
  if (month %in% c("11", "12")) {
    year_month <- paste0("2022", month)
  } else {
    year_month <- paste0("2023", month)
  }
  
  file_name <- paste0(directory_path, "/", year_month, "-divvy-tripdata.csv")
  df_name <- paste0(tolower(year_month), "_df")
  
  # Use tryCatch to handle errors
  tryCatch({
    if (file.exists(file_name)) {
      monthly_dfs[[df_name]] <- read_csv(file_name)
    } else {
      warning(paste("File not found:", file_name))
    }
  }, error = function(e) {
    warning(paste("Error reading file:", file_name, "\n", conditionMessage(e)))
  })
}

# Assign the data frames to individual variables
list2env(monthly_dfs, envir = .GlobalEnv)

# Merge all of the data frames into one year view
cyclistic_df <- do.call(rbind, monthly_dfs)

# Remove individual month data frames (excluding cyclistic_df) to clear up space in the environment 
rm(list = setdiff(ls(), "cyclistic_df"))

# Create a new data frame to contain new columns
cyclistic_date <- data.frame(cyclistic_df)

# Calculate ride length by subtracting ended_at time from started_at time and convert it to minutes
cyclistic_date$ride_length <- difftime(cyclistic_date$ended_at, cyclistic_date$started_at, units = "mins")

# Create columns for: day of week, month, day, year, time, hour
cyclistic_date$date <- as.Date(cyclistic_date$started_at)  # Default format is yyyy-mm-dd, use start date
cyclistic_date$day_of_week <- format(cyclistic_date$date, "%A")  # Create column for day of week
cyclistic_date$month <- format(cyclistic_date$date, "%m")  # Create column for month
cyclistic_date$day <- format(cyclistic_date$date, "%d")  # Create column for day
cyclistic_date$year <- format(cyclistic_date$date, "%Y")  # Create column for year

# Format time as HH:MM:SS and create columns for time and hour
cyclistic_date$started_at <- as.POSIXct(cyclistic_date$started_at, format="%Y-%m-%d %H:%M:%S")
cyclistic_date$time <- format(cyclistic_date$started_at, "%H:%M:%S")  # Create column for time
cyclistic_date$hour <- hour(cyclistic_date$started_at)  # Create column for hour

# Create column for different seasons: Spring, Summer, Fall, Winter
cyclistic_date <- cyclistic_date %>% mutate(
  # Using case_when to assign seasons based on the month
  season = case_when(
    month %in% c("03", "04", "05") ~ "Spring",  # March, April, May
    month %in% c("06", "07", "08") ~ "Summer",  # June, July, August
    month %in% c("09", "10", "11") ~ "Fall",    # September, October, November
    month %in% c("12", "01", "02") ~ "Winter"   # December, January, February
  )
)

# Create column for different time_of_day: Night, Morning, Afternoon, Evening
cyclistic_date <- cyclistic_date %>% mutate(
  # Using case_when to assign time_of_day based on the hour
  time_of_day = case_when(
    hour %in% c("0", "1", "2", "3", "4", "5") ~ "Night",            # 12:00 AM - 5:59 AM
    hour %in% c("6", "7", "8", "9", "10", "11") ~ "Morning",        # 6:00 AM - 11:59 AM
    hour %in% c("12", "13", "14", "15", "16", "17") ~ "Afternoon",  # 12:00 PM - 5:59 PM
    hour %in% c("18", "19", "20", "21", "22", "23") ~ "Evening"     # 6:00 PM - 11:59 PM
  )
)

# Clean the data

# Remove rows with NA values
cyclistic_date <- na.omit(cyclistic_date)

# Remove duplicate rows
cyclistic_date <- distinct(cyclistic_date)

# Remove rows where ride_length is 0 or negative
cyclistic_date <- cyclistic_date[!(cyclistic_date$ride_length <= 0),]

# Remove columns not needed: ride_id, start_station_id, end_station_id, start_lat, start_long, end_lat, end_lng
cyclistic_date <- cyclistic_date %>%
  select(-c(ride_id, start_station_id, end_station_id, start_lat, start_lng, end_lat, end_lng))

# View the final data
View(cyclistic_date)

#-----------------------------------------TOTAL RIDES--------------------------------------

# Calculate the total number of rides
total_rides <- nrow(cyclistic_date)

#-----------------MEMBER TYPE---------------------

# Count the number of rides for each member type
member_type_counts <- cyclistic_date %>%
  group_by(member_casual) %>% 
  count(member_casual)

#----------------TYPE OF BIKE---------------------

# Count the rides for each member type and bike type
bike_type_counts_by_member <- cyclistic_date %>%
  group_by(member_casual, rideable_type) %>% 
  count(rideable_type)

# Count the total rides for each bike type
total_bike_type_counts <- cyclistic_date %>%
  group_by(rideable_type) %>% 
  count(rideable_type)

#-------------------HOUR--------------------------

# Total rides by member type per hour
rides_by_member_type_per_hour <- cyclistic_date %>%
  group_by(member_casual, hour) %>% 
  count() %>% 
  print(n = Inf, na.print = "NA") # Print entire tibble and handle NA values

# Total rides per hour
total_rides_per_hour <- cyclistic_date %>%
  count(hour) %>% 
  print(n = Inf, na.print = "NA") # Print entire tibble and handle NA values

#----------------------TIME OF DAY-----------------------

# Define a function to calculate total rides for a specific time_of_day
total_rides_by_time_of_day <- function(data, time_of_day) {
  data %>%
    filter(time_of_day == time_of_day) %>% 
    count(member_casual, time_of_day) %>% 
    print()
}

# Calculate and print total rides for each time_of_day
time_of_day_list <- c("Morning", "Afternoon", "Evening", "Night")

for (time in time_of_day_list) {
  total_rides_by_time_of_day(cyclistic_date, time)
}

# Calculate and print total rides for all times of day
cyclistic_date %>%
  group_by(member_casual, time_of_day) %>% 
  count() %>% 
  print()

# Calculate and print total rides for each time_of_day without grouping by member_casual
cyclistic_date %>%
  group_by(time_of_day) %>% 
  count() %>% 
  print()

#----------------DAY OF THE WEEK------------------

# Total rides by member type and day_of_week
cyclistic_date %>%
  group_by(member_casual, day_of_week) %>% 
  count() %>% 
  print()

# Total rides by day_of_week
cyclistic_date %>%
  group_by(day_of_week) %>% 
  count() %>% 
  print()

#----------------DAY OF THE MONTH-----------------

# Total rides by member type and day
cyclistic_date %>%
  group_by(member_casual, day) %>% 
  count() %>% 
  print(n = 62)

# Total rides by day
cyclistic_date %>%
  group_by(day) %>% 
  count() %>% 
  print(n = 31)

#---------------------MONTH-----------------------

# Total rides by member type and month
cyclistic_date %>%
  group_by(member_casual, month) %>% 
  count() %>% 
  mutate(month = month.name[as.numeric(month)]) %>%  # Convert numeric month to month name
  print(n = 24)

# Total rides by month
cyclistic_date %>%
  group_by(month) %>% 
  count() %>% 
  mutate(month = month.name[as.numeric(month)]) %>%  # Convert numeric month to month name
  print(n = 12)

#--------------------SEASON-----------------------

# Function to calculate total rides by season
calculate_season_rides <- function(data, season) {
  data %>%
    group_by(member_casual) %>% 
    filter(season == season) %>% 
    count(season)
}

#-----spring-------
spring_rides_by_member <- calculate_season_rides(cyclistic_date, "Spring")
spring_total_rides <- calculate_season_rides(cyclistic_date, "Spring")

#-----summer-------
summer_rides_by_member <- calculate_season_rides(cyclistic_date, "Summer")
summer_total_rides <- calculate_season_rides(cyclistic_date, "Summer")

#-----fall-------
fall_rides_by_member <- calculate_season_rides(cyclistic_date, "Fall")
fall_total_rides <- calculate_season_rides(cyclistic_date, "Fall")

#-----winter-------
winter_rides_by_member <- calculate_season_rides(cyclistic_date, "Winter")
winter_total_rides <- calculate_season_rides(cyclistic_date, "Winter")

#-----all seasons-------

# Total rides by member type and season
all_season_rides_by_member <- cyclistic_date %>%
  group_by(season, member_casual) %>% 
  count(season)

# Total rides by season
all_season_total_rides <- cyclistic_date %>%
  group_by(season) %>% 
  count(season)

#------------------------------------AVERAGE RIDE LENGTH-----------------------------------

# Print the average of ride_length
print(mean(cyclistic_date$ride_length))

#------------------MEMBER TYPE--------------------

# Average ride length by member type
cyclistic_date %>%
  group_by(member_casual) %>% 
  summarise(across(ride_length, mean, .names = "{.col}_avg"))

#----------------TYPE OF BIKE---------------------

# Total rides by member type and rideable type
total_rides_by_type <- cyclistic_date %>%
  group_by(member_casual, rideable_type) %>% 
  summarise(total_rides = n())

# Average ride length by member type and rideable type
average_ride_length_by_type <- cyclistic_date %>%
  group_by(member_casual, rideable_type) %>% 
  summarise(average_ride_length = mean(ride_length))

# Total rides by rideable type
total_rides_by_rideable <- cyclistic_date %>%
  group_by(rideable_type) %>% 
  summarise(total_rides = n())

# Average ride length by rideable type
average_ride_length_by_rideable <- cyclistic_date %>%
  group_by(rideable_type) %>% 
  summarise(average_ride_length = mean(ride_length))

#-----------------------HOUR-------------------------

# Calculate the average ride length by member type and hour
average_ride_by_hour_member <- cyclistic_date %>% 
  group_by(hour, member_casual) %>% 
  summarise_at(vars(ride_length), list(time = mean)) %>% 
  print(n = 10)  # Display the first 10 rows

# Calculate the average ride length by hour
average_ride_by_hour <- cyclistic_date %>% 
  group_by(hour) %>% 
  summarise_at(vars(ride_length), list(time = mean)) %>% 
  print(n = 10)  # Display the first 10 rows

#--------------------TIME OF DAY---------------------

#----morning----

# Calculate the average ride length by member type in the morning
average_ride_morning_member <- cyclistic_date %>% 
  group_by(member_casual) %>% 
  filter(time_of_day == "Morning") %>% 
  summarise_at(vars(ride_length), list(time = mean))

# Calculate the overall average ride length in the morning
average_ride_morning <- cyclistic_date %>% 
  filter(time_of_day == "Morning") %>% 
  summarise_at(vars(ride_length), list(time = mean))

#----afternoon----

# Calculate the average ride length by member type in the afternoon
average_ride_afternoon_member <- cyclistic_date %>% 
  group_by(member_casual) %>% 
  filter(time_of_day == "Afternoon") %>% 
  summarise_at(vars(ride_length), list(time = mean))

# Calculate the overall average ride length in the afternoon
average_ride_afternoon <- cyclistic_date %>% 
  filter(time_of_day == "Afternoon") %>% 
  summarise_at(vars(ride_length), list(time = mean))

#----evening----

# Calculate the average ride length by member type in the evening
average_ride_evening_member <- cyclistic_date %>% 
  group_by(member_casual) %>% 
  filter(time_of_day == "Evening") %>% 
  summarise_at(vars(ride_length), list(time = mean))

# Calculate the overall average ride length in the evening
average_ride_evening <- cyclistic_date %>% 
  filter(time_of_day == "Evening") %>% 
  summarise_at(vars(ride_length), list(time = mean))

#----night----

# Calculate the average ride length by member type at night
average_ride_night_member <- cyclistic_date %>% 
  group_by(member_casual) %>% 
  filter(time_of_day == "Night") %>% 
  summarise_at(vars(ride_length), list(time = mean))

# Calculate the overall average ride length at night
average_ride_night <- cyclistic_date %>% 
  filter(time_of_day == "Night") %>% 
  summarise_at(vars(ride_length), list(time = mean))

#---all times of day---

# Calculate the average ride length by member type for all times of day
average_ride_all_times_member <- cyclistic_date %>% 
  group_by(time_of_day, member_casual) %>% 
  summarise_at(vars(ride_length), list(time = mean))

# Calculate the overall average ride length for all times of day
average_ride_all_times <- cyclistic_date %>% 
  group_by(time_of_day) %>% 
  summarise_at(vars(ride_length), list(time = mean))

#-------------------DAY OF THE WEEK-----------------

# Calculate the average ride length by member type for each day of the week
average_ride_day_of_week_member <- cyclistic_date %>% 
  group_by(member_casual, day_of_week) %>% 
  summarise_at(vars(ride_length), list(time = mean))

# Calculate the overall average ride length for each day of the week
average_ride_day_of_week <- cyclistic_date %>% 
  group_by(day_of_week) %>% 
  summarise_at(vars(ride_length), list(time = mean))

#-----------------DAY OF THE MONTH------------------

# Calculate the average ride length by member type for each day of the month
average_ride_day_member <- cyclistic_date %>% 
  group_by(day, member_casual) %>% 
  summarise_at(vars(ride_length), list(time = mean)) %>% 
  print(n=62)  # Lets you view the entire tibble

# Calculate the overall average ride length for each day of the month
average_ride_day <- cyclistic_date %>% 
  group_by(day) %>% 
  summarise_at(vars(ride_length), list(time = mean)) %>% 
  print(n=31)  # Lets you view the entire tibble

#---------------------MONTH--------------------------

# Calculate the average ride length by member type for each month
average_ride_month_member <- cyclistic_date %>% 
  group_by(month, member_casual) %>% 
  summarise_at(vars(ride_length), list(time = mean)) %>% 
  print(n=24)  # Lets you view the entire tibble

# Calculate the overall average ride length for each month
average_ride_month <- cyclistic_date %>% 
  group_by(month) %>% 
  summarise_at(vars(ride_length), list(time = mean))

#----------------------SEASON-------------------------

#-----spring------

# Calculate the average ride length by member type for spring
average_ride_spring_member <- cyclistic_date %>% 
  group_by(member_casual) %>% 
  filter(season == "Spring") %>% 
  summarise_at(vars(ride_length), list(time = mean))

# Calculate the overall average ride length for spring
average_ride_spring <- cyclistic_date %>% 
  filter(season == "Spring") %>% 
  summarise_at(vars(ride_length), list(time = mean))

#-----summer------

# Calculate the average ride length by member type for summer
average_ride_summer_member <- cyclistic_date %>% 
  group_by(member_casual) %>% 
  filter(season == "Summer") %>% 
  summarise_at(vars(ride_length), list(time = mean))

# Calculate the overall average ride length for summer
average_ride_summer <- cyclistic_date %>% 
  filter(season == "Summer") %>% 
  summarise_at(vars(ride_length), list(time = mean))

#-----fall------

# Calculate the average ride length by member type for fall
average_ride_fall_member <- cyclistic_date %>% 
  group_by(member_casual) %>% 
  filter(season == "Fall") %>% 
  summarise_at(vars(ride_length), list(time = mean))

# Calculate the overall average ride length for fall
average_ride_fall <- cyclistic_date %>% 
  filter(season == "Fall") %>% 
  summarise_at(vars(ride_length), list(time = mean))

#-----winter-----

# Calculate the average ride length by member type for winter
average_ride_winter_member <- cyclistic_date %>% 
  group_by(member_casual) %>% 
  filter(season == "Winter") %>% 
  summarise_at(vars(ride_length), list(time = mean))

# Calculate the overall average ride length for winter
average_ride_winter <- cyclistic_date %>% 
  filter(season == "Winter") %>% 
  summarise_at(vars(ride_length), list(time = mean))

#----all seasons----

# Calculate the average ride length by member type for all seasons
average_ride_all_seasons_member <- cyclistic_date %>% 
  group_by(season, member_casual) %>% 
  summarise_at(vars(ride_length), list(time = mean))

# Calculate the overall average ride length for all seasons
average_ride_all_seasons <- cyclistic_date %>% 
  group_by(season) %>% 
  summarise_at(vars(ride_length), list(time = mean))











