#load libraries 
library(tidyverse) #calculations
library(lubridate) #dates 
library(hms) #time
library(data.table) #exporting data frame

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

# Calculate ride length by subtracting ended_at time from started_at time and convert to minutes
cyclistic_date$ride_length <- as.numeric(difftime(cyclistic_df$ended_at, cyclistic_df$started_at, units = "mins"))

# Round ride_length to one decimal place
cyclistic_date$ride_length <- round(cyclistic_date$ride_length, digits = 1)

# Create columns for day of week, month, day, year, time, and hour

# Convert started_at to Date format
cyclistic_date$date <- as.Date(cyclistic_date$started_at)

# Calculate and create a column for the day of the week
cyclistic_date$day_of_week <- wday(cyclistic_date$started_at)
cyclistic_date$day_of_week <- format(cyclistic_date$date, "%A")

# Create columns for month, day, and year
cyclistic_date$month <- format(cyclistic_date$date, "%m")
cyclistic_date$day <- format(cyclistic_date$date, "%d")
cyclistic_date$year <- format(cyclistic_date$date, "%Y")

# Format time as HH:MM:SS and create a new column for time
cyclistic_date$time <- format(cyclistic_date$date, "%H:%M:%S")
cyclistic_date$time <- as_hms(cyclistic_date$started_at)

# Create a new column for the hour
cyclistic_date$hour <- hour(cyclistic_date$time)


# Create a column for different seasons: Spring, Summer, Fall, Winter

cyclistic_date <- cyclistic_date %>% 
  mutate(season = case_when(
    month %in% c("03", "04", "05") ~ "Spring",
    month %in% c("06", "07", "08") ~ "Summer",
    month %in% c("09", "10", "11") ~ "Fall",
    month %in% c("12", "01", "02") ~ "Winter"
  ))


# Create a column for different time_of_day: Night, Morning, Afternoon, Evening

cyclistic_date <- cyclistic_date %>% 
  mutate(time_of_day = case_when(
    hour %in% c("0", "1", "2", "3", "4", "5") ~ "Night",
    hour %in% c("6", "7", "8", "9", "10", "11") ~ "Morning",
    hour %in% c("12", "13", "14", "15", "16", "17") ~ "Afternoon",
    hour %in% c("18", "19", "20", "21", "22", "23") ~ "Evening"
  ))

# Create a column for the month using the full month name

cyclistic_date <- cyclistic_date %>% 
  mutate(month = case_when(
    month == "01" ~ "January",
    month == "02" ~ "February",
    month == "03" ~ "March",
    month == "04" ~ "April",
    month == "05" ~ "May",
    month == "06" ~ "June",
    month == "07" ~ "July",
    month == "08" ~ "August",
    month == "09" ~ "September",
    month == "10" ~ "October",
    month == "11" ~ "November",
    month == "12" ~ "December"
  ))

# Clean the data
cyclistic_date <- cyclistic_date %>%
  na.omit() %>%              # Remove rows with NA values
  distinct() %>%             # Remove duplicate rows
  filter(ride_length > 0) %>% # Remove rows where ride_length is 0 or negative
  select(-c(ride_id, start_station_id, end_station_id, start_lat, start_lng, end_lat, end_lng)) # Remove unnecessary columns

# View the final data
View(cyclistic_date)

#created a new dataframe to visualize
cyclistic_viz <- cyclistic_date

#clean the data
cyclistic_viz <- cyclistic_viz %>%  #remove columns not needed: start_station_name, end_station_name, time, started_at, ended_at
  select(-c(start_station_name, end_station_name, time, started_at, ended_at))

#download the new data as a .csv file
fwrite(cyclistic_viz,"cyclistic_data.csv")