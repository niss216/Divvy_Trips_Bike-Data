# INSTALL PACKAGES & LIBRARIES

install.packages("tidyverse")
install.packages("ggplot2")
install.packages("lubridate")
library ("tidyverse")
library("ggplot2")
library("lubridate")

#====================================================================
getwd() #displays my working directory &sets to simplify calls to data  
setwd("C:/Users/laptop/OneDrive/Documents/cyclistic_case study IN R") 
 
#====================================================================
# STEP 1: COLLECT THE DATA ( Upload Divvy Data sets)

q2_2019 <- read_csv("Divvy_Trips_2019_Q2.csv")
q3_2019 <- read_csv("Divvy_Trips_2019_Q3.csv")
q4_2019 <- read_csv("Divvy_Trips_2019_Q4.csv")
q1_2020 <- read_csv("Divvy_Trips_2020_Q1.csv")

#====================================================================

# STEP 2: INSPECT THE DATA..COMPARE COLUMN NAMES FOR ALL THE TABLES TO MATCH THE COLUMN NAMES & THE DATA CONCESTENCY ALSO,BEFFOR THE MERGE PROCESS IN ONE TABLE.

colnames(q3_2019)

colnames(q4_2019)

colnames(q2_2019)

colnames(q1_2020)

#=====================================================================

# Rename columns to make them consistent with q1_2020

(q4_2019 <- rename(q4_2019
                   ,ride_id = trip_id
                   ,rideable_type = bikeid 
                   ,started_at = start_time  
                   ,ended_at = end_time  
                   ,start_station_name = from_station_name 
                   ,start_station_id = from_station_id 
                   ,end_station_name = to_station_name 
                   ,end_station_id = to_station_id 
                   ,member_casual = usertype))


(q3_2019 <- rename(q3_2019
                   ,ride_id = trip_id
                   ,rideable_type = bikeid 
                   ,started_at = start_time  
                   ,ended_at = end_time  
                   ,start_station_name = from_station_name 
                   ,start_station_id = from_station_id 
                   ,end_station_name = to_station_name 
                   ,end_station_id = to_station_id 
                   ,member_casual = usertype))

(q2_2019 <- rename(q2_2019
                   ,ride_id = "01 - Rental Details Rental ID"
                   ,rideable_type = "01 - Rental Details Bike ID" 
                   ,started_at = "01 - Rental Details Local Start Time"  
                   ,ended_at = "01 - Rental Details Local End Time"  
                   ,start_station_name = "03 - Rental Start Station Name" 
                   ,start_station_id = "03 - Rental Start Station ID"
                   ,end_station_name = "02 - Rental End Station Name" 
                   ,end_station_id = "02 - Rental End Station ID"
                   ,member_casual = "User Type"))

# Inspect the tables and look for inconsistencies

str(q1_2020)
str(q4_2019)
str(q3_2019)
str(q2_2019)

# Convert ride_id and rideable_type to character according to q1_2020 data, so that they can stack correctly

q4_2019 <-  mutate(q4_2019, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type)) 
q3_2019 <-  mutate(q3_2019, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type)) 
q2_2019 <-  mutate(q2_2019, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type)) 

#=============================================================================
# merge the four table together into one table to form a big data frame.

all_trips <- bind_rows(q2_2019, q3_2019, q4_2019, q1_2020)

#delete unnecessary columns from (trip_data)..as this data(fields) was dropped beginning in 2020

all_trips <- all_trips %>%  
  select(-c(start_lat, start_lng, end_lat, end_lng, birthyear, gender, "01 - Rental Details Duration In Seconds Uncapped", "05 - Member Details Member Birthday Year", "Member Gender", "tripduration"))
#=========================================================================

# STEP 3: CLEAN UP AND ADD SOME DATA TO PREPARE FOR ANALYSIS

# 1... Inspect the new table that has been created
colnames(all_trips)  #List of column names.
nrow(all_trips)      #How many rows are in data frame.
dim(all_trips)       #Dimensions of the data frame.
head(all_trips)      #Show the first 6 rows of data frame.Also tail(qs_raw)
str(all_trips)       #See list of columns and data types (numeric, character, etc)
summary(all_trips)   #Statistical summary of data. Mainly for numeric

# There are a few problems we will need to fix:

# (1)..replace "Subscriber" with "member" and "Customer" with "casual" in member_casual Column according to the current nomenclature  for The Divvy company in year[2020]  .
 table(all_trips$member_casual)# show seeing how many observations fall under each usertype 
 
 all_trips=all_trips %>%
   mutate(member_casual=recode(member_casual
                               ,"Subscriber" ="member"
                               ,"Customer" = "casual"))
 #for check
 table(all_trips$member_casual)
 head(all_trips)
 
# (2)..add some additional columns of data -- such as day, month, year -- that provide additional opportunities to aggregate the data by it.
 all_trips$date        <- as.Date(all_trips$started_at)
 all_trips$month       <- format(as.Date(all_trips$date),"%m")
 all_trips$day         <- format(as.Date(all_trips$date),"%d")
 all_trips$year        <- format(as.Date(all_trips$date),"%y")
 all_trips$day_of_week <- format(as.Date(all_trips$date),"%A")
 
 # (3)..add a calculated field for the length of ride (in seconds)
 
 all_trips$ride_length <-difftime(all_trips$ended_at,all_trips$started_at)
 
 #======================================================================================================================================================
 
 # STEP 4: conduct descriptive analyzing after adding the new columns.
 
 
 # 1- convert ride_length from factor to Numeric
 
 str(all_trips)
 is.factor(all_trips$ride_length)
 
 all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
 is.numeric(all_trips$ride_length)
 str(all_trips)
 

 ----------------
 
 # 2- There are some rides where tripduration shows up as negative, including several hundred rides where Divvy took bikes out of circulation for Quality Control reasons. We will delete these rides.
 # SO,We will create a new version of the data frame, as the data will be cleaned and removed.

 all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length<0),]
 
 # 3-	Analysis on ride length:
 
 summary (all_trips_v2$ride_length)

 # 4- Compare and summarize the members and casual members(users):[for taking the good Solution according to the result we will find out]
 
 aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)
 aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)
 aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)
 aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)
 
 # 5- See the average ride time by each day for members vs casual users(To find out what most Crowded Day to continue analyzing.
 
 aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual+all_trips_v2$day_of_week,FUN = mean)

 # 6- Notice that the days of the week are out of order. Let's fix that.

 all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
 
 aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual+all_trips_v2$day_of_week, FUN = mean)
 
 # 7- analyze ridership data by type and weekday

 all_trips_v2 %>% 
   group_by(member_casual,day_of_week)  %>% #groups by usertype and weekday
   summarise(number_of_rides=n(),average_duration=mean(ride_length))  %>% 
   #calculates the number of rides and average duration 
   arrange(member_casual,day_of_week)   # sorts
 
 #=========================================================================================================================================================
  #  step 5: Visualization 
 
 #  1- visualize the number of rides by rider type:
 

 all_trips_v2 %>% 
   
   group_by(member_casual, day_of_week) %>%
   summarise(number_of_rides = n(),average_duration = mean(ride_length)) %>% 
   arrange(member_casual, day_of_week)  %>% 
   ggplot(aes(x = day_of_week, y = number_of_rides, fill = member_casual)) +
   geom_col(position = "dodge")
 

 #  2- visualize the average_duration by rider type:
 
 all_trips_v2 %>% 
   
   group_by(member_casual, day_of_week) %>%
   summarise(number_of_rides = n(),average_duration = mean(ride_length)) %>% 
   arrange(member_casual, day_of_week)  %>% 
   ggplot(aes(x = day_of_week, y = average_duration, fill = member_casual)) +
   geom_col(position = "dodge")
 

   
 #...............................................................................................
   
# EXPORT SUMMARY FILE FOR FURTHER ANALYSIS
 #for example
 counts <- aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)
 write.csv(counts, file = "C:/Users/laptop/OneDrive/Documents/cyclistic_case study IN R/avg_ride_length.csv")
 
 #another example  
 conclusion <-all_trips_v2 %>% 
   group_by(member_casual,day_of_week)  %>% 
   summarise(number_of_rides=n(),average_duration=mean(ride_length))  %>% 
   #calculates the number of rides and average duration 
   arrange(member_casual,day_of_week)   
   write.csv(  conclusion, file = "C:/Users/laptop/OneDrive/Documents/cyclistic_case study IN R/num_rides&avg_ride_length.csv")
 
 ####........... DONE
 

 