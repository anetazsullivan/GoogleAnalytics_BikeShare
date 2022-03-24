install.packages("tidyverse")
install.packages("lubridate")
install.packages("ggplot2")
library(tidyverse)
library(lubridate)
library(ggplot2)

getwd()
setwd("/Users/anetasullivan")

library(readr)

q2_2019 <- read_csv("Desktop/Qs/Divvy_Trips_2019_Q2.csv")
q3_2019 <- read_csv("Desktop/Qs/Divvy_Trips_2019_Q3.csv")
q4_2019 <- read_csv("Desktop/Qs/Divvy_Trips_2019_Q4.csv")
q1_2020 <- read_csv("Desktop/Qs/Divvy_Trips_2020_Q1.csv")

colnames(q3_2019)
colnames(q4_2019)
colnames(q2_2019)
colnames(q1_2020)

library(dplyr)
install.packages("here")
library(here)

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




str(q3_2019)
str(q4_2019)
str(q2_2019)
str(q1_2020)

q4_2019 <- mutate(q4_2019,ride_id=as.character(ride_id),
                  rideable_type=as.character(rideable_type))
q3_2019 <-  mutate(q3_2019, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type)) 
q2_2019 <-  mutate(q2_2019, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type))

colnames(q3_2019)
colnames(q4_2019)
colnames(q2_2019)
colnames(q1_2020)


all_data <- bind_rows(q4_2019,q3_2019,q2_2019,q1_2020)

all_data <- all_data %>% 
  select(-c(gender,
            birthyear,
            "01 - Rental Details Duration In Seconds Uncapped",
            "Member Gender",
            "05 - Member Details Member Birthday Year",
            start_lat,
            start_lng,
            end_lat,
            end_lng,
            tripduration))

colnames(all_data)
nrow(all_data)
dim(all_data) #dimensions of teh framework
head(all_data)
str(all_data)

View(all_data)

table(all_data$member_casual)

all_data <-  all_data %>% 
  mutate(member_casual = recode(member_casual,
                                "Subscriber" = "member",
                                "Customer" = "casual"))

all_data$date <- as.Date(all_data$started_at)
all_data$month <- format(as.Date(all_data$date), "%m")
all_data$day <- format(as.Date(all_data$date), "%d")
all_data$year <- format(as.Date(all_data$date), "%Y")
all_data$day_of_week <- format(as.Date(all_data$date), "%A")

all_data$ride_length <- difftime(all_data$ended_at,all_data$started_at)

str(all_data)

is.factor(all_data$ride_length)
is.character(all_data$ride_length)

all_data$ride_length <- as.numeric(as.character(all_data$ride_length))
is.numeric(all_data$ride_length)

all_data_V2 <- all_data[!(all_data$start_station_name == "HQ QR"| 
                            all_data$ride_length<0),]
mean(all_data_V2$ride_length)
median(all_data_V2$ride_length)
max(all_data_V2$ride_length)
min(all_data_V2$ride_length)

summary(all_data_V2$ride_length)

aggregate(all_data_V2$ride_length ~ all_data_V2$member_casual, FUN = mean)
aggregate(all_data_V2$ride_length ~ all_data_V2$member_casual, FUN = median)
aggregate(all_data_V2$ride_length ~ all_data_V2$member_casual, FUN = max)
aggregate(all_data_V2$ride_length ~ all_data_V2$member_casual, FUN = min)

aggregate(all_data_V2$ride_length ~ all_data_V2$member_casual + all_data_V2$day_of_week, FUN = mean)

all_data_V2$day_of_week <- ordered(all_data_V2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

aggregate(all_data_V2$ride_length ~ all_data_V2$member_casual + all_data_V2$day_of_week, FUN = mean)


all_data_V2 %>% 
  mutate(weekday=wday(started_at,label = TRUE)) %>% 
  group_by(member_casual,weekday) %>% 
  summarise(number_of_rides=n(),
            average_duration=mean(ride_length)) %>% 
  arrange(member_casual,weekday)

all_data_V2 %>% 
  mutate(weekday=wday(started_at,label = TRUE)) %>% 
  group_by(member_casual,weekday) %>% 
  summarise(number_of_rides=n(),
            average_duration=mean(ride_length)) %>% 
  arrange(member_casual,weekday) %>% 
  ggplot(aes(x=weekday,y=number_of_rides,fill=member_casual)) +
  geom_col(position="dodge")
  
all_data_V2 %>% 
  mutate(weekday=wday(started_at,label = TRUE)) %>% 
  group_by(member_casual,weekday) %>% 
  summarise(number_of_rides=n(),
            average_duration = mean(ride_length)) %>% 
  arrange(member_casual,weekday) %>% 
  ggplot(aes(x=weekday, y=average_duration,fill=member_casual)) +
  geom_col(position = "dodge")


write.csv(counts,file = '/Users/anetasullivan/Desktop/avr_ride_length.csv')

getwd()
setwd("/Users/anetasullivan/Desktop")

write.csv(all_data_V2,file = '/Users/anetasullivan/Desktop/all_data.csv')


