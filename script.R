install.packages("tidyverse")
library(tidyverse)
library(stringr)
library(dplyr)
library(lubridate)
library(ggplot2)

#import files
trips_2019 <- read.csv("Divvy_Trips_2019_Q1.csv")
trips_2020 <- read.csv("Divvy_Trips_2020_Q1.csv")

#delete empty rows
trips_2019 <- trips_2019[complete.cases(trips_2019 ), ]
trips_2020 <- trips_2020[complete.cases(trips_2020 ), ]

#trim whitespace
trips_2019 %>% 
  mutate_if(is.character, str_trim)
trips_2020 %>% 
  mutate_if(is.character, str_trim)

#change started_at and ended_at columns in trips_2020
colnames(trips_2020)[3] <- "start_time"
colnames(trips_2020)[4] <- "end_time"


#make ride_length column
trips_2019$ride_length <- difftime(trips_2019$end_time, trips_2019$start_time, units="mins")
trips_2020$ride_length <- difftime(trips_2020$end_time, trips_2020$start_time, units="mins")

#make day_of_week column
trips_2019$day_of_week <- wday(trips_2019$start_time, label=TRUE)
trips_2020$day_of_week <- wday(trips_2020$start_time, label=TRUE)

#match column names and values 2020 matches 2019
colnames(trips_2020)[1] <- "trip_id"
colnames(trips_2019)[6] <- "start_station_id"
colnames(trips_2019)[7] <- "start_station_name"
colnames(trips_2019)[8] <- "end_station_id"
colnames(trips_2019)[9] <- "end_station_name"
colnames(trips_2020)[13] <- "usertype"

#match corresponding values in usertype col (i.e. member = subscriber)
trips_2019$usertype <- replace(trips_2019$usertype, trips_2019$usertype == "Subscriber", "member")
trips_2019$usertype <- replace(trips_2019$usertype, trips_2019$usertype == "Customer", "casual")

#merge the data frames
merged_trips <- merge(trips_2019, trips_2020, all=TRUE)

#delete rows with ride_length >= 50 hours
merged_trips_trimmed <- filter(merged_trips, merged_trips$ride_length < 3000 )

#make summary data frame for untrimmed merged_trips (row 1 = 2019, row 2 = 2020, row 3 = merged)
summary_df <- data.frame(mean_ride_length = c(0,0,0),
                         max_ride_length  = c(0,0,0),
                         mode_day_of_week = c(0,0,0))

#get mean of ride length for each data frame
summary_df$mean_ride_length <- c(mean(trips_2019$ride_length),
                                 mean(trips_2020$ride_length),
                                 mean(merged_trips$ride_length))

#get max ride length for each data frame
summary_df$max_ride_length <- c(max(trips_2019$ride_length),
                                 max(trips_2020$ride_length),
                                 max(merged_trips$ride_length))

#get mode of day_of_week
#create mode function
find_mode <- function(x) {
  u <- unique(x)
  tab <- tabulate(match(x, u))
  u[tab == max(tab)]
}
#get mode
summary_df$mode_day_of_week <- c(find_mode(trips_2019$day_of_week),
                                find_mode(trips_2020$day_of_week),
                                find_mode(merged_trips$day_of_week))

#get mean ride lengths for each usertype
means_by_usertype <- data.frame(c(mean((filter(merged_trips_trimmed, usertype == "member"))$ride_length),
                                  mean((filter(merged_trips_trimmed, usertype == "casual"))$ride_length)))

#get max by usertype
maxs_by_usertype <- data.frame(c(max((filter(merged_trips, usertype == "member"))$ride_length),
                                 max((filter(merged_trips, usertype == "casual"))$ride_length)))

#make a bar chart that counts trips for each day of the week
plot_trips_by_day <- ggplot(merged_trips_trimmed, aes(day_of_week)) + geom_bar()
plot_trips_by_day_member <- ggplot(filter(merged_trips_trimmed, usertype == "member"), aes(day_of_week)) + 
  geom_bar(fill = "blue") + 
  ggtitle("Count of Trips by Day of the Week (Members)") + 
  labs(x = "Day of Week")
plot_trips_by_day_casual <- ggplot(filter(merged_trips_trimmed, usertype == "casual"), aes(day_of_week)) + 
  geom_bar(fill = "red") + 
  ggtitle("Count of Trips by Day of the Week (Casuals)") + 
  labs(x = "Day of Week")

#make df to get mean ride lengths based on usertype and day of week
mean_ride_lengths_by_usertype_and_day_of_week <-
  data.frame(day_of_week = c("Sun","Mon","Tue","Wed","Thu","Fri","Sat"),
             members = c(0,0,0,0,0,0,0),
             casuals = c(0,0,0,0,0,0,0))
#sunday
mean_ride_lengths_by_usertype_and_day_of_week$members[1] <-
  mean(merged_trips_trimmed[merged_trips_trimmed$day_of_week == "Sun" & merged_trips_trimmed$usertype == "member", "ride_length"])

mean_ride_lengths_by_usertype_and_day_of_week$casuals[1] <-
  mean(merged_trips_trimmed[merged_trips_trimmed$day_of_week == "Sun" & merged_trips_trimmed$usertype == "casual", "ride_length"])

#monday
mean_ride_lengths_by_usertype_and_day_of_week$members[2] <-
  mean(merged_trips_trimmed[merged_trips_trimmed$day_of_week == "Mon" & merged_trips_trimmed$usertype == "member", "ride_length"])

mean_ride_lengths_by_usertype_and_day_of_week$casuals[2] <-
  mean(merged_trips_trimmed[merged_trips_trimmed$day_of_week == "Mon" & merged_trips_trimmed$usertype == "casual", "ride_length"])

#tuesday
mean_ride_lengths_by_usertype_and_day_of_week$members[3] <-
  mean(merged_trips_trimmed[merged_trips_trimmed$day_of_week == "Tue" & merged_trips_trimmed$usertype == "member", "ride_length"])

mean_ride_lengths_by_usertype_and_day_of_week$casuals[3] <-
  mean(merged_trips_trimmed[merged_trips_trimmed$day_of_week == "Tue" & merged_trips_trimmed$usertype == "casual", "ride_length"])

#wednesday
mean_ride_lengths_by_usertype_and_day_of_week$members[4] <-
  mean(merged_trips_trimmed[merged_trips_trimmed$day_of_week == "Wed" & merged_trips_trimmed$usertype == "member", "ride_length"])

mean_ride_lengths_by_usertype_and_day_of_week$casuals[4] <-
  mean(merged_trips_trimmed[merged_trips_trimmed$day_of_week == "Wed" & merged_trips_trimmed$usertype == "casual", "ride_length"])

#thursday
mean_ride_lengths_by_usertype_and_day_of_week$members[5] <-
  mean(merged_trips_trimmed[merged_trips_trimmed$day_of_week == "Thu" & merged_trips_trimmed$usertype == "member", "ride_length"])

mean_ride_lengths_by_usertype_and_day_of_week$casuals[5] <-
  mean(merged_trips_trimmed[merged_trips_trimmed$day_of_week == "Thu" & merged_trips_trimmed$usertype == "casual", "ride_length"])

#friday
mean_ride_lengths_by_usertype_and_day_of_week$members[6] <-
  mean(merged_trips_trimmed[merged_trips_trimmed$day_of_week == "Fri" & merged_trips_trimmed$usertype == "member", "ride_length"])

mean_ride_lengths_by_usertype_and_day_of_week$casuals[6] <-
  mean(merged_trips_trimmed[merged_trips_trimmed$day_of_week == "Fri" & merged_trips_trimmed$usertype == "casual", "ride_length"])

#saturday
mean_ride_lengths_by_usertype_and_day_of_week$members[7] <-
  mean(merged_trips_trimmed[merged_trips_trimmed$day_of_week == "Sat" & merged_trips_trimmed$usertype == "member", "ride_length"])

mean_ride_lengths_by_usertype_and_day_of_week$casuals[7] <-
  mean(merged_trips_trimmed[merged_trips_trimmed$day_of_week == "Sat" & merged_trips_trimmed$usertype == "casual", "ride_length"])

#make plottable data frame for visualization
days_of_week <- rep(c("Sun","Mon","Tue","Wed","Thu","Fri","Sat"), 2)
mean_ride_length <-  c(mean_ride_lengths_by_usertype_and_day_of_week$members, mean_ride_lengths_by_usertype_and_day_of_week$casuals)
usertype <- c(rep("member", 7), rep("casual", 7))
final_vis_df <- data.frame(days_of_week, mean_ride_length, usertype)
final_vis_df$days_of_week <- factor(final_vis_df$days_of_week, levels = c("Sun","Mon","Tue","Wed","Thu","Fri","Sat"))


#make grouped bar chart to visualize means
final_data_vis_mean <- ggplot(final_vis_df , aes(x=days_of_week, y=mean_ride_length, fill=usertype)) +
  geom_bar(position="dodge", stat="identity") + 
  labs(title = "Average Trip Duration Throughout the Week",
       x = "Day of Week", y = "Average Ride Duration",
       fill = "Customer Type")

