# Bellbeat Analysis - FitBit as a Proxy
## Cleaning finished 01/31/22 by Allysun Rapp - V 1.0
### Setting Up My Environment
library(tidyverse)
library(here)
library(skimr)
library(janitor)
library(dplyr)
library(readr)
library(lubridate)
library(chron)
library(scales)
library(rmdformats)

head(clean.wl2)

### Processing and Cleaning the Data Sets
#### Daily Activity
##### Reading the Data In
dailyActivity <- read_csv("Fitabase Data 4.12.16-5.12.16/dailyActivity_merged.csv")

dailySleep <- read_csv("Fitabase Data 4.12.16-5.12.16/sleepDay_merged.csv")

##### Checking the data

n_distinct(dailyActivity$Id)

###### There are 33 distinct ids in this data set, but only 30 reported 
###### participants. Who are the extra ID numbers? Possibly 1 user with 2 
###### different devices. Without more information (i.e. user name), there is 
###### no way to determine if there is data from the same user.

n_distinct(dailySleep$Id)

###### Only 24 unique IDs for daily sleep. Indicates that not all participants 
###### use this feature. 


##### Combining Daily Sleep and Daily Activity Data
dailySleep <- dailySleep %>%
  rename(ActivityDate = SleepDay)
dailySleep$ActivityDate <- as.Date(dailySleep$ActivityDate, format="%m/%d/%Y") 
dailyActivity$ActivityDate <- as.Date(dailyActivity$ActivityDate, format="%m/%d/%Y")

dailyActivity1 <- merge(dailyActivity, dailySleep, by = c("Id", "ActivityDate"),
                        all = TRUE)

view(dailyActivity1)
skim_without_charts(dailyActivity1)

##### Cleaning the Combined Data
clean.da1 <- clean_names(dailyActivity1)

clean.da2 <- distinct(clean.da1)

clean.da2$id <- as.character(clean.da2$id)

  
#### Hourly Activity
##### Reading the Data In
hourlyCal <- read_csv("Fitabase Data 4.12.16-5.12.16/hourlyCalories_merged.csv")

hourlySteps <- read_csv("Fitabase Data 4.12.16-5.12.16/hourlySteps_merged.csv")

##### Combining Hourly Calories, Hourly Intensity, and Hourly Steps Data
hourlyActivity1 <- merge(hourlyCal, hourlySteps, by = c("Id", "ActivityHour"))

view(hourlyActivity1)

##### Checking the data
skim_without_charts(hourlyActivity1)

n_distinct(hourlyActivity1$Id)

###### 33 participants again found for hourly activity.

##### Cleaning the Combined Data
clean.ha <- clean_names(hourlyActivity1)

clean.ha1 <- distinct(clean.ha)

clean.ha1$id <- as.character(clean.ha1$id)

clean.ha2 <- separate(clean.ha1,
                      activity_hour,
                      c("date", "time"), 
                      sep = " ",
                      extra = "merge")
clean.ha2$time <- format(strptime(clean.ha2$time, format = "%I:%M:%S %p"), 
                         "%H:%M:%S")
view(clean.ha2)


  
#### Heart Rate (Seconds)
##### Reading the Data In
secHR <- read_csv("Fitabase Data 4.12.16-5.12.16/heartrate_seconds_merged.csv")

##### Checking the data
skim_without_charts(secHR)

n_distinct(secHR$Id)

###### Only 14 unique participant IDs for heart rate, indicates that not all 
###### participants use this feature. 

##### Cleaning the Data
clean.hr <- clean_names(secHR)

clean.hr2 <- distinct(clean.hr)

clean.hr$id = as.character(clean.hr$id)
  
#### Weight Log
##### Reading the Data In
WeightLog <- read_csv("Fitabase Data 4.12.16-5.12.16/weightLogInfo_merged.csv")

##### Checking the data
skim_without_charts(WeightLog)

n_distinct(WeightLog$Id)

###### Only 8 unique participant IDs for weight log, indicates that not all 
###### participants use this feature.

##### Cleaning the Data
clean.wl <- clean_names(WeightLog)

clean.wl$id = as.character(clean.wl$id)

clean.wl0 <- separate(clean.wl,
                      date,
                      c("date", "time"), 
                      sep = " ",
                      extra = "merge")
clean.wl1 <- subset(clean.wl0, select = -c(3))

clean.wl1$date <- as.Date(clean.wl1$date, format="%m/%d/%Y") 

clean.wl2 <- distinct(clean.wl1)

view(clean.wl2)


  
### Analyzing the Data
#### Check Number of Observations for Each Dataframe
nrow(clean.da2)
nrow(clean.ha2)
nrow(clean.hr2)
nrow(clean.wl2)

view(clean.da2)

#### Summary Statistics for Each Dataframe
clean.da2 %>%  
  select(total_steps,
         sedentary_minutes,
         calories, total_minutes_asleep, 
         total_time_in_bed) %>%
  summary()

clean.ha2 %>%
  select(step_total,
         calories) %>%
  summary()

clean.hr2 %>%
  select(value) %>%
  summary()

clean.wl2 %>%
  select(bmi) %>%
  summary()

###### Shows 1 day BMI outlier. Investigated further:

which(clean.wl2$bmi > 47)

###### There is only one entry by the participant id with the outlying value for
###### BMI. It is possible this user had a faulty device and switched devices
###### to continue participation in the study. However, there is no way of 
###### knowing if the participant is associated with another id. 

### Visualizing the Data
#### Daily Activity
##### Relationship Between Steps and Sedentary Minutes
ggplot(data=clean.da2, aes(x=total_steps, y=sedentary_minutes)) + 
  geom_point(color = "#0C695D") +
  geom_smooth(method=lm, color = "black") + labs(x = "Total Steps", 
    y = "Sedentary Minutes") +
  theme(plot.background = element_rect(fill = "#E0F2F2"),
        axis.title = element_text(color = "#404040"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "lightgrey")) 

###### No significant findings.

##### Relationship Between Day of the Week and Average Steps
day <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", 
         "Saturday", "Sunday")
day.f <- factor(day, levels = day)
levels(day.f)
clean.da2$weekday <- weekdays(clean.da2$activity_date) %>%
  factor(day, levels = day.f)
wkday.avg.steps <- aggregate(total_steps ~ weekday, data = clean.da2, mean)
ggplot(wkday.avg.steps, aes(weekday, total_steps)) + 
  geom_col(color = "#0c695d", fill = "#0c695d") +
  labs(x = "Day of the Week", y = "Average Steps per Day") +
  theme(plot.background = element_rect(fill = "#E0F2F2"),
      axis.title = element_text(color = "#404040"),
      panel.background = element_rect(fill = "white"),
      panel.grid.major = element_line(color = "lightgrey"),
      axis.text.x = element_text(angle = 45))

###### No significant findings; 
###### Saturday is the most active day (avg 9,871 steps) and Sunday is the 
###### least (avg 7,298 steps). Overall avg is 8,515 steps per day. 


##### Number of Entries by Date
num.entries <- clean.da2 %>%
  group_by(activity_date)%>%
  summarize(n=n())
ggplot(num.entries, aes(x = activity_date, y= n)) + geom_col(color = "#FFFFFF", 
    fill = "#0c695d") + scale_y_continuous(breaks= pretty_breaks()) +
  geom_smooth(color = "black") + labs(x = "Date", y = "Number of Entries", 
    title = "Number of Entries per Day") +
  theme(plot.background = element_rect(fill = "#E0F2F2"),
        axis.title = element_text(color = "#404040"),
        title = element_text(color = "#404040"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "lightgrey"),
        axis.text.x = element_text(angle = 45))

###### Slight decline in daily entries as time goes on. Decline becomes most 
###### noticeable during the 4th and 5th week. However, the decline is not
###### very significant. More data is needed.

#### Daily Sleep
##### Relationship Between Day of the Week and Minutes Asleep
clean.da2$weekday <- weekdays(clean.da2$activity_date) %>%
  factor(weekday, levels = day.f)
wkday.avg.sleep <- aggregate(total_minutes_asleep ~ weekday, data = 
                               clean.da2, mean)
ggplot(wkday.avg.sleep, aes(weekday, total_minutes_asleep)) + 
  geom_col(color = "#FFFFFF", fill = "#0c695d") +
  labs(x = "Day of the Week", y = "Total Minutes Asleep", 
       title = "Minutes of Sleep per Weekday") +
  theme(plot.background = element_rect(fill = "#E0F2F2"),
        axis.title = element_text(color = "#404040"),
        title = element_text(color = "#404040"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "lightgrey"),
        axis.text.x = element_text(angle = 45))

#### Hourly Activity
##### Relationship Between Time of Day and Average Steps
avg.steps <- aggregate(step_total ~ time, data = clean.ha2, mean)
ggplot(data=avg.steps, aes(x = time, y= step_total)) + 
  geom_col(color = "#FFFFFF", fill = "#0c695d") +
  labs(x = "Time", y = "Average Steps", title = "Average Steps per Hour") +
  theme(plot.background = element_rect(fill = "#E0F2F2"),
        axis.title = element_text(color = "#404040"),
        title = element_text(color = "#404040"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "lightgrey"),
        axis.text.x = element_text(angle = 45))