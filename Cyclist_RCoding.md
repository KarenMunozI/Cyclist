### Cyclistic_Exercise_Full_Year_Analysis ###
---
title: "Cyclist Project"
author: "Karen Muñoz"
date: "2023-11-01"
output: html_document
---
## Introduction

Hello, everyone!
I wanted to introduce myself before showing you my work.
My name is Karen, I am a Chilean translator turned data analyst who is currently living in Germany.
I decided to look for a new career path at the ripe age of 28 haha in the year 2023, I stumbled across Coursera and started my learning journey that has led me to this moment.
Not really knowing what I was getting myself into, I started learning SEO online, then I started another course on Marketing and E-Commerce and finally I got a scholarship to study Data Analysis and Project Management, both of which are Google Certificates.
This is the Capstone Project of the Data Analysis Online Course.
The material and questions were given to me to prepare a Case Study and showcase my knowledge.
For those of you who are not in the data science road, there are six steps for data analysis: 
* Ask * Prepare * Process * Analyse * Share * Act

So, let's start:
### The business goal: Design marketing strategies aimed at converting casual riders into annual members. 
In order to do that, however, the marketing analyst team needs to better understand how annual members and casual riders differ, why casual
riders would buy a membership, and how digital media could affect their marketing tactics.


# # # # # # # # # # # # # # # # # # # # # # # 
#Install required packages
#tidyverse for data import and wrangling
#libridate for date functions
#ggplot for visualization
# # # # # # # # # # # # # # # # # # # # # # #  

library(tidyverse)  #helps wrangle data
library(lubridate)  #helps wrangle date attributes
library(ggplot2)  #helps visualize data


#=====================
# STEP 1: COLLECT DATA
#=====================
#Upload Divvy datasets (csv files) here

```{r setting up the environment, message=FALSE, warning=FALSE}
install.packages("tidyverse")
library(tidyverse)
install.packages("readr")
library(readr) 
Divvy_Trips_2020_Q1 <- read_csv("Divvy_Trips_2020_Q1.csv")
View(Divvy_Trips_2020_Q1)

```


#====================================================
# STEP 2: WRANGLE DATA 
#====================================================
I want to emphasize that I will be working ONLY with the first quarter of 2020 due to technical difficulties (R would crash if I upload more files because I am using the free version). This means, the data only covers January, February, and March. Some analysis, such as seasonal, will be irrelevant but will be performed for the purpose of this exercise. 
 Inspect the dataframe and look for incongruencies


#======================================================
# STEP 3: CLEAN UP AND ADD DATA TO PREPARE FOR ANALYSIS
#======================================================
#Inspect the new dataframe that has been created

```{r cleaning, message=TRUE, warning=TRUE}
install.packages("janitor")
library(janitor)
#I will review the column names to check if they need to be renamed using clean_names() or if they are ready to be worked with. In this case, they are all lowercase, a reasonable length and don't have any  unnecessary additional characters. They are easy to read and cite, so we won't be doing any changes.

colnames(Divvy_Trips_2020_Q1) 	#to see the column names
head(Divvy_Trips_2020_Q1)		#to see the first six rows
str(Divvy_Trips_2020_Q1)		#to see a list of columns and data types
summary(Divvy_Trips_2020_Q1)	#for statistical data
```
I will then turn the strings into time strings to later calculate how long each ride took.

```{r transforming into timestamps}
install.packages("lubridate")
library(lubridate)
Divvy_Trips_2020_Q1$started_at <- ymd_hms(Divvy_Trips_2020_Q1$started_at)
Divvy_Trips_2020_Q1$ended_at <- ymd_hms(Divvy_Trips_2020_Q1$ended_at)

```

#Remove "bad" data
I noticed there are negative time calculations. This is due to faulty data probably either due to bike maintenance or error by the user. Since this data is irrelevant, I will apply a filter to leave any trip duration under three minutes out of the equation. 
In a normal environment, I would ask what to do. 


```{r duration column, message=FALSE}

Divvy_Trips_2020_Q1$duration <- difftime(Divvy_Trips_2020_Q1$ended_at, Divvy_Trips_2020_Q1$started_at, units = "mins")
```

#We will create a new version of the dataframe (“clean_Divvy” from now on) since data is being removed:

```{r cleaning irrelevant data}
install.packages("dplyr")
library(dplyr)
clean_Divvy <- Divvy_Trips_2020_Q1 %>%
  filter(duration > 3)
```


Now that we have only relevant information about the trips, let's dive into the data! We are only going to use the new dataframe.

#=====================================
# STEP 4: CONDUCT DESCRIPTIVE ANALYSIS
#=====================================

#Let’s see how many rides were performed by each membership type: 

```{r membership type count}
clean_Divvy %>%
  group_by(member_casual) %>%
  summarise(count = n())
```
There are more annual members than casual riders for this service. 


#Let's see how they differ on a daily basis: 

```{r daily use by membership}
library(dplyr)
library(lubridate)

daily_use <- clean_Divvy %>%
  mutate(date = as.Date(started_at)) %>%
  group_by(date, member_casual) %>%   #to see each day summary by membership type
  summarise(count = n())
print(mean_duration_by_day)

#and now let's plot

ggplot(daily_use, aes(x = date, y = count)) +
  geom_line() + 
  labs(title = "Use by Date",
       x = "Date",
       y = "No. Rides") +
  theme_minimal() +
facet_wrap(~member_casual) #add if we want to separate both membership types


```


#Descriptive analysis on ride duration
#Let's analyse how long each membership type uses the service

```{r mean time of duration by membership}
library(dplyr)

clean_Divvy %>%
  group_by(member_casual) %>%
  summarise(mean_duration = mean(duration))
  
```
However, casual members have a higher average length. 
That is unusual Let's see why:

#max duration by membership type 

```{r max time of duration by membership}
library(dplyr)

clean_Divvy %>%
  group_by(member_casual) %>%
  summarise(mean_duration = max(duration))
  
```

# Compare members and casual users
There is a considerably higher number of annual members than of casual members.
Casual members have a higher average length of ride with a max of 156450.40 min. 
The longest ride by a casual member was over 108 days! 
That is very unusual, but as we can see below, the three longest rides for casual riders are all over 80 days long. We will be keeping the data since I don't have further information about these cases.

#See the average ride time each day for both membership types

```{r duration by day}
library(dplyr)
library(lubridate)

mean_duration_by_day <- clean_Divvy %>%
  mutate(date = as.Date(started_at)) %>%
  group_by(date, member_casual) %>%   #to see each day summary by membership type
  summarize(mean_duration = mean(duration, na.rm = TRUE))

print(mean_duration_by_day)

```
Visualization: ride duration by day
Let’s plot ride duration by day: 


```{r plotting duration by day}
install.packages("ggplot2")
library(ggplot2)


ggplot(mean_duration_by_day, aes(x = date, y = mean_duration )) +
  geom_line() + 
  labs(title = "Mean Duration by Date",
       x = "Date",
       y = "Mean Duration") +
  theme_minimal() +
facet_wrap(~member_casual) #add if we want to separate both membership types

```

#Now let' see what type of bike is preferred by each membership type: 

```{r preferred bike by membership}

library(dplyr)
clean_Divvy %>%
  group_by(member_casual) %>%
    group_by(rideable_type) %>%
  summarise(count = n())

# the data base only includes docked_bike

```
#Let’s see which stations are preferred:
#I can create a new data frame with the frequency of each start station for each member type. A new temporary dataframe is necessary. This information would be helpful for advertising targeting.
Casual members:
```{r casual members dataframe}
#Casual membership

library(dplyr)
casual_members <- clean_Divvy  %>%
  filter(member_casual== "casual")
  
#Let's discover what are the most used start and end stations
start_stations_casual <- casual_members %>%
  group_by(start_station_name) %>%
  summarise(count = n())
start_stations_casual %>%
  arrange(-count)

end_stations_casual  <- casual_members %>%
  group_by(end_station_name) %>%
  summarise(count = n())
end_stations_casual %>%
  arrange(-count)
```

The most popular start and end stations for this membership type are located near parks or touristic attractions. We could conclude that casual members use the service for leisure activities. 


And now the same process for annual memberships:

```{r member members dataframe}

#Annual memberships
library(dplyr)
member_members <- clean_Divvy  %>%
  filter(member_casual== "member")
  
#let's discover what are the most used start and end stations
start_stations_member <- member_members %>%
  group_by(start_station_name) %>%
  summarise(count = n())
start_stations_member %>%
  arrange(-count)


end_stations_member  <- member_members %>%
  group_by(end_station_name) %>%
  summarise(count = n())
end_stations_member %>%
  arrange(-count)
```

The stations most used by annual members (start and end) were located downtown near train and subway stations. The bikes are used presumably for commuting. 

#Now let's see what time of the day each membership is more active

```{r active time by membership type}
#A new column with the time of the day would be very useful to order them and create visualizations
library(lubridate)

clean_Divvy$started_at <- ymd_hms(clean_Divvy$started_at)
#let's separate the started_at column to see what time there is a higher demand for bikes by annual members. 

library(dplyr)
time_of_day <- clean_Divvy %>%
  mutate(
    hour = hour(ymd_hms(started_at))
  ) %>%
  mutate(
    time_of_day = case_when(
      hour >= 5 & hour < 12 ~ "Morning",
      hour >= 12 & hour < 17 ~ "Afternoon",
      hour >= 17 & hour < 21 ~ "Evening",
      TRUE ~ "Night"
    )
  )


```
Visualization: time of day by membership type
And let's plot all our data:

```{r ggplotting time for both}
install.packages("ggplot2")
library(ggplot2)
library(dplyr)

ggplot(data= time_of_day) + 
  geom_bar(mapping = aes(x= hour, fill= time_of_day))
           
```
Interesting!
There is a higher demand at 8 am and at 4-5 pm. 

Let's break it down by membership type:

```{r plotting time facet by member} 
library(ggplot2)
library(dplyr)

ggplot(data= time_of_day) + 
  geom_bar(mapping = aes(x= hour, fill= time_of_day))+
  facet_wrap(~member_casual)
           
```
It draws my attention how casual riders mostly use the service past noon while the annual members use the bikes during morning (7-8) and evening (4-6). This could back-up our earlier conclusion that annual members use the bikes for commuting while casual members use them for leisure.

#Let's see what day of the week the users are most active by membership type:

```{r weekday by membership} 
library(lubridate)
clean_Divvy$weekday_name <- wday(clean_Divvy$started_at, label = TRUE, abbr = FALSE) 

library(ggplot2)
library(dplyr)

ggplot(data= clean_Divvy) + 
  geom_bar(mapping = aes(x= weekday_name, fill= weekday_name))+
  facet_wrap(~member_casual) +
  theme(axis.text.x = element_text(angle=45))
```

Clearly, the casual riders use the service most often on Sunday, which confirms our hypothesis that they use the bikes for "leisure", while annual members use them the most during the week presumambly to "commute". 

We can also analyse our data by seasonal trends: 

```{r analysis by season}
library(dplyr)
library(lubridate)

#A new column with the seasons would be very useful to order them and create visualizations

clean_Divvy$started_at <- ymd_hms(clean_Divvy$started_at)
#let's separate the started_at column t


seasons <- clean_Divvy %>%
  mutate(
    seasons = month(ymd_hms(started_at))
  ) %>%
  mutate(
    seasons = case_when(
      month(ymd_hms(started_at)) %in%  3:5 ~ "Spring",
      month(ymd_hms(started_at)) %in%  6:8 ~ "Summer",
      month(ymd_hms(started_at)) %in%  9:11 ~ "Autumn",
      month(ymd_hms(started_at)) %in%  c(12, 1, 2) ~ "Winter",
      TRUE ~ NA_character_  # For dates that don't fall into any category
    )
  )
```
Visualization: seasonal trends
And now ploting: 
```{r plotting by season}
library(ggplot2)

ggplot(data= seasons) + 
  geom_bar(mapping = aes(x= seasons, fill= seasons))+
  facet_wrap(~member_casual) +
  theme(axis.text.x = element_text(angle=45))
```




 
The data only includes trips from the first quarter of 2021 (January to March), so this information is not so relevant. However, for the purpose of this exercise, I also performed this operation. 





#=================================================
# STEP 5: EXPORT SUMMARY FILE FOR FURTHER ANALYSIS
#=================================================




