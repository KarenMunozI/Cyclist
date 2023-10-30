# Cyclist
Data Analysis Google Certification Capstone Project
---
title: "Cyclist_CaseStudy"
author: "Karen Muñoz"
date: "2023-10-30"
output: html_document: default

---

```{r setup}
knitr::opts_chunk$set(echo = TRUE)
```

## Introduction

Hello, everyone!
I wanted to introduce myself before showing you my work.
My name is Karen, I am a Chilean translator turned data analyst who is currently living in Germany.
I decided to look for a new career path at the ripe age of 28 haha in the year 2023, I stumbled across Coursera and started my learning journey that has led me to this moment.
Not really knowing what I was getting myself into, I started learning SEO, then I started a course on Marketing and E-Commerce and finally I got an scholarship to study Data Analysis and Project Management, both of which are Google Certificates.
This is the Capstone Project of the Data Analysis Online Course.
The material and questions were given to me to prepare a Case Study and showcase my knowledge.
For those of you who are not in the data science road, there are six steps for data analysis: ● Ask ● Prepare ● Process ● Analyse ● Share ● Act

So, let's start:

## Ask

Guiding questions ● What is the problem you are trying to solve?
I will try to produce a report with the following deliverables: 1.
A clear statement of the business task 2.
A description of all data sources used 3.
Documentation of any cleaning or manipulation of data 4.
A summary of your analysis 5.
Supporting visualizations and key findings 6.
Top three recommendations based on my analysis to answer the question *"How do annual members and casual riders use Cyclistic bikes differently?"*.
I will calculate the the number of trips per membership type, the average length of trip per membership type, the type of bike preferred by membership type, and the preferred start and end stations.

● How can your insights drive business decisions?
A data anaylist's insights are well-researched and based on data-analysis.
The insights influence business decision to a great extent since they lead to recommendations.

## Prepare

Guiding questions ● Where is your data located?
The .cvs data is located in zip files in an index made available by Motivate International Inc. ● How is the data organized?
The data is organized yearly and quarterly.
● Are there issues with bias or credibility in this data?
Does your data ROCCC?
ROCCC which stands for Reliable, Original, Comprehensive, Current, and Cited.
The It was made available by Motivate International Inc. under [this licence](https://divvybikes.com/data-license-agreement).

Reliable --- Yes, since there is a high number of respondents.
Original --- Yes, it is provided by the company itself.
It is first-party data.
Comprehensive --- Yes, it includes duration, start and end stations.
All the information needed to answer the business question.
Current --- Yes, the data is current and updated on a monthly schedule.
However, I will be using a data set from the first quarter of 2021, since it is the last data set available at a bigger scale.
The other file availables are monthly.
Cited --- The company collects the data itself, but I am unsure of the process, hence unknown.
● How are you addressing licensing, privacy, security, and accessibility?
The licence has already been cited.
The data was anonymized and therefore private.
The data is stored in the hard drive of my computer The data is open-source, so it is available for free downloading.
● How did you verify the data's integrity?
I checked the website and the licence.
● How does it help you answer your question?
It gives me information about the different type of riders.
● Are there any problems with the data?
There is a lot of information and the files are separate by month, which doesn't allow for a through case study.
Therefore I will be using data that is not current (first quarter of 2020) for this Case Study.

First, I will set up the environment:

```{r setting up the environment, message=TRUE, warning=TRUE}
install.packages("tidyverse")
library(tidyverse)
install.packages("readr")
```

and load the dataset:

```{r loading dataset, warning=TRUE}
library(readr)
Divvy_Trips_2020_Q1 <- read_csv("Capstone_Cyclistic/Divvy_Trips_2020_Q1.csv")

```

## Process

Guiding questions ● What tools are you choosing and why?
I will be working with R since it manages big quantities of data and allows for visualizations.
It also simplifies creating a notebook and outlining my though process.
● Have you ensured your data's integrity?
The data is backed up on the hard drive of my computer and the cloud.
I am the only one with access to the data, meanign I am the only person who can edit it.
I will also log all the steps for cleaning so that the process is transparent.

● What steps have you taken to ensure that your data is clean?
I will fix or remove incorrect, corrupted, incorrectly formatted, duplicate or incomplete data from the data set.
● How can you verify that your data is clean and ready to analyze?
I will use functions in R such as *clean_names()* or *glimpse()*.\
● Have you documented your cleaning process so you can review and share those results?
I will document the process using R markdown.

```{r cleaning, message=TRUE, warning=TRUE}
install.packages("janitor")
library(janitor)
# I will review the column names to check if they need to be renamed using clean_names() or if they are ready to be worked with. In this case, they are all lowercase, a reasonable length and don't have any  unnecessary additional characters. They are easy to read and cite, so we won't be doing any changes.

colnames(Divvy_Trips_2020_Q1)
```

I will be dividing the start_at and end_at columns so that the date and time are separate to better measure the length of each ride.

```{r separate columns}
library(tidyr)
Trips_1 <- separate(Divvy_Trips_2020_Q1, col = started_at, into = c("start_date", "start_time"), sep = " ", remove = TRUE)  

# Please note that I am not sure how to run the separate function with two columns at the same time, so I had to run it twice and therefore I ended up with more databases than I needed. 
Trips <- separate(Trips_1, col = ended_at, into = c("end_date", "end_time"), sep = " ",remove = TRUE)

# I also removed the original columns but didn't lose any information.
remove("Trips_1")
```

I will then turn the strings into time strings to later calculate how long each ride took.

```{r transforming into timestamps}
install.packages("lubridate")
library(lubridate)
library(hms)
Trips$start_time <-  as_hms(Trips$start_time)
Trips$end_time <- as_hms(Trips$end_time)

```

To calculate the duration I will crate a new column and measure the difference in minutes:

```{r duration column, message=FALSE}

Trips$duration <- difftime(Trips$end_time, Trips$start_time, units = "mins")
```

I will now perform calculations based on the type of members the users are:

```{r mean time of duration by membership}
library(dplyr)
Trips %>%
  group_by(member_casual) %>%
  summarise(mean_duration = mean(duration))


```

As seen in the result, casual members use the bikes for longer time than the annual members.

```{r preferred bike by membership}
library(dplyr)
Trips %>%
  group_by(member_casual) %>%
  count(Trips$rideable_type == "docked_bike")

# the data base only includes docked_bike

```

I can create a new data frame with the frequency of each start station for each member type.
This information would be helpful for advertising locations.

```{r casual members dataframe}
library(dplyr)
casual_members <- Trips  %>%
  filter(member_casual== "casual") %>%
  count(casual_members$start_station_name)
```

```{r member member dataframe}

members_members <- Trips  %>%
  filter(member_casual== "member")
  count(members_members, start_station_name)
```

```{r }
count(members_members, start_station_name)

```
