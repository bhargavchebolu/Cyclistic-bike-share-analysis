# Cyclistic Bike Share Analysis

This case study is a capstone project of Google Data Analytics Professional Certificate. The details such as scenario, about the company and the steps I followed to solve this case study are discussed below.  

#### Scenario

A junior data analyst working in the marketing analyst team at Cyclistic, a bike-share company in Chicago. The director of marketing believes the company’s future success depends on maximizing the number of annual memberships. Therefore, your team wants to understand how casual riders and annual members use Cyclistic bikes differently. From these insights, your team will design a new marketing strategy to convert casual riders into annual members. But first, Cyclistic executives must approve your recommendations, so they must be backed up with compelling data insights and professional data visualizations.

#### Characters and teams

● Cyclistic: A bike-share program that features more than 5,800 bicycles and 600 docking stations. Cyclistic sets itself apart by also offering reclining bikes, hand tricycles, and cargo bikes, making bike-share more inclusive to people with disabilities and riders who can’t use a standard two-wheeled bike. The majority of riders opt for traditional bikes; about 8% of riders use the assistive options. Cyclistic users are more likely to ride for leisure, but about 30% use them to commute to work each day.

● Lily Moreno: The director of marketing and your manager. Moreno is responsible for the development of campaigns and initiatives to promote the bike-share program. These may include email, social media, and other channels.

● Cyclistic marketing analytics team: A team of data analysts who are responsible for collecting, analyzing, and reporting data that helps guide Cyclistic marketing strategy. You joined this team six months ago and have been busy learning about Cyclistic’s mission and business goals — as well as how you, as a junior data analyst, can help Cyclistic achieve them.

● Cyclistic executive team: The notoriously detail-oriented executive team will decide whether to approve the recommended marketing program.

#### About the company 

In 2016, Cyclistic launched a successful bike-share offering. Since then, the program has grown to a fleet of 5,824 bicycles that are geotracked and locked into a network of 692 stations across Chicago. The bikes can be unlocked from one station and returned to any other station in the system anytime. 
Until now, Cyclistic’s marketing strategy relied on building general awareness and appealing to broad consumer segments. One approach that helped make these things possible was the flexibility of its pricing plans: single-ride passes, full-day passes, and annual memberships. Customers who purchase single-ride or full-day passes are referred to as casual riders. Customers who purchase annual memberships are Cyclistic members. 
Cyclistic’s finance analysts have concluded that annual members are much more profitable than casual riders. Although the pricing flexibility helps Cyclistic attract more customers, Moreno believes that maximizing the number of annual members will be key to future growth. Rather than creating a marketing campaign that targets all-new customers, Moreno believes there is a very good chance to convert casual riders into members. She notes that casual riders are already aware of the Cyclistic program and have chosen Cyclistic for their mobility needs. 

Moreno has set a clear goal: Design marketing strategies aimed at converting casual riders into annual members. In order to do that, however, the marketing analyst team needs to better understand how annual members and casual riders differ, why casual riders would buy a membership, and how digital media could affect their marketing tactics. Moreno and her team are interested in analyzing the Cyclistic historical bike trip data to identify trends.

The question to answer: How do annual members and casual riders use Cyclistic bikes differently?


Followed the steps of the data analysis process: ask, prepare, process, analyze, share, and act to help the company to change the casual riders to annual members by giving top three recommendations. 

To start the analysis in R programming, some packages need to be installed.
```
install.packages("tidyverse") #tidyverse for data import and wrangling
install.packages("lubridate") #lubridate for date functions
install.packages("ggplot2") #ggplot for visualization

library(tidyverse)  #helps wrangle data
library(lubridate)  #helps wrangle date attributes
library(ggplot2)  #helps visualize data
setwd(".../Case Study 1/csv") #sets your working directory to simplify calls to data
getwd() #displays your working directory
```

### STEP 1: Collecting data from the csv files

##### Upload Divvy datasets (csv files) here
```
Q1_2022 <- read_csv("Divvy_Trips_2022_Q1.csv") # First three months have been comnibed into one file in MS Excel 
Apr_2022 <- read_csv("202204-divvy-tripdata.csv")
May_2022 <- read_csv("202205-divvy-tripdata.csv")
Jun_2022 <- read_csv("202206-divvy-tripdata.csv")
Jul_2022 <- read_csv("202207-divvy-tripdata.csv")
Aug_2022 <- read_csv("202208-divvy-tripdata.csv")
Sep_2022 <- read_csv("202209-divvy-tripdata.csv")
Oct_2022 <- read_csv("202210-divvy-tripdata.csv")
Nov_2022 <- read_csv("202211-divvy-tripdata.csv")
Dec_2022 <- read_csv("202212-divvy-tripdata.csv")
```

### STEP 2: Wrangling data and combining into a single file. In MS Excel, first 3 months data is combined as first quarter. Remaining files are left individually as they are difficult to combine into quarters because of large data.

##### Compare column names each of the filesWhile the names don't have to be in the same order, they do need to match perfectly before we can use a command to join them into one file
```
colnames(Q1_2022)
colnames(Apr_2022)
colnames(May_2022)
colnames(Jun_2022)
colnames(Jul_2022)
colnames(Aug_2022)
colnames(Sep_2022)
colnames(Oct_2022)
colnames(Nov_2022)
colnames(Dec_2022)
```

##### Inspect the dataframes and look for incongruencies
```
str(Q1_2022)
str(Apr_2022)
str(May_2022)
str(Jun_2022)
str(Jul_2022)
str(Aug_2022)
str(Sep_2022)
str(Oct_2022)
str(Nov_2022)
str(Dec_2022)
```

##### Stack individual quarter's data frames into one big data frame
```
all_trips <- bind_rows(Q1_2022, Apr_2022, May_2022, Jun_2022, Jul_2022, Aug_2022, Sep_2022, Oct_2022, Nov_2022, Dec_2022)
```
### STEP 3: Cleaning and adding the data to prepare for analysis. New columns were added such as date, month, day, and year to analyze data based on them. Bad data is removed to ensure the correct results. Here the ride length less than zero seconds is removed and also the null values from the dataset. 

##### Inspect the new table that has been created
```
colnames(all_trips)  #List of column names
nrow(all_trips)  #How many rows are in data frame?
dim(all_trips)  #Dimensions of the data frame?
head(all_trips)  #See the first 6 rows of data frame.  Also tail(all_trips)
str(all_trips)  #See list of columns and data types (numeric, character, etc)
summary(all_trips)  #Statistical summary of data. Mainly for numerics
```
##### Begin by seeing how many observations fall under each usertype
```
table(all_trips$member_casual)
```
##### Add columns that list the date, month, day, and year of each ride
##### This will allow us to aggregate ride data for each month, day, or year ... before completing these operations we could only aggregate at the ride level
```
all_trips_clean <- all_trips %>% 
  select(started_at) %>% 
  mutate(started_at = dmy_hm(started_at)) ## Reformat to datetime
all_trips$date <- all_trips_clean
all_trips$date <- as.Date(all_trips$date$started_at) #The default format is yyyy-mm-dd
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")
all_trips$ride_length2 <- as.difftime(all_trips$ride_length, units = "mins") #"difftime" class
all_trips$ride_length <- as.numeric(all_trips$ride_length2, units = "secs")
```
##### Remove "bad" data
##### The dataframe includes a few hundred entries when bikes were taken out of docks and checked for quality by Divvy or ride_length was negative
##### We will create a new version of the dataframe (v2) since data is being removed
```
all_trips_v2 <- all_trips[!(all_trips$ride_length<0),]
glimpse(all_trips_v2) 
all_trips_v2 <- na.omit(all_trips_v2) # Removes null values
```
### STEP 4: Conducting descriptive analysis. Performed descriptive analysis on ride length and compared with members and casual riders. Then carried out the visualizations to find the how members and casual riders use the bikes differently. 

##### Descriptive analysis on ride_length (all figures in seconds)
```
mean(all_trips_v2$ride_length) #straight average (total ride length / rides)
median(all_trips_v2$ride_length) #midpoint number in the ascending array of ride lengths
max(all_trips_v2$ride_length) #longest ride
min(all_trips_v2$ride_length) #shortest ride
```
##### You can condense the four lines above to one line using summary() on the specific attribute
```
summary(all_trips_v2$ride_length)
```
##### Compare members and casual users
```
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)
```
##### See the average ride time by each day for members vs casual users
```
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)
```
##### Notice that the days of the week are out of order. Let's fix that.
```
all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
```
##### Now, let's run the average ride time by each day for members vs casual users

##### analyze ridership data by type and weekday
```
all_trips_v2 %>% 
  mutate(day_of_week = wday(started_at, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(member_casual, day_of_week) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n()							#calculates the number of rides and average duration 
            ,average_duration = mean(ride_length)) %>% 		# calculates the average duration
  arrange(member_casual, day_of_week)		
```
##### Let's visualize the number of rides by rider type
```
all_trips_v2 %>% 
  mutate(day_of_week = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, day_of_week) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, day_of_week)  %>% 
  ggplot(aes(x = day_of_week, y = number_of_rides, fill = member_casual)) +
  geom_col(width=0.5, position = position_dodge(width=0.5)) + labs(title ="Number of rides by rider type")      
```  
  
![num-of-rides](https://user-images.githubusercontent.com/65249485/214398336-b02382db-5ce5-40c8-b37d-ab92f45d3ca5.jpeg)
                                                                                                           


##### Let's create a visualization for average duration
```
all_trips_v2 %>% 
  mutate(day_of_week = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, day_of_week) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, day_of_week)  %>% 
  ggplot(aes(x = day_of_week, y = average_duration, fill = member_casual)) +
  geom_col(width=0.5, position = position_dodge(width=0.5)) + labs(title ="Average duration taken by members and casual riders")
```

![avg-duration](https://user-images.githubusercontent.com/65249485/214400397-a71fa685-c1d1-42f5-a2ea-0f21d992af44.jpeg)



##### Let's create a visualization for total riders taken by members and casual riders by month
```
all_trips_v2 %>% 
  group_by(member_casual, month) %>% 
  summarise(number_of_rides = n(), .groups = "drop") %>% 
  arrange(member_casual, month)  %>% 
  ggplot(aes(x = month, y = number_of_rides, fill = member_casual)) +
  geom_col(width=0.5, position = position_dodge(width=0.5)) + labs(title ="Total rides taken members and casual riders by month")
```


![tot-rides-month](https://user-images.githubusercontent.com/65249485/214401046-f82971c2-ee46-4565-a9e9-43abdc16779d.jpeg)





##### From the visualizations the key findings are as follows:

1.	Every day casual riders spent more than 20 minutes on bikes while members spent less than 15 minutes.
2.	Members takes a greater number of rides than the casual riders every day. 
3.	Total number of rides are more in June, July and August by both casual riders and the members.

##### Top three recommendations based on the analysis:

1.	Giving special discount on annual membership for casual riders who spent more than 20 minutes on the bikes. 
2.	Promoting the benefits of annual membership in June, July, and August. 
3.	Providing comfortable bikes for annual members and normal bikes for casual riders. 
