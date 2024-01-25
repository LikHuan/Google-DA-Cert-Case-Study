setwd("C:/Users/User/Desktop/R (Google)/Case Study 2/Data")
library(tidyverse)
library(dplyr)
library(ggplot2)
library(skimr)
library(janitor)


activity= read.csv('Activity.csv')
sleep= read.csv('Sleep.csv')
weight= read.csv('Weight.csv')
hourly_step= read.csv('Hourly_Steps.csv')
colnames(hourly_step)

#Cleaning first DF
#dropped columns that were not useful
colnames(activity)
activity=subset(activity, select= c(Id,ActivityDate,Day,TotalSteps,TotalDistance,Calories))

#Finding number of participants
sum(n_distinct(activity$Id))

#Finding duplicate
nrow(activity[duplicated(activity), ])

#changing 'day' column to actual name of the day

for (i in 1:nrow(activity)){
  if (activity[i,3] == 1){
    activity[i,3] = 'Sunday'
  }else if (activity[i,3] == 2){
    activity[i,3] = 'Monday'
  }else if (activity[i,3] == 3){
    activity[i,3] = 'Tuesday'
  }else if (activity[i,3] == 4){
    activity[i,3] = 'Wednesday'
  }else if (activity[i,3] == 5){
    activity[i,3] = 'Thursday'
  }else if (activity[i,3] == 6){
    activity[i,3] = 'Friday'
  }else if (activity[i,3] == 7){
    activity[i,3] = 'Saturday'
  }
}
#Renaming ActivityDate to Date
activity= rename(activity, Date= ActivityDate)

colnames(activity)



#Cleaning 2nd DF
colnames(sleep)
TotalMinBed= c()

#Finding duplicate
nrow(sleep[duplicated(sleep), ])
sleep = sleep%>% distinct
nrow(sleep[duplicated(sleep), ])


#changing 'day' column to actual name of the day

for (i in 1:nrow(sleep)){
  if (sleep[i,3] == 1){
    sleep[i,3] = 'Sunday'
  }else if (sleep[i,3] == 2){
    sleep[i,3] = 'Monday'
  }else if (sleep[i,3] == 3){
    sleep[i,3] = 'Tuesday'
  }else if (sleep[i,3] == 4){
    sleep[i,3] = 'Wednesday'
  }else if (sleep[i,3] == 5){
    sleep[i,3] = 'Thursday'
  }else if (sleep[i,3] == 6){
    sleep[i,3] = 'Friday'
  }else if (sleep[i,3] == 7){
    sleep[i,3] = 'Saturday'
  }
}

#Renaming SleepDay to Date
sleep= rename(sleep,Date=SleepDay)
colnames(sleep)

#Cleaning 3rd DF
#dropped columns that were not useful
colnames(weight)
weight= subset(weight, select= c(Id, Date, WeightKg, BMI))

#Finding duplicate
nrow(weight[duplicated(weight), ])


#Cleaning 4th DF

#changing 'Day' column to actual name of the day

for (i in 1:nrow(hourly_step)){
  if (hourly_step[i,3] == 1){
    hourly_step[i,3] = 'Sunday'
  }else if (hourly_step[i,3] == 2){
    hourly_step[i,3] = 'Monday'
  }else if (hourly_step[i,3] == 3){
    hourly_step[i,3] = 'Tuesday'
  }else if (hourly_step[i,3] == 4){
    hourly_step[i,3] = 'Wednesday'
  }else if (hourly_step[i,3] == 5){
    hourly_step[i,3] = 'Thursday'
  }else if (hourly_step[i,3] == 6){
    hourly_step[i,3] = 'Friday'
  }else if (hourly_step[i,3] == 7){
    hourly_step[i,3] = 'Saturday'
  }
}

#Renaming Activity Hour to Date
hourly_step= rename(hourly_step,Date=ActivityHour)
colnames(hourly_step)

#Finding duplicate
nrow(hourly_step[duplicated(hourly_step), ])


#Start analysis

#Steps taken and date
ggplot(data=activity)+
  geom_bar(mapping= aes(x= Date, y= TotalSteps), stat='identity')+
  labs (title= "The amount of steps taken by Date")+
  theme_minimal()

#Steps taken and date(Individual)
ggplot(data=activity)+
  geom_bar(mapping= aes(x= Date, y= TotalSteps), stat='identity')+
  labs (title= "The amount of steps taken by Date")+
  facet_wrap(~Id)+
  theme_minimal()

#What day has the most steps taken
ggplot(data = activity) +
  geom_bar(mapping = aes(x = Day, y = TotalSteps, fill = Day), 
           stat = "identity")+
  labs(title= "Steps taken each day")

#What day has the most steps taken(Individual)
ggplot(data = activity) +
  geom_bar(mapping = aes(x = Day, y = TotalSteps, fill = Day), 
           stat = "identity")+
  facet_wrap(~Id)+
  labs(title= "Steps taken each day")

#Calories burnt against Date
ggplot(data = activity) +
  geom_point(mapping = aes(x = Date, y = Calories), color = 'blue') +
  labs(title = "Calories burnt against Date", ylab= 'Calories Burnt')

#Calories burnt against Date (Individual)
ggplot(data = activity) +
  geom_point(mapping = aes(x = Date, y = Calories), color = 'blue') +
  geom_line(mapping = aes(x = Date, y = Calories, group = Id), color = 'red') +
  labs(title = "Calories burnt against Date", ylab = 'Calories Burnt') +
  facet_wrap(~Id)

#Calories and steps taken
corr= cor(activity$Calories,activity$TotalSteps)
corr=round(corr,4)

ggplot(data= activity)+
  geom_line(mapping= aes(x= Calories, y= TotalSteps), color='orange')+
  annotate("text", x=4500, y=10000, label= paste("Correlation=",corr), color='black')+
  labs(title= "TotalSteps against Calories")

#Calories and steps taken (Individual)
ggplot(data= activity)+
  geom_line(mapping= aes(x= Calories, y= TotalSteps), color='orange')+
  labs(title= "TotalSteps against Calories")+
  facet_wrap(~Id)


#2nd df analysis

#Sleep patterns throughout different days
ggplot(data=sleep) +
  geom_bar(mapping = aes(x=Day, y= TotalMinutesAsleep, fill=Day), stat='identity')+
  labs (title= "The amount of sleep per day")

#Sleep patterns throughout different days (Individuals)
ggplot(data=sleep) +
  geom_bar(mapping = aes(x=Day, y= TotalMinutesAsleep, fill=Day), stat='identity')+
  facet_wrap(~Id)+
  labs (title= "The amount of sleep per day")


#3rd df analysis
#Looking at changes in BMI and Weight over time
ggplot(weight) +
  geom_point(mapping = aes(x=Date, y= BMI), color= 'blue')+
  labs (title= "BMI against Date")

ggplot(weight) +
  geom_point(mapping = aes(x=Date, y= WeightKg), color= 'blue')+
  labs (title= "Weight against Date")

#Looking at changes in BMI and Weight over time (Individual)
ggplot(weight) +
  geom_point(mapping = aes(x=Date, y= BMI), color= 'red')+
  geom_line(mapping= aes(x=Date, y= BMI, group= Id), color= 'purple')+
  facet_wrap(~Id)+
  labs (title= "BMI against Date")

ggplot(weight) +
  geom_point(mapping = aes(x=Date, y= WeightKg), color= 'red')+
  geom_line(mapping= aes(x=Date, y= WeightKg, group= Id), color= 'purple')+
  facet_wrap(~Id)+
  labs (title= "Weight against Date")


#4th df analysis

#Finding the total steps according to time (hour)
ggplot(hourly_step)+
  geom_bar(mapping = aes(x= Hour, y=StepTotal, fill= Hour), stat= 'identity')

#Finding the total steps according to time (hour) [Individual]
ggplot(hourly_step)+
  geom_bar(mapping = aes(x= Hour, y=StepTotal, fill = Hour), stat= 'identity')+
  facet_wrap(~Id)

#Finding the total steps according to time (day)
ggplot(hourly_step)+
  geom_bar(mapping= aes(x= Day, y= StepTotal, fill= Day), stat= 'identity')

#Finding the total steps according to time (day) [Individual]
ggplot(hourly_step)+
  geom+bar(mapping= aes(x= Day, y= StepTotal, fill= Day), stat= 'identity')
  

#Merging the 1st(activity) and 2nd(sleep) df

merged = merge(activity,sleep, by = c('Id', 'Date'))
str(merged)
colnames(merged)

#Checking number of rows
nrow(activity)
nrow(sleep)
nrow(merged)

#Checking the calories against time spent trying to sleep
ggplot(merged)+
  geom_point(mapping= aes(x=TimeSpentTryingToSleep, y=Calories)) +
  geom_line(mapping= aes(x=TimeSpentTryingToSleep, y=Calories))

#Checking the calories against time spent trying to sleep (Individual)
ggplot(merged)+
  geom_point(mapping= aes(x=TimeSpentTryingToSleep, y=Calories)) +
  geom_line(mapping= aes(x=TimeSpentTryingToSleep, y=Calories), color= 'red') +
  labs(title= 'TimeSpentTryingToSleep against Calories Burnt')+
  facet_wrap(~Id)
