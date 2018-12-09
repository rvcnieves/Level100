######### 5.5.2 ####################
### 1. Currently dep_time and sched_dep_time are convenient to look at, but hard to compute with because they’re not really continuous numbers. Convert them to a more convenient representation of number of minutes since midnight.


library(nycflights13)
library(dplyr)
library(tidyverse)

flights_minutesfrommidnight<-mutate(flights, dep_time_since_midnight = flights$dep_time%/%100/24*1440+flights$dep_time%%100, sched_dep_time_from_midnight = flights$sched_dep_time %/% 100/24*1440 + flights$sched_dep_time %% 100)

View(flights_minutesfrommidnight)

### 2. Compare air_time with arr_time - dep_time. What do you expect to see? What do you see? What do you need to do to fix it?

### Answer : I expect and see a result that is not valid.  To fix it, need to convert arr_time and dep_time to minutes from midnight. Also, need to make sure that those trips that started before midnight and finished after midnight are identified to make sure that when we calculate the difference between arr_time and dep_time. For those cases, we need to add the time in minutes from dep_time to midnight and the time from midnight to arr_time in minutes.

flights_minutesfrommidnight<-mutate(flights, dep_time_since_midnight = flights$dep_time%/%100/24*1440+flights$dep_time%%100, sched_dep_time_from_midnight = flights$sched_dep_time %/% 100/24*1440 + flights$sched_dep_time %% 100,arr_time_since_midnight = flights$arr_time%/%100/24*1440+flights$arr_time%%100)

flights_finishing_nextday <- filter(flights_minutesfrommidnight,flights_minutesfrommidnight$dep_time > flights_minutesfrommidnight$arr_time)
View(flights_finishing_nextday)

res_2 <- (1440 - flights_finishing_nextday$dep_time_since_midnight + flights_finishing_nextday$arr_time_since_midnight)
View(res_2)

### 3. Compare dep_time, sched_dep_time, and dep_delay. How would you expect those three numbers to be related?

###  Answer, dep_delay should be the difference between dep_time and sched_dep_time in minutes, for example.  So, we need to convert them to minutes from midnight in order to calculate the difference and get the dep_delay paying attention to those flights that where scheduled before midnight and left after midnight.  For these calculations, is better to convert time to epoch to facilitate these calculations.

### 4. Find the 10 most delayed flights using a ranking function. How do you want to handle ties? Carefully read the documentation for min_rank().

delayed_top10 <- mutate(flights, top10_delayed = min_rank(desc(arr_delay)))
View(arrange(delayed_top10, top10_delayed))

### 5. What does 1:3 + 1:10 return? Why?

1:3 + 1:10

####  It returns a vector 1 2 3 and add it to another vector (1 2 3 4 5 ... 10).  So the first 3 items of each vector are added and the others are assumed to be 0 + the longer vector values for each position.


######### 5.6.7 ###########
### 2. Come up with another approach that will give you the same output as not_cancelled %>% count(dest) and not_cancelled %>% count(tailnum, wt = distance) (without using count()).

not_cancelled <- flights %>% 
  filter(!is.na(dep_delay), !is.na(arr_delay))

not_cancelled %>% count(dest)

not_cancelled %>% count(tailnum, wt = distance) 

####  ANSWER
not_cancelled %>% 
  group_by(dest) %>%
  summarise(n = n())


not_cancelled %>%
  group_by(tailnum) %>%
  summarize(n = sum(distance))



### 3. Our definition of cancelled flights (is.na(dep_delay) | is.na(arr_delay) ) is slightly suboptimal. Why? Which is the most important column?

#### ANSWER :  It is suboptimal since flights that did not departed will not have arrival related data.  Accordingly, we could get the relevant data by just using (is.na(dep_delay)).
  


### 4.  Look at the number of cancelled flights per day. Is there a pattern? Is the proportion of cancelled flights related to the average delay?

flights %>% 
  group_by(year,month,day) %>%
  summarise(cancelled = sum(is.na(dep_delay)),
            n= n(),
            mean_dep_delay = mean(dep_delay,na.rm=TRUE),
            mean_arr_delay = mean(arr_delay,na.rm=TRUE)) %>%
  ggplot(aes(y=cancelled/n)) +
  geom_point(aes(x=mean_dep_delay), colour='green', alpha=0.5) + 
               geom_point(aes(x=mean_arr_delay), colour='blue', alpha=0.5) + 
               labs(x= 'Average delay per day', y = "Cancelled flights per day")

###  Answer: There is a pattern that shows that after you get closer to average delay per day of more than 25 mins in average, there is the tendency to have more cancelled flights per day in average.  (For example, a snow storm.) 



###### 5.  Which carrier has the worst delays? Challenge: can you disentangle the effects of bad airports vs. bad carriers? Why/why not? (Hint: think about flights %>% group_by(carrier, dest) %>% summarise(n()))

###  worst delays
flights %>%
  group_by(carrier) %>%
  summarize(mean_delay = mean(arr_delay, na.rm = TRUE)) %>%
  arrange(desc(mean_delay))

### bad airports vs bad carriers
flights %>%
group_by(carrier, dest) %>%
  summarize(mean_delay = mean(arr_delay, na.rm = TRUE)) %>%
  group_by(carrier) %>%
  #summarize(mean_delay_mad = mad(mean_delay, na.rm = TRUE)) %>%
  #arrange(desc(mean_delay_mad))

summarize(mean_delay_sd = sd(mean_delay, na.rm = TRUE)) %>%
  arrange(desc(mean_delay_sd))

### Comparing the result from "bad airports vs bad carriers" and the ones from "worst delays," will help us understand which carriers have worst delays vs delays caused by bad airports.  By calculating the median absolute deviation or the standard deviation, will help us group carriers by the ones that have more constant delays (less variation) vs the ones that have higher delay variations.




### 6. What does the sort argument to count() do. When might you use it?
  
#### answer:   The sort argument to count() does sort the resulting values after applying count in descending order of n.  It could be used when using arrange() in order to discover the most common value which could result in the most important measure.



################  5.7.1  ########################


##### 1. Refer back to the lists of useful mutate and filtering functions. Describe how each operation changes when you combine it with grouping.

# ANSWER When combining mutate/filtering functions with grouping, the mutate/filtering will be applied to the groups and not to the whole dataframe.


##### 2. Which plane (tailnum) has the worst on-time record?

flights %>%
  
  filter(!is.na(arr_delay)) %>%
  mutate(non_cancelled = !is.na(arr_time),
         on_time = non_cancelled & arr_delay > 0) %>%
  group_by(tailnum) %>%  
  summarise(on_time_mean = mean(on_time)) %>%
  arrange(desc(min_rank(on_time_mean)))
  
##### 3.   What time of day should you fly if you want to avoid delays as much as possible?
  
flights %>%
  group_by(hour) %>%
  summarise(arr_delay = mean(arr_delay, na.rm = TRUE)) %>%
  arrange(arr_delay)

##### 4.   For each destination, compute the total minutes of delay. For each flight, compute the proportion of the total delay for its destination.


#### for each destination...
flights %>%
  filter(!is.na(dep_delay)) %>%
  group_by(dest) %>%
  summarise(sum_delay = sum(arr_delay, na.rm = TRUE)) %>%
  arrange(desc(sum_delay))

#### for each flight...


View(flights %>%
  filter(!is.na(arr_delay), arr_delay > 0) %>%  ###  ignore cancelled flights and on-time flights.
  group_by(dest) %>%
  mutate(arr_delay_total = sum(arr_delay),
         arr_delay_prop = arr_delay / arr_delay_total)%>%
  arrange(desc(arr_delay_prop))
  )


##### 5. Delays are typically temporally correlated: even once the problem that caused the initial delay has been resolved, later flights are delayed to allow earlier flights to leave. Using lag(), explore how the delay of a flight is related to the delay of the immediately preceding flight.

flights %>%
  arrange(year, month, day, hour, minute) %>%
  group_by(origin) %>%
  mutate(preceding_dep_delay = lag(dep_delay)) %>%
  ggplot(aes(x = preceding_dep_delay, y = dep_delay)) +
  geom_point() +
  geom_smooth()


##### 6. Look at each destination. Can you find flights that are suspiciously fast? (i.e. flights that represent a potential data entry error). Compute the air time a flight relative to the shortest flight to that destination. Which flights were most delayed in the air?

View(flights %>%
  filter(!is.na(arr_delay), arr_delay > 0) %>% 
  group_by(origin,dest) %>%
  mutate(speed = distance / air_time) %>%
  arrange(-speed,origin,dest)
)

###   shortest flight


View(flights %>%
       filter(!is.na(arr_delay), arr_delay > 0) %>% 
       group_by(origin,dest) %>%
       
       arrange(air_time,origin,dest)
)

##### 7. Find all destinations that are flown by at least two carriers. Use that information to rank the carriers.

##### 8. For each plane, count the number of flights before the first delay of greater than 1 hour.