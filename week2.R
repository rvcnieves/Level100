##### 4.4 #1-3

#1.
# 
my_variable <- 10
my_varıable
# 
# #### Need to change the charter between r and a to i.
my_variable

#2.

# #install.packages("tidyverse")  ##  Need to run if applicable
# 
library(tidyverse)

data("mpg")
data("diamonds")
ggplot(data = mpg)
geom_point(mapping = aes(x = displ, y = hwy))


filter(mpg, cyl == 8)
filter(diamonds, carat > 3)

#3
#We get Keyboard Shortcut Quick Reference. 
#Also, we can get there by selecting the "Tools" menu and 
#selecting the "Keyboard Shortcuts Help" option. 


#################################
####### 5.2.4 #1-4

# 1. Find all flights that
# 
# . Had an arrival delay of two or more hours

View(filter(flights, (arr_delay >= 120 )))

# . Flew to Houston (IAH or HOU)
View(filter(flights, (dest %in% c('IAH','HOU') )))

# . Were operated by United, American, or Delta
View(filter(flights, (carrier %in% c('UA','AA','DL'))))

# . Departed in summer (July, August, and September)
View(filter(flights, (month %in% c(7,8,9))))

# . Arrived more than two hours late, but didn’t leave late
View(filter(flights, (arr_delay >= 120 & dep_delay <= 0)))

# . Were delayed by at least an hour, but made up over 30 minutes in flight
View(filter(flights, (arr_delay <= 0 & dep_delay >= 60)))

# . Departed between midnight and 6am (inclusive)
View(filter(flights, (dep_time >= 2400 | dep_time < 601)))

# 
# 2. Another useful dplyr filtering helper is between(). What does it do? Can you use it to simplify the code needed to answer the previous challenges?
#     between() will filter between a range of values where the boundary is also included.  For example:

View(filter(flights, (between(arr_delay,120,max(flights$arr_delay,na.rm = TRUE)) )))

View(filter(flights, (between(flights$month,7,9) )))

View(filter(flights, (between(flights$dep_time,2400,2459) | between(flights$dep_time,1,601))))



# 3.  How many flights have a missing dep_time? 
#       We could use the following command to get the number of missing dep_time by looking how many NA are in the dataset:

sum(is.na(flights$dep_time))

#     What other variables are missing?

#       With this command we can get that in addition to dep_time,
#       dep_delay, arr_time, arr_Delay, tailnum, air_time have missing values:
colSums(is.na(flights))

#     What might these rows represent?
#       Most probable they represent cancelled and/or diverted flights from their original destination.



# 4. Why is NA ^ 0 not missing? 

#   Because any value elevated to the power of 0 will always result to 1.

# Why is NA | TRUE not missing? 
#   Because any value compared to TRUE in an OR statement will always result to TRUE.


# Why is FALSE & NA not missing? 
#   Because any value compared to FALSE in an AND statement will always result to False.


#Can you figure out the general rule? (NA * 0 is a tricky counterexample!)

#   Any calculation made with missing values where the result will be always the same no matter what value is used, will not result in a missing value.

#########
#### 5.3.1 #1-4

# How could you use arrange() to sort all missing values to the start? (Hint: use is.na()).
# 
df <- tibble(x = c(5, 2, NA))
arrange(df, desc(is.na(x)))

# Sort flights to find the most delayed flights. 
View(arrange(flights,desc(dep_delay)))
# Find the flights that left earliest.
View(arrange(flights,dep_delay))


# Sort flights to find the fastest flights.
# 
View(arrange(flights,air_time))


# Which flights travelled the longest?
View(arrange(flights,desc(distance)))

# Which travelled the shortest?
View(arrange(flights,(distance)))


#################################################
# 5.4.1 #2-4


# Brainstorm as many ways as possible to select dep_time, dep_delay, arr_time, and arr_delay from flights.
# 
select(flights,dep_time,dep_delay,arr_time,arr_delay)
select(flights,-(everything()),dep_time, dep_delay, arr_time, arr_delay)
select(flights,everything(),-(year:day),-sched_dep_time,-sched_arr_time,-(carrier:time_hour))
select(flights,dep_time,dep_delay:arr_time,arr_delay)
select(flights,dep_time:arr_delay,-sched_dep_time,-sched_arr_time)
select(flights,-year,-month,-day,-sched_dep_time,-sched_arr_time,-carrier,-flight,-tailnum,-origin,-dest,-air_time,-distance,-hour,-minute,-time_hour)


# What happens if you include the name of a variable multiple times in a select() call?
#   
select(flights,dep_time,dep_delay,arr_time,dep_time,arr_delay,dep_time,dep_time)

#   The output will include only unique columns and will not include repeated columns.

# What does the one_of() function do? Why might it be helpful in conjunction with this vector?
#   
vars <- c("year", "month", "day", "dep_delay", "arr_delay")
select(flights,one_of(vars))
#     It will select one of each flights' variable names given in the vars vector.  This will be helpful to select variables/columns from a tibble by preparing a name vector before performing the select function.

# Does the result of running the following code surprise you? 
select(flights, contains("TIME"))
#     No, it does not surprise me considering we are selecting all variable/columns names that have time in it. 

#How do the select helpers deal with case by default? 
#     The default is set to ignore.case = TRUE, you have to set it to FALSE if you want it to be case sensitive

#How can you change that default?
#   
select(flights, contains("TIME", ignore.case = FALSE))


