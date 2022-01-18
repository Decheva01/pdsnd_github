# example questions:
# 1 Popular times of travel (i.e., occurs most often in the start time)
# What is the most common month?
# What is the most common day of week?
# What is the most common hour of day?

library(tidyverse)
library(lubridate)
library(ggplot2)
library(rlang)

# import original data:
chicago <- read_csv("C:\\Users\\Cvetana\\Documents\\Udacity projects\\R project Udacity\\csv files\\chicago.csv")
new_york <- read_csv("C:\\Users\\Cvetana\\Documents\\Udacity projects\\R project Udacity\\csv files\\new-york-city.csv")
washington <- read_csv("C:\\Users\\Cvetana\\Documents\\Udacity projects\\R project Udacity\\csv files\\washington.csv")

# add city as prefix to column names of each city table:
colnames(chicago) <- paste("Chicago", colnames(chicago), sep = " ")
colnames(new_york) <- paste("New York", colnames(new_york), sep = " ")
colnames(washington) <- paste("Washington", colnames(washington), sep = " ")

# Concatenate city tables to list:
city_list01 <- c(chicago, new_york, washington)
# Convert list to tibble:
city_tibble01 <- as_tibble(city_list01)

##############
#Q1 popular times of travel

city_tibble_Q1 <- city_tibble01 %>% mutate(`Chicago month` = month(`Chicago Start Time`, label = TRUE),
                                           `New York month` = month(`New York Start Time`, label = TRUE),
                                           `Washington month` = month(`Washington Start Time`, label = TRUE),
                                           `Chicago DOW` = wday(`Chicago Start Time`, label = TRUE),
                                           `New York DOW` = wday(`New York Start Time`, label = TRUE),
                                           `Washington DOW` = wday(`New York Start Time`, label = TRUE),
                                           `Chicago hour` = hour(`Chicago Start Time`),
                                           `New York hour` = hour(`New York Start Time`),
                                           `Washington hour` = hour(`Washington Start Time`))

# save pop times as list:
list_pop_times <- city_tibble_Q1 %>% 
  select(`Chicago month`, `New York month`, `Washington month`, `Chicago DOW`, `New York DOW`, `Washington DOW`,
         `Chicago hour`, `New York hour`, `Washington hour`)

# turn list into tibble:
tibble_pop_times <- as_tibble(list_pop_times)

#####
# Q1 stats:

#create empty list for loop output:
list_output_stats_Q1 <- c()

# for loop puts results into list_output_stats_Q1 
for (i in seq_along(list_pop_times)) {
  list_output_stats_Q1[[i]] <- tibble_pop_times %>%
    group_by(!!list_pop_times[[i]]) %>%
    summarize(count01 = n()) %>%
    arrange(desc(count01))
}

#pass column names as names of elements in list:
names(list_output_stats_Q1) <- colnames(tibble_pop_times)
# call result list with list names:
list_output_stats_Q1
#############

# Q1 graphs:

# plots to list:

# make empty list:
list_Q1_plot_output <- c()

# for loop saves plots in list:
for (i in seq_along(list_pop_times)) {
  list_Q1_plot_output[[i]] <- tibble_pop_times %>%
             ggplot(aes(x = !!list_pop_times[[i]])) +
             geom_bar() +
             xlab(word(colnames(tibble_pop_times)[[i]], -1)) +
             ggtitle(str_c("Popular times of travel:", "\n", colnames(tibble_pop_times)[[i]]))
}

#pass column names as names of elements in list:
names(list_Q1_plot_output) <- colnames(tibble_pop_times)

# call list_Q1_plot_output:
list_Q1_plot_output

##############
# Call Q1-1:

# Most popular month (months sorted by count, bar chart of count):
list_output_stats_Q1$`Chicago month`
list_Q1_plot_output$`Chicago month`

list_output_stats_Q1$`New York month`
list_Q1_plot_output$`New York month`

list_output_stats_Q1$`Washington month`
list_Q1_plot_output$`Washington month`

# Most popular day of the week (DOW sorted by count, bar chart of count):

list_output_stats_Q1$`Chicago DOW`
list_Q1_plot_output$`Chicago DOW`

list_output_stats_Q1$`New York DOW`
list_Q1_plot_output$`New York DOW`

list_output_stats_Q1$`Washington DOW`
list_Q1_plot_output$`Washington DOW`

# Most popular hour of departure (hour sorted by count, bar chart of count):

list_output_stats_Q1$`Chicago hour`
list_Q1_plot_output$`Chicago hour`

list_output_stats_Q1$`New York hour`
list_Q1_plot_output$`New York hour`

list_output_stats_Q1$`Washington hour`
list_Q1_plot_output$`Washington hour`


############
library(patchwork)

# Add tripple plots with patchwork:

month_plot <- list_Q1_plot_output$`Chicago month` + 
  list_Q1_plot_output$`New York month` +
  list_Q1_plot_output$`Washington month`

DOW_plot <- list_Q1_plot_output$`Chicago DOW` +
  list_Q1_plot_output$`New York DOW` +
  list_Q1_plot_output$`Washington DOW`
