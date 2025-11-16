#Luke Catalano
#Eleanor Stoever
#Final Code Set
rm(list=ls())

library(dplyr)
library(lubridate)
library(tidyr)
library(sandwich)
library(lmtest)
library(ggplot2)

data <- read.csv('data/motor.csv')

# Policy date
policy_date <- as.Date("2025-01-05")

# January 2nd is a Monday, start on the beginning of the first week
start_date <- as.Date('2023-01-02')
end_date <- as.Date(Sys.Date())

# We need to reformat the date so we can subset the data:
data$CRASH.DATE <- as.Date(data$CRASH.DATE, format = "%m/%d/%Y")
main_data <- subset(data, CRASH.DATE >= start_date & CRASH.DATE <= end_date 
                    & LATITUDE != 0 & BOROUGH == "MANHATTAN")


# Define weekly bins using `floor_date` to ensure weeks start on Monday
main_data$week <- as.numeric(as.factor(floor_date(main_data$CRASH.DATE, "week")))
main_data$week_start <- as.Date("2023-01-02") + (main_data$week - 1) * 7

#Establish a post variable for crashes after the policy date
main_data$post <- ifelse(main_data$week_start >= policy_date, 1, 0)

#Main treatment variable
main_data$treat <- ifelse(main_data$LATITUDE <= 40.7650, 1, 0)

# Filter Lower and Upper Manhattan
lower_manhattan <- main_data[main_data$LATITUDE <= 40.7650, ]
upper_manhattan <- main_data[main_data$LATITUDE > 40.7650, ]

# Count crashes per week
lower_counts <- lower_manhattan %>%
  group_by(week) %>%
  summarise(lower_crash_count = n(), .groups = 'drop')

upper_counts <- upper_manhattan %>%
  group_by(week) %>%
  summarise(upper_crash_count = n(), .groups = 'drop')

# Create full series of weeks
weeks <- data.frame(week = seq(min(main_data$week, na.rm = TRUE), 
                               max(main_data$week, na.rm = TRUE), 
                               by = 1))

# Join counts with full sequence of weeks
weekly_crash_data <- weeks %>%
  left_join(lower_counts, by = "week") %>%
  left_join(upper_counts, by = "week") %>%
  replace_na(list(lower_crash_count = 0, upper_crash_count = 0))

# Exclude the last week (incomplete data)
max_week <- max(weekly_crash_data$week)
weekly_crash_data <- weekly_crash_data %>%
  filter(week != max_week)

# Pivot to long format for DiD regression model (2 obs for each week)
did_data <- weekly_crash_data %>%
  pivot_longer(cols = c(lower_crash_count, upper_crash_count),
               names_to = "group",
               values_to = "crash_count") %>%
  mutate(treat = ifelse(group == "lower_crash_count", 1, 0))

# start date for each week, as a column in did_data
did_data$week_start <- as.Date("2023-01-02") + (did_data$week - 1) * 7

# Create the post variable based on if policy date is after week_start
did_data$post <- ifelse(did_data$week_start >= policy_date, 1, 0)

# Interaction term
did_data$post_treat <- did_data$post * did_data$treat

# Estimate the Diff-in-Diff model
did_model <- glm(crash_count ~ post + treat + post_treat, data = did_data)

# Show summary of the model
summary(did_model)

robust_se <- vcovHC(did_model, type = "HC1")  # Robust standard errors

# Display results with robust standard errors
coeftest(did_model, vcov = robust_se)


# Parallel Trends Visualization
ggplot(did_data, aes(x = week_start, y = crash_count, color = factor(treat))) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  geom_vline(xintercept = policy_date, linetype = "dashed", color = "black") +
  labs(
    title = "Crash Trends Before and After Policy Implementation",
    x = "Week Start Date",
    y = "Weekly Crash Count",
    color = "Group"  # Changes the legend title
  ) +
  scale_color_manual(
    values = c("cornflowerblue", "brown2"),  # Custom colors for each group (optional)
    labels = c("Upper Manhattan", "Lower Manhattan")  # Custom labels for the legend
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)  # Centers the title
  )


#Balance Table for pre-treatment observable characteristics
mean(lower_manhattan$NUMBER.OF.PERSONS.KILLED[lower_manhattan$post == 0])
mean(upper_manhattan$NUMBER.OF.PERSONS.KILLED[upper_manhattan$post == 0])
t_test_result <- t.test(NUMBER.OF.PERSONS.KILLED ~ treat, 
                        data = main_data[main_data$post == 0, ])
t_test_result

mean(lower_manhattan$NUMBER.OF.PERSONS.INJURED[lower_manhattan$post == 0])
mean(upper_manhattan$NUMBER.OF.PERSONS.INJURED[upper_manhattan$post == 0])
t_test_result <- t.test(NUMBER.OF.PERSONS.INJURED ~ treat, 
                        data = main_data[main_data$post == 0, ])
t_test_result

mean(lower_manhattan$NUMBER.OF.MOTORIST.INJURED[lower_manhattan$post == 0])
mean(upper_manhattan$NUMBER.OF.MOTORIST.INJURED[upper_manhattan$post == 0])
t_test_result <- t.test(NUMBER.OF.PERSONS.KILLED ~ treat, 
                        data = main_data[main_data$post == 0, ])
t_test_result


#pull the crash time
main_data$CRASH.TIME <- as.POSIXct(main_data$CRASH.TIME, format="%H:%M")

#Extract the hour and minute
main_data$hour <- as.numeric(format(main_data$CRASH.TIME, "%H"))
main_data$minute <- as.numeric(format(main_data$CRASH.TIME, "%M"))
lower_manhattan <- main_data[main_data$LATITUDE <= 40.7650, ]
upper_manhattan <- main_data[main_data$LATITUDE > 40.7650, ]

mean(lower_manhattan$hour)
mean(upper_manhattan$hour)
t_test_result <- t.test(hour ~ treat, data = main_data[main_data$post == 0, ])
t_test_result

mean(lower_manhattan$minute)
mean(upper_manhattan$minute)
t_test_result <- t.test(minute ~ treat, data = main_data[main_data$post == 0, ])
t_test_result