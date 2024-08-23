# 2024-05-22
# Final Updates: 2024-07-22, by LB

# Script to analyze and generate plots of DO data. See 2024-07-22_do_summary for all plots and other findings.

library(dplyr)
library(here)
library(ggplot2)
library(lubridate)
library(tidyr)

# Read in data from folder -----------------------------------------------------

dat_sat <- readRDS(here("data/dissolved_oxygen_percent_saturation.rds"))

dat_mgl <- readRDS(here("data/dissolved_oxygen_uncorrected_mg_per_l.rds"))

temp <- readRDS(here("data/temperature_degree_c.rds"))

deployment_list <- readRDS(here("data/deployment_list.rds"))

curr_deployments <- readRDS(here("data/currently_deployed_stations.rds"))

# Initialize a list of all counties
county_list <- unique(dat_sat$county)

# Rough work --------------------------------------------------------------
# This chunk includes mainly practice with dplyr and exploring the data sets. Not necessary to run for final analysis.

data <- dat_sat %>%
  select(county:station, timestamp_utc:last_col())

data_h <- data %>%
  filter(county=="Halifax")

data_suspect <- data_h %>%
  filter(rolling_sd_flag_dissolved_oxygen_percent_saturation=="3") %>%
  mutate(obs_month = month(timestamp_utc))

data_suspect_by_month <- data_suspect %>%
  group_by(obs_month) %>%
  count() %>%
  rename(num_observations = n)

mgl_counties = unique(dat_mgl$county)

# Plotting function (%sat) -------------------------------------------------------

# Plot the total number of suspect flags in each county broken down by month of observation
# x is the county name (a string)

get_suspect_occurrence_by_county_sat <- function(x){
  d <- dat_sat %>%
    select(county:station, timestamp_utc:last_col()) %>%
    filter(county==x & rolling_sd_flag_dissolved_oxygen_percent_saturation=="3") %>%
    mutate(obs_month = month(timestamp_utc, label=T, abbr=T)) %>% #label=TRUE
    group_by(obs_month) %>%
    count() %>%
    rename(num_observations=n)

  p <- ggplot(d, aes(x=obs_month, y=num_observations, fill=obs_month)) +
    geom_bar(stat="identity", fill="#2da8eb") +
    ggtitle(x)

  return(p)
}

# Test results

halifax <- get_suspect_occurrence_by_county_sat("Halifax")

annapolis <- get_suspect_occurrence_by_county_sat("Annapolis")

digby <- get_suspect_occurrence_by_county_sat("Digby")

yarmouth <- get_suspect_occurrence_by_county_sat("Yarmouth")

# Apply function to all counties
lapply(county_list_test, get_suspect_occurrence_by_county_sat)

# plotting function (mgl) -------------------------------------------------------
# Same function purpose as before, simply using mgl data

get_suspect_occurrence_by_county_mgl <- function(x){
  d <- dat_mgl %>%
    select(county:station, timestamp_utc:last_col()) %>%
    filter(county==x & rolling_sd_flag_dissolved_oxygen_uncorrected_mg_per_l=="3") %>%
    mutate(obs_month = month(timestamp_utc, label=T, abbr=T)) %>%
    group_by(obs_month) %>%
    count() %>%
    rename(num_observations=n)

  p <- ggplot(d, aes(x=obs_month, y=num_observations, fill=obs_month)) +
    geom_bar(stat="identity", fill="#2da8eb") +
    ggtitle(x)

  return(p)
}

# Test the function
get_suspect_occurrence_by_county_mgl("Halifax")

# plotting function (temp) -------------------------------------------------------
# Again, the same function as before, but now using temperature data


get_suspect_occurrence_by_county_temp <- function(x){
  d <- temp %>%
    select(county:station, timestamp_utc:last_col()) %>%
    filter(county==x & qc_flag_temperature_degree_c=="3") %>%
    mutate(obs_month = month(timestamp_utc, label=T, abbr=T)) %>%
    group_by(obs_month) %>%
    count() %>%
    rename(num_observations=n)

  p <- ggplot(d, aes(x=obs_month, y=num_observations, fill=obs_month)) +
    geom_bar(stat="identity", fill="#2da8eb") +
    ggtitle(x)

  return(p)
}

# Test the Function
get_suspect_occurrence_by_county_temp("Halifax")


# Updating suspect flags by month figures --------------------------------------

# Recall the old Plot
get_suspect_occurrence_by_county_sat("Halifax")

# New plotting function
# The output of the function is now a proportional stacked bar chart, which much better represents the underlying pattern

get_suspect_occurrence_by_county_updated_sat <- function(x){
  d <- dat_sat %>%
    select(county:station, timestamp_utc:last_col()) %>%
    filter(county==x) %>%
    mutate(obs_month = month(timestamp_utc, label=T, abbr=T)) %>%
    group_by(rolling_sd_flag_dissolved_oxygen_percent_saturation, obs_month) %>%
    count() %>%
    rename(num_observations=n, flag=rolling_sd_flag_dissolved_oxygen_percent_saturation)

  ggplot(d, aes(x=obs_month, y=num_observations, fill=flag)) +
    geom_bar(position=position_fill(reverse=F), stat="identity") +
    ylab("Fraction of Total Observations") +
    xlab("Month") +
    ggtitle(x)
}

# Test the function
get_suspect_occurrence_by_county_updated_sat("Halifax")

get_suspect_occurrence_by_county_updated_sat("Antigonish")

# Percentage of 'good' data ----------------------------------------------------
# For the sake of this analysis, good means before the first suspect flag

# Initialize a dataframe to store results after looping through all deployments
percent_good_data=matrix(rep(NA,(3*398)),ncol=3)
percent_good_data=data.frame(percent_good_data)

# Name the columns
names(percent_good_data)[1] <- "deployment_id"
names(percent_good_data)[2] <- "good_data_%" # Indicated the percentage of good data for each deployment
names(percent_good_data)[3] <- "under_5_days_since_depl_start" # Y or N. Y if the first suspect flag is under 5 days since the deployment start. N otherwise.

# Loop through all deployments
for(i in 1:398){
  percent_good_data[i,1]=i

  if(i != 136 & i != 210 & i != 211 & i != 281 & i != 299){ #These deployment_id's only have flag #2, and thus cannot be included
    if(i %in% unique(dat_sat$deployment_id)){ # Filtering down to those deployments in dat_sat
      depl <- dat_sat %>%
        filter(deployment_id==i)

      depl_first_flag <- depl %>%
        filter(rolling_sd_flag_dissolved_oxygen_percent_saturation=="3")

      depl_first_flag <- depl_first_flag$timestamp_utc[1] # Identifying the timestamp of the first suspect flag

      good_data <- as.numeric(dat_sat %>%
                                filter(deployment_id==i & timestamp_utc < depl_first_flag) %>%
                                count()) # Counting the number of observations before the first flag

      bad_data <- as.numeric(dat_sat %>%
                               filter(deployment_id==i & timestamp_utc >= depl_first_flag) %>%
                               count()) # Counting the number of observations after the first flag

      percent_good_data[i,2]=round((good_data/(good_data+bad_data)), digits=4) # Inserting the percentage into the dataframe

      if(as.numeric(difftime(depl_first_flag, depl$timestamp_utc[1], units="days")) < 5){ # Checking to see if the first flag is within 5 days of the deployment start
        percent_good_data[i,3] = "Y"
      }
    }
  }
}

# Identify quick summary statistics
mean(percent_good_data[,2], na.rm=T)
median(percent_good_data[,2], na.rm=T)


# Plot a quick histogram of all the percentages of good data (Notice the skewness)
good_data_percentages <- na.omit(percent_good_data$`good_data_%`)
hist(good_data_percentages, ylim=c(0,130))

# Filter down to those deployments with the first flag over five days since deployment
# (Notice the bin of smallest percentages is the only difference from the plot above)
x <- percent_good_data %>%
  filter(is.na(under_5_days_since_depl_start))

good_data_percentages_over_five <- na.omit(x$`good_data_%`)

# Plot
hist(good_data_percentages_over_five, ylim=c(0,130))

# Examine summary statistics
mean(good_data_percentages_over_five)
median(good_data_percentages_over_five)


# A simple scatter plot
plot(good_data_percentages)
# Is there any correlation between location (deployment_id) and amount of good data?
# The clump of really low percentages at the beginning is all of the Antigonish county deployments

# Plot a nicer scatter plot, colored by county
percent_good_data_updated <- inner_join(percent_good_data, deployment_list, join_by(deployment_id))

y <- percent_good_data_updated %>%
  filter(!is.na(`good_data_%`))

ggplot(y, aes(x=deployment_id, y=`good_data_%`, fill=county)) +
  geom_point(aes(color=county))

# Plot another scatter plot, zoomed in on the first four counties
z <- percent_good_data_updated %>%
  filter(!is.na(`good_data_%`) & county <= "Digby")

ggplot(z, aes(x=deployment_id, y=`good_data_%`, fill=county)) +
  geom_point(aes(color=county))

# Plotting individual deployments ------------------------------------------

# Create a function to generate a plot for each deployment
deployment_plot <- function(x){
  d <- dat_sat %>%
    select(county:depl_end, timestamp_utc:last_col()) %>%
    filter(deployment_id==x) %>%
    mutate(obs_month = month(timestamp_utc, label=T, abbr=T)) %>%
    group_by(rolling_sd_flag_dissolved_oxygen_percent_saturation, obs_month) %>%
    count() %>%
    rename(num_observations=n, flag=rolling_sd_flag_dissolved_oxygen_percent_saturation) #%>%

  ggplot(d, aes(x=obs_month, y=num_observations, fill=flag)) +
    geom_bar(position=position_fill(reverse=TRUE), stat="identity") +
    ggtitle(paste(deployment_list[x, 3], deployment_list[x, 4], sep=": "))
}

# Test the function
deployment_plot(11)


# Average number of days before first suspect flag -----------------------------
# Generate a graph showing the average number of days before the first suspect flag

## dat_sat ---------------------------------------------------------------------

mean_days_by_month_2 <- rep(NA, 12)
n_depl_by_month <- rep(NA, 12)
n_depl_by_month_with_observations <- rep(NA,12)

for(j in 1:12){
  curr_month_deployments <- deployment_list %>%
    filter(month(depl_start)==j)

  temporary_data <- dat_sat %>%
    filter(month(depl_start)==j)

  n_depl_by_month_with_observations[j] = length(unique(temporary_data$deployment_id))

  days_before_first_flag <- rep(NA, length(curr_month_deployments$deployment_id))

  n_depl_by_month[j] <- length(curr_month_deployments$deployment_id)

  for(i in 1:length(curr_month_deployments$deployment_id)){
    data <- dat_sat %>%
      filter(deployment_id == curr_month_deployments$deployment_id[i] & rolling_sd_flag_dissolved_oxygen_percent_saturation == 3)

    first_flag <- data$timestamp_utc[1]

    days_before_first_flag[i] <- as.numeric(difftime(first_flag, data$depl_start[1], units="days"))
  }

  mean_days_by_month_2[j]=mean(days_before_first_flag, na.rm=T)
}


mean_days_by_month <- data.frame(cbind(depl_month=1:12, mean_days_by_month_2, n_depl_by_month, n_depl_by_month_with_observations))

mean_days_by_month <- mean_days_by_month %>%
  mutate(depl_month=factor(month.abb[depl_month], levels=month.abb))

ggplot(mean_days_by_month, aes(x=depl_month, y=mean_days_by_month_2)) +
  geom_bar(stat="identity", fill="#49A2DE") +
  geom_text(aes(label=n_depl_by_month_with_observations), vjust=-0.3) +
  ggtitle("Average Number Of Days Before First Suspect Flag (%sat)") +
  xlab("Month of Deployment") +
  ylab("Mean Number Of Days")


## dat_mgl ---------------------------------------------------------------------

mean_days_by_month_2 <- rep(NA, 12)
n_depl_by_month <- rep(NA, 12)
n_depl_by_month_with_observations <- rep(NA,12)

for(j in 1:12){
  curr_month_deployments <- deployment_list %>%
    filter(month(depl_start)==j)

  temporary_data <- dat_mgl %>%
    filter(month(depl_start)==j)

  n_depl_by_month_with_observations[j] = length(unique(temporary_data$deployment_id))

  days_before_first_flag <- rep(NA, length(curr_month_deployments$deployment_id))

  n_depl_by_month[j] <- length(curr_month_deployments$deployment_id)

  for(i in 1:length(curr_month_deployments$deployment_id)){
    data <- dat_mgl %>%
      filter(deployment_id == curr_month_deployments$deployment_id[i] & rolling_sd_flag_dissolved_oxygen_uncorrected_mg_per_l == 3)

    first_flag <- data$timestamp_utc[1]

    days_before_first_flag[i] <- as.numeric(difftime(first_flag, data$depl_start[1], units="days"))
  }

  mean_days_by_month_2[j]=mean(days_before_first_flag, na.rm=T)
}


mean_days_by_month <- data.frame(cbind(depl_month=1:12, mean_days_by_month_2, n_depl_by_month, n_depl_by_month_with_observations))

mean_days_by_month <- mean_days_by_month %>%
  mutate(depl_month=factor(month.abb[depl_month], levels=month.abb))

ggplot(mean_days_by_month, aes(x=depl_month, y=mean_days_by_month_2)) +
  geom_bar(stat="identity", fill="#49A2DE") +
  geom_text(aes(label=n_depl_by_month_with_observations), vjust=-0.3) +
  ggtitle("Average Number Of Days Before First Suspect Flag (mg/L)") +
  xlab("Month of Deployment") +
  ylab("Mean Number Of Days")


# Confirm all mgl deployments are deployed in May or November
unique(dat_mgl$depl_start)

test <- dat_mgl %>%
  filter(month(depl_start)==5)

length(unique(test$deployment_id))


## Temp ------------------------------------------------------------------------
# Fair warning, this loop takes a while to run

mean_days_by_month_2 <- rep(NA, 12)
n_depl_by_month <- rep(NA, 12)
n_depl_by_month_with_observations <- rep(NA,12)

for(j in 1:12){
  curr_month_deployments <- deployment_list %>%
    filter(month(depl_start)==j)

  temporary_data <- temp %>%
    filter(month(depl_start)==j)

  n_depl_by_month_with_observations[j] = length(unique(temporary_data$deployment_id))

  days_before_first_flag <- rep(NA, length(curr_month_deployments$deployment_id))

  n_depl_by_month[j] <- length(curr_month_deployments$deployment_id)

  for(i in 1:length(curr_month_deployments$deployment_id)){
    data <- temp %>%
      filter(deployment_id == curr_month_deployments$deployment_id[i] & qc_flag_temperature_degree_c == 3)

    first_flag <- data$timestamp_utc[1]

    days_before_first_flag[i] <- as.numeric(difftime(first_flag, data$depl_start[1], units="days"))
  }

  mean_days_by_month_2[j]=mean(days_before_first_flag, na.rm=T)
}

mean_days_by_month <- data.frame(cbind(depl_month=1:12, mean_days_by_month_2, n_depl_by_month, n_depl_by_month_with_observations))

mean_days_by_month <- mean_days_by_month %>%
  mutate(depl_month=factor(month.abb[depl_month], levels=month.abb))

ggplot(mean_days_by_month, aes(x=depl_month, y=mean_days_by_month_2)) +
  geom_bar(stat="identity", fill="#49A2DE") +
  geom_text(aes(label=n_depl_by_month_with_observations), vjust=-0.3) +
  ggtitle("Average Number Of Days Before First Suspect Flag (Temp)") +
  xlab("Month of Deployment") +
  ylab("Mean Number Of Days")


# Current Deployments ----------------------------------------------------------
# Attach deployment_id to list of current deployments
# Further analysis may focus solely on deployments in this list

curr_deployments <- read.csv(here("data/currently_deployed_stations.csv"), header=T)

curr_deployment_info=inner_join(curr_deployments, deployment_list, join_by(station))

curr_deployment_info <- curr_deployment_info %>%
  arrange(deployment_id)

saveRDS(curr_deployment_info, here("data/currently_deployed_stations.rds"))
