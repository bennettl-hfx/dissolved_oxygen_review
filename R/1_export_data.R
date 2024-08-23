# 2024-05-22
# Updated: 2024-07-22, by LB

# This script imports all CMP data and exports separate data files for
# dissolved oxygen (% sat), dissolved oxygen (mg/L) and temperature
# These smaller files are faster to read in for analysis

# qc_flag_variable column is the "worst" flag assigned to the observation

# must be connected to the Perennia VPN to import data


# Packages ----------------------------------------------------------------

library(dplyr)
library(here)
library(sensorstrings)

# Import all data ---------------------------------------------------------

dat_all <- ss_import_data() %>%
  filter(!(station %in% c("Piper Lake", "Hourglass Lake", "Sissiboo")))

# columns to keep for each dataset
key_cols <- c("county", "waterbody", "station", "lease",
              "latitude", "longitude", "deployment_range",
              "sensor_type", "sensor_serial_number",
              "timestamp_utc", "sensor_depth_at_low_tide_m")


# Dissolved oxygen (% sat) ------------------------------------------------

dat_sat <- dat_all %>%
  filter(!is.na(dissolved_oxygen_percent_saturation)) %>%
  select(
    all_of(key_cols),
    dissolved_oxygen_percent_saturation,
    rolling_sd_flag_dissolved_oxygen_percent_saturation
  )


# Dissolved oxygen (mg/L) -------------------------------------------------

dat_mgL <- dat_all %>%
  filter(!is.na(dissolved_oxygen_uncorrected_mg_per_l)) %>%
  select(
    all_of(key_cols),
    dissolved_oxygen_uncorrected_mg_per_l,
    rolling_sd_flag_dissolved_oxygen_uncorrected_mg_per_l
  )


# Temperature -------------------------------------------------------------

temp <- dat_all %>%
  filter(!is.na(temperature_degree_c)) %>%
  select(
    all_of(key_cols),
    temperature_degree_c,
    qc_flag_temperature_degree_c
  )


# Data Cleaning and Organizing -------------------------------------------------

## Split deployment range into depl_start and depl_end columns ------------------

dat_mgL <- dat_mgL %>%
  separate_wider_delim(deployment_range, names = c("depl_start", "depl_end"), delim = " to ") %>%
  mutate(NEW_start_date = as.Date(as.character(depl_start), format = "%Y-%b-%d"),
         NEW_end_date = as.Date(as.character(depl_end), format = "%Y-%b-%d"),
         depl_start=NULL,
         depl_end=NULL) %>%
  rename(depl_start=NEW_start_date, depl_end=NEW_end_date) %>%
  relocate(depl_start, .after=longitude) %>%
  relocate(depl_end, .after=depl_start)


dat_sat <- dat_sat %>%
  separate_wider_delim(deployment_range, names = c("depl_start", "depl_end"), delim = " to ") %>%
  mutate(NEW_start_date = as.Date(as.character(depl_start), format = "%Y-%b-%d"),
         NEW_end_date = as.Date(as.character(depl_end), format = "%Y-%b-%d"),
         depl_start=NULL,
         depl_end=NULL) %>%
  rename(depl_start=NEW_start_date, depl_end=NEW_end_date) %>%
  relocate(depl_start, .after=longitude) %>%
  relocate(depl_end, .after=depl_start)


temp <- temp %>%
  separate_wider_delim(deployment_range, names = c("depl_start", "depl_end"), delim = " to ") %>%
  mutate(NEW_start_date = as.Date(as.character(depl_start), format = "%Y-%b-%d"),
         NEW_end_date = as.Date(as.character(depl_end), format = "%Y-%b-%d"),
         depl_start=NULL,
         depl_end=NULL) %>%
  rename(depl_start=NEW_start_date, depl_end=NEW_end_date) %>%
  relocate(depl_start, .after=longitude) %>%
  relocate(depl_end, .after=depl_start)



## Generate and Save Deployment List (with deployment_id) -----------------------
# Upon investigation, all deployments from the three data sets are contained in temp.
# Therefore, the deployment ID can be generated from temp alone

deployment_list_full <- temp %>%
  group_by(county, station, depl_start, depl_end) %>%
  mutate(deployment_id = cur_group_id()) %>%
  select(deployment_id, county, station, depl_start, depl_end)

deployment_list <- unique(deployment_list_full) %>%
  arrange(deployment_id)


saveRDS(deployment_list, here("data/deployment_list.rds"))


## Copy deployment_id to full datasets ------------------------------------------

all_dat_mgl=inner_join(dat_mgL, deployment_list, join_by(station, depl_start, depl_end))

dat_mgL <- all_dat_mgl %>%
  mutate(county.y=NULL) %>%
  rename(county=county.x) %>%
  relocate(deployment_id, .before=depl_start)


all_dat_sat=inner_join(dat_sat, deployment_list, join_by(station, depl_start, depl_end))

dat_sat <- all_dat_sat %>%
  mutate(county.y=NULL) %>%
  rename(county=county.x) %>%
  relocate(deployment_id, .before=depl_start)


all_temp=inner_join(temp, deployment_list, join_by(station, depl_start, depl_end))

temp <- all_temp %>%
  mutate(county.y=NULL) %>%
  rename(county=county.x) %>%
  relocate(deployment_id, .before=depl_start)


## Save all necessary files -----------------------------------------------------

saveRDS(dat_sat, here("data/dissolved_oxygen_percent_saturation.rds"))

saveRDS(dat_mgL, here("data/dissolved_oxygen_uncorrected_mg_per_l.rds"))

saveRDS(temp, here("data/temperature_degree_c.rds"))
