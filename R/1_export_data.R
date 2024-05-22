# 2024-05-22

# This script imports all CMP data and exports separate data files for
## dissolved oxygen (% sat), dissolved oxygen (mg/L) and temperature
# These smaller files are faster to read in for analysis

# qc_flag_variable column is the "worst" flag assigned to the observation

# must be connected to the Perennia VPN to import data


# packages ----------------------------------------------------------------

library(dplyr)
library(here)
library(sensorstrings)

# import all data ---------------------------------------------------------

dat_all <- ss_import_data() %>%
  filter(!(station %in% c("Piper Lake", "Hourglass Lake", "Sissiboo")))

# columns to keep for each dataset
key_cols <- c("county", "waterbody", "station", "lease",
              "latitude", "longitude", "deployment_range",
              "sensor_type", "sensor_serial_number",
              "timestamp_utc")

# dissolved oxygen (% sat) ------------------------------------------------

dat_sat <- dat_all %>%
  filter(!is.na(dissolved_oxygen_percent_saturation)) %>%
  select(
    all_of(key_cols),
    dissolved_oxygen_percent_saturation,
    qc_flag_dissolved_oxygen_percent_saturation
  )

saveRDS(dat_sat, here("data/dissolved_oxygen_percent_saturation.rds"))


# dissolved oxygen (mg/L) -------------------------------------------------

dat_mgL <- dat_all %>%
  filter(!is.na(dissolved_oxygen_mg_per_l)) %>%
  select(
    all_of(key_cols),
    dissolved_oxygen_mg_per_l,
    qc_flag_dissolved_oxygen_mg_per_l
  )

saveRDS(dat_mgL, here("data/dissolved_oxygen_mg_per_l.rds"))


# temperature -------------------------------------------------------------

temp <- dat_all %>%
  filter(!is.na(temperature_degree_c)) %>%
  select(
    all_of(key_cols),
    temperature_degree_c,
    qc_flag_temperature_degree_c
  )

saveRDS(dat_mgL, here("data/temperature_degree_c.rds"))

