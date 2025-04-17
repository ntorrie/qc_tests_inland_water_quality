#################################
# Add check for duplicate rows!
###################################

library(dplyr)
library(purrr)
library(qaqcmar)
library(sensorstrings)
library(here)
source(here("functions/qc_assemble_inland_county_data.R"))
source(here("functions/ss_export_inland_data.R"))


path_dat <- file.path("R:/data_branches/inland_water_quality/processed_data/qc_data")

dat <- qc_assemble_inland_county_data(path = path_dat)

unique(dat$waterbody)
unique(dat$station)


# open data portal (summary flags) -------------------------------------------
# remove the qc_test_variable columns (leaving only the max flag col)
thresholds <-
  read.csv(
    "C:/Users/Nicole Torrie/Documents/R/cmp_code/inland_thresholds_analysis/thresholds_inland.csv")

rm_cols <- thresholds %>%
  distinct(qc_test, variable) %>%
  mutate(rm_cols = paste(qc_test, "flag", variable, sep = "_"))

rm_cols <- sort(c(
  rm_cols$rm_cols,
  "human_in_loop_flag_dissolved_oxygen_percent_saturation",
  "human_in_loop_flag_dissolved_oxygen_uncorrected_mg_per_l",
  "human_in_loop_flag_salinity_psu",
  "human_in_loop_flag_sensor_depth_measured_m",
  "human_in_loop_flag_temperature_degree_c",
  "qc_flag_sensor_depth_measured_m") #TODO: develop thresholds and add this variable back in if more data is collected/processed
)

column_order = c(
  "county",
  "waterbody",
  "station",
  "latitude",
  "longitude",
  "deployment_range",
  "string_configuration",
  "sensor_type",
  "sensor_serial_number",
  "timestamp_utc",
  "sensor_depth_at_deployment_m",
  "sensor_depth_measured_m",
  "depth_crosscheck_flag",
  "dissolved_oxygen_percent_saturation",
  "temperature_degree_c",
  "qc_flag_dissolved_oxygen_percent_saturation",
#  "qc_flag_sensor_depth_measured_m",
  "qc_flag_temperature_degree_c"
)


dat %>%
  select(-any_of(rm_cols)) %>%
  qc_assign_flag_labels() %>%
  rename(sensor_depth_at_deployment_m = sensor_depth_at_low_tide_m) %>%
  select(column_order, everything()) %>%
  ss_export_data_files(export_rds = TRUE)


# read in and view rds ------------------------------------------------------
dat_rds <- readRDS("R:/data_branches/inland_water_quality/processed_data/assembled_data/2025-04-15_inland_water_quality_dataset.rds")


# cmar county data (all flags) --------------------------------------------

# remove columns that are all NA
dat %>%
  select_if(~ !all(is.na(.))) %>%
  ss_export_data_files(export_csv = FALSE)

# # export locally
# dat %>%
#   select_if(~ !all(is.na(.))) %>%
#   ss_export_county_files(
#     county = county,
#     output_path = here(),
#     export_csv = FALSE
#   )


# test plot -------------------------------------------------------------
library(ggplot2)
dat_i <- filter(dat_rds, station == "Tusket River 3")

q <- ggplot(
  dat_i,
  aes(x = timestamp_utc, y = temperature_degree_c, 
      col = qc_flag_temperature_degree_c)) +
  geom_point(size = 0.7) + 
  scale_color_manual(breaks = c("Pass", "Not Evaluated", "Suspect/Of Interest", "Fail"),
                     values = c("darkgreen", "darkgray", "orange", "red"))
q

