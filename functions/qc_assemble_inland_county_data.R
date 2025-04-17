# function to read in and format ALL data from the inland datasets qc_data folder
# for the open data portal

# path: the path to the qc_data folder

# NOTE: This function will assemble ALL .rds files within the qc_data folder.
# YOU MUST tweak the below code if you are interested in leaving out some of 
# the datasets in the qc_data folder, or only working with data from ONE county



#function for inland datasets
qc_assemble_inland_county_data <- function(path = NULL) {
  
  if(is.null(path)) {
    path <- file.path(
      "R:/data_branches/inland_water_quality/processed_data/qc_data")
  }
  
  # column order ------------------------------------------------------------
  
  # use for the join and to order columns in output
  depl_cols <- c(
    "county",
    "waterbody",
    "station",
   # "lease",
    "latitude" ,
    "longitude" ,
    "deployment_range"   ,
    "string_configuration",
    "sensor_type",
    "sensor_serial_number",
    "timestamp_utc"  ,
    "sensor_depth_at_low_tide_m", #RENAME to sensor_depth_at_deployment
    "depth_crosscheck_flag",
    "hil_comment"
  )
  
  var_cols <- c(
    "dissolved_oxygen_percent_saturation"   ,
   # "dissolved_oxygen_uncorrected_mg_per_l",
   # "salinity_psu",
    "sensor_depth_measured_m",
    "temperature_degree_c"
  )
  
  thresholds <-
    read.csv(
      "C:/Users/Nicole Torrie/Documents/R/cmp_code/inland_thresholds_analysis/thresholds_inland.csv")
  
  qc_test_cols <- thresholds %>%
    select(qc_test, variable) %>%
    distinct() %>%
    filter(qc_test != "depth_crosscheck") %>%
    mutate(col_name = paste(qc_test, "flag", variable, sep = "_")) %>%
    arrange(qc_test)
  
  qc_test_cols <- sort(c(
    qc_test_cols$col_name,
    "human_in_loop_flag_dissolved_oxygen_percent_saturation",
  #  "human_in_loop_flag_dissolved_oxygen_uncorrected_mg_per_l",
  #  "human_in_loop_flag_salinity_psu",
  #  "human_in_loop_flag_sensor_depth_measured_m",
    "human_in_loop_flag_temperature_degree_c")
  )
  
  
  qc_max_cols <- c(
    "qc_flag_dissolved_oxygen_percent_saturation"   ,
 #   "qc_flag_dissolved_oxygen_uncorrected_mg_per_l",
 #   "qc_flag_salinity_psu",
    "qc_flag_sensor_depth_measured_m",
    "qc_flag_temperature_degree_c"
  )
  
  # all columns that should be in the data
  all_cols <- c(depl_cols,  var_cols, qc_test_cols, qc_max_cols)
  df <- data.frame(matrix(nrow = 1, ncol = length(all_cols)))
  colnames(df) <- all_cols
  
  # list all files in county folder
  # depls <- list.files(
  #   paste(path, county, sep = "/"),
  #   pattern = ".rds",
  #   full.names = TRUE
  # )
  
  depls <- list.files(
    paste(path),
    pattern = ".rds",
    full.names = TRUE,
    recursive = TRUE
  )
  
  
  # read in data, bind together
  dat <- depls %>%
    map(readRDS) %>%
    list_rbind()
  
  # if any needed columns are NOT in dat, add them as na
  dat %>%
    bind_rows(df) %>%
    filter(row_number() != n()) %>% # last row will be all NA, so need to remove it
    select(all_of(all_cols))  # fix the column order
  
}



