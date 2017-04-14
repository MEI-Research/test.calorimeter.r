## sample burn data
burn_file <- "~/Dropbox/MEI/calorimeter/mei.calorimeter.r/tests/data/Ch4_Propane-1-Burn-131023_Raw.csv"
burn_df <- read.csv(burn_file, skip = 3)
burn_df_names <- names(read.csv(burn_file, skip = 1))
names(burn_df) <- burn_df_names

## ugh, this file is missing the seconds in the time field... 
tz_format_burn <- "%m/%d/%Y %H:%M"
tz_format <- "%Y-%m-%dT%H:%M:%SZ"
burn_df$Time <- as.POSIXct(burn_df$Time, format = tz_format_burn)
burn_df$timestamp <- as.POSIXct(burn_df$Time, format = tz_format_burn)

min_burn <- format(min(burn_df$Time), format = tz_format)
max_burn <- format(max(burn_df$Time), format = tz_format)

settings_burn <- data.frame(key = c("deriv_window", "seconds", "volume", "config",
                                    "gas", "InO2", "InCO2"),
                            value = c("8", "60", "30000", "Pull - Differential",
                                      "Propane", "20", "0.3"))

params_burn <- list(timetags = data.frame(start_time = c("2013-10-23T12:00:00Z",
                                                         min_burn),
                                          end_time = c("2013-10-24T12:00:00Z", max_burn),
                                          label = c("null_period", "burn_period")),
                    settings = settings_burn)

values <- data.frame(key = c("participant", "chamber_id", "study_id", "gas"),
                     value = c("222", "1", "101", "CO2"))


## straight from data.frame
burn_data <- burn_df

library(devtools)
library(dplyr)
load_all("/home/erik/Dropbox/MEI/NCI/pilr_r_packages/pilrapi/pilrapi/")
load_all("/home/erik/Dropbox/MEI/NCI/pilr_r_packages/pilr.utils.r")
load_all("/home/erik/Dropbox/MEI/calorimeter/mei.calorimeter.r")

build("/home/erik/Dropbox/MEI/calorimeter/mei.calorimeter.r")

nulled   <- burn_data %>% apply_null_offset(params_burn)
sloped   <- nulled %>% apply_slope_offset(params_burn)
haldaned <- sloped %>% deriv_haldane(params_burn)
burned  <- haldaned %>% burn_summary(params_burn)

data <- list(burned = burned, haldaned = haldaned, values = values)
params <- params_burn
