## how can we set up a call to cal_process_human?
library(devtools)
library(jsonlite)
library(dplyr)
library(pilr.api.r)

load_all("/home/erik/Dropbox/MEI/NCI/pilr_r_packages/pilrapi/pilrapi/")
load_all("/home/erik/Dropbox/MEI/NCI/pilr_r_packages/pilr.utils.r")
load_all("/home/erik/Dropbox/MEI/calorimeter/mei.calorimeter.r")

## cal_data
sample_file <- "~/Github/s4r-general/s4r-webapp/data/sample-dataset-data/calorimeter/calorimeter-sample.json"

tz_format <- "%Y-%m-%dT%H:%M:%SZ"

cal_data <- fromJSON(txt = sample_file) %>% toJSON() %>% json_to_pilr()

## Date conversions. These will eventually (soon) live in the
## json_to_pilr function (or other), because we'll have the schemas
## available, at least when called through the r processing tool
cal_data$timestamp <- as.POSIXct(cal_data$timestamp, format = tz_format)
cal_data$Time <- as.POSIXct(cal_data$Time, format = tz_format)

## min/max human
min_human <- format(min(cal_data$timestamp), format = tz_format)
max_human <- format(max(cal_data$timestamp), format = tz_format)

min_human2 <- format(min(cal_data$timestamp + 100), format = tz_format)
max_human2 <- format(max(cal_data$timestamp - 100), format = tz_format)

# Should be a list of data frames
settings_human <- list(deriv_window = data.frame(epoch = "epoch_1", value = 8, period = "active_period"),
                       seconds = data.frame(epoch = "epoch_1", value = 60, period = "active_period"),
                       volume = data.frame(epoch = "epoch_1", value = 30000, period = "active_period"),
                       configuration = data.frame(epoch = "epoch_1", value = "Push - Differential", period = "active_period"),
                       in_o2 = data.frame(epoch = "epoch_1", value = 20.93, period = "active_period"),
                       in_co2 = data.frame(epoch = "epoch_1", value = 0, period = "active_period"))

## cal_params
cal_params_human <- list(timetags = data.frame(start_time = c("2014-01-01T12:00:00Z",
                                                   min_human, min_human2),
                             end_time = c("2014-01-02T12:00:00Z", max_human, max_human2),
                             label = c("null_period", "human_time", "human_time")),
                         settings = settings_human)

## call sample function
#load_all("/home/erik/Dropbox/MEI/calorimeter/mei.calorimeter.r")

nulled   <- cal_data %>% apply_null_offset(cal_params_human)
sloped   <- nulled %>% apply_slope_offset(cal_params_human)
haldaned <- sloped %>% deriv_haldane(cal_params_human)
humaned  <- haldaned %>% human_summary(cal_params_human)





## how can I get some sample infusion data? 
inf_file <- "~/Dropbox/MEI/calorimeter/mei.calorimeter.r/tests/data/infusion-1-Infusion-130809.csv"
inf_df <- read.csv(inf_file, skip = 3)
inf_df_names <- names(read.csv(inf_file, skip = 1))
names(inf_df) <- inf_df_names

tz_format_inf <- "%m/%d/%Y %H:%M:%S"
inf_df$Time <- as.POSIXct(inf_df$Time, format = tz_format_inf)
inf_df$timestamp <- as.POSIXct(inf_df$Time, format = tz_format_inf)

min_infusion <- format(min(inf_df$Time), format = tz_format)
max_infusion <- format(max(inf_df$Time), format = tz_format)

settings_infusion <- data.frame(key = c("deriv_window", "seconds", "volume", "config",
                                    "mfc_co2", "mfc_n2"),
                                value = c("8", "60", "30000", "Push - Differential",
                                    "MFC 2", "MFC 3"))

cal_params_infusion <- list(timetags = data.frame(start_time = c("2014-01-01T12:00:00Z",
                                                      min_infusion),
                                end_time = c("2014-01-02T12:00:00Z", max_infusion),
                                label = c("null_period", "infusion_period")),
                            settings = settings_infusion)

## straight from data.frame
infusion_data <- inf_df

load_all("/home/erik/Dropbox/MEI/NCI/pilr_r_packages/pilrapi/pilrapi/")
load_all("/home/erik/Dropbox/MEI/NCI/pilr_r_packages/pilr.utils.r")
load_all("/home/erik/Dropbox/MEI/calorimeter/mei.calorimeter.r")

nulled   <- infusion_data %>% apply_null_offset(cal_params_infusion)
sloped   <- nulled %>% apply_slope_offset(cal_params_infusion)
haldaned <- sloped %>% deriv_haldane(cal_params_infusion)
(infusioned  <- haldaned %>% infusion_summary(cal_params_infusion))



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
(burned  <- haldaned %>% burn_summary(params_burn))


