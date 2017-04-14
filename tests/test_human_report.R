## how can we set up a call to cal_process_human?
library(devtools)
library(jsonlite)
library(dplyr)

load_all("/home/erik/Dropbox/MEI/NCI/pilr_r_packages/pilrapi/pilrapi/")
load_all("/home/erik/Dropbox/MEI/NCI/pilr_r_packages/pilr.utils.r")
load_all("/home/erik/Dropbox/MEI/NCI/pilr_r_packages/pilr.dpu.r")
load_all("/home/erik/Dropbox/MEI/calorimeter/mei.calorimeter.r")

## cal_data
sample_file <- "/home/erik/Dropbox/MEI/NCI/s4r-general/s4r-webapp/data/sample-dataset-data/calorimeter/calorimeter-sample.json"

tz_format <- "%Y-%m-%dT%H:%M:%SZ"

cal_data <- fromJSON(txt = sample_file) %>% toJSON() %>% json_to_pilr()

## Date conversions. These will eventually (soon) live in the
## json_to_pilr function (or other), because we'll have the schemas
## available, at least when called through the r processing tool
cal_data$timestamp <- as.POSIXct(cal_data$timestamp, format = tz_format)

## min/max human
min_human <- format(min(cal_data$timestamp), format = tz_format)
max_human <- format(max(cal_data$timestamp), format = tz_format)

min_human2 <- format(min(cal_data$timestamp + 100), format = tz_format)
max_human2 <- format(max(cal_data$timestamp - 100), format = tz_format)

settings_human <- data.frame(key = c("deriv_window", "seconds", "volume", "config", "eb_pred"),
                       value = c("8", "60", "30000", "Push - Differential", "70"))

timetags_human <- data.frame(start_time = c("2014-01-01T12:00:00Z",
                                 min_human, min_human2),
                             end_time = c("2014-01-02T12:00:00Z", max_human, max_human2),
                             label = c("null_period", "human_time", "human_time"))

## cal_params
cal_params_human <- list(timetags = timetags_human,
                         settings = settings_human)


cal_data2 <- list(calrq = cal_data, timetags = timetags_human)

nulled   <- cal_data2 %>% apply_null_offset(cal_params_human)
sloped   <- nulled %>% apply_slope_offset(cal_params_human)
haldaned <- sloped %>% deriv_haldane(cal_params_human)
humaned  <- haldaned %>% human_summary(cal_params_human)

## values stub
values <- data.frame(key = c("particpant", "weight_in", "weight_out", "nitrogen",
                         "energy_intake_measured",
                         "energy_intake_predicted", "diet", "exercise_level",
                         "chamber_id", "study_id", "visit_id", "treatment"),
                     value = c("222", "175", "174", "0.3", "2000", "2200",
                         "Normal", "Light", "1", "101", "1", "NA"))


list(participant = 222, weight_in = 175)

values[1:2,]

data <- list(humaned = humaned,
             haldaned = haldaned,
             values = values)
params <- cal_params_human

## now can call pilr_report assuming data and params are available

## 
pilr_dpu("process_cal_human",
         package = "mei.calorimeter.r",
         data = data,
         dpu_params = params)

