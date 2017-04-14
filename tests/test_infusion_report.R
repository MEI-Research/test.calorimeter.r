library(dplyr)

load_all("/home/erik/Dropbox/MEI/NCI/pilr_r_packages/pilrapi/pilrapi/")
load_all("/home/erik/Dropbox/MEI/NCI/pilr_r_packages/pilr.utils.r")
load_all("/home/erik/Dropbox/MEI/NCI/pilr_r_packages/pilr.dpu.r")
load_all("/home/erik/Dropbox/MEI/NCI/pilr_r_packages/pilrdash")
load_all("/home/erik/Dropbox/MEI/calorimeter/mei.calorimeter.r")

inf_file <- "~/Dropbox/MEI/calorimeter/mei.calorimeter.r/tests/data/infusion-1-Infusion-130809.csv"
inf_df <- read.csv(inf_file, skip = 3)
inf_df_names <- names(read.csv(inf_file, skip = 1))
names(inf_df) <- inf_df_names

tz_format_inf <- "%m/%d/%Y %H:%M:%S"
tz_format <- "%Y-%m-%dT%H:%M:%SZ"
inf_df$Time <- as.POSIXct(inf_df$Time, format = tz_format_inf)
inf_df$timestamp <- as.POSIXct(inf_df$Time, format = tz_format_inf)

min_infusion <- format(min(inf_df$Time), format = tz_format)
max_infusion <- format(max(inf_df$Time), format = tz_format)

settings_infusion <- data.frame(key = c("deriv_window", "seconds", "volume", "config",
                                    "CO2_MFC", "N2_MFC"),
                                value = c("8", "60", "30000", "Push - Differential",
                                    "MFC 2", "MFC 3"))

values <- data.frame(key = c("study_id", "chamber_id"),
                     value = c("study 1234", "Chamber Z"))

timetags = data.frame(start_time = c("2014-01-01T12:00:00",
                          min_infusion),
    end_time = c("2014-01-02T12:00:00", max_infusion),
    label = c("null_period", "infusion_period"))

cal_params_infusion <- list(values = values,
                            settings = settings_infusion)

## straight from data.frame
infusion_data <- list(calrq = inf_df, timetags = timetags)

nulled   <- infusion_data %>% apply_null_offset(cal_params_infusion)
sloped   <- nulled %>% apply_slope_offset(cal_params_infusion)
haldaned <- sloped %>% deriv_haldane(cal_params_infusion)
(infusioned  <- haldaned %>% infusion_summary(cal_params_infusion))

data <- list(infusion = infusioned, haldane = haldaned,
             timetags = timetags)
params <- cal_params_infusion

## 
pilr_dpu("infusion_report",
         package = "mei.calorimeter.r",
         data = data,
         dpu_params = params,
         return_type = "r:pdf")
