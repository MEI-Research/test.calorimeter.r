#' calorimeter
#'
#' @name calorimeter
#' @docType package

segment <- function(allData, start, end) {
    if(!"POISXt" %in% class(allData$Time)) {
        allData$Time <- as.POSIXct(allData$Time, format = "%m/%d/%Y %H:%M:%S")
    }

    subset(allData,
           allData$Time >= as.POSIXct(start, format = "%Y-%m-%d %H:%M:%S") &
           allData$Time <= as.POSIXct(end, format = "%Y-%m-%d %H:%M:%S"))
}

compute_settings <- function(settings) {

    ## unique values only
    settings <- unique(settings)

    getSetting <- function(setting) {
        if(setting %in% settings$key) {
            ret <- settings$value[settings$key == setting]
        } else {
            ret <- NA
        }
        ret
    }

    ## refactor? 
    data.frame(setting_config = getSetting("cal_config"),
               setting_O2 = getSetting("InO2"),
               setting_CO2 = getSetting("InCO2"),
               setting_volume = getSetting("volume"),
               setting_diet = getSetting("diet"),
               setting_exercise_level = getSetting("Exercise_Level"),
               setting_weight_in = getSetting("Weight_In"),
               setting_weight_out = getSetting("Weight_Out"),
               setting_treatment = getSetting("Treatment"),
               setting_nitrogen = getSetting("Nitrogen"),
               setting_ei_pred = getSetting("energy_intake_pred"),
               setting_ei_meas = getSetting("energy_intake_meas"),
               setting_ex_ee_pred = getSetting("exercise_ee_pred"),
               setting_eb_pred = getSetting("energy_balance_pred"),
               setting_tef_pred = getSetting("TEF_Predicted"),
               setting_seconds = getSetting("seconds"))[1,]
}

compute_settings <- function(settings) {
    settings <- unique(settings)        #get rid of duplicates
    data.frame(setting_config = settings$value[settings$key == "cal_config"],
               setting_O2 = settings$value[settings$key == "InO2"],
               setting_CO2 = settings$value[settings$key == "InCO2"],
               setting_volume = settings$value[settings$key == "volume"],
               setting_seconds = settings$value[settings$key == "seconds"],
               setting_N2_MFC = settings$value[settings$key == "mfc_N2"],
               setting_CO2_MFC = settings$value[settings$key == "mfc_CO2"])[1,]
}


compute_settings <- function(settings) {
    data.frame(setting_config = settings$value[settings$key == "cal_config"],
               setting_O2 = settings$value[settings$key == "InO2"],
               setting_CO2 = settings$value[settings$key == "InCO2"],
               setting_volume = settings$value[settings$key == "volume"],
               setting_seconds = settings$value[settings$key == "seconds"],
               setting_CO2_MFC = settings$value[settings$key == "gastype"])[1, ]
}

