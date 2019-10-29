#'@export
apply_null_offset <- function(data, params, ...)
{
    calrq <- data$calrq
    
    event_tags <- data$event_tags
    
    settings <- params$settings

    config <- pilr.utils.r::get_setting("configuration", params$settings) 
    
    ## InO2 setting
    ## only use the setting *if* InflowO2 values are missing

    if(is.null(calrq$InflowO2) || all(is.na(calrq$InflowO2)) ||
       grepl("differential", config, ignore.case = TRUE)) {
        if(pilr.utils.r::has_setting("in_o2", params$settings)) {
            ino2_setting <- pilr.utils.r::get_setting("in_o2", settings) %>%
                pilr.utils.r::safe_numeric()
            calrq$InflowO2 <- ino2_setting
        }  # else {
           # stop("O2-In Constant setting required.")
      #  }
    }
    
    ## InCO2 setting
    if(is.null(calrq$InflowCO2) || all(is.na(calrq$InflowCO2)) ||
       grepl("differential", config, ignore.case = TRUE)) {
        if(pilr.utils.r::has_setting("in_co2", params$settings)) {
            inco2 <- pilr.utils.r::get_setting("in_co2", settings) %>%
                pilr.utils.r::safe_numeric()
            calrq$InflowCO2 <- inco2
        } # else {
          #  stop("CO2-In Constant setting required.")
       # }
    }

    if(length(event_tags)) {
        tt <- pilr.utils.r::apply_event_tags(calrq$timestamp, event_tags)
        ## data.frame of times in null_period
        if(any(event_tags$tags == "Null")) {
            null_period <- pilr.utils.r::subset_event_tags("Null", calrq, tt)
        } else {
            null_period <- data.frame()
        }

        if(nrow(null_period) == 0) {
            null_offset_O2 <- 0
            null_offset_CO2 <- 0
        } else {
            null_offset_O2 <- mean(null_period$InflowO2 -
                                   null_period$OutflowO2, na.rm = TRUE)
            
            null_offset_CO2 <- mean(null_period$InflowCO2 -
                                    null_period$OutflowCO2, na.rm = TRUE)
        }
    } else {
        ## no event_tags passed in 
        null_offset_O2 <- 0
        null_offset_CO2 <- 0
    }
    
    calrq$nulled_OutflowO2  <- calrq$OutflowO2 + null_offset_O2
    calrq$null_offset_O2 <- null_offset_O2

    calrq$nulled_OutflowCO2 <- calrq$OutflowCO2 + null_offset_CO2
    calrq$null_offset_CO2 <- null_offset_CO2

    calrq$nulled_InflowO2 <- calrq$InflowO2
    calrq$nulled_InflowCO2 <- calrq$InflowCO2

    ## may want to consider returning a list here, containing the data
    ## and possibly a message iwth the null_offset values? 
    
    list(nulled = calrq, event_tags = event_tags)
}
