## this is our entry point from opencpu
#'@export
process_cal_infusion <- function(data, params, ...) {

    if(missing(params)) {
        params <- list()
    }
        
    if(!length(params$settings)) {
        stop("No settings were received.")
    }
    
    if(!length(data$event_tags)) {
        stop("No event tags dataset was received.")
    }
    
    ret <- data %>% apply_null_offset(params) %>%
        apply_slope_offset(params) %>%
            deriv_haldane(params)

    infusion <- ret %>% infusion_summary(params)

    ## add metadata to infusion data.frame for return data, how are we
    ## going to do this in general?
    
    infusion$pt <- unique(data$calrq$pt)[1]
    infusion$id <- sapply(1:nrow(infusion)*0,uuid::UUIDgenerate)
      # system(paste0("uuid", " -v4", " -n", nrow(infusion)), intern = TRUE)
    infusion$timestamp <- format(as.POSIXct(infusion$start_time), format = "%Y-%m-%dT%H:%M:%SZ")

    ret$haldane$pt <- unique(data$calrq$pt)[1]
    ret$haldane$id <- sapply(1:nrow(ret$haldane)*0,uuid::UUIDgenerate)
      # system(paste0("uuid", " -v4", " -n", nrow(ret$haldane)),
      #                       intern = TRUE)
    ret$haldane$timestamp <- format(ret$haldane$Time, format = "%Y-%m-%dT%H:%M:%SZ")

    ## workaround bug in event tag data where sub-second accuracy is
    ## given
    ## event_tags data comes in as YYYY-MM-DDTHH:MM:SSZ
    event_tags  <- data$event_tags

    event_tags$start_time <- ifelse(nchar(event_tags$start_time) == 24,
                                    paste0(substr(event_tags$start_time, 1, 19), "Z") ,
                                    event_tags$start_time)

    event_tags$end_time <- ifelse(nchar(event_tags$end_time) == 24,
                                    paste0(substr(event_tags$end_time, 1, 19), "Z") ,
                                    event_tags$end_time)

    rep_data <- list(infusion = infusion, haldane = ret$haldane,
                     event_tags = event_tags)

    base64_rep <- infusion_report(rep_data, params, ...)
    
    # Save processing time
    tm <- as.POSIXlt(Sys.time(), "UTC", "%Y-%m-%dT%H:%M:%S")
    infusion$Processed <- strftime(tm , "%Y-%m-%dT%H:%M:%S%z")
    ret$haldane$Processed <- strftime(tm , "%Y-%m-%dT%H:%M:%S%z")

    ## try only saving cols we need
    keep <- c("Time", "Processed", "recalc_vo2", "recalc_vco2",
              "recalc_ee", "recalc_rq", "nulled_outflow_o2",
              "nulled_outflow_co2", "nulled_inflow_o2",
              "nulled_inflow_co2", "do2", "dco2", "inflow_rate",
              "outflow_rate", "id", "pt", "timestamp")

    datasets <- list(infusion = infusion, haldane = ret$haldane[keep])
    files    <- list(infusion = jsonlite::unbox(base64_rep))
    
    list(datasets = datasets, files = files)
}

infusion_summary <- function(data, params, ...) {

    calrq <- data$haldane
    calrq <- calrq[order(calrq$Time), ]

    settings <- params$settings

    ## event_tags data comes in as YYYY-MM-DDTHH:MM:SSZ
    event_tags  <- data$event_tags
    ## workaround bug in event tag data where sub-second accuracy is
    ## given
    event_tags$start_time <- ifelse(nchar(event_tags$start_time) == 24,
                                    paste0(substr(event_tags$start_time, 1, 19), "Z") ,
                                    event_tags$start_time)

    event_tags$end_time <- ifelse(nchar(event_tags$end_time) == 24,
                                    paste0(substr(event_tags$end_time, 1, 19), "Z") ,
                                    event_tags$end_time)


    
    event_tags <- event_tags[order(event_tags$start_time), ]
    tag_table <- pilr.utils.r::apply_event_tags(calrq$Time, event_tags)
    tag_labels <- pilr.utils.r::list_event_tags(event_tags)

    ## add in the Valid data tag, and subset appropriately
    if(any(event_tags$tags == "Invalid Data")) {
        tag_table$TT_ValidData <- !tag_table$TT_InvalidData
        calrq <- pilr.utils.r::subset_event_tags("ValidData", calrq, tag_table)
        tag_table <- subset(tag_table, TT_ValidData == TRUE)

                ## now we can remove both valid and invalid cols
        tag_table <-
            tag_table[!names(tag_table) %in%
                      c("TT_ValidData", grep("TT_InvalidData", names(tag_table), value = TRUE))]
    }

    if(length(tag_labels) > 0) {
        tag_summary <- pilr.utils.r::fapply_event_tags(calrq, tag_table,
                                                     compute_infusion_summary,
                                                     settings)
    }

    ret <- do.call(rbind, tag_summary)

    ## remove Null row 
    subset(ret, tag_label != "Null")
}

compute_infusion_summary <- function(data, tag_label, settings, ...) {
    vo2_exp <- 3.941
    vco2_exp <- 1.104
    
    # Create array of mean MFC values
    mfcarray <- c(mean(data$calrq$MFCFlow_1),
                  mean(data$calrq$MFCFlow_2),
                  mean(data$calrq$MFCFlow_3),
                  mean(data$calrq$MFCFlow_4))
    
    # If CO2 MFC is not set, set to second largest flow
    if (!length(settings$CO2_MFC$value)){
      settings$CO2_MFC$value <- paste('MFC',order(mfcarray,decreasing=T)[2])
    }
    
    # If N2 MFC is not set, set to largest flow
    if (!length(settings$N2_MFC$value)){
      settings$N2_MFC$value <- paste('MFC',order(mfcarray,decreasing=T)[1])
    }
    
    mfc <- get_mfc_data(data, pilr.utils.r::get_setting("CO2_MFC", settings) %>%
                        as.character,
                        pilr.utils.r::get_setting("N2_MFC", settings) %>%
                        as.character)
  
    VO2_exp <- mean( data$nulled_inflow_o2 * (mfc$CO2 + mfc$N2)) / 100
    VCO2_exp <- mean(mfc$CO2)
    EE_exp <- (vo2_exp * VO2_exp + vco2_exp * VCO2_exp)
    RQ_exp <- VCO2_exp / VO2_exp

    if(!grepl("ShortCircuit", tag_label)) {
        VO2_meas <- mean(data$recalc_vo2) 
        VCO2_meas <- mean(data$recalc_vco2) 
        EE_meas <- mean(data$recalc_ee)
        RQ_meas <- mean(data$recalc_rq)
    } else {
        VO2_meas <- mean(data$recalc_vo2_0vol) 
        VCO2_meas <- mean(data$recalc_vco2_0vol) 
        EE_meas <- mean(data$recalc_ee_0vol)
        RQ_meas <- mean(data$recalc_rq_0vol)
    }
  
    VO2_err <- (VO2_meas - VO2_exp) / VO2_exp * 100
    VCO2_err <- (VCO2_meas - VCO2_exp) / VCO2_exp * 100
    EE_err <- (EE_meas - EE_exp) / EE_exp * 100
    RQ_err <- (RQ_meas - RQ_exp) / RQ_exp * 100

    null_offset_o2 <- round(data$null_offset_O2[1], 2)
    null_offset_co2 <- round(data$null_offset_CO2[1], 2)

    round_data <- lapply(data.frame(vco2_meas = VCO2_meas,
                                    vo2_meas = VO2_meas,
                                    ee_meas = EE_meas,
                                    rq_meas = RQ_meas,
                                    vco2_exp = VCO2_exp,
                                    vo2_exp = VO2_exp,
                                    ee_exp = EE_exp,
                                    rq_exp = RQ_exp,
                                    vco2_err = VCO2_err,
                                    vo2_err = VO2_err,
                                    ee_err = EE_err,
                                    rq_err = RQ_err,
                                    null_offset_o2, null_offset_co2),
                         round, 4)

    ## what time variable do we use?
    round_data$start_time <- as.character(min(data$Time))
    round_data$end_time   <- as.character(max(data$Time))
    round_data$tag_label   <- gsub("TT_", "", tag_label)
    
    as.data.frame(round_data)
}

get_mfc_data <- function(data, mfc_CO2, mfc_N2) {
    CO2 <- data[[paste("MFCFlow", substr(strsplit(mfc_CO2, " ")[[1]][2], 1, 1), sep = "_")]]
    N2 <- data[[paste("MFCFlow", substr(strsplit(mfc_N2, " ")[[1]][2], 1 , 1), sep = "_")]]

    data.frame(CO2 = as.numeric(as.character(CO2)),
               N2 = as.numeric(as.character(N2)))
}