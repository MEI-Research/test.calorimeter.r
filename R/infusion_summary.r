## this is our entry point from opencpu
#'@export
#'@import plyr xtable
process_cal_infusion <- function(data, params, ...) {
    
    message('Starting infusion summary job')
  
    if(missing(params)) {
        params <- list()
    }
        
    if(!length(params$settings)) {
        stop("No settings were received.")
    }
    
    if(!length(data$event_tags)) {
        stop("No event tags dataset was received.")
    }
  
  if(!length(data$calrq$Time)) {
    stop("No data sent, check tags and period!")
  }
  
    ## verify that we have a Infusion Study 
    if(!any(grepl("Infusion Study", data$event_tags$tags))) {
    stop("Use the event tag editor to tag a 'Infusion Study' event.")
    }
    
  ## workaround bug in event tag data where sub-second accuracy is
  ## given
  event_tags  <- data$event_tags
  
  data$event_tags$start_time <- ifelse(nchar(event_tags$start_time) == 24,
                                       paste0(substr(event_tags$start_time, 1, 19), "Z") ,
                                       event_tags$start_time)
  
  data$event_tags$end_time <- ifelse(nchar(event_tags$end_time) == 24,
                                     paste0(substr(event_tags$end_time, 1, 19), "Z") ,
                                     event_tags$end_time)
    
    ret <- data %>% apply_null_offset(params) %>%
        apply_slope_offset(params) %>%
            deriv_haldane(params)
    
    ## Organize each visit into a list
    visits <- c(tags = c())
    visitIndex <- which(event_tags$tags %in% "Infusion Study")
    for (i in 1:length(visitIndex)) {
      if (i == length(visitIndex)) {
        visits[i] <- list(tags = event_tags$tags[visitIndex[i]:length(event_tags$tags)])
      }
      else {
        visits[i] <- list(tags = event_tags$tags[visitIndex[i]:visitIndex[i+1]-1])
      }
    }
    
    # For each visit
    for (i in 1:length(visits)) {
      ret_sub <- c()
      ret$haldane$timestamp <- as.POSIXct(ret$haldane$timestamp, format = "%Y-%m-%dT%H:%M:%SZ")
      
      # Find first and last data within visit
      if (i == length(visits)) {
        startTime <- as.POSIXct(event_tags$start_time[visitIndex[i]], format = "%Y-%m-%dT%H:%M:%SZ")
        endTime <- as.POSIXct(event_tags$end_time[visitIndex[i]], format = "%Y-%m-%dT%H:%M:%SZ")
      }
      else {
        startTime <- as.POSIXct(event_tags$start_time[visitIndex[i]], format = "%Y-%m-%dT%H:%M:%SZ")
        endTime <- as.POSIXct(event_tags$end_time[visitIndex[i]], format = "%Y-%m-%dT%H:%M:%SZ")
      }
      
      event_tags$start_time_formatted <- as.POSIXct(event_tags$start_time, format = "%Y-%m-%dT%H:%M:%SZ")
      event_tags$end_time_formatted <- as.POSIXct(event_tags$end_time, format = "%Y-%m-%dT%H:%M:%SZ")
      
      # Get data within visit
      ret_sub$haldane <- subset(ret$haldane, (ret$haldane$timestamp >= startTime & ret$haldane$timestamp <= endTime))
      ret_sub$event_tags <- subset(event_tags, (event_tags$start_time_formatted >= startTime & event_tags$end_time_formatted <= endTime))
      
      # Run infusion summary and merge each visit result
      if (i == 1) {
        infusion <- ret_sub %>% infusion_summary(params)
      }
      else {
        infusion <- rbind(human, ret_sub %>% infusion_summary(params))
      }
    }
    
    # infusion <- ret %>% infusion_summary(params)

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
    calrq <- calrq[order(calrq$Time) , ]

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
    
    vo2_constant <- 3.941
    vco2_constant <- 1.104
    nitrogen_constant <- -2.17
    
    # Create array of mean MFC values
    mfcarray <- c(mean(as.numeric(data$MFCFlow_1)),
                  mean(as.numeric(data$MFCFlow_2)),
                  mean(as.numeric(data$MFCFlow_3)),
                  mean(as.numeric(data$MFCFlow_4)))
    
    # If CO2 MFC is not set, set to second largest flow
    if (as.character(pilr.utils.r::get_setting("CO2_MFC",settings)) == "list()"){
      # settings$CO2_MFC$value <- paste('MFC',order(mfcarray,decreasing=T)[2])
      # message("Auto-detected CO2 MFC")
      stop("CO2 MFC not set: check participant settings")
    }
    
    # If N2 MFC is not set, set to largest flow
    if (as.character(pilr.utils.r::get_setting("N2_MFC",settings)) == "list()"){
      # settings$N2_MFC$value <- paste('MFC',order(mfcarray,decreasing=T)[1])
      # message("Auto-detected N2 MFC")
      stop("N2 MFC not set: check participant settings")
    }
    
    mfc <- get_mfc_data(data, pilr.utils.r::get_setting("CO2_MFC", settings) %>%
                        as.character,
                        pilr.utils.r::get_setting("N2_MFC", settings) %>%
                        as.character)

    config <- pilr.utils.r::get_setting("configuration",
                                        settings)
    volume <- pilr.utils.r::get_setting("volume",
                                            settings)
    
    if (pilr.utils.r::has_setting("expected",
                              settings)){
      expected <- pilr.utils.r::get_setting("expected",
                                          settings)  
    } else if (config == 'Push - Absolute' | config == 'Push - Differential') {
      expected <- "Push - Convert to Pull"
      message("No equation configuration found - using Push -> Pull conversion method")
    } else {
      expected <- "Pull - Normal"
      message("No equation configuration found - using Pull method")
    }
    #
   
    
    # Determine form of equations/expected and execute
    if(grepl("Push - Corrected Derivative", expected, ignore.case = TRUE)) {
      if(grepl("push", config, ignore.case = TRUE)) {
        message("using config: push, using expected: corrected derivative")
        
        # Push Method
        VO2_exp <- mean((data$InflowRate * data$nulled_inflow_o2 * mfc$N2)/(mfc$N2 * 100 + data$InflowRate * data$nulled_inflow_n2)) * 1000
        VCO2_exp <- mean((data$InflowRate * (data$nulled_inflow_co2 * mfc$N2 - mfc$CO2 * data$nulled_inflow_n2)) / (-mfc$N2 * 100 - data$InflowRate * data$nulled_inflow_n2)) * 1000
        
        # Derivative correction
        d_corr = (data$InflowRate - VO2_exp + VCO2_exp) / (data$InflowRate + mfc$N2 + mfc$CO2)
        
        # Recalc VO2 VCO2
        data$haldane_outflow <- ( data$InflowRate * (data$nulled_inflow_n2/100) - (data$dn2/100) * volume * d_corr) / (data$nulled_outflow_n2/100)
        data$haldane_outflow_0vol <- ( data$InflowRate * (data$nulled_inflow_n2/100) ) / (data$nulled_outflow_n2/100)
        
        data$recalc_vo2 <- 10/1000 * (data$InflowRate * data$nulled_inflow_o2 - data$haldane_outflow * data$nulled_outflow_o2 - data$do2 * volume * d_corr)
        data$recalc_vco2 <- -10/1000 * (data$InflowRate * data$nulled_inflow_co2 - data$haldane_outflow * data$nulled_outflow_co2 - data$dco2 * volume * d_corr)
        
        data$recalc_ee <- ((vo2_constant * data$recalc_vo2 +
                              vco2_constant * data$recalc_vco2)) +
          (nitrogen_constant * data$nitrogen / 1440)
        
        data$recalc_rq <- data$recalc_vco2 / data$recalc_vo2
        
        # 0 vol eqns
        data$recalc_vo2_0vol <- 10/1000 * (data$InflowRate * data$nulled_inflow_o2 - data$haldane_outflow_0vol * data$nulled_outflow_o2)
        data$recalc_vco2_0vol <- -10/1000 * (data$InflowRate * data$nulled_inflow_co2 - data$haldane_outflow_0vol * data$nulled_outflow_co2)
        
        data$recalc_ee_0vol <- ((vo2_constant * data$recalc_vo2_0vol +
                                   vco2_constant * data$recalc_vco2_0vol)) +
          (nitrogen_constant * data$nitrogen / 1440)
        
        data$recalc_rq_0vol <- data$recalc_vco2_0vol / data$recalc_vo2_0vol
      
      } else {
        stop("wrong expected equations for pull calorimeter")
      }
    } else if(grepl("Push - Convert to Pull", expected, ignore.case = TRUE)) {
      if(grepl("push", config, ignore.case = TRUE)) {
        message("using config: push, using expected: converted to pull")
        
        # Calculate 'Outflow Rate'
        OutflowRate = data$InflowRate + mfc$N2 + mfc$CO2
        
        # Pull Method
        VO2_exp <- mean((data$nulled_inflow_o2 * mfc$N2)/data$nulled_inflow_n2) * 1000
        VCO2_exp <- mean((mfc$CO2 * data$nulled_inflow_n2 - mfc$N2 * data$nulled_inflow_co2) / data$nulled_inflow_n2) * 1000
        
        # Recalc VO2 VCO2
        data$haldane_inflow <- ( OutflowRate * (data$nulled_outflow_n2/100) + (data$dn2/100) * volume) / (data$nulled_inflow_n2/100)
        data$haldane_inflow_0vol <- ( OutflowRate * (data$nulled_outflow_n2/100) ) / (data$nulled_inflow_n2/100)
     
        data$recalc_vo2 <- 10/1000 * (data$haldane_inflow * data$nulled_inflow_o2 - OutflowRate * data$nulled_outflow_o2 - data$do2 * volume)
        data$recalc_vco2 <- -10/1000 * (data$haldane_inflow * data$nulled_inflow_co2 - OutflowRate * data$nulled_outflow_co2 - data$dco2 * volume)
        
        data$recalc_ee <- ((vo2_constant * data$recalc_vo2 +
                              vco2_constant * data$recalc_vco2)) +
          (nitrogen_constant * data$nitrogen / 1440)
        
        data$recalc_rq <- data$recalc_vco2 / data$recalc_vo2
        
        # 0 vol eqns
        data$recalc_vo2_0vol <- 10/1000 *(data$haldane_inflow * data$nulled_inflow_o2 - OutflowRate * data$nulled_outflow_o2)
        data$recalc_vco2_0vol <- -10/1000 * (data$haldane_inflow * data$nulled_inflow_co2 - OutflowRate * data$nulled_outflow_co2)
        
        data$recalc_ee_0vol <- ((vo2_constant * data$recalc_vo2_0vol +
                                   vco2_constant * data$recalc_vco2_0vol)) +
          (nitrogen_constant * data$nitrogen / 1440)
        
        data$recalc_rq_0vol <- data$recalc_vco2_0vol / data$recalc_vo2_0vol
        
      } else {
        stop("wrong expected equations for pull calorimeter")
      }
    } else if(grepl("Pull - Normal", expected, ignore.case = TRUE)) {
      if(grepl("pull", config, ignore.case = TRUE)) {
        message("using config: pull, using expected: pull normal")
        
        # Pull Method
        VO2_exp <- mean((data$nulled_inflow_o2 * mfc$N2)/data$nulled_inflow_n2) * 1000
        VCO2_exp <- mean((mfc$CO2 * data$nulled_inflow_n2 - mfc$N2 * data$nulled_inflow_co2) / data$nulled_inflow_n2) * 1000
        
        # Recalc VO2 VCO2
        data$haldane_inflow <- ( data$OutflowRate * (data$nulled_outflow_n2/100) + (data$dn2/100) * volume) / (data$nulled_inflow_n2/100)
        data$haldane_inflow_0vol <- ( data$OutflowRate * (data$nulled_outflow_n2/100) ) / (data$nulled_inflow_n2/100)
        
        data$recalc_vo2 <- 10/1000 * (data$haldane_inflow * data$nulled_inflow_o2 - data$OutflowRate * data$nulled_outflow_o2 - data$do2 * volume)
        data$recalc_vco2 <- -10/1000 * (data$haldane_inflow * data$nulled_inflow_co2 - data$OutflowRate * data$nulled_outflow_co2 - data$dco2 * volume)
        
        data$recalc_ee <- ((vo2_constant * data$recalc_vo2 +
                              vco2_constant * data$recalc_vco2)) +
          (nitrogen_constant * data$nitrogen / 1440)
        
        data$recalc_rq <- data$recalc_vco2 / data$recalc_vo2
        
        # 0 vol eqns
        data$recalc_vo2_0vol <- 10/1000 *(data$haldane_inflow * data$nulled_inflow_o2 - data$OutflowRate * data$nulled_outflow_o2)
        data$recalc_vco2_0vol <- -10/1000 * (data$haldane_inflow * data$nulled_inflow_co2 - data$OutflowRate * data$nulled_outflow_co2)
        
        data$recalc_ee_0vol <- ((vo2_constant * data$recalc_vo2_0vol +
                                   vco2_constant * data$recalc_vco2_0vol)) +
          (nitrogen_constant * data$nitrogen / 1440)
        
        data$recalc_rq_0vol <- data$recalc_vco2_0vol / data$recalc_vo2_0vol
        
      } else {
        stop("wrong expected equations for push calorimeter")
      }
    } else {
      message("using approximate equations, please update settings")
      # Use old eqns
      # Expected values
      VO2_exp <- (mean( data$nulled_inflow_o2 * (mfc$CO2 + mfc$N2)) / 100) * 1000
      VCO2_exp <- mean(mfc$CO2) * 1000
    }
    
    EE_exp <- (vo2_exp * VO2_exp + vco2_exp * VCO2_exp)
    RQ_exp <- VCO2_exp / VO2_exp

    if(!grepl("ShortCircuit", tag_label)) {
        VO2_meas <- mean(data$recalc_vo2) * 1000
        VCO2_meas <- mean(data$recalc_vco2) * 1000
        EE_meas <- mean(data$recalc_ee) * 1000
        RQ_meas <- mean(data$recalc_rq)
    } else {
        VO2_meas <- mean(data$recalc_vo2_0vol) * 1000
        VCO2_meas <- mean(data$recalc_vco2_0vol) * 1000
        EE_meas <- mean(data$recalc_ee_0vol) * 1000
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
