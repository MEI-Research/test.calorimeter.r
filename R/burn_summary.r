process_cal_burn <- function(data, params, ...) {

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

    burn <- ret %>% burn_summary(params)

    ## add metadata to burn data.frame for return data
    
    burn$pt <- unique(data$calrq$pt)[1]
    burn$id <- sapply(1:nrow(burn)*0,uuid::UUIDgenerate)
      # system(paste0("uuid", " -v4", " -n", nrow(burn)), intern = TRUE)
    burn$timestamp <- format(burn$start_time, format = "%Y-%m-%dT%H:%M:%SZ")

    ret$haldane$pt <- unique(data$calrq$pt)[1]
    ret$haldane$id <- sapply(1:nrow(ret$haldane)*0,uuid::UUIDgenerate)
      
      #system(paste0("uuid", " -v4", " -n", nrow(ret$haldane)),
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

    rep_data <- list(burn = burn, haldane = ret$haldane,
                     event_tags = event_tags)

    ## generate the report
    base64_rep <- burn_report(rep_data, params, ...)
    
    # Save processing time
    tm <- as.POSIXlt(Sys.time(), "UTC", "%Y-%m-%dT%H:%M:%S")
    burn$Processed <- strftime(tm , "%Y-%m-%dT%H:%M:%S%z")
    ret$haldane$Processed <- strftime(tm , "%Y-%m-%dT%H:%M:%S%z")
    
    ## try only saving cols we need
    keep <- c("Time", "Processed", "haldane", "recalc_vo2", "recalc_vco2",
              "recalc_ee", "recalc_rq", "nulled_outflow_o2",
              "nulled_outflow_co2", "nulled_inflow_o2",
              "nulled_inflow_co2", "do2", "dco2", "inflow_rate",
              "outflow_rate", "id", "pt", "timestamp")


    ## return two datasets
    datasets <- list(burn = burn, haldane = ret$haldane[keep])
    files    <- list(burn = jsonlite::unbox(base64_rep))
    
    list(datasets = datasets, files = files)
}

burn_summary <- function(data, params) {
    # calrq <- data$haldane$datasets$haldane
    calrq <- data$haldane
    calrq <- calrq[order(calrq$Time), ]

    settings <- params$settings

    ## event_tags data comes in as YYYY-MM-DDTHH:MM:SSZ
    event_tags  <- data$event_tags

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
        tag_summary <- pilr.utils.r::fapply_event_tags(calrq, tag_table, compute_burn_summary,
                                       settings)
    }

    ret <- do.call(rbind, tag_summary)

    ## remove Null tag, burn weight will be 0 during null period, so
    ## there is no error. Concordia validation will fail if we include
    ## this without setting vco2_err and vo2_err to a
    ## value. Alternatively, let's just remove the row
    subset(ret, tag_label != "Null")
}

gas_constants <- list("[Ethanol]" =
                      list(lo2 = 1.460,
                           lco2 = 0.973,
                           RQ_exp = 0.67),
                      "[Methanol]" =
                      list(lo2 = 1.049,
                           lco2 = 0.700,
                           RQ_exp = 0.67),
                      "[Propane]" =
                      list(lo2 = 2.550,
                           lco2 = 1.530,
                           RQ_exp = 0.60))

compute_burn_summary <- function(data, tag_label, settings, ...) {
    vo2_constant <- 3.941
    vco2_constant <- 1.104

    gas <- pilr.utils.r::get_setting("gas", settings) %>% as.character
        
    ## compute scale weight summaries over the course of the burn
    weight_diff <- c(NA, diff(data$BurnWeight))
    total_weight_diff <- head(data$BurnWeight, n = 1) - tail(data$BurnWeight, n = 1)

    ## would like weights to be calculated minute by minute, current
    ## weight minus previous weight (so, diff in R). It is nice to be able
    ## to graph expected vs measured.

    ## user will need to be able to select the type of burn: ethanol,
    ## methanol, or propane. get the constants for their selection
    constants <- gas_constants[[gas]]

    ## expected for burns
    VO2_exp <- total_weight_diff * constants$lo2 / 1000
    VCO2_exp <- total_weight_diff * constants$lco2 / 1000
    EE_exp <- (vo2_constant * VO2_exp) + (vco2_constant * VCO2_exp)

    ## measured for burns
    VO2_meas <- sum(as.numeric(data$recalc_vo2), na.rm = TRUE) / 1000
    VCO2_meas <- sum(as.numeric(data$recalc_vco2), na.rm = TRUE) / 1000
    RQ_meas <- VCO2_meas / VO2_meas

    ## recovery measures
    VO2_err<- VO2_meas / VO2_exp * 100
    VCO2_err <- VCO2_meas / VCO2_exp * 100

    EE_meas <- sum(as.numeric(data$recalc_ee), na.rm = TRUE) / 1000
    
    EE_err <- (EE_meas  - EE_exp) / EE_exp * 100
    RQ_err <- (RQ_meas - constants$RQ_exp) / constants$RQ_exp * 100

    ret_df <- lapply(data.frame(vco2_meas = VCO2_meas,
                                vo2_meas = VO2_meas,
                                ee_meas = EE_meas,
                                rq_meas = RQ_meas,
                                vco2_exp = VCO2_exp,
                                vo2_exp = VO2_exp,
                                ee_exp = EE_exp,
                                rq_exp = constants$RQ_exp,
                                vco2_err = VCO2_err,
                                vo2_err = VO2_err,
                                ee_err = EE_err,
                                rq_err = RQ_err),
                     round, 4)

    ret_df$start_time <- as.character(min(data$Time))
    ret_df$end_time <- as.character(max(data$Time))

    ## null offsets, unique value
    ret_df$null_offset_o2 <- round(data$null_offset_O2[1], 2)
    ret_df$null_offset_co2 <- round(data$null_offset_CO2[1], 2)

    ## tag label
    ret_df$tag_label <- gsub("TT_", "", tag_label)
    
    as.data.frame(ret_df)
}
