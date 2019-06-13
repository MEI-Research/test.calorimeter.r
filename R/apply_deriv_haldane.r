## this is our entry point from pilr on file upload
#'@export
apply_haldane <- function(data, params, ...) {
  if (length(data$calrq) == 0)
  {
    stop("No data sent, check tags and period!")
  }
  haldane <- apply_null_offset(data, params) %>%
    apply_slope_offset(params) %>%
    deriv_haldane(params)
  
  # Save processing time
  tm <- as.POSIXlt(Sys.time(), "UTC", "%Y-%m-%dT%H:%M:%S")
  haldane$haldane$Processed <- strftime(tm , "%Y-%m-%dT%H:%M:%S%z")
  
  # Add Activity column from raw calrq data
  cal_seconds <-
    median(diff(as.POSIXlt(data$calrq$Time, format = "%Y-%m-%dT%H:%M:%SZ")))
  units(cal_seconds) <- "secs"
  cal_seconds <- as.numeric(cal_seconds)
  
  # Add Activity
  if (!is.null(data$calrq$Activity)) {
    haldane$haldane$Activity <- data$calrq$Activity
    haldane$haldane$Activity_Rate <-
      data$calrq$Activity / cal_seconds
  } else {
    haldane$haldane$Activity <- integer(length(haldane$haldane$Time))
  }
  
  # try only saving cols we need
  keep <-
    c(
      "Time",
      "Processed",
      "haldane",
      "recalc_vo2",
      "recalc_vco2",
      "recalc_vo2_0vol",
      "recalc_vco2_0vol",
      "haldane_outflow_0vol",
      "haldane_inflow_0vol",
      "OutflowRate",
      "InflowRate",
      "haldane_outflow",
      "haldane_inflow",
      "recalc_ee",
      "recalc_rq",
      "nulled_outflow_o2",
      "nulled_inflow_n2",
      "nulled_outflow_n2",
      "nulled_outflow_co2",
      "nulled_inflow_o2",
      "nulled_inflow_co2",
      "do2",
      "dco2",
      "dn2",
      "inflow_rate",
      "MFCFlow_1",
      "MFCFlow_2",
      "MFCFlow_3",
      "MFCFlow_4",
      "MFCFlow_5",
      "outflow_rate",
      "id",
      "pt",
      "timestamp",
      "nitrogen",
      "np_rq",
      "protein_ox",
      "cho_ox",
      "fat_ox",
      "Activity",
      "Activity_Rate"
    )
  
  # Save output datasets and files in list
  ret <-
    list(datasets = list(haldane = haldane$haldane[colnames(haldane$haldane) %in% keep]),
         files = list())
  
  ret
}

#'@export
deriv_haldane <- function(data, params, ...) {
  haldane <- data$sloped
  # aldane$Time <- as.POSIXlt(haldane$Time, format = "%Y-%m-%dT%H:%M:%SZ")
  # stop(haldane$Time)
  haldane <- haldane[order(haldane$Time), ]
  
  event_tags <- data$event_tags
  
  ## change to consistent names for this workunit
  haldane$nulled_outflow_o2 <- haldane$nulled_OutflowO2
  haldane$nulled_outflow_co2 <- haldane$nulled_OutflowCO2
  haldane$nulled_inflow_o2 <- haldane$nulled_InflowO2
  haldane$nulled_inflow_co2 <- haldane$nulled_InflowCO2
  haldane$nulled_inflow_n2 <-
    100 - haldane$nulled_inflow_o2 - haldane$nulled_inflow_co2
  haldane$nulled_outflow_n2 <-
    100 - haldane$nulled_outflow_o2 - haldane$nulled_outflow_co2
  
  ## make sure all are non-missing (NA, NaN)
  ## per convo with erica, check that these are all non NA
  if (length(haldane$InflowRate)) {
    haldane$inflow_rate <- haldane$InflowRate
  } else {
    haldane$inflow_rate <- 0
  }
  
  if (length(haldane$OutflowRate)) {
    haldane$outflow_rate <- haldane$OutflowRate
  } else {
    haldane$outflow_rate <- 0
  }
  
  
  
  config <- pilr.utils.r::get_setting("configuration",
                                      params$settings)
  cal_volume <- pilr.utils.r::get_setting("volume",
                                          params$settings) %>%
    pilr.utils.r::safe_numeric()
  
  cal_seconds <-
    median(diff(as.POSIXlt(haldane$Time, format = "%Y-%m-%dT%H:%M:%SZ")))
  units(cal_seconds) <- "secs"
  cal_seconds <- as.numeric(cal_seconds)
  
  deriv_window <-
    pilr.utils.r::get_setting("deriv_window", params$settings,
                              required = FALSE) %>%
    pilr.utils.r::safe_numeric()
  
  deriv_window <- ifelse(is.na(deriv_window), 8, deriv_window)
  
  ## calc derivatives and haldane
  haldane$do2 <- derivative(
    haldane$nulled_outflow_o2,
    data_interval = cal_seconds,
    derivative_window = deriv_window
  )
  
  haldane$dco2 <- derivative(haldane$nulled_outflow_co2,
                             data_interval = cal_seconds)
  
  haldane$dn2 = -haldane$do2 - haldane$dco2
  
  ## stop(paste(capture.output(summary(haldane)), collapse = "\n"))
  
  ## Create data frame of N2 values and date_time
  if (pilr.utils.r::has_setting("multiple_n2", params$settings)) {
    if (!is.na(pilr.utils.r::get_setting("multiple_n2", params$settings)))
    {
      ## Interpret N2 array variable JSON
      from_json <- fromJSON(params$settings$multiple_n2$value[[1]])
      
      n2_df <- data.frame(date = c(),
                          time = c(),
                          value = c())
      default_N2 <- FALSE
      avg_N2 <- TRUE
      
      if (!(pilr.utils.r::has_setting("urine_nitrogen_start_time", params$settings))) {
        # (is.null(params$settings$urine_nitrogen_start_time$value[[1]])) {
        
        # use avging method if start time is present
        avg_N2 <- FALSE
      }
      
      # Check if N2 values are empty
      if (is.na(as.data.frame(from_json[1, 1])[[2, 5]])) {
        # no value set for n2
        default_N2 <- TRUE
        n2_df <-
          data.frame(
            datetime = as.POSIXct("2012-01-01 01:00:00", format = "%Y-%m-%d %H:%M:%S"),
            value = 0
          )
        message('No value set for N2, using 0')
      } else {
        # Build array of N2 values
        for (i in 1:nrow(from_json)) {
          temp <- as.data.frame(from_json[i, 1])
          if (avg_N2) {
            # Use start value for first iteration
            if (i == 1) {
              t1 <-
                as.POSIXct(
                  pilr.utils.r::get_setting("urine_nitrogen_start_time", params$settings),
                  format = "%Y-%m-%dT%H:%M:%SZ"
                )
              # as.POSIXct(params$settings$urine_nitrogen_start_time$value[[1]],
              #            format = "%Y-%m-%dT%H:%M:%SZ")
            } else{
              t1 <-
                as.POSIXct(paste(
                  as.Date(from_json[i - 1, 1][[1]]$value[[2]]),
                  from_json[i -
                              1, 1][[1]]$value[[3]]
                ), format = "%Y-%m-%d %H:%M:%S")
            }
            t2 <-
              as.POSIXct(paste(as.Date(temp$value[[2]]), temp$value[[3]]), format = "%Y-%m-%d %H:%M:%S")
            d_time = as.numeric(difftime(t2, t1, units = "mins"))
            if (d_time < 0) {
              stop('negative delta T for urinary N2, check dates: ',
                   t1,
                   ' to ',
                   t2)
            }
            # Convert static value to average by minutes
            n2_avg <- temp$value[[1]] / d_time
            n2_df <-
              rbind(n2_df,
                    data.frame(
                      datetime = as.POSIXct(paste(
                        as.Date(temp$value[[2]]),
                        temp$value[[3]]
                      ), format =
                        "%Y-%m-%d %H:%M:%S"),
                      value = n2_avg
                    ))
          } else {
            n2_df <-
              rbind(n2_df,
                    data.frame(
                      datetime = as.POSIXct(paste(
                        as.Date(temp$value[[2]]),
                        temp$value[[3]]
                      ), format =
                        "%Y-%m-%d %H:%M:%S"),
                      value = temp$value[[1]]
                    ))
          }
        }
      }
    } else if (pilr.utils.r::has_setting("urine_nitrogen", params$settings)) {
      n2_val <-
        pilr.utils.r::get_setting("urine_nitrogen", params$settings)
      n2_df <-
        data.frame(
          datetime = as.POSIXct("2012-01-01 01:00:00", format = "%Y-%m-%d %H:%M:%S"),
          value = n2_val
        )
    } else {
      n2_df <- -1
    }
  } else {
    n2_df <- -1
  }
  
  # Convert from 24h to minutely
  # n2_df$value <- n2_df$value / (60*24)
  
  if (grepl("push", config, ignore.case = TRUE)) {
    message("using config: push")
    ret <-
      calc_push(haldane, cal_volume, cal_seconds, n2_df = n2_df)
  } else {
    message("using config: pull")
    ret <-
      calc_pull(haldane, cal_volume, cal_seconds, n2_df = n2_df)
  }
  
  list(haldane = ret, event_tags = event_tags)
}


#'@export
derivative <- function(x,
                       data_interval,
                       derivative_window = 8,
                       average_points = 1) {
  # Calculate derivative using linear interpolation
  # Initialize
  dVector <- rep(0, length(x))
  for (i in 1:length(x)) {
    # Set d/dt to zero if there are not enough points
    if (i > derivative_window / 2 &
        i < length(x) - (derivative_window / 2) & !all(is.na(x)))
    {
      # Make fit array
      fitdata <-
        list(data = x[(i - derivative_window / 2):(i + derivative_window / 2)],
             time = (1:(derivative_window + 1)) * (data_interval / 60))
      # Calculate fit
      dVector[i] = lm(formula = data ~ time, fitdata, na.action = na.exclude)$coefficients[2]
    } else
    {
      # Not enough points -> set to zero
      dVector[i] <- 0
    }
  }
  # Convert to time series to match old implementation
  dVector <- as.ts(dVector)
  
  dVector[is.na(dVector)] <- 0
  dVector
}

vo2_constant <- 3.941
vco2_constant <- 1.104
nitrogen_constant <- -2.17

calc_push <- function(data, volume, cal_seconds, n2_df) {
  if (!length(data$InflowRate)) {
    stop("InflowRate not present, is calorimeter configuration setting correct?")
  }
  
  ## Add nitrogen column; datetime is start datetime for each collection
  if (n2_df == -1) {
    data$nitrogen <- 0
  }
  else if (!length(n2_df)) {
    stop("N2 Values not set in participant variables")
  }
  else {
    data$nitrogen <- n2_df$value[1]
    if (length(n2_df) > 1) {
      for (i in 2:nrow(n2_df)) {
        data$nitrogen[as.POSIXct(data$timestamp, format = "%Y-%m-%dT%H:%M:%SZ") >= n2_df$datetime[i]] <-
          n2_df$value[i]
      }
    }
  }
  
  data$haldane_outflow <-
    (data$InflowRate * (data$nulled_inflow_n2 / 100) - (data$dn2 / 100) * volume) / (data$nulled_outflow_n2 /
                                                                                       100)
  
  ## calc VO2 and VCO2 in ml/min
  data$recalc_vo2 <-
    10 / 1000 * (
      data$InflowRate * data$nulled_inflow_o2 - data$haldane_outflow * data$nulled_outflow_o2 - data$do2 * volume
    )
  
  data$recalc_vco2 <-
    -10 / 1000 * (
      data$InflowRate * data$nulled_inflow_co2 - data$haldane_outflow * data$nulled_outflow_co2 - data$dco2 * volume
    )
  
  data$recalc_ee <- ((
    vo2_constant * data$recalc_vo2 +
      vco2_constant * data$recalc_vco2
  )) +
    (nitrogen_constant * data$nitrogen / 1440)
  
  data$recalc_rq <- data$recalc_vco2 / data$recalc_vo2
  
  ## add these in so they are available for short circuit infusions
  ## they are the same calculations for recalc_vo2 and recalc_vco2,
  ## except for the term involving volume
  data$haldane_outflow_0vol <-
    (data$InflowRate * (data$nulled_inflow_n2 / 100)) / (data$nulled_outflow_n2 /
                                                           100)
  
  data$recalc_vo2_0vol <-
    10 / 1000 * (
      data$InflowRate * data$nulled_inflow_o2 - data$haldane_outflow_0vol * data$nulled_outflow_o2
    )
  
  data$recalc_vco2_0vol <-
    -10 / 1000 * (
      data$InflowRate * data$nulled_inflow_co2 - data$haldane_outflow_0vol * data$nulled_outflow_co2
    )
  
  data$recalc_ee_0vol <- ((
    vo2_constant * data$recalc_vo2_0vol +
      vco2_constant * data$recalc_vco2_0vol
  )) +
    (nitrogen_constant * data$nitrogen / 1440)
  
  data$recalc_rq_0vol <-
    data$recalc_vco2_0vol / data$recalc_vo2_0vol
  
  ## Minute by minute npRQ calculation
  data$np_rq <-
    (data$recalc_vco2 - (4.97 * data$nitrogen)) / (data$recalc_vo2 - (5.95 * data$nitrogen))
  
  ## Minute by minute ProOx calculation
  data$protein_ox <- (data$nitrogen * 6.26) / 0.966
  
  ## Minute by minute ChoOx/FatOx calculation
  data$cho_ox <-
    (4.113 * data$recalc_vco2) - (2.907 * data$recalc_vo2) -
    (0.375 * data$protein_ox)
  
  data$fat_ox <-
    (1.689 * data$recalc_vo2) - (1.689 * data$recalc_vco2) -
    (0.324 * data$protein_ox)
  
  data
}

calc_pull <- function(data, volume, cal_seconds, n2_df) {
  if (!length(data$OutflowRate)) {
    stop("OutflowRate not present, is calorimeter configuration setting correct?")
  }
  
  ## Add nitrogen column; datetime is start datetime for each collection
  if (!length(n2_df)) {
    stop("N2 Values not set in participant variables")
  } else {
    data$nitrogen <- n2_df$value[1]
    if (length(n2_df) > 1) {
      for (i in 2:nrow(n2_df)) {
        data$nitrogen[data$timestamp >= n2_df$datetime[i]] <- n2_df$value[i]
      }
    }
  }
  data$haldane_inflow <-
    (data$OutflowRate * (data$nulled_outflow_n2 / 100) + (data$dn2 / 100) * volume) / (data$nulled_inflow_n2 /
                                                                                         100)
  
  data$recalc_vo2 <-
    10 / 1000 * (
      data$haldane_inflow * data$nulled_inflow_o2 - data$OutflowRate * data$nulled_outflow_o2 - data$do2 * volume
    )
  
  data$recalc_vco2 <-
    -10 / 1000 * (
      data$haldane_inflow * data$nulled_inflow_o2 - data$OutflowRate * data$nulled_outflow_o2 - data$dco2 * volume
    )
  
  ## calc VO2 and VCO2 in ml/min
  # Old version of equations
  #data$recalc_vo2 <- (data$OutflowRate * (data$nulled_inflow_o2 * data$haldane -
  #                                          data$nulled_outflow_o2) -
  #                      volume * data$do2) / 100
  #
  #data$recalc_vco2 <- (data$OutflowRate *
  #                       (-1 * data$nulled_inflow_co2 * data$haldane +
  #                          data$nulled_outflow_co2) +
  #                       volume * data$dco2) / 100
  #
  ## already divided by 1000
  data$recalc_ee <- ((
    vo2_constant * data$recalc_vo2 +
      vco2_constant * data$recalc_vco2
  )) +
    (nitrogen_constant * data$nitrogen / 1440)
  
  data$recalc_rq <- data$recalc_vco2 / data$recalc_vo2
  
  ## add these in so they are available for short circuit infusions
  ## they are the same calculations for recalc_vo2 and recalc_vco2,
  ## except for the term involving volume
  
  data$recalc_vo2 <-
    10 * (
      data$haldane_inflow * data$nulled_inflow_o2 - data$OutflowRate * data$nulled_outflow_o2
    )
  
  data$recalc_vco2 <-
    -10 * (
      data$haldane_inflow * data$nulled_inflow_o2 - data$OutflowRate * data$nulled_outflow_o2
    )
  
  ## already divided by 1000
  data$recalc_ee_0vol <- ((
    vo2_constant * data$recalc_vo2_0vol +
      vco2_constant * data$recalc_vco2_0vol
  )) +
    (nitrogen_constant * data$nitrogen / 1440)
  
  data$recalc_rq_0vol <-
    data$recalc_vco2_0vol / data$recalc_vo2_0vol
  
  data
}
