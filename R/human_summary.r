## this is our entry point from opencpu
#'@export
#'@import jsonlite dplyr ggvis uuid
process_cal_human <- function(data, params, ...) { 

  message('Starting human summary job')
  
  event_tags  <- data$event_tags
  
  ## workaround bug in event tag data where sub-second accuracy is
  ## given
  data$event_tags$start_time <- ifelse(nchar(event_tags$start_time) == 24,
                                       paste0(substr(event_tags$start_time, 1, 19), "Z") ,
                                       event_tags$start_time)
  
  data$event_tags$end_time <- ifelse(nchar(event_tags$end_time) == 24,
                                     paste0(substr(event_tags$end_time, 1, 19), "Z") ,
                                     event_tags$end_time)
  
  
  if(missing(params)) {
    params <- list()
  }
  
  if(!length(params$settings)) {
    stop("No settings were received.")
  }
  
  if(!length(data$event_tags)) {
    stop("No event tags dataset was received. Are event tags set for this participant?")
  }
  
  if(!length(data$calrq$Time)) {
    stop("No data sent, check tags and period!")
  }
  
  if (pilr.utils.r::has_setting("equation",params$settings)) {
    equation <- pilr.utils.r::get_setting("equation",
                                          params$settings)  
  } else {
    # Default for old projects
    equation <- "Updated"
    # equation <- "Old"
  }
  
  if (equation == "Updated" || equation == "[Updated]"){
    message("Using updated equations")
    ret <- data %>% apply_null_offset(params) %>%
      apply_slope_offset(params) %>%
      deriv_haldane(params)
  } else {
    message("Using old equations")
    ret <- data %>% apply_null_offset(params) %>%
      apply_slope_offset(params) %>%
      deriv_haldane_old(params)
  }
  
  # Fix missing cols
  if (is.null(data$calrq$Activity))
  {
    data$calrq$Activity <- integer(length(data$calrq$Time))
  }
  
  ## Organize each visit into a list
  visits <- c(tags = c())
  visitIndex <- which(event_tags$tags %in% "Human Study")
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
    
    # Run human summary and merge each visit result
    if (i == 1) {
      human <- ret_sub %>% human_summary(params)
    }
    else {
      human <- rbind(human, ret_sub %>% human_summary(params))
    }
  }
  
  
  ## add metadata to human data.frame for return data, how are we
  ## going to do this in general?
  
  human$pt <- unique(data$calrq$pt)[1]
  human$id <- sapply(1:nrow(human)*0,uuid::UUIDgenerate)
  human$timestamp <- format(human$start_time, format = "%Y-%m-%dT%H:%M:%SZ")
  
  
  ret$haldane$pt <- unique(data$calrq$pt)[1]
  
  ret$haldane$id <- sapply(1:nrow(ret$haldane)*0,uuid::UUIDgenerate)
  ret$haldane$timestamp <- format(ret$haldane$Time, format = "%Y-%m-%dT%H:%M:%SZ")
  
  
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
  
  ## the report needs the human summary and haldane data
  rep_data <- list(human = human, haldane = ret$haldane,
                   event_tags = event_tags)
  
  ## we need to throw in the tag_table on params for the plots
  base64_rep <- human_report(rep_data, params, ...)
  
  # Save processing time
  tm <- as.POSIXlt(Sys.time(), "UTC", "%Y-%m-%dT%H:%M:%S")
  human$Processed <- strftime(tm , "%Y-%m-%dT%H:%M:%S%z")
  ret$haldane$Processed <- strftime(tm , "%Y-%m-%dT%H:%M:%S%z")
  
  ## try only saving cols we need
  keep <- c("Time", "Processed", "recalc_vo2", "recalc_vco2",
            "recalc_ee", "recalc_rq", "nulled_outflow_o2",
            "nulled_outflow_co2", "nulled_inflow_o2",
            "nulled_inflow_co2", "do2", "dco2", "inflow_rate",
            "outflow_rate", "id", "pt", "timestamp", "nitrogen",
            "np_rq", "protein_ox", "cho_ox", "fat_ox", "haldane")
  
  
  ## return two datasets
  datasets <- list(human = human, haldane = ret$haldane[keep %in% colnames(ret$haldane)])
  files    <- list(human = jsonlite::unbox(base64_rep))
  
  list(datasets = datasets, files = files)
}

#'@export
human_summary <- function(data, params, ...) {
  calrq <- data$haldane
  calrq <- calrq[order(calrq$Time) , ]
  
  ## obtain input objects
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
  
  
  ## add new sleep event_tags for special activity analysis
  if(pilr.utils.r::has_timetag("Sleep", event_tags)) {
    message("Sleep event tag found.")
    sleep_times <- subset(event_tags, tags == "Sleep")
    sleep_times$tags <- "SleepNoActivity"
    
    ## workaround bug in event tag data where 'pt' field is
    ## present in some data fields.
    names(sleep_times) <- ifelse(names(sleep_times) == "pt.1", "pt", names(sleep_times))
    event_tags <- rbind(event_tags, sleep_times)
  }
  
  event_tags <- event_tags[order(event_tags$start_time), ]
  tag_table <- pilr.utils.r::apply_event_tags(calrq$Time, event_tags)
  
  ## verify that we have a Human Study 
  if(!any(grepl("Human Study", event_tags$tags))) {
    stop("Use the event tag editor to tag a 'Human Study' event.")
  }
  
  calrq <- pilr.utils.r::subset_event_tags("HumanStudy", calrq, tag_table)
  tag_table <- subset(tag_table, TT_HumanStudy == TRUE)
  
  ## only keep Human Study times. This avoids an issue where
  ## non-human study times woule be classified as non-excercise
  ## times below
  
  ## all_human_tags <- unique(grep("Human Study", event_tags$tags, value = TRUE))
  ## all_human_tags <- paste0("TT_", gsub(" ", "", all_human_tags))
  
  ## log_list <- lapply(all_human_tags, function(x) {
  ##     tag_table[, x] == TRUE
  ## })
  
  ## if(length(log_list) > 1) {
  ##     calrq <- calrq[tag_table[[do.call(`&`, log_list)]], ]
  ##     tag_table <- tag_table[do.call(`&`, log_list), , drop = FALSE]
  ## } else {
  ##     calrq <- calrq[tag_table[[log_list[[1]]]], ]
  ##     tag_table <- tag_table[log_list[[1]], , drop = FALSE] 
  ## }
  
  ## add in the Valid data tag, and subset appropriately
  if(any(event_tags$tags == "Invalid Data")) {
    tag_table$TT_ValidData <- !tag_table$TT_Invalid
    calrq <- pilr.utils.r::subset_event_tags("ValidData", calrq, tag_table)
    tag_table <- subset(tag_table, TT_ValidData == TRUE)
    
    ## now we can remove both valid and invalid cols
    tag_table <-
      tag_table[!names(tag_table) %in%
                  c("TT_ValidData", grep("TT_InvalidData", names(tag_table), value = TRUE))]
  }
  
  
  ## add in the non-exercise tag. this is nice but maybe a function
  ## with good name to carry this out?
  if(length(tag_table$TT_Exercise)) {
    tag_table$TT_NonExercise <- !tag_table$TT_Exercise
  }
  
  tag_labels <- pilr.utils.r::list_event_tags(event_tags)
  
  if(length(tag_labels) > 0) {
    tag_summary <- pilr.utils.r::fapply_event_tags(calrq, tag_table,
                                                   compute_human_summary,
                                                   settings)
  }
  
  ret <- do.call(rbind, tag_summary)
  
  ## remove Null tag
  ret <- subset(ret, tag_label != "Null")
  
  # Interpret rest duration array variable JSON
  #from_json <- fromJSON(params$settings$rest_durations$value[[1]])
  
  if (pilr.utils.r::has_setting("rest_durations",params$settings)) {
    from_json <- pilr.utils.r::get_setting("rest_durations",params$settings)
    from_json <- fromJSON(from_json)
    
    # Create data frame of rest durations
    rest_df <- data.frame(post_meal = c(), rest = c())
    for (i in 1:length(from_json)) {
      temp <- as.data.frame(from_json[i,1])
      rest_df <- rbind(rest_df, data.frame(post_meal = temp$value[[1]],
                                           rest = temp$value[[2]]))
    }
  } else {
    # Create value for check later
    rest_df <- data.frame(0)
  }
  # rest_df <- data.frame(1)
  # rest_df$post_meal <- 360
  # rest_df$rest <- 1080
  # rest_df <- rest_df[2:3]
  
  ret$tef1 <- NA
  meal_count <- 1
  
  ## get MR for THREE post meals
  if("Post Meal 1" %in% event_tags$tags & "Rest" %in% event_tags$tags) {
    
    
    ree <- compute_human_summary(pilr.utils.r::subset_event_tags("Rest", calrq, tag_table),
                                 "Rest", settings, do_tf_correct = FALSE)
    
    mr_pre <- ree$mr * rest_df$rest[meal_count]  #18 hours
    
    mr_post  <- compute_human_summary(pilr.utils.r::subset_event_tags("PostMeal1", calrq, tag_table),
                                      "PostMeal1", settings,
                                      do_tf_correct = FALSE)$mr * rest_df$post_meal[meal_count]  #6 hours
    
    ee_pre <- ree$ee            
    # ret$tef_measured <- (mr_pre + mr_post) / (ee_pre * 1440 / ree$minutes)
    ret$tef_measured <- ((mr_pre * 360) + (mr_post * 1080)) / (ree$minutes)
    
    ret$tef1[ret$tag_label == "PostMeal1"] <- ret$tef_measured[1]
    
  }
  if (nrow(rest_df) >= 2) {meal_count <- 2}
  
  ## get MR for post
  if("Post Meal 2" %in% event_tags$tags & "Rest" %in% event_tags$tags) {
    
    ree <- compute_human_summary(pilr.utils.r::subset_event_tags("Rest", calrq, tag_table),
                                 "Rest", settings, do_tf_correct = FALSE)
    
    mr_pre <- ree$mr * rest_df$rest[meal_count]  #18 hours
    
    mr_post  <- compute_human_summary(pilr.utils.r::subset_event_tags("PostMeal2", calrq, tag_table),
                                      "PostMeal2", settings,
                                      do_tf_correct = FALSE)$mr * rest_df$post_meal[meal_count]  #6 hours
    
    ee_pre <- ree$ee               
    # ret$tef_measured <- (mr_pre + mr_post) / (ee_pre * 1440 / ree$minutes)
    ret$tef_measured <- ((mr_pre * 360) + (mr_post * 1080)) / (ree$minutes)
    ret$tef1[ret$tag_label == "PostMeal2"] <- ret$tef_measured[1]
    
  }
  if (nrow(rest_df) >= 3) {meal_count <- 3}
  
  ## get MR for post
  if("Post Meal 3" %in% event_tags$tags & "Rest" %in% event_tags$tags) {
    
    ree <- compute_human_summary(pilr.utils.r::subset_event_tags("Rest", calrq, tag_table),
                                 "Rest", settings, do_tf_correct = FALSE)
    
    mr_pre <- ree$mr * rest_df$rest[meal_count]  #18 hours
    
    mr_post  <- compute_human_summary(pilr.utils.r::subset_event_tags("PostMeal3", calrq, tag_table),
                                      "PostMeal3", settings,
                                      do_tf_correct = FALSE)$mr * rest_df$post_meal[meal_count]  #6 hours
    
    ee_pre <- ree$ee               
    # ret$tef_measured <- (mr_pre + mr_post) / (ee_pre * 1440 / ree$minutes)
    ret$tef_measured <- ((mr_pre * 360) + (mr_post * 1080)) / (ree$minutes)
    ret$tef1[ret$tag_label == "PostMeal3"] <- ret$tef_measured[1]
  }
  
  ## get MR for post
  if("Post Meal" %in% event_tags$tags & "Rest" %in% event_tags$tags) {
    ree <- compute_human_summary(pilr.utils.r::subset_event_tags("Rest", calrq, tag_table),
                                 "Rest", settings, do_tf_correct = FALSE)
    
    mr_pre <- ree$mr * 1080  #18 hours
    
    mr_post  <- compute_human_summary(pilr.utils.r::subset_event_tags("PostMeal", calrq, tag_table),
                                      "PostMeal", settings,
                                      do_tf_correct = FALSE)$mr * 360  #6 hours
    
    ee_pre <- ree$ee               
    # ret$tef_measured <- (mr_pre + mr_post) / (ee_pre * 1440 / ree$minutes)
    ret$tef_measured <- ((mr_pre * 360) + (mr_post * 1080)) / (ree$minutes)
    ret$tef1 <- ret$tef_measured
    ## Note: remove tef_measured when instrument dataset def updated
  } else {
    ret$tef_measured <- 0
    ret$tef1 <- ret$tef_measured
    ## Note: remove tef_measured when instrument dataset def updated
  }
  
  ## add regression intercept
  ## but only do this if we have enough datapoints (31)
  ret$tef2 <- NA
  ret$rmr_intercept <- NA
  ret$rmr_slope <- NA
  
  if(nrow(calrq) >= 30) {
    # Calculate regression/tef2 for entire study
    if("Human Study" %in% event_tags$tags) {
      
      rmr_data <- pilr.utils.r::subset_event_tags("HumanStudy", calrq, tag_table)
      
      sampling_seconds <- pilr.utils.r::get_setting("read_interval",
                                                    params$settings) %>%
        pilr.utils.r::safe_numeric()
      
      fm1 <- calculate_rmr(rmr_data, sampling_seconds)
      ei_meas <- pilr.utils.r::get_setting("energy_intake_measured", settings,
                                           required = FALSE) %>%
        pilr.utils.r::safe_numeric()
      ei_meas <- ifelse(is.na(ei_meas), 0, ei_meas)
      
      ## only want to add this to the Human Study row
      ret$rmr_intercept[ret$tag_label == "HumanStudy"] <- coef(fm1)[1]
    
      ## add the adjusted rmr intercept
      ## What to return if ei_meas is 0, I suppose Inf, but is that an error?
      sleep <- subset(ret, tag_label == "Sleep")
      if(nrow(sleep)) {
        ## which ee to use? 
        tef2 <- (((ret$rmr_intercept -
                     (sleep$ee / 1440)) / ei_meas ) *
                   (15 * 60) * 100)
        
        ## might have divided by 0 above with ei_meas if the
        ## setting wasn't present
        ret$tef2[ret$tag_label == "HumanStudy"] <- ifelse(!is.finite(tef2), 0, tef2)
      } else {
        ret$tef2[ret$tag_label == "HumanStudy"] <- 0
      }
      
      ret$rmr_slope[ret$tag_label == "HumanStudy"] <- coef(fm1)[2]
    }
    ## For each meal calculate rmr_intercept and then tef2
    if("Regression 1" %in% event_tags$tags) {
      
      rmr_data <- pilr.utils.r::subset_event_tags("Regression1", calrq, tag_table)
      
      sampling_seconds <- pilr.utils.r::get_setting("read_interval",
                                                    params$settings) %>%
        pilr.utils.r::safe_numeric()
      
      fm1 <- calculate_rmr(rmr_data, sampling_seconds)
      ei_meas <- pilr.utils.r::get_setting("energy_intake_measured", settings,
                                           required = FALSE) %>%
        pilr.utils.r::safe_numeric()
      ei_meas <- ifelse(is.na(ei_meas), 0, ei_meas)
      
      ret$rmr_intercept[ret$tag_label == "Regression1"] <- coef(fm1)[1]
      
      
      
      ## add the adjusted rmr intercept
      ## What to return if ei_meas is 0, I suppose Inf, but is that an error?
      sleep <- subset(ret, tag_label == "Sleep")
      if(nrow(sleep)) {
        ## which ee to use? 
        tef2 <- (((ret$rmr_intercept -
                     (sleep$ee / 1440)) / ei_meas ) *
                   (15 * 60) * 100)
        ## might have divided by 0 above with ei_meas if the
        ## setting wasn't present
        ret$tef2[ret$tag_label == "Regression1"] <- ifelse(!is.finite(tef2), 0, tef2)
      } else {
        ret$tef2[ret$tag_label == "Regression1"] <- 0
      }
      
      ret$rmr_slope[ret$tag_label == "Regression1"] <- coef(fm1)[2]
    }
    if("Regression 2" %in% event_tags$tags) {
      
      rmr_data <- pilr.utils.r::subset_event_tags("Regression2", calrq, tag_table)
      
      sampling_seconds <- pilr.utils.r::get_setting("read_interval",
                                                    params$settings) %>%
        pilr.utils.r::safe_numeric()
      
      fm1 <- calculate_rmr(rmr_data, sampling_seconds)
      ei_meas <- pilr.utils.r::get_setting("energy_intake_measured", settings,
                                           required = FALSE) %>%
        pilr.utils.r::safe_numeric()
      ei_meas <- ifelse(is.na(ei_meas), 0, ei_meas)
      
      ret$rmr_intercept[ret$tag_label == "Regression2"] <- coef(fm1)[1]
      
      
      
      ## add the adjusted rmr intercept
      ## What to return if ei_meas is 0, I suppose Inf, but is that an error?
      sleep <- subset(ret, tag_label == "Sleep")
      if(nrow(sleep)) {
        ## which ee to use? 
        tef2 <- (((ret$rmr_intercept -
                     (sleep$ee / 1440)) / ei_meas ) *
                   (15 * 60) * 100)
        ## might have divided by 0 above with ei_meas if the
        ## setting wasn't present
        ret$tef2[ret$tag_label == "Regression2"] <- ifelse(!is.finite(tef2), 0, tef2)
      } else {
        ret$tef2[ret$tag_label == "Regression2"] <- 0
      }
      
      ret$rmr_slope[ret$tag_label == "Regression2"] <- coef(fm1)[2]
    }
    if("Regression 3" %in% event_tags$tags) {
      
      rmr_data <- pilr.utils.r::subset_event_tags("Regression3", calrq, tag_table)
      
      sampling_seconds <- pilr.utils.r::get_setting("read_interval",
                                                    params$settings) %>%
        pilr.utils.r::safe_numeric()
      
      fm1 <- calculate_rmr(rmr_data, sampling_seconds)
      ei_meas <- pilr.utils.r::get_setting("energy_intake_measured", settings,
                                           required = FALSE) %>%
        pilr.utils.r::safe_numeric()
      ei_meas <- ifelse(is.na(ei_meas), 0, ei_meas)
      
      ret$rmr_intercept[ret$tag_label == "Regression3"] <- coef(fm1)[1]
      
      
      
      ## add the adjusted rmr intercept
      ## What to return if ei_meas is 0, I suppose Inf, but is that an error?
      sleep <- subset(ret, tag_label == "Sleep")
      if(nrow(sleep)) {
        ## which ee to use? 
        tef2 <- (((ret$rmr_intercept -
                     (sleep$ee / 1440)) / ei_meas ) *
                   (15 * 60) * 100)
        ## might have divided by 0 above with ei_meas if the
        ## setting wasn't present
        ret$tef2[ret$tag_label == "Regression3"] <- ifelse(!is.finite(tef2), 0, tef2)
      } else {
        ret$tef2[ret$tag_label == "Regression3"] <- 0
      }
      
      ret$rmr_slope[ret$tag_label == "Regression3"] <- coef(fm1)[2]
    }
    
    # if (pilr.utils.r::has_timetag("Regression Data", event_tags)) {
    #   rmr_data <- pilr.utils.r::subset_event_tags("RegressionData", calrq, tag_table)
    # } else {
    #   rmr_data <- pilr.utils.r::subset_event_tags("HumanStudy", calrq, tag_table)
    # }
    # 
    #  sampling_seconds <- pilr.utils.r::get_setting("read_interval",
    #                                               params$settings) %>%
    #   pilr.utils.r::safe_numeric()
    # 
    #  # Fix when no activity column present
    #  act_test <- rmr_data$Activity
    #  if(is.null(act_test)){
    #    rmr_data$Activity <- 0
    #  }
    # 
    # fm1 <- calculate_rmr(rmr_data, sampling_seconds)
    # ei_meas <- pilr.utils.r::get_setting("energy_intake_measured", settings,
    #                                      required = FALSE) %>%
    #   pilr.utils.r::safe_numeric()
    # ei_meas <- ifelse(is.na(ei_meas), 0, ei_meas)
    # 
    # ## only want to add this to the Human Study row
    # ret$rmr_intercept <- ifelse(ret$tag_label == "HumanStudy",
    #                             coef(fm1)[1], 0)
    # 
    # ## add the adjusted rmr intercept
    # ## What to return if ei_meas is 0, I suppose Inf, but is that an error?
    # sleep <- subset(ret, tag_label == "Sleep")
    # if(nrow(sleep)) {
    #   ## which ee to use? 
    #   ret$tef2 <- (((ret$rmr_intercept -
    #                    (sleep$ee / 1440)) / ei_meas ) *
    #                  (15 * 60) * 100)
    #   ## might have divided by 0 above with ei_meas if the
    #   ## setting wasn't present
    #   ret$tef2 <- ifelse(!is.finite(ret$tef2), 0, ret$tef2)
    # } else {
    #   ret$tef2 <- 0
    # }
    # 
    # ret$rmr_slope <- coef(fm1)[2]
    
  } else {
    ## not enough datapoints to fit regression
    ret$rmr_intercept <- 0
    ret$rmr_slope <- 0
    ret$tef2 <- 0
  }
  
  ## Add notes to dataset
  ret$note <- ""
  for (i in 1:nrow(ret)) {
    for (j in 1:nrow(event_tags)) {
      if ((abs(difftime(as.POSIXlt(event_tags$start_time[j], format = "%Y-%m-%dT%H:%M:%SZ"), as.POSIXlt(ret$start_time[i], format = "%Y-%m-%dT%H:%M:%SZ"), units="secs")) <= 60) &&
          (abs(difftime(as.POSIXlt(event_tags$end_time[j], format = "%Y-%m-%dT%H:%M:%SZ"), as.POSIXlt(ret$end_time[i], format = "%Y-%m-%dT%H:%M:%SZ"), units="secs")) <= 60)) {
        ret$note[i] <- event_tags$note[j]
      }
    }
  }
  ## Set TEF2 to NA for unused tags
  ret$tef2[ret$tef2==0] <- NA
  
  ret
}

#'@export
compute_human_summary <- function(data, tag_label, settings,
                                  do_tf_correct = TRUE) {
  ##3
  if(length(grep("TT_SleepNoActivity",tag_label)) != 0) {
    data <- subset(data, Activity <= pilr.utils.r::get_setting("sleep_threshold", settings))
  }
  
  if(length(grep("TT_DayActivity",tag_label)) != 0) {
    data <- subset(data, Activity <= pilr.utils.r::get_setting("activity_threshold", settings))
  }
  
  sampling_seconds <- median(diff(as.POSIXlt(data$Time, format = "%Y-%m-%dT%H:%M:%SZ")))
  units(sampling_seconds) <- "secs"
  sampling_seconds <- as.numeric(sampling_seconds)
  
  ei_meas <- pilr.utils.r::get_setting("energy_intake_measured", settings,
                                       required = FALSE) %>% pilr.utils.r::safe_numeric()
  ei_meas <- ifelse(is.na(ei_meas), 0, ei_meas)
  
  ## compute minutes correctly for NON 1-minute intervals, uses
  ## seconds setting
  samples_per_minute <- 60 / sampling_seconds
  minutes <- nrow(data) / samples_per_minute
  
  if(do_tf_correct) {
    tf_correct <- 1440 / minutes
  } else {
    tf_correct <- 1
  }
  
  ## adjust summary values to 24-hour rate
  Activity <- sum(data$Activity, na.rm = TRUE) * tf_correct
  
  ## we'll start with an empty data.frame and keep augmenting
  ## columns, ensuring each added value only has a single row per
  ## *this* event tag.
  
  ret <- list()
  ret$minutes <- minutes
  
  ## beginning and end times of segment
  ret$start_time <- as.character(min(data$Time, na.rm = TRUE))
  ret$end_time <- as.character(max(data$Time, na.rm = TRUE))
  
  ## the null offsets used for the study
  ret$null_offset_o2 <- data$null_offset_O2[1]
  ret$null_offset_co2 <- data$null_offset_CO2[1]
  
  ## transform for below sum, not included in output
  data$adj_ee <- data$recalc_ee + (-2.17 * data$nitrogen / 1440)
  data$adj_ee_nn <- data$recalc_ee    ## nn = no nitrogen
  
  ## sums with 24-hour correction factor
  ret$ee <- sum(data$adj_ee, na.rm = TRUE) * tf_correct
  ret$ee_nn <- sum(data$adj_ee_nn, na.rm = TRUE) * tf_correct
  ret$vo2 <- sum(data$recalc_vo2, na.rm = TRUE) * tf_correct
  ret$vco2 <- sum(data$recalc_vco2, na.rm = TRUE) * tf_correct
  
  ## sums without 24-hour correction factor
  ret$ee_no_tf_correct <- mean(data$adj_ee, na.rm = TRUE) 
  ret$ee_nn_no_tf_correct <- mean(data$adj_ee_nn, na.rm = TRUE) 
  ret$vo2_no_tf_correct <- mean(data$recalc_vo2, na.rm = TRUE)  
  ret$vco2_no_tf_correct <- mean(data$recalc_vco2, na.rm = TRUE)
  
  ## should this be the 24-hour corrected version?
  ret$eb_measured <- ei_meas - ret$ee
  
  # Average over each event
  ret$np_rq <- mean(data$np_rq, na.rm = TRUE)
  
  ret$protein_ox <- mean(data$protein_ox, na.rm = TRUE)
  
  ret$cho_ox <- mean(data$cho_ox, na.rm = TRUE)
  
  ret$fat_ox <- mean(data$fat_ox, na.rm = TRUE)
  
  ret$mr <- mean(data$adj_ee, na.rm = TRUE)
  
  ## do this after calculating the above
  ret$rq <- ret$vco2 / ret$vo2
  
  ret$activity <- round(Activity, 4)
  ret$activity_minutes <- ret$activity / 60 
  ret$activity_pct <- ret$activity / 1440   
  
  if(!grepl("TT_Exercise", tag_label)) {
    ## What ee to use here? 
    ret$spa <- sum(ifelse(data$Activity > 0, data$adj_ee, 0), na.rm = TRUE)
  } else {
    ret$spa <- 0
  }
  
  
  
  ret$tag_label <- gsub("TT_", "", tag_label)
  as.data.frame(ret)
}
