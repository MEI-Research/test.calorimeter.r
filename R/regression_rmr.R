## calculate RMR intercept
#'@export
calculate_rmr <- function(data, sampling_seconds) {
  
    # fix NA activity
  for (i in 1:(length(data$Activity)))
  {
    if (is.null(data$Activity[i])) {
      data$Activity[i] <- 0
    }
    else if (is.na(data$Activity[i]))
    {
      data$Activity[i] <- 0
    }
  }
  
    samples_per_minute <- 60 / sampling_seconds
    minutes <- nrow(data) / samples_per_minute
    
    
    output15 <- zoo::rollapply(data[c("Activity", "recalc_ee")],
                               15 * samples_per_minute,
                               (sum),
                               by = 15 * samples_per_minute,
                               by.column=TRUE, align='right')
    
    output15_df <- as.data.frame(output15)
    
    names(output15_df) <- c("activity", "ee")
    
    ## transforms per erica on 7/8
    output15_df$ee <- output15_df$ee / (15 * samples_per_minute)
    output15_df$activity <- (output15_df$activity  / 60 /15 ) * 100 #activity percent
    
    lm(ee ~ activity, data = output15_df)
}

