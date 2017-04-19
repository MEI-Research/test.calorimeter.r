## this is our entry point from opencpu
#'@export
calc_event_tags <- function(data, params, ...) {
  ## Settings
  # Derivative window for change detection
  derivative_window <- 2
  # Number of times above mean for break detection
  num_mean <- 1.5
  # Number of points above deriv thold to count as a break
  deriv_thold <- 2
  # How close values need to be to fullscale to avoid cropping
  cutoffpercent <- .02
  # Min number of points for measurement after data cleansing
  minpoints <- 5
  # Warmup
  # Set to 0 for no warmup
  warmup <- .5
  # Tag
  tag <- 'Infusion Study'
  # Number of points to cut from start
  start_cut = 2
  # Number of points to cut from end
  end_cut = 2
  # Limit for zero
  zero_limit = .002
  
  data_interval = 60
  derivative_window = 8
  
  #data_interval <- pilr.utils.r::get_setting("data_interval", params$settings,
  #                                           required = FALSE) %>%
  #  pilr.utils.r::safe_numeric()
  #
  #derivative_window <- pilr.utils.r::get_setting("deriv_window", params$settings,
  #                                               required = FALSE) %>%
  #  pilr.utils.r::safe_numeric()
  
  
  # cal_seconds <- pilr.utils.r::get_setting("read_interval", params$settings) %>% pilr.utils.r::safe_numeric()
  cal_seconds <- median(diff(as.POSIXlt(data$calrq$Time, format = "%Y-%m-%dT%H:%M:%SZ")))
  units(cal_seconds) <- "secs"
  cal_seconds <- as.numeric(cal_seconds)
  
  # Find out how many MFCs are present in file (up to 4, sequentially)
  # return as numMFC
  for (i in 1:4)
  {
    if (!is.null(eval(parse(text = paste("data$calrq$MFCFlow_",i,sep = "")))))
    {
      numMFC <- i
    }
  }
  
  # Initalize a vector for storing data about breakpoints we find in the MFC data
  processing <- matrix(FALSE, length(data$calrq[,1]),numMFC)
  
  # For each MFC, find out when it is in use
  for (i in 1:numMFC)
  {
    # We'll try to figure out where the data intervals are now
    # Take the derivative, assume values above the average of the
    # derivative mean the value has switched.
    mfcderiv <- as.numeric(abs(derivative(eval(parse(text = paste("data$calrq$MFCFlow_",i,sep = ""))), data_interval, derivative_window)))
    # plot(as.numeric(abs(derivative(eval(parse(text = paste("data$calrq$MFCFlow_",i,sep = ""))), data_interval, derivative_window))), type ="l")
    
    # Initialize a vector for counting changes in mfcderiv
    k <- 0
    
    # Find sequences of mfcderiv above our cutoff for detecting a change in flow
    # (constant * mean deriv)
    cutoff <- mfcderiv > num_mean*mean(mfcderiv)
    for (j in 1:length(mfcderiv))
    {
      if (cutoff[j] == TRUE)
      {
        # start counting as above mean thold
        k <- k + 1
      }
      else {
        # stop counting and check if count > count thold
        if (k >= deriv_thold)
        {
          # record as a break
          processing[j,i] <- TRUE
        }
        # reset count to find next break
        k <- 0
      }
    }
  }
  
  # Set beginning and end to TRUE since derivative can't catch changes at zero
  processing[length(processing[,1]),] = TRUE
  processing[1,] = TRUE
  
  # Create matric for tracking MFC data
  MFCpts <- matrix(data=NA,nrow=max(colSums(processing)),ncol=numMFC)
  
  # Scan data from each MFC and record breakpoints
  for (i in 1:numMFC)
  {
    # where does the flow change?
    changes <- which(processing[,i])
    
    # need at least 2 points
    if (length(changes)>1)
    {
      for (j in 1:(length(changes)-1))
      {
        # Store data
        MFCpts[j,i] <- changes[j]
        # Will overwrite if > 1 point set because endpoint is shared with startpoint
        MFCpts[j+1,i] <- changes[j+1]
      }
    }
  }
  
  # Initialize results matrix
  # not sure how long this will be but it won't be longer than import
  # maybe there is a better way to do this
  # create an index so we can cut it later and keep track of where we are
  # results <- matrix(data = NA, nrow = length(import[,1]), ncol = MFCoff + 8, dimnames = list(1:length(import[,1]),c(colnames(import[,1:MFCoff]),"MFC mean","BIOS mean","MFC sdev","Bios sdev", "MFC","Number of Points","Start Point","End point")))
  # resultsindex <- 1
  
  # Create matric for tracking MFC data
  MFCpts_combined <- matrix(data=NA,nrow=0,ncol=2)
  
  # Calibration processing
  # Cycle through MFCs
  for (i in 1:numMFC)
  {
    # non-NA data
    breaks <- MFCpts[!is.na(MFCpts[,i]),i]
    
    # get lengths of breaks
    lengths <- breaks[2:length(breaks)]-breaks[1:length(breaks)-1]
    
    # make sure we have data to process
    if (!length(lengths) == 0)
    {
      # define limits for outliers as percent of fullscale
      mfcdata <- as.numeric(eval(parse(text = paste("data$calrq$MFCFlow_",i,sep = ""))))
      # Could use this if we don't use all of the mfcdata
      # mfcdata <- mfcdata[breaks[1]:breaks[length(breaks)]]
      
      limit <- max(mfcdata[!is.na(mfcdata)])*cutoffpercent
      limit <- max(c(limit,zero_limit))
      
      # Cycle through breaks
      for (j in 1:(length(breaks)-1))
      {
        # get indicies of data in this segment
        points <- breaks[j]:breaks[j+1]
        
        # If this is the warm up period, omit first half of data
        if (lengths[j] > 1.5 * median(lengths))
        {
          points <- points[(round(length(points)*warmup)+1):length(points)]
        }
        
        # exclude MFC data with NA
        points <- points[!is.na(mfcdata[points])]
        
        # calc mean for cutoff test
        mfcmean = median(mfcdata[points])
        
        # Crop out points that fail our cutoff tests
        # MFC test
        mfccut <- xor(abs(mfcdata[points] - mfcmean) > limit, mfcmean < limit)
        
        # save to points vector
        points <- points[!mfccut]
        
        # combine data for these points
        # exclude Notes and Time for avg
        # check that we have enough points
        if (length(points) > minpoints)
        {
          points = points[start_cut:(length(points)-end_cut)]
          points_start = points[1]
          points_end = points[length(points)]
          
          MFCpts_combined <- rbind(MFCpts_combined, c(points_start,points_end))
         
          # OK to add to results matrix
          # first add all non MFC data
          
          # Date is first value
          # results[resultsindex,1] <- import[points[1],1]
          
          # All others are avg
          # results[resultsindex,2:MFCoff] <- colMeans(import[points,2:MFCoff])
          
          # results[resultsindex,(MFCoff+1):(MFCoff+8)] <- c(mean(import[points,MFCoff+i]), mean(import[points,2]), sd(import[points,MFCoff+i]),  sd(import[points,2]), i, length(points), points[1], points[length(points)])
          
          # resultsindex <- resultsindex + 1
        }
      }
    }
  }
  
  MFCpts_combined <- unique(MFCpts_combined[order(MFCpts_combined[,1]),])
  MFCpts_processed <- matrix(data=NA,nrow=0,ncol=2)
  startvals <- unique(MFCpts_combined[,1])
  
  # Fix overlapping tags
  for (i in 1:length(startvals)){
    
    MFCpts_processed <- rbind(MFCpts_processed, c(startvals[i], min(MFCpts_combined[(MFCpts_combined[,2] - startvals[i]) > 0,2])))
    
  }
    
  # Initalize data frame
  event_tags <- data.frame(start_time = character(length(MFCpts_processed[,1])),
                           end_time = character(length(MFCpts_processed[,1])),
                           verified = logical(length(MFCpts_processed[,1])),
                           tags = I(as.list(rep(NA,length(MFCpts_processed[,1])))),
                           note = character(length(MFCpts_processed[,1])),
                           number = character(length(MFCpts_processed[,1])),
                           id = character(length(MFCpts_processed[,1])),
                           pt = character(length(MFCpts_processed[,1])),
                           timestamp = character(length(MFCpts_processed[,1])),
                           local_time = character(length(MFCpts_processed[,1])),
                           index = logical(length(MFCpts_processed[,1])), stringsAsFactors = FALSE)
  
  # Create event tags
  for (i in 1:length(MFCpts_processed[,1])){
    
    start_time = as.character(data$calrq$Time[MFCpts_processed[i,1]])
    end_time = data$calrq$Time[MFCpts_processed[i,2]]
    verified = FALSE
    tags = list(tag)
    note = 'Auto tagged'
    number = i
    id = uuid::UUIDgenerate(0)
    pt = data$calrq$pt[1]
    timestamp = data$calrq$Time[MFCpts_processed[i,1]]
    local_time = data$calrq$Time[MFCpts_processed[i,1]]
    index = NA

    event_tags$start_time[i] = start_time
    event_tags$end_time[i] = end_time
    event_tags$verified[i] = verified
    event_tags$tags[i] = I(tags)
    event_tags$note[i] = note
    event_tags$number[i] = number
    event_tags$id[i] = id
    event_tags$pt[i] = pt
    event_tags$timestamp[i] = timestamp
    event_tags$local_time[i] = local_time
    event_tags$index[i] = index
  }
  
  # Return event tags
  ret$event_tags <- event_tags
  
  as.data.frame(ret)
}