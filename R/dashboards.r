## VO2 and VCO2 Line Graph
vo2_vco2_line <- function(data, params, ...) {
  
  data$calrq <- cbind(data$calrq$data, data$calrq$metadata)
  
  # Remove outliers
  data$calrq$VO2[data$calrq$VO2 < 0] <- 0
  data$calrq$VCO2[data$calrq$VCO2 < 0] <- 0
  
  # Convert timestamp
  data$calrq$timestamp <- as.POSIXct(data$calrq$timestamp, format = "%Y-%m-%dT%H:%M:%SZ")
  
  # Setup dataframe for ggvis
  df <- rbind(data.frame(timestamp = data$calrq$timestamp, value = data$calrq$VO2, Data = "VO2"),
              data.frame(timestamp = data$calrq$timestamp, value = data$calrq$VCO2, Data = "VCO2"))
  
  df %>% ggvis(~timestamp, ~value, stroke = ~Data) %>%
    layer_lines() %>%
    add_axis("x", title = "",
             properties = axis_props(labels = list(angle = 45, align = "left"))) %>%
    add_axis("y", title = "")
  
}


## Binary heatmap for CalRQ Data
# 0 = No data = white
# 1 = Data Uploaded = yellow
# 2 = Haldane ran = light green
# 3 = Summary ran = dark green
binary_heatmap <- function(data, params, ...) {
  
  data$calrq <- cbind(data$calrq$data, data$calrq$metadata)
  data$haldane <- cbind(data$haldane$data, data$haldane$metadata)
  data$human <- cbind(data$human$data, data$human$metadata)
  
  # Add day column to dataframe
  data$calrq$day <- substring(data$calrq$timestamp, 0, 10)
  data$haldane$day <- substring(data$haldane$timestamp, 0, 10)
  data$human$day <- substring(data$human$timestamp, 0, 10)
  
  # Subset to unique days and initialize dataframe
  pts <- unique(data$calrq$pt)
  days <- data.frame(day = unique(data$calrq$day), pt = pts[1], value = "No Data", color = "#ffffff")
  
  # For each participant and day, assign value 0 to 3
  temp <- days
  for (i in 1:length(pts)) {
    temp$value <- "No Data"
    temp$pt <- pts[i]
    temp$color = "#ffffff"
    pt_calrq <- subset(data$calrq, pt == pts[i])
    pt_haldane <- subset(data$haldane, pt == pts[i])
    pt_human <- subset(data$human, pt == pts[i])
    for (j in 1:nrow(temp)) {
      if (days$day[j] %in% unique(pt_calrq$day)) {
            temp$value[j] <- "Uploaded"
            temp$color[j] <- "#ffff00"
        }
      if (days$day[j] %in% unique(pt_haldane$day)) {
            temp$value[j] <- "Processed"
            temp$color[j] <- "#99ff66"
        }
      if (days$day[j] %in% unique(pt_human$day)) {
            temp$value[j] <- "Summarized"
            temp$color[j] <- "#009900"
        }
    }
    if (i == 1) {
      days <- temp
    }
    else {
      days <- rbind(days, temp)
    }
  }
  
  # Construct heatmap
  days %>%
    ggvis(~day, ~pt, fill := ~color) %>%
    layer_rects(height = band(), width = band()) %>%
    scale_nominal("fill", range = c("#ffffff", "#ffff00", "#99ff66", "#009900")) %>%
    scale_nominal("x", padding = 0, points = FALSE) %>%
    scale_nominal("y", padding = 0, points = FALSE) %>%
    scale_nominal("x", name = "xcenter", padding = 1, points = TRUE) %>%
    scale_nominal("y", name = "ycenter", padding = 1, points = TRUE) %>%
    add_axis("x", title = "",
             properties = axis_props(labels = list(angle = 45, align = "left"))) %>%
    add_axis("y", title = "") %>%
    add_legend("fill", values = c("No Data", "Uploaded", "Processed", "Summarized"), title = "Data Status")
  
}