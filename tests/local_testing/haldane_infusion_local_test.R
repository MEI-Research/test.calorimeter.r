# Load necessary packages
library(pilr.api.r)
library(uuid)
library(dplyr)
library(jsonlite)
library(base64enc)

# Set options for server, project, access code
options(pilr_server_default = "http://qa.pilrhealth.com")
options(pilr_project_default = "equation_test_6-5-17")
options(pilr_default_access_code = "52a5d010-61d1-44bc-94fa-40eba6230afe")
participant = "Infusion2"
# options(pilr_project_default = "infusiontest-4-1-17")
# options(pilr_default_access_code = "e3c682d7-2689-40ac-82e2-a0e29b4793ff")
# participant = "equationtest"

# Retrieve data (can append more datasets to the list if workunit requires them)
data <-
  list(calrq = read_pilr(
    data_set = "pilrhealth:calrq:calrq_data",
    schema = "1",
    query_params = list(participant = participant)
  ))

# Fix MFC data
data$calrq$MFCFlow_1 = as.numeric(data$calrq$MFCFlow_1)
data$calrq$MFCFlow_2 = as.numeric(data$calrq$MFCFlow_2)
data$calrq$MFCFlow_3 = as.numeric(data$calrq$MFCFlow_3)
data$calrq$MFCFlow_4 = as.numeric(data$calrq$MFCFlow_4)

# Set your params (participant variables and instrument settings)
params = list(
  settings = read_pilr_params(
    data_set = "pilrhealth:calrq:calrq_data",
    schema = "1",
    instrument = "calrq",
    participant = participant,
    period = "active_period"
  )
)
params$package <- "test.calorimeter.r"

# Fix N2 array
multiple_n2 <-
  eval(parse(
    "",
    n = NULL,
    gsub('\n', "", params$settings$multiple_n2$value, fixed = TRUE)
  ))
fields = list()

for (i in 1:length(multiple_n2$fields))
{
  fields[i] = list(as.data.frame(multiple_n2$fields[[i]]))
  fields[[i]]$value = list(
    as.numeric(multiple_n2$fields[[i]]$value[1]),
    list('$date' = multiple_n2$fields[[i]]$value[2]),
    multiple_n2$fields[[i]]$value[3]
  )
  print(i)
}
# end

json_frame = data.frame(matrix(1, 2))
json_frame$fields = fields
json_frame <- subset(json_frame, select = -1)
params$settings$multiple_n2$value <- toJSON(json_frame)

# Remap variables with changed names (set in job def JSON)
params$settings$volume <- params$settings$chamber_volume
params$settings$CO2_MFC <- params$settings$co2_mfc
params$settings$N2_MFC <- params$settings$n2_mfc

pilr.utils.r::get_setting("CO2_MFC", params$settings)

# Run haldane transform
data$haldane <- apply_haldane(data, params)

# Get data for human summary
data$event_tags <-
  list(
    calrq = read_pilr(
      data_set = "pilrhealth:time_analysis:tagged_event",
      schema = "1",
      query_params = list(participant = participant)
    )
  )
data$event_tags <- data$event_tags$calrq

# Calculate event tags
# data$event_tags2 <- calc_event_tags(data, params)

# Run human summary
ret <- process_cal_infusion(data, params)
