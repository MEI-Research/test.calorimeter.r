# Load necessary packages
library(pilr.api.r)
library(uuid)
library(dplyr)
library(jsonlite)
library(base64enc)
# options(stringsAsFactors = FALSE)

# Clear old values
remove(params)

# Set options for server, project, access code
options(pilr_server_default = "http://cloud.pilrhealth.com")
options(pilr_project_default = "demoproject")
options(pilr_default_access_code = "a145fa3e-b2f5-4116-88dd-7f7123790024")
participant = "DemoParticipant4"
  # "500-0024v1_N2Test"
  # "500-0024v1"

# options(pilr_project_default = "500_practiceproject")
# options(pilr_default_access_code = "e98b5906-546a-4a8a-827a-4f840ec20aaa")
# participant = "500-0024v1"

# Retrieve data (can append more datasets to the list if workunit requires them)
data <- list(calrq = read_pilr(data_set = "pilrhealth:calrq:calrq_data", schema = "1", 
                               query_params = list(participant = participant)))

# Set your params (participant variables and instrument settings)
params = list(settings = read_pilr_params(data_set = "pilrhealth:calrq:calrq_data", schema = "1", instrument = "calrq", participant = participant, period = "active_period", epoch = "epoch_1"))

# Add package information to params
params$package <- "test.calorimeter.r"

# Fix multiple N2 array
testlist <- eval(parse("", n=NULL, gsub('\n', "", params$settings$multiple_n2$value, fixed = TRUE)))
fields = list()
for (i in 1:length(testlist$fields))
{
  fields[i] = list(as.data.frame(testlist$fields[[i]]))
  fields[[i]]$value = list(as.numeric(testlist$fields[[i]]$value[1]),list('$date' = testlist$fields[[i]]$value[2]),testlist$fields[[i]]$value[3])
  print(i)
}
end

test = data.frame(matrix(1,6))
test$fields = fields
test <- subset(test, select = -1)
params$settings$multiple_n2$value <- toJSON(test)

# Fix rest duration
testlist <- eval(parse("", n=NULL, gsub('\n', "", params$settings$rest_durations$value, fixed = TRUE)))
fields = list()
for (i in 1:length(testlist$fields))
{
  fields[i] = list(as.data.frame(testlist$fields[[i]]))
  fields[[i]]$value[1] = list(as.numeric(testlist$fields[[i]]$value[1]))
  fields[[i]]$value[2] = list(as.numeric(testlist$fields[[i]]$value[2]))
  print(i)
}
end

test = data.frame(matrix(1,1))
test$fields = fields
test <- subset(test, select = -1)
params$settings$rest_durations$value <- toJSON(test)

# Remap variables with changed names (set in job def JSON)
params$settings$volume <- params$settings$chamber_volume

# Run haldane transform
data$haldane <- apply_haldane(data, params)

# Get data for human summary
data$event_tags <- list(calrq = read_pilr(data_set = "pilrhealth:time_analysis:tagged_event", schema = "1", 
                                          query_params = list(participant = participant)))
data$event_tags <- data$event_tags$calrq

# Run human summary
ret <- process_cal_human(data, params)
