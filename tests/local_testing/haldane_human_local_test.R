# Load necessary packages
library(pilr.api.r)
library(uuid)
library(dplyr)
library(jsonlite)
library(base64enc)

# Clear old values
remove(params)

# Set options for server, project, access code
options(pilr_server_default = "http://qa.pilrhealth.com")
options(pilr_project_default = "equation_test_6-5-17")
options(pilr_default_access_code = "52a5d010-61d1-44bc-94fa-40eba6230afe")
participant = "Human"

# Retrieve data (can append more datasets to the list if workunit requires them)
data <- list(calrq = read_pilr(data_set = "pilrhealth:calrq:calrq_data", schema = "1", 
                               query_params = list(participant = participant)))

# Set your params (participant variables and instrument settings)
params = list(settings = read_pilr_params(data_set = "pilrhealth:calrq:calrq_data", schema = "1", instrument = "calrq", participant = participant, period = "active_period"))

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

test = data.frame(matrix(1,2))
test$fields = fields
test <- subset(test, select = -1)
params$settings$multiple_n2$value <- toJSON(test)

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
