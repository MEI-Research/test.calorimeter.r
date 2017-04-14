test <- params$settings$multiple_n2$value
gsub("\n","",test)

test2 <-
  list(fields = list(
    list(
      min = c(0, NA, NA),
      max = c(10, NA, NA),
      description = c(
        "Urine Nitrogen Mass",
        "Urine Nitrogen Collection Date",
        "Urine Nitrogen Collection Time"
      ),
      name = c(
        "Urine Nitrogen Mass",
        "Urine Nitrogen Collection Date",
        "Urine Nitrogen Collection Time"
      ),
      value = c("1", "2017-04-02T00:00:00Z", "02:02:22"),
      code = c(
        "calrq:urine_nitrogen_value",
        "calrq:urine_nitrogen_date",
        "calrq:urine_nitrogen_time"
      ),
      type = c("double_setting", "date_setting", "time_setting"),
      units = c("", "",  "")
    ),
    list(
      min = c(0, NA, NA),
      max = c(10, NA, NA),
      description = c(
        "Urine Nitrogen Mass",
        "Urine Nitrogen Collection Date",
        "Urine Nitrogen Collection Time"
      ),
      name = c(
        "Urine Nitrogen Mass",
        "Urine Nitrogen Collection Date",
        "Urine Nitrogen Collection Time"
      ),
      value = c("2", "2017-04-03T00:00:00Z", "03:00:00"),
      code = c(
        "calrq:urine_nitrogen_value",
        "calrq:urine_nitrogen_date",
        "calrq:urine_nitrogen_time"
      ),
      type = c("double_setting", "date_setting", "time_setting"),
      units = c("", "", "")
    )
  ))

list(
  fields = list(
    list(
      min = c(0, NA, NA),
      max = c(10, NA, NA),
      description = c(
        \"Urine Nitrogen Mass\", \"Urine Nitrogen Collection Date\", \"Urine Nitrogen Collection Time\"), name = c(\"Urine Nitrogen Mass\", \"Urine Nitrogen Collection Date\", \"Urine Nitrogen Collection Time\"), value = c(\"1\", \"2017-04-02T00:00:00Z\", \"02:02:22\"), code = c(\"calrq:urine_nitrogen_value\", \"calrq:urine_nitrogen_date\", \"calrq:urine_nitrogen_time\"), type = c(\"double_setting\", \"date_setting\", \"time_setting\"), units = c(\"\", \"\", \"\")), list(min = c(0, NA, NA), max = c(10, NA, NA), description = c(\"Urine Nitrogen Mass\", \"Urine Nitrogen Collection Date\", \"Urine Nitrogen Collection Time\"), name = c(\"Urine Nitrogen Mass\", \"Urine Nitrogen Collection Date\", \"Urine Nitrogen Collection Time\"), value = c(\"2\", \"2017-04-03T00:00:00Z\", \"03:00:00\"), code = c(\"calrq:urine_nitrogen_value\", \"calrq:urine_nitrogen_date\", \"calrq:urine_nitrogen_time\"), type = c(\"double_setting\", \"date_setting\", \"time_setting\"), units = c(\"\", \"\", \"\"))))
        