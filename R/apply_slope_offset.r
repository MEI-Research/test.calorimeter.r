#'@export
apply_slope_offset <- function(data, params, ...) {
    ## apply algorithm from spreadsheet
    sloped <- data$nulled
    event_tags <- data$event_tags
    list(sloped = sloped, event_tags = event_tags)
}
