year_month_to_last_date <- function(x) {
  dt <- parse_date_time(x, orders = "b-Y")

  dt %>%
    `%m+%`(months(1)) %>%
    floor_date(unit = "month") -
    days(1)
}
