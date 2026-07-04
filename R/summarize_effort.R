#' Title
#'
#' @param x A dataframe of checklist effort time data
#'
#' @returns A gt() table
#'
#' @export
#' @examples
summarize_effort <- function(x) {
  x |>
    dplyr::select(
      duration_hours_total,
      duration_hours_diurnal,
      duration_hours_nocturnal,
      duration_hours_unknown
    ) |>
    rename(
      Total = duration_hours_total,
      Diurnal = duration_hours_diurnal,
      Nocturnal = duration_hours_nocturnal,
      Unknown = duration_hours_unknown
    ) |>
    pivot_longer(everything(), names_to = "Effort type", values_to = "Value") |>
    mutate(Value = round(Value, 2)) |>
    gt()
}
