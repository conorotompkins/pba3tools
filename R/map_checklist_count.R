#' Title
#'
#' @param x An sf dataframe with checklist longitudes and latitudes
#'
#' @returns A maplibre map of checklist locations
#'
#' @export
map_checklist_count <- function(x) {
  exp_seq <- function(start, end, length) {
    if (start <= 0 || end <= 0) {
      stop("start and end must be > 0")
    }
    if (length < 1 || length != as.integer(length)) {
      stop("length must be a positive integer")
    }

    x <- exp(seq(log(start), log(end), length.out = length))

    x <- round(x, 0)
  }

  count_range <- exp_seq(
    min(x$checklist_count),
    max(x$checklist_count),
    length = 10
  ) |>
    unique()

  circle_sizes <- exp_seq(
    3,
    15,
    length(count_range)
  )

  bounds_sf <- st_buffer(x, 1000)

  maplibre() |>
    fit_bounds(bounds_sf, animate = FALSE) |>
    add_circle_layer(
      id = "count-circles",
      source = x,
      circle_radius = step_expr(
        column = "checklist_count",
        base = 2,
        values = count_range,
        stops = circle_sizes
      ),
      circle_color = "#1f78b4",
      circle_opacity = 0.8,
      circle_stroke_color = "#ffffff",
      circle_stroke_width = 1,
      tooltip = "checklist_count"
    ) |>
    add_legend(
      legend_title = "Checklists",
      values = count_range,
      colors = rep("#1f78b4", length(circle_sizes)),
      type = "categorical",
      sizes = circle_sizes,
      position = "top-right",
      patch_shape = "circle"
    )
}
