#wrapper functions that feed summarize_season

calc_checklist_count <- function(x) {
  x |>
    distinct(pba3_block, checklist_id) |>
    summarize(checklist_count = n_distinct(checklist_id), .by = pba3_block) |>
    collect()
}

calc_species_observed <- function(x) {
  x |>
    select(pba3_block, common_name) |>
    distinct() |>
    summarize(species_observed = n_distinct(common_name), .by = pba3_block) |>
    collect()
}

calc_atlasers <- function(x) {
  x |>
    distinct(pba3_block, observer_id) |>
    collect() |>
    separate_rows(observer_id, sep = ",") |>
    summarize(birders = n_distinct(observer_id), .by = pba3_block)
}

calc_block_effort <- function(x) {
  x |>
    distinct(pba3_block, checklist_id, duration_minutes, effort_distance_km) |> #for each checklist, find max of duration minutes and effort distance
    summarize(
      duration_minutes = max(duration_minutes, na.rm = TRUE),
      effort_distance_km = max(effort_distance_km, na.rm = TRUE),
      .by = c(pba3_block, checklist_id)
    ) |>
    summarize(
      duration_hours_total = sum(duration_minutes, na.rm = TRUE) / 60,
      effort_distance_km = sum(effort_distance_km, na.rm = TRUE),
      .by = pba3_block
    ) |>
    collect()
}
