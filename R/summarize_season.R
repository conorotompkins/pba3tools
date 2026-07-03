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
