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

calc_species_coded <- function(x) {
  x |>
    collect() |>
    distinct(pba3_block, common_name, breeding_category_desc, breeding_rank) |>
    group_by(pba3_block, common_name) |>
    filter(breeding_rank == max(breeding_rank)) |>
    ungroup() |>
    count(pba3_block, breeding_category_desc, breeding_rank) |>
    select(-breeding_rank) |>
    pivot_wider(names_from = breeding_category_desc, values_from = n)
}

calc_nocturnal_diurnal_effort <- function(x, y, z) {
  block_dn_raw <- x |>
    distinct(
      pba3_block,
      checklist_id,
      observer_id,
      observation_datetime,
      longitude,
      latitude,
      duration_minutes
    ) |>
    collect() |>
    left_join(y) |>
    mutate(
      observation_datetime = case_when(
        !is.na(observation_datetime_fixed) ~ observation_datetime_fixed, #replace inconsistent start times with modal start time for the checklist
        .default = observation_datetime
      )
    ) |>
    select(-c(observation_datetime_fixed, observer_id)) |>
    #for each checklist, find max of duration minutes and effort distance
    summarize(
      observation_datetime = min(observation_datetime, na.rm = TRUE),
      duration_minutes = max(duration_minutes, na.rm = TRUE),
      .by = c(pba3_block, checklist_id, longitude, latitude)
    ) |>
    mutate(
      #if all checklists for a block have NA duration_minutes, max(duration_minutes) is -Inf. Replace with 0
      duration_minutes = case_when(
        duration_minutes == -Inf ~ 0,
        .default = duration_minutes
      )
    ) |>
    left_join(
      z,
      by = join_by(
        longitude,
        latitude,
        observation_datetime
      )
    ) |>
    mutate(
      flag_is_diurnal_checklist = between(
        observation_datetime,
        sunrise - minutes(40),
        sunset + minutes(20)
      ),
      checklist_type = case_when(
        flag_is_diurnal_checklist == TRUE ~ "diurnal",
        flag_is_diurnal_checklist == FALSE ~ "nocturnal",
        is.na(flag_is_diurnal_checklist) ~ "unknown"
      )
    ) |>
    select(-flag_is_diurnal_checklist)

  block_dn_summary <- block_dn_raw |>
    summarize(
      duration_hours = sum(duration_minutes, na.rm = TRUE) / 60,
      .by = c(pba3_block, checklist_type)
    ) |>
    pivot_wider(
      names_from = checklist_type,
      values_from = duration_hours,
      names_prefix = "duration_hours_"
    ) |>
    select(
      pba3_block,
      duration_hours_diurnal,
      duration_hours_nocturnal,
      duration_hours_unknown
    ) |>
    mutate(
      duration_hours_diurnal = coalesce(duration_hours_diurnal, 0),
      duration_hours_nocturnal = coalesce(duration_hours_nocturnal, 0),
      duration_hours_unknown = coalesce(duration_hours_unknown, 0)
    )

  block_dn_date <- block_dn_raw |>
    mutate(
      observation_date = as_date(observation_datetime)
    ) |>
    summarize(
      duration_hours = sum(duration_minutes, na.rm = TRUE) / 60,
      .by = c(pba3_block, checklist_type, observation_date)
    ) |>
    pivot_wider(
      names_from = checklist_type,
      values_from = duration_hours,
      names_prefix = "duration_hours_"
    ) |>
    select(
      pba3_block,
      observation_date,
      duration_hours_diurnal,
      duration_hours_nocturnal,
      duration_hours_unknown
    ) |>
    group_nest(pba3_block, .key = "effort_breakdown")

  block_dn_summary <- block_dn_summary |>
    left_join(block_dn_date)
}

calc_breeding_season_coverage <- function(x, y) {
  x |>
    mutate(
      observation_month = month(observation_datetime, abbr = TRUE, label = TRUE)
    ) |>
    distinct(pba3_block, observation_month) |>
    collect() |>
    inner_join(
      y |> filter(season == "Breeding"),
      by = c("observation_month" = "month")
    ) |>
    summarize(
      breeding_season_months_covered = n_distinct(observation_month),
      .by = pba3_block
    )
}
