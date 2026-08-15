#' Helper functions that feed [summarize_season()]
#'
#' Each `calc_*()` function below computes one block-level metric from an
#' eBird checklist table (as produced by `auk`) and is combined with the
#' others inside [summarize_season()] via a series of `left_join()`s on
#' `pba3_block`.
#' @name summarize_season_helpers
NULL

#' Count distinct checklists per block
#'
#' @param x A dataframe (or lazy tbl) of eBird checklists generated via auk,
#'   containing `pba3_block` and `checklist_id` columns.
#'
#' @returns A dataframe with one row per `pba3_block` and a `checklist_count`
#'   column giving the number of distinct checklists submitted in that block.
#'
#' @export
calc_checklist_count <- function(x) {
  x |>
    distinct(pba3_block, checklist_id) |>
    summarize(checklist_count = n_distinct(checklist_id), .by = pba3_block) |>
    collect()
}

#' Count distinct species observed per block
#'
#' @param x A dataframe (or lazy tbl) of eBird checklists generated via auk,
#'   containing `pba3_block` and `common_name` columns.
#'
#' @returns A dataframe with one row per `pba3_block` and a
#'   `species_observed` column giving the number of distinct species
#'   reported in that block.
#'
#' @export
calc_species_observed <- function(x) {
  x |>
    select(pba3_block, common_name) |>
    distinct() |>
    summarize(species_observed = n_distinct(common_name), .by = pba3_block) |>
    collect()
}

#' Count distinct birders (atlasers) per block
#'
#' `observer_id` may contain multiple, comma-separated observer IDs for a
#' single checklist (shared/group checklists); this function splits those
#' out before counting so each birder is counted once per block.
#'
#' @param x A dataframe (or lazy tbl) of eBird checklists generated via auk,
#'   containing `pba3_block` and `observer_id` columns.
#'
#' @returns A dataframe with one row per `pba3_block` and a `birders` column
#'   giving the number of distinct observers who submitted checklists in
#'   that block.
#'
#' @export
calc_atlasers <- function(x) {
  x |>
    distinct(pba3_block, observer_id) |>
    collect() |>
    separate_rows(observer_id, sep = ",") |>
    summarize(birders = n_distinct(observer_id), .by = pba3_block)
}

#' Summarize total survey effort per block
#'
#' For each checklist, takes the maximum reported `duration_minutes` and
#' `effort_distance_km` (guarding against duplicate rows per checklist),
#' then sums these across all checklists in a block.
#'
#' @param x A dataframe (or lazy tbl) of eBird checklists generated via auk,
#'   containing `pba3_block`, `checklist_id`, `duration_minutes`, and
#'   `effort_distance_km` columns.
#'
#' @returns A dataframe with one row per `pba3_block` and columns
#'   `duration_hours_total` (total survey time, in hours) and
#'   `effort_distance_km` (total distance traveled, in km).
#'
#' @export
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

#' Count species by highest breeding code reached, per block
#'
#' For each species observed in a block, keeps only its highest-ranked
#' breeding code (via `breeding_rank`) and tallies how many species reached
#' each `breeding_category_desc` (e.g. "Observed", "Possible", "Probable",
#' "Confirmed").
#'
#' @param x A dataframe (or lazy tbl) of eBird checklists generated via auk,
#'   containing `pba3_block`, `common_name`, `breeding_category_desc`, and
#'   `breeding_rank` columns.
#'
#' @returns A wide dataframe with one row per `pba3_block` and one column
#'   per `breeding_category_desc`, giving the count of species that reached
#'   that breeding category as their highest code in that block.
#'
#' @export
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

#' Classify and summarize checklist effort as diurnal or nocturnal
#'
#' Each checklist's start time is classified as `"diurnal"` (within 40
#' minutes before sunrise through 20 minutes after sunset), `"nocturnal"`
#' (outside that window), or `"unknown"` (missing sunrise/sunset data), then
#' effort hours are summed per block, both overall and by observation date.
#'
#' @param x A dataframe (or lazy tbl) of eBird checklists generated via auk,
#'   containing `pba3_block`, `checklist_id`, `observer_id`,
#'   `observation_datetime`, `longitude`, `latitude`, and
#'   `duration_minutes` columns.
#' @param y A dataframe with the modal observation datetime per checklist
#'   (columns `checklist_id` and `observation_datetime_fixed`), used to
#'   correct inconsistent checklist start times.
#' @param z A dataframe with sunrise and sunset times for each checklist
#'   location, joinable to `x` by `longitude`, `latitude`, and
#'   `observation_datetime`, with `sunrise` and `sunset` columns.
#'
#' @returns A dataframe with one row per `pba3_block` and:
#'   * `duration_hours_diurnal`, `duration_hours_nocturnal`,
#'     `duration_hours_unknown`: total effort hours of each type, and
#'   * `effort_breakdown`: a list-column of nested dataframes giving the
#'     same three duration columns broken down by `observation_date`.
#'
#' @export
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
    select(-c(observation_datetime_fixed, observer_id))

  #for each checklist, find max of duration minutes and effort distance
  #the max() warning for all-NA duration_minutes groups is suppressed here
  #since the resulting -Inf is expected and replaced with 0 immediately below
  block_dn_raw <- suppressWarnings(
    block_dn_raw |>
      summarize(
        observation_datetime = min(observation_datetime, na.rm = TRUE),
        duration_minutes = max(duration_minutes, na.rm = TRUE),
        .by = c(pba3_block, checklist_id, longitude, latitude)
      )
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
    select(-flag_is_diurnal_checklist) |>
    mutate(
      checklist_type = factor(
        checklist_type,
        levels = c("diurnal", "nocturnal", "unknown")
      )
    )

  block_dn_summary <- block_dn_raw |>
    summarize(
      duration_hours = sum(duration_minutes, na.rm = TRUE) / 60,
      .by = c(pba3_block, checklist_type)
    ) |>
    pivot_wider(
      names_from = checklist_type,
      values_from = duration_hours,
      names_prefix = "duration_hours_",
      names_expand = TRUE
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
      names_prefix = "duration_hours_",
      names_expand = TRUE
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

#' Count breeding-season months covered per block
#'
#' @param x A dataframe (or lazy tbl) of eBird checklists generated via auk,
#'   containing `pba3_block` and `observation_datetime` columns.
#' @param y A dataframe with the PBA3 seasons and calendar month mappings,
#'   containing `season` and `month` columns (`month` matching the
#'   abbreviated month labels derived from `x$observation_datetime`).
#'
#' @returns A dataframe with one row per `pba3_block` and a
#'   `breeding_season_months_covered` column giving the number of distinct
#'   calendar months (restricted to the "Breeding" season in `y`) with at
#'   least one checklist in that block.
#'
#' @export
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

#' Count nocturnal species coded per block
#'
#' Counts species from a known nocturnal-species list that reached a
#' breeding code of "Possible" or higher (`breeding_rank >= 2`) in each
#' block. Blocks with no qualifying nocturnal species are included with a
#' count of 0 rather than being dropped.
#'
#' @param x A dataframe (or lazy tbl) of eBird checklists generated via auk,
#'   containing `pba3_block`, `common_name`, and `breeding_rank` columns.
#' @param y A dataframe with the common names of nocturnal species, joined
#'   to `x` by `common_name`.
#'
#' @returns A dataframe with one row per `pba3_block` (every block present
#'   in `x`) and a `nocturnal_species_coded` column giving the number of
#'   distinct nocturnal species coded at "Possible" or higher in that block.
#'
#' @export
calc_nocturnal_species_coded <- function(x, y) {
  x |>
    semi_join(y, by = "common_name") |>
    filter(breeding_rank >= 2) |>
    summarize(
      nocturnal_species_coded = n_distinct(common_name),
      .by = pba3_block
    ) |>
    collect() |>
    complete(
      pba3_block = x |> distinct(pba3_block) |> collect() |> pull()
    ) |>
    mutate(nocturnal_species_coded = coalesce(nocturnal_species_coded, 0))
}

#' Build a block-level summary of eBird checklist activity for one season
#'
#' Filters an eBird checklist table down to the calendar months belonging to
#' `season_filter`, then computes and joins a full set of per-block metrics
#' (checklist counts, species observed, birder counts, effort hours,
#' breeding codes, diurnal/nocturnal effort, breeding season coverage, and
#' nocturnal species coded) via the `calc_*()` helper functions in this
#' file. See [calc_checklist_count()], [calc_species_observed()],
#' [calc_atlasers()], [calc_block_effort()], [calc_species_coded()],
#' [calc_nocturnal_diurnal_effort()], [calc_breeding_season_coverage()],
#' and [calc_nocturnal_species_coded()] for details on each metric.
#'
#' @param checklist_df A dataframe (or lazy tbl) of eBird checklists
#'   generated via auk, with an `observation_month` column matching the
#'   `month` column in `seasons_df`.
#' @param seasons_df A dataframe of PBA3 seasons and calendar month
#'   mappings, with `season` and `month` columns. Defaults to `seasons`.
#' @param season_filter A single season name (matching a value in
#'   `seasons_df$season`, e.g. `"Breeding"`) used to restrict
#'   `checklist_df` to the relevant months.
#' @param ob_dt_fixed A dataframe with the modal observation datetime per
#'   checklist (columns `checklist_id` and `observation_datetime_fixed`),
#'   passed through to [calc_nocturnal_diurnal_effort()].
#' @param location_sunrise_sunset A dataframe with sunrise and sunset times
#'   for each checklist location, passed through to
#'   [calc_nocturnal_diurnal_effort()].
#' @param nocturnal_species A dataframe with the common names of nocturnal
#'   species, passed through to [calc_nocturnal_species_coded()].
#'
#' @returns A dataframe with one row per `pba3_block` present in
#'   `checklist_df` containing all of the metrics computed by the
#'   `calc_*()` helper functions, joined together on `pba3_block`. Blocks
#'   with no checklists in `checklist_df` are not included; callers that
#'   need a complete set of blocks (e.g. including geometry) should
#'   `left_join()` the result onto their own block reference table, as done
#'   in `scripts/04_summarize_block_effort.R`.
#'
#' @export
summarize_season <- function(
  checklist_df,
  seasons_df = seasons,
  season_filter,
  ob_dt_fixed,
  location_sunrise_sunset,
  nocturnal_species
) {
  print(season_filter)
  season_filtered_df <- seasons_df |>
    filter(season == season_filter)

  checklist_df <- checklist_df |>
    semi_join(
      season_filtered_df,
      by = join_by(observation_month == month)
    )

  print("calculating checklist counts")
  block_checklist_count <- calc_checklist_count(checklist_df)

  print("calculating species observed")
  block_species_observed <- calc_species_observed(checklist_df)

  print("calculating birders")
  block_birders <- calc_atlasers(checklist_df)

  print("calculating effort summary")
  block_effort <- calc_block_effort(checklist_df)

  print("calculating species codes")
  block_species_coded <- calc_species_coded(checklist_df)

  print("calculating diurnal/nocturnal effort")
  block_nocturnal_diurnal <- calc_nocturnal_diurnal_effort(
    checklist_df,
    ob_dt_fixed,
    location_sunrise_sunset
  )

  print("calculating breeding season coverage")
  block_breeding_season_coverage <- calc_breeding_season_coverage(
    checklist_df,
    seasons_df
  )
  # Note: calc_breeding_season_coverage() always restricts `seasons_df` to
  # the "Breeding" season internally, so it needs the full (unfiltered)
  # `seasons_df` argument here, not `season_filtered_df`.

  print("calculating nocturnal species coded")
  block_nocturnal_species_coded <- calc_nocturnal_species_coded(
    checklist_df,
    nocturnal_species
  )

  df_list <- list(
    block_checklist_count,
    block_species_observed,
    block_birders,
    block_effort,
    block_species_coded,
    block_nocturnal_diurnal,
    block_breeding_season_coverage,
    block_nocturnal_species_coded
  )

  block_summary <- reduce(df_list, left_join, by = "pba3_block")

  block_summary
}
