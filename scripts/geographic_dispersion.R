library(tidyverse)
library(sfcentral)
library(sf)
library(arrow)
library(geoarrow)
library(sfarrow)
library(mapgl)

ebd_df <- open_dataset("data/pa_breeding_bird_atlas_processed.parquet")

block_summary <- open_dataset(
  "data/block_summary_seasons.parquet"
) |>
  mutate(
    duration_hours_total = round(duration_hours_total, 2),
    effort_distance_km = round(effort_distance_km, 2)
  ) |>
  select(
    pba3_block,
    block_name,
    block_region,
    block_county,
    season,
    species_observed,
    Observed,
    Possible,
    Probable,
    Confirmed,
    checklist_count,
    birders,
    duration_hours_total,
    duration_hours_diurnal,
    duration_hours_nocturnal,
    duration_hours_unknown,
    effort_distance_km,
    pct_missing_pba2_confirmations,
    pct_coded_atlas_comparison,
    pba3_pba2_coded_count_compare_pct,
    breeding_season_months_covered,
    nocturnal_species_coded,
    geometry
  ) |>
  filter(season == "All seasons") |>
  st_as_sf() |>
  collect()

glimpse(block_summary)

maplibre_view(
  block_summary,
  column = "duration_hours_total",
  legend_positon = "top-right"
)

ebd_df |>
  head() |>
  glimpse()

checklist_coords <- ebd_df |>
  distinct(pba3_block, checklist_id, latitude, longitude) |>
  collect() |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

checklist_coords |>
  slice_sample(prop = .1) |>
  ggplot() +
  geom_sf()

checklist_coords |>
  st_sd_distance() |>
  ggplot() +
  geom_sf() +
  geom_sf(
    data = checklist_coords |>
      slice_sample(prop = .1)
  )

checklist_coords |>
  st_sd_ellipse() |>
  ggplot() +
  geom_sf() +
  geom_sf(
    data = checklist_coords |>
      slice_sample(prop = .1)
  )

safe_ellipse <- possibly(st_sd_ellipse, otherwise = NULL)

ellipses_by_block <- checklist_coords |>
  group_by(pba3_block) |>
  group_modify(
    ~ {
      result <- safe_ellipse(.x)
      if (is.null(result)) tibble() else result
    }
  ) |>
  ungroup()

ellipses_by_block |>
  ggplot(aes(eccentricity)) +
  geom_histogram()

block_summary <- left_join(
  block_summary,
  select(ellipses_by_block, pba3_block, eccentricity),
  by = "pba3_block"
)

maplibre_view(
  block_summary,
  column = "eccentricity",
  legend_positon = "top-right"
)

block_summary |>
  ggplot(aes(checklist_count, eccentricity)) +
  geom_point(alpha = .1) +
  scale_x_log10()

block_summary |>
  ggplot(aes(checklist_count, eccentricity)) +
  geom_point(alpha = .1)

test_block_min <- block_summary |>
  st_drop_geometry() |>
  as_tibble() |>
  filter(checklist_count >= 20) |>
  slice_min(n = 20, order_by = eccentricity) |>
  select(pba3_block, block_name)

test_block_max <- block_summary |>
  st_drop_geometry() |>
  as_tibble() |>
  filter(checklist_count >= 20) |>
  slice_max(n = 20, order_by = eccentricity) |>
  select(pba3_block, block_name)

block_locations_min <- ebd_df |>
  inner_join(test_block_min) |>
  select(pba3_block, block_name, longitude, latitude) |>
  arrange(pba3_block) |>
  distinct() |>
  collect() |>
  st_as_sf(coords = c("longitude", "latitude"), crs = "NAD83")

block_locations_max <- ebd_df |>
  inner_join(test_block_max) |>
  select(pba3_block, block_name, longitude, latitude) |>
  arrange(pba3_block) |>
  distinct() |>
  collect() |>
  st_as_sf(coords = c("longitude", "latitude"), crs = "NAD83")

maplibre() |>
  fit_bounds(st_buffer(block_locations_max, 1000)) |>
  add_circle_layer(source = block_locations_max, id = "locations")

block_locations_min |>
  filter(
    pba3_block == block_locations_min |> slice_head(n = 1) |> pull(pba3_block)
  ) |>
  ggplot() +
  geom_sf() +
  facet_wrap(vars(block_name)) +
  scale_x_continuous(expand = expansion(mult = .2)) +
  scale_y_continuous(expand = expansion(mult = .2)) +
  labs(
    title = "Evenly distributed checklist locations",
  ) +
  theme_bw() +
  theme(axis.text = element_blank())

block_locations_max |>
  filter(
    pba3_block == block_locations_max |> slice_head(n = 1) |> pull(pba3_block)
  ) |>
  ggplot() +
  geom_sf() +
  facet_wrap(vars(block_name)) +
  scale_x_continuous(expand = expansion(mult = .2)) +
  scale_y_continuous(expand = expansion(mult = .2)) +
  labs(
    title = "Unevenly distributed checklist locations",
  ) +
  theme_bw() +
  theme(axis.text = element_blank())
