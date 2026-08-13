library(tidyverse)
library(auk)
library(sf)
library(arrow)
library(geoarrow)
library(tictoc)
library(mapgl)
library(glue)

options(scipen = 999, digits = 4)

theme_set(theme_bw())

source("R/mode.R")
source("R/summarize_season.R")

#block name lookup file
block_name_lookup <- read_csv("data/block_name_lookup.csv") |>
  distinct(block_id, region, block_name, cnty_name) |>
  rename(pba3_block = block_id, block_region = region, block_county = cnty_name)

#location sunrise/sunset
location_sunrise_sunset <- read_parquet(
  "data/location_sunrise_sunset.parquet"
)

breeding_lookup <- tibble(
  breeding_category = c("0", "C1", "C2", "C3", "C4"), #consider C0 instead of 0 to be consistent
  breeding_category_desc = c(
    "Not Observed",
    "Observed",
    "Possible",
    "Probable",
    "Confirmed"
  ),
  breeding_rank = c(0:4)
)

#nocturnal priority species
nocturnal_species <- read_csv("data/nocturnal_priority_species.csv")

#checklists
tic()
ebd_df <- open_dataset("data/pa_breeding_bird_atlas_processed.parquet")
toc()

ebd_df <- ebd_df |>
  left_join(breeding_lookup, by = join_by(breeding_category))

dupe_start_times <- ebd_df |>
  select(checklist_id, observation_datetime) |>
  distinct() |>
  count(checklist_id) |>
  filter(n > 1) |>
  collect()

#find modal start time per checklist. I will use that for all observers for each checklist.
ob_dt_fixed <- ebd_df |>
  distinct(
    pba3_block,
    checklist_id,
    observer_id,
    observation_datetime
  ) |>
  semi_join(dupe_start_times) |>
  collect() |>
  separate_longer_delim(observer_id, delim = ",") |>
  mutate(
    observation_datetime_fixed = mode(observation_datetime),
    .by = checklist_id
  ) |>
  distinct(checklist_id, observation_datetime_fixed)

pba2_blocks <- st_read("data/PABBA_2nd/PABBA_2nd.shp") |>
  select(BLOCK_ID) |>
  rename(pba2_block = BLOCK_ID)

#distinct of checklist coordinates and pba3_block
checklist_pba3_block <- ebd_df |>
  distinct(pba3_block, checklist_id, longitude, latitude) |>
  collect() |>
  st_as_sf(coords = c("longitude", "latitude"))

st_crs(checklist_pba3_block) <- st_crs(pba2_blocks)

#calculate centroid of all checklist coordinates in each pba3_block
pba3_centroids <- checklist_pba3_block |>
  drop_na(pba3_block) |>
  group_by(pba3_block) |>
  slice_sample(n = 1000) |>
  summarize() |>
  st_convex_hull() |>
  st_point_on_surface()

#join pba2 blocks with pba3 centroids
tic()
block_checklist_geo <- st_join(
  pba2_blocks,
  pba3_centroids,
  join = st_covers,
  largest = FALSE
)
toc()

block_checklist_geo <- block_checklist_geo |>
  mutate(
    pba2_block = case_when(
      pba3_block == "40075F2SE" ~ 4932,
      .default = pba2_block
    ),
  ) |>
  filter(!(pba2_block == 4932 & is.na(pba3_block)))

#seasons
seasons <- tibble(
  season = c(rep(c("All seasons"), 12), rep("Breeding", 5), rep("Winter", 3)),
  month = c(
    month.abb,
    c("Apr", "May", "Jun", "Jul", "Aug"),
    c("Dec", "Jan", "Feb")
  )
)


#compare block breeding rank between PBA2 and PBA3
#expected species based on PBA2
pbba2_df <- read_csv("data/PBBA2_block_species_codes.csv") |>
  rename(
    block = 1,
    pba3_block = 2,
    block_name = 3
  ) |>
  select(1:220) |>
  pivot_longer(
    -c(1:3),
    names_to = "common_name",
    values_to = "breeding_category_desc"
  ) |>
  filter(!str_detect(common_name, "N/A")) |>
  #update common names to new taxonomy
  mutate(
    common_name = case_when(
      common_name == "Yellow Warbler" ~ "Northern Yellow Warbler",
      common_name == "Warbling Vireo" ~ "Eastern Warbling Vireo",
      common_name == "Barn Owl" ~ "American Barn Owl",
      common_name == "Herring Gull" ~ "American Herring Gull",
      common_name == "Northern Goshawk" ~ "American Goshawk",
      common_name == "House Wren" ~ "Northern House Wren",
      common_name == "Western Cattle Egret" ~ "Western Cattle-Egret",
      .default = common_name
    ),
  ) |>
  mutate(
    breeding_category_desc = case_when(
      breeding_category_desc == "Observed/Possible" ~ "Observed",
      .default = breeding_category_desc
    )
  )

pba2_breeding_rank_max <- pbba2_df |>
  left_join(breeding_lookup, by = join_by(breeding_category_desc)) |>
  distinct(pba3_block, common_name, breeding_category_desc, breeding_rank) |>
  rename(
    pba2_breeding_category_max = breeding_category_desc,
    pba2_breeding_rank_max = breeding_rank
  )

pba3_breeding_rank_max <- ebd_df |>
  semi_join(
    seasons |> filter(season == "Breeding"),
    by = c("observation_month" = "month")
  ) |>
  collect() |>
  group_by(pba3_block, common_name) |>
  filter(breeding_rank == max(breeding_rank)) |>
  ungroup() |>
  distinct(pba3_block, common_name, breeding_category_desc, breeding_rank) |>
  rename(
    pba3_breeding_category_max = breeding_category_desc,
    pba3_breeding_rank_max = breeding_rank
  )

pba2_confirmed_blocks <- distinct(pba2_breeding_rank_max, pba3_block)

pba3_confirmed_blocks <- distinct(pba3_breeding_rank_max, pba3_block)

#compare blocks that exist in PBA2 or PBA3
atlas_max_breeding_rank_comparison <- bind_rows(
  pba2_breeding_rank_max |> distinct(pba3_block, common_name),
  pba3_breeding_rank_max |> distinct(pba3_block, common_name)
) |>
  distinct() |>
  left_join(pba2_breeding_rank_max, by = join_by(pba3_block, common_name)) |>
  left_join(pba3_breeding_rank_max, by = join_by(pba3_block, common_name)) |>
  replace_na(list(
    pba2_breeding_category_max = "Not Observed",
    pba2_breeding_rank_max = 0,
    pba3_breeding_category_max = "Not Observed",
    pba3_breeding_rank_max = 0
  )) |>
  left_join(block_name_lookup, by = join_by(pba3_block)) |>
  mutate(
    block_name = coalesce(
      block_name,
      "Unknown block name"
    ),
    block_region = coalesce(
      block_region,
      "Unknown region"
    ),
    block_county = coalesce(
      block_county,
      "Unknown county"
    )
  ) |>
  select(pba3_block, block_name, block_region, everything())

atlas_block_comparison <- atlas_max_breeding_rank_comparison |>
  summarize(
    species_count_pba2 = sum(pba2_breeding_rank_max > 0),
    species_coded_pba2 = sum(pba2_breeding_rank_max > 1),
    species_count_pba3 = sum(pba3_breeding_rank_max > 0),
    species_coded_pba3 = sum(pba3_breeding_rank_max > 1),
    pct_missing_pba2_confirmations = mean(
      pba2_breeding_rank_max == 4 & pba3_breeding_rank_max < 4,
      na.rm = TRUE
    ),
    pct_coded_atlas_comparison = mean(
      pba3_breeding_rank_max >= pba2_breeding_rank_max,
      na.rm = TRUE
    ),
    .by = pba3_block
  ) |>
  mutate(
    pba3_pba2_coded_count_compare_pct = species_coded_pba3 / species_coded_pba2
  ) |>
  arrange(desc(species_count_pba3)) |>
  mutate(season = "Breeding")

test_data <- ebd_df |>
  filter(pba3_block == "40080D1SE") #Pittsburgh West SE

calc_checklist_count_results <- calc_checklist_count(test_data)

summarized_season_results <- summarize_season(
  test_data,
  season_filter = "All seasons"
)

summarized_season_results

saveRDS(
  summarized_season_results,
  "tests/testthat/fixtures/summarized_season_results.rds"
)
