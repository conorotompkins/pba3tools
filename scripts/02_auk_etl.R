library(tidyverse)
library(auk)
library(tictoc)
library(sf)
library(tools)
library(arrow)

source("R/year_month_to_last_date.R")

auk_file <- "data/ebd_US-PA_202401_202606_smp_relJun-2026/ebd_US-PA_202401_202606_smp_relJun-2026.txt"

file.exists(auk_file) == TRUE

month_regex <- "(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)"

release_pattern <- "(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)-\\d{4}.txt"

ebird_release <- str_extract(auk_file, release_pattern) |>
  file_path_sans_ext()

ebird_release

write_file(ebird_release, "data/ebird_release.txt")

output_file <- "data/pa_breeding_bird_atlas_data_raw.txt"

date_start <- lubridate::ymd("2024-01-01")

date_end <- year_month_to_last_date(ebird_release)

paste(date_start, date_end) |> print()

tic()
ebd <- auk_file |>
  auk_ebd() |>
  auk_date(date = c(date_start, date_end)) |> #need to update every refresh
  auk_project("Pennsylvania Bird Atlas") |>
  auk_filter(file = output_file, overwrite = TRUE) |>
  # 4. read text file into r data frame
  read_ebd()
toc()
# 1041.117 sec elapsed

glimpse(ebd)

ebd <- ebd |>
  mutate(across(c(breeding_code, breeding_category), str_squish)) |>
  mutate(breeding_category = coalesce(breeding_category, "C1")) |> #should this be C0 instead of C1?
  mutate(
    observation_month = month(observation_date, label = TRUE, abbr = TRUE),
    observation_datetime = str_c(
      observation_date,
      time_observations_started,
      sep = " "
    ) |>
      ymd_hms(tz = "America/New_York")
  ) |>
  rename(pba3_block = atlas_block)

write_parquet(ebd, "data/pa_breeding_bird_atlas_processed.parquet")

ebd |> distinct(project_names)

ebd |>
  st_as_sf(coords = c("longitude", "latitude")) |>
  slice_sample(prop = .1) |>
  ggplot() +
  geom_sf(alpha = .01, size = .5) +
  theme_void()

ebd |>
  distinct(observation_date) |>
  filter(observation_date == max(observation_date))

ebd |>
  distinct(checklist_id) |>
  nrow()
