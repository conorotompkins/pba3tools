library(tidyverse)
library(arrow)
library(lubridate)
library(infer)

options(scipen = 999, digits = 4)

set.seed(1234)

theme_set(theme_bw(base_size = 12))

breeding_lookup <- read_parquet("data/breeding_lookup.parquet")

ebd_df <- open_dataset("data/pa_breeding_bird_atlas_processed.parquet") |>
  left_join(breeding_lookup) |>
  mutate(
    observation_date_yday = yday(observation_date),
    observation_date_year = year(observation_date),
    observation_date_month = month(observation_date, label = TRUE, abbr = TRUE)
  )

observers <- ebd_df |>
  filter(common_name == "Yellow-bellied Sapsucker") |>
  filter(breeding_code == "FL") |>
  collect() |>
  separate_rows(observer_id, sep = ",") |>
  count(observer_id, sort = TRUE)

observers

suspect_observer_id <- "obsr111149"

ebd_df |>
  filter(observer_id == suspect_observer_id) |>
  filter(common_name == "Yellow-bellied Sapsucker") |>
  filter(breeding_code == "FL") |>
  select(
    pba3_block,
    observer_id,
    checklist_id,
    starts_with("observation"),
    breeding_code
  ) |>
  collect() |>
  separate_rows(observer_id, sep = ",") |>
  glimpse()

obs_df <- ebd_df |>
  filter(common_name == "Yellow-bellied Sapsucker") |>
  filter(breeding_code == "FL") |>
  select(
    pba3_block,
    observer_id,
    checklist_id,
    starts_with("observation"),
    common_name,
    breeding_code
  ) |>
  collect() |>
  separate_rows(observer_id, sep = ",") |>
  mutate(
    suspect_observer_flag = case_when(
      observer_id == suspect_observer_id ~ "suspect",
      .default = "population"
    ) |>
      as.factor()
  )

obs_df |> glimpse()
#output
# Rows: 177
# Columns: 14
# $ pba3_block             <chr> "41076C3CE", "41076C3CE", "41075C2NW", "41075C2NW", "41076D7NW", "41076D7NW", "41076D7NW", "41076D7NW", "41080F1CW", "41080F1…
# $ observer_id            <chr> "obsr903100", "obsr38868", "obsr685551", "obsr928407", "obsr22956", "obsr608601", "obsr627332", "obsr1426024", "obsr1395650",…
# $ checklist_id           <chr> "G15106393", "G15106393", "G15175472", "G15175472", "G12589323", "G12589323", "G12589323", "G12589323", "G12612427", "G126124…
# $ observation_count      <chr> "3", "3", "4", "4", "3", "3", "3", "3", "6", "6", "2", "2", "2", "2", "2", "2", "1", "1", "1", "3", "3", "2", "2", "1", "1", …
# $ observation_date       <date> 2025-06-27, 2025-06-27, 2025-07-08, 2025-07-08, 2024-06-13, 2024-06-13, 2024-06-13, 2024-06-13, 2024-06-19, 2024-06-19, 2025…
# $ observation_type       <chr> "Traveling", "Traveling", "Traveling", "Traveling", "Traveling", "Traveling", "Traveling", "Traveling", "Traveling", "Traveli…
# $ observation_month      <ord> Jun, Jun, Jul, Jul, Jun, Jun, Jun, Jun, Jun, Jun, Jul, Jul, Jul, Jul, Jul, Jul, Aug, Aug, Aug, Aug, Aug, Aug, Aug, Aug, Aug, …
# $ observation_datetime   <dttm> 2025-06-27 10:01:00, 2025-06-27 10:01:00, 2025-07-08 07:56:00, 2025-07-08 07:56:00, 2024-06-13 09:09:00, 2024-06-13 09:09:00…
# $ observation_date_yday  <int> 178, 178, 189, 189, 165, 165, 165, 165, 171, 171, 207, 207, 209, 209, 209, 209, 215, 215, 215, 215, 215, 215, 215, 222, 222, …
# $ observation_date_year  <int> 2025, 2025, 2025, 2025, 2024, 2024, 2024, 2024, 2024, 2024, 2025, 2025, 2025, 2025, 2025, 2025, 2025, 2025, 2025, 2025, 2025,…
# $ observation_date_month <chr> "Jun", "Jun", "Jul", "Jul", "Jun", "Jun", "Jun", "Jun", "Jun", "Jun", "Jul", "Jul", "Jul", "Jul", "Jul", "Jul", "Aug", "Aug",…
# $ common_name            <chr> "Yellow-bellied Sapsucker", "Yellow-bellied Sapsucker", "Yellow-bellied Sapsucker", "Yellow-bellied Sapsucker", "Yellow-belli…
# $ breeding_code          <chr> "FL", "FL", "FL", "FL", "FL", "FL", "FL", "FL", "FL", "FL", "FL", "FL", "FL", "FL", "FL", "FL", "FL", "FL", "FL", "FL", "FL",…
# $ suspect_observer_flag  <fct> not suspect, not suspect, not suspect, not suspect, not suspect, not suspect, not suspect, not suspect, not suspect, not susp...

obs_df |>
  ggplot(aes(observation_date_yday)) +
  geom_histogram()

obs_df |>
  ggplot(aes(
    x = observation_date_yday,
    y = suspect_observer_flag,
    group = suspect_observer_flag
  )) +
  geom_boxplot() +
  geom_rug() +
  labs(
    title = glue::glue(
      "Comparing suspect observer {suspect_observer_id} vs. entire Atlas observer population"
    ),
    y = "Observation day of year",
    caption = "Yellow-bellied Sapsucker, Code FL"
  ) +
  theme(plot.title = element_text(size = 12))

obs_df |>
  ggplot(aes(observation_date_yday)) +
  geom_density() +
  geom_rug() +
  facet_wrap(vars(suspect_observer_flag), ncol = 1, scales = "free_y")

#difference in means
# calculate the observed statistic
observed_statistic <- obs_df |>
  specify(observation_date_yday ~ suspect_observer_flag) |>
  calculate(stat = "diff in means", order = c("suspect", "population"))

observed_statistic
#value is -0.216

# generate the null distribution with randomization
null_dist_2_sample <- obs_df |>
  specify(observation_date_yday ~ suspect_observer_flag) |>
  hypothesize(null = "independence") |>
  generate(reps = 10000, type = "permute") |>
  calculate(stat = "diff in means", order = c("suspect", "population"))

# visualize the randomization-based null distribution and test statistic!
null_dist_2_sample |>
  visualize() +
  shade_p_value(observed_statistic, direction = "two-sided")

# calculate the p value from the randomization-based null
# distribution and the observed statistic
p_value_2_sample <- null_dist_2_sample |>
  get_p_value(obs_stat = observed_statistic, direction = "two-sided")

p_value_2_sample
#value is 0.984

test_df <- ebd_df |>
  filter(common_name == "Yellow-bellied Sapsucker") |>
  filter(breeding_code == "FL") |>
  select(
    pba3_block,
    observer_id,
    checklist_id,
    starts_with("observation"),
    common_name,
    breeding_code
  ) |>
  collect() |>
  separate_rows(observer_id, sep = ",")
test_mean_shift <- function(x, suspect_observer_id) {
  obs_df <- x |>
    mutate(
      suspect_observer_flag = case_when(
        observer_id == suspect_observer_id ~ "suspect",
        .default = "population"
      ) |>
        as.factor()
    )

  #difference in means
  # calculate the observed statistic
  observed_statistic <- obs_df |>
    specify(observation_date_yday ~ suspect_observer_flag) |>
    calculate(stat = "diff in means", order = c("suspect", "population"))

  # generate the null distribution with randomization
  null_dist_2_sample <- obs_df |>
    specify(observation_date_yday ~ suspect_observer_flag) |>
    hypothesize(null = "independence") |>
    generate(reps = 1000, type = "permute") |>
    calculate(stat = "diff in means", order = c("suspect", "population"))

  # visualize the randomization-based null distribution and test statistic!
  plot_pvalue_null_dist <- null_dist_2_sample |>
    visualize() +
    shade_p_value(observed_statistic, direction = "two-sided")

  # calculate the p value from the randomization-based null
  # distribution and the observed statistic
  p_value_2_sample <- null_dist_2_sample |>
    get_p_value(obs_stat = observed_statistic, direction = "two-sided")

  p_value <- pull(p_value_2_sample, p_value)

  f_statistic <- pull(observed_statistic, stat)

  plot_boxplot <- obs_df |>
    ggplot(aes(
      x = observation_date_yday,
      y = suspect_observer_flag,
      group = suspect_observer_flag
    )) +
    geom_boxplot() +
    geom_rug() +
    labs(
      title = glue::glue(
        "Comparing suspect observer {suspect_observer_id} vs. entire Atlas observer population"
      ),
      subtitle = str_c(
        "P-value: ",
        p_value,
        "\n",
        "Mean shift: ",
        round(f_statistic, 2)
      ),
      y = "Observation day of year",
      caption = "Yellow-bellied Sapsucker, Code FL"
    ) +
    theme(plot.title = element_text(size = 12))

  list(
    "p_value" = p_value,
    "f_statistic" = pull(observed_statistic, stat),
    "boxplot" = plot_boxplot,
    "dist_plot" = plot_pvalue_null_dist
  )
}


fn_test <- test_mean_shift(test_df, suspect_observer_id = "obsr555649")

fn_test

full_test <- observers |>
  filter(n > 1) |>
  mutate(
    mean_test = map(
      observer_id,
      ~ test_mean_shift(x = test_df, suspect_observer_id = .x)
    )
  )

full_test_results <- full_test |>
  mutate(
    p_value = map_dbl(mean_test, "p_value"),
    mean_shift = map_dbl(mean_test, "f_statistic")
  )

full_test_results |>
  ggplot(aes(p_value)) +
  geom_boxplot() +
  geom_rug()

full_test_results |>
  ggplot(aes(p_value, mean_shift)) +
  geom_point()

full_test_results |>
  slice_min(p_value, n = 15) |>
  arrange(desc(p_value)) |>
  mutate(plots = map(mean_test, "boxplot")) |>
  pull(plots) |>
  patchwork::wrap_plots(guides = "collect", axes = "collect", ncol = 3)
