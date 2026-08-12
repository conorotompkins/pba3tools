expected <- readRDS(test_path("fixtures", "summarized_season_results.rds"))

test_that("calc_checklist_count counts distinct checklists per block", {
  x <- tibble(
    pba3_block = c("A", "A", "A", "B"),
    checklist_id = c("c1", "c1", "c2", "c3")
  )

  result <- calc_checklist_count(x) |> arrange(pba3_block)

  expect_equal(
    result,
    tibble(pba3_block = c("A", "B"), checklist_count = c(2L, 1L))
  )
})

test_that("calc_species_observed counts distinct species per block", {
  x <- tibble(
    pba3_block = c("A", "A", "A", "B"),
    common_name = c("Robin", "Robin", "Blue Jay", "Robin")
  )

  result <- calc_species_observed(x) |> arrange(pba3_block)

  expect_equal(
    result,
    tibble(pba3_block = c("A", "B"), species_observed = c(2L, 1L))
  )
})

test_that("calc_atlasers counts distinct observers, splitting comma-separated ids", {
  x <- tibble(
    pba3_block = c("A", "A", "B"),
    observer_id = c("obs1,obs2", "obs1", "obs3")
  )

  result <- calc_atlasers(x) |> arrange(pba3_block)

  expect_equal(
    result,
    tibble(pba3_block = c("A", "B"), birders = c(2L, 1L))
  )
})

test_that("calc_block_effort sums max duration and distance per block", {
  x <- tibble(
    pba3_block = c("A", "A", "A", "B"),
    checklist_id = c("c1", "c1", "c2", "c3"),
    duration_minutes = c(60, 90, 30, 45),
    effort_distance_km = c(1, 2, 0.5, 3)
  )

  result <- calc_block_effort(x) |> arrange(pba3_block)

  # Block A: checklist c1 contributes its max (90 min, 2 km), c2 contributes
  # (30 min, 0.5 km); block B has a single checklist.
  expect_equal(
    result,
    tibble(
      pba3_block = c("A", "B"),
      duration_hours_total = c((90 + 30) / 60, 45 / 60),
      effort_distance_km = c(2 + 0.5, 3)
    )
  )
})

test_that("calc_species_coded keeps the max breeding rank per species and pivots wide", {
  x <- tibble(
    pba3_block = c("A", "A", "A", "B"),
    common_name = c("Robin", "Robin", "Blue Jay", "Robin"),
    breeding_category_desc = c("Observed", "Confirmed", "Possible", "Probable"),
    breeding_rank = c(1, 4, 2, 3)
  )

  result <- calc_species_coded(x) |> arrange(pba3_block)

  # Block A: Robin's max rank is Confirmed (4); Blue Jay is Possible (2).
  # Block B: Robin is Probable (3).
  expect_equal(
    result,
    tibble(
      pba3_block = c("A", "B"),
      Confirmed = c(1L, NA_integer_),
      Possible = c(1L, NA_integer_),
      Probable = c(NA_integer_, 1L)
    )
  )
})

test_that("calc_nocturnal_diurnal_effort classifies checklists by sunrise/sunset", {
  x <- tibble(
    pba3_block = c("A", "A"),
    checklist_id = c("c1", "c2"),
    observer_id = c("obs1", "obs2"),
    observation_datetime = as_datetime(c(
      "2024-06-01 08:00:00",
      "2024-06-01 23:00:00"
    )),
    longitude = c(-79.9, -79.9),
    latitude = c(40.4, 40.4),
    duration_minutes = c(60, 30)
  )

  # No modal-start-time corrections needed for this test.
  y <- tibble(
    checklist_id = character(0),
    observation_datetime_fixed = as_datetime(character(0))
  )

  z <- tibble(
    longitude = c(-79.9, -79.9),
    latitude = c(40.4, 40.4),
    observation_datetime = as_datetime(c(
      "2024-06-01 08:00:00",
      "2024-06-01 23:00:00"
    )),
    sunrise = as_datetime(c("2024-06-01 05:45:00", "2024-06-01 05:45:00")),
    sunset = as_datetime(c("2024-06-01 20:30:00", "2024-06-01 20:30:00"))
  )

  result <- calc_nocturnal_diurnal_effort(x, y, z)

  # c1 (08:00) falls within sunrise-40min/sunset+20min -> diurnal (1 hr);
  # c2 (23:00) falls outside -> nocturnal (0.5 hr).
  expect_equal(result$pba3_block, "A")
  expect_equal(result$duration_hours_diurnal, 1)
  expect_equal(result$duration_hours_nocturnal, 0.5)
  expect_equal(result$duration_hours_unknown, 0)

  effort_breakdown <- result$effort_breakdown[[1]]
  expect_equal(nrow(effort_breakdown), 1)
  expect_equal(effort_breakdown$observation_date, as_date("2024-06-01"))
  expect_equal(effort_breakdown$duration_hours_diurnal, 1)
  expect_equal(effort_breakdown$duration_hours_nocturnal, 0.5)
})

test_that("calc_breeding_season_coverage counts distinct breeding months observed", {
  x <- tibble(
    pba3_block = c("A", "A", "A", "B"),
    observation_datetime = as_datetime(c(
      "2024-05-01",
      "2024-06-01",
      "2024-12-01",
      "2024-05-01"
    ))
  )

  y <- tibble(
    season = c(rep("Breeding", 5), rep("Winter", 3)),
    month = c("Apr", "May", "Jun", "Jul", "Aug", "Dec", "Jan", "Feb")
  )

  result <- calc_breeding_season_coverage(x, y) |> arrange(pba3_block)

  # Block A: May and Jun count (Dec is Winter, excluded); block B: May only.
  expect_equal(
    result,
    tibble(pba3_block = c("A", "B"), breeding_season_months_covered = c(2L, 1L))
  )
})

test_that("calc_nocturnal_species_coded counts coded nocturnal species, defaulting to 0", {
  x <- tibble(
    pba3_block = c("A", "A", "B", "B"),
    common_name = c(
      "Eastern Screech-Owl",
      "Robin",
      "Eastern Screech-Owl",
      "Barred Owl"
    ),
    breeding_rank = c(2, 3, 1, 0)
  )

  y <- tibble(common_name = c("Eastern Screech-Owl", "Barred Owl"))

  result <- calc_nocturnal_species_coded(x, y) |> arrange(pba3_block)

  # Block A: Screech-Owl at rank 2 counts (Robin isn't a nocturnal species).
  # Block B: Screech-Owl (rank 1) and Barred Owl (rank 0) are both below the
  # rank >= 2 threshold, so the count defaults to 0 via complete()/coalesce().
  expect_equal(
    result,
    tibble(pba3_block = c("A", "B"), nocturnal_species_coded = c(1L, 0L))
  )
})

test_that("summarize_season output has expected structure", {
  expect_s3_class(expected, "data.frame")
  expect_equal(nrow(expected), 1)

  expect_named(
    expected,
    c(
      "pba3_block",
      "checklist_count",
      "species_observed",
      "birders",
      "duration_hours_total",
      "effort_distance_km",
      "Confirmed",
      "Observed",
      "Possible",
      "Probable",
      "duration_hours_diurnal",
      "duration_hours_nocturnal",
      "duration_hours_unknown",
      "effort_breakdown",
      "breeding_season_months_covered",
      "nocturnal_species_coded"
    )
  )

  expect_equal(expected$pba3_block, "40080D1SE")
})

test_that("summarize_season effort hours are internally consistent", {
  duration_components <- expected$duration_hours_diurnal +
    expected$duration_hours_nocturnal +
    expected$duration_hours_unknown

  expect_equal(
    duration_components,
    expected$duration_hours_total,
    tolerance = 1e-6
  )
})

test_that("effort_breakdown is a nested tibble with per-date effort", {
  effort_breakdown <- expected$effort_breakdown[[1]]

  expect_s3_class(effort_breakdown, "data.frame")
  expect_named(
    effort_breakdown,
    c(
      "observation_date",
      "duration_hours_diurnal",
      "duration_hours_nocturnal",
      "duration_hours_unknown"
    )
  )
})

test_that("breeding_season_months_covered is between 0 and 5", {
  expect_gte(expected$breeding_season_months_covered, 0)
  expect_lte(expected$breeding_season_months_covered, 5)
})

test_that("nocturnal_species_coded is non-negative", {
  expect_gte(expected$nocturnal_species_coded, 0)
})
