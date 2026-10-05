library(shiny)
library(bslib)
library(tidyverse)
library(reactable)
library(shinyWidgets)
library(sf)
library(arrow)
library(geoarrow)
library(tictoc)
library(mapgl)
library(quarto)
library(gt)
library(tsibble)
#library(basemaps)

options(shiny.fullstacktrace = FALSE)

#set_defaults(map_service = "esri", map_type = "natgeo_world_map")

####focal species from amber
focal_species <- read_csv("data/focal_species.csv")

####ebird release
ebird_release <- read_file("data/ebird_release.txt")

####breeding bird calendar
breeding_calendar_raw <- read_csv("data/breeding_calendar.csv")

breeding_calendar_long <- breeding_calendar_raw |>
  mutate(across(everything(), \(x) replace_na(x, ""))) |>
  select(-taxonomic_order) |>
  pivot_longer(
    cols = -c(common_name, priority_species),
    names_to = "week",
    values_to = "breeding_calendar_code"
  ) |>
  mutate(year_week_index = row_number(), .by = common_name)

year_week_index_df <- breeding_calendar_long |>
  distinct(year_week_index, week)

breeding_calendar_long <- breeding_calendar_long |>
  select(-year_week_index)

breeding_calendar_wide <- breeding_calendar_long |>
  pivot_wider(names_from = week, values_from = breeding_calendar_code)

breeding_color_formatting <- function(x) {
  colDef(
    style = function(value) {
      if (value == "B") {
        color <- "#c6e0b4"
      } else if (value == "E") {
        color <- "#ffe699"
      } else if (value == "N") {
        color <- "#bdd7ee"
      } else if (value == "M") {
        color <- "#fdbbd4"
      } else {
        color <- "white"
      }
      list(background = color, alpha = .5)
    }
  )
}

breeding_season_glossary_table <- tibble(
  Season = c("B", "E", "M", "N"),
  Description = c(
    "Breeding season only",
    "Either migration or breeding",
    "Migration",
    "Non-breeding season"
  )
)

#formatting for glossary
season_glossary_cols <- names(breeding_season_glossary_table)

season_glossary_col_styles <- map(
  season_glossary_cols,
  breeding_color_formatting
)

names(season_glossary_col_styles) <- season_glossary_cols

current_date <- Sys.Date()

current_year <- year(current_date)

format_date <- function(x) {
  str_c(current_year, "-", x) |> ydm()
}

####safe dates
safe_dates <- read_csv("data/bird_safe_dates.csv") |>
  select(
    common_name,
    safe_date_probable_start,
    safe_date_probable_end,
    safe_date_possible_start,
    safe_date_possible_end
  ) |>
  mutate(across(
    c(
      safe_date_probable_start,
      safe_date_probable_end,
      safe_date_possible_start,
      safe_date_possible_end
    ),
    format_date
  )) |>
  rename_with(.fn = ~ str_remove(.x, "safe_"), .cols = contains("date")) |>
  arrange(date_probable_start)

####block effort
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
    effort_breakdown,
    geometry
  )

effort_breakdown <- block_summary |>
  collect() |>
  st_drop_geometry() |>
  select(pba3_block, season, effort_breakdown) |>
  unnest(effort_breakdown)

block_summary <- block_summary |>
  select(-effort_breakdown) |>
  st_as_sf()

pba3_region_block_hierarchy <- block_summary |>
  distinct(block_region, pba3_block, block_name) |>
  filter(block_region != "Unknown region") |>
  arrange(block_region, pba3_block)

pba3_region_block_hierarchy <- pba3_region_block_hierarchy |>
  group_by(block_region) |>
  group_map(~ set_names(.x$pba3_block, .x$block_name)) |>
  set_names(unique(
    arrange(pba3_region_block_hierarchy, block_region)$block_region
  ))

ebd_df <- open_dataset("data/pa_breeding_bird_atlas_processed.parquet")

seasons <- read_parquet("data/seasons.parquet")

#block-atlas comparison
missing_pba2_breeding_category_obs <- read_parquet(
  "data/missing_pba2_breeding_category_obs.parquet"
)

completion_table <- block_summary |>
  filter(season == "All seasons") |>
  st_drop_geometry() |>
  as_tibble() |>
  drop_na(pba3_block) |>
  arrange(block_region, block_county, block_name) |>
  replace_na(list(Observed = 0, Possible = 0, Probable = 0, Confirmed = 0)) |>
  mutate(
    species_coded = Possible + Probable + Confirmed,
    possible_pct = Possible / species_coded,
    probable_pct = Probable / species_coded,
    confirmed_pct = Confirmed / species_coded
  ) |>
  mutate(
    flag_coded_species = species_coded >= 70,
    flag_confirmed_pct = confirmed_pct >= .25,
    flag_possible_pct = possible_pct <= .5,
    flag_coded_atlas_comparison = pba3_pba2_coded_count_compare_pct >= .8,
    flag_10_effort_hours = duration_hours_total >= 10,
    flag_breeeding_season_coverage = breeding_season_months_covered >= 3,
    flag_nocturnal_coverage = nocturnal_species_coded >= 2 |
      duration_hours_nocturnal >= 2
  ) |>
  rowwise() |>
  mutate(
    all_criteria_complete = flag_coded_species &
      flag_confirmed_pct &
      flag_possible_pct &
      flag_coded_atlas_comparison &
      flag_10_effort_hours &
      flag_breeeding_season_coverage &
      flag_nocturnal_coverage
  ) |>
  ungroup() |>
  select(
    pba3_block,
    block_name,
    block_county,
    all_criteria_complete,
    starts_with("flag")
  ) |>
  collect()

# Define server logic required to draw a histogram ----
server <- function(input, output, session) {
  current_month <- month(Sys.Date(), label = TRUE, abbr = TRUE) |>
    as.character()

  breeding_calendar <- eventReactive(
    c(
      input$toggle_current_month,
      input$toggle_exclude_na_code,
      input$toggle_show_priority_column
    ),
    {
      #filter with input$toggle_current_month
      x <- if (input$toggle_current_month) {
        current_month_index <- year_week_index_df |>
          filter(str_detect(week, current_month)) |>
          slice_head(n = 1) |>
          pull(year_week_index)

        breeding_calendar_long |>
          left_join(year_week_index_df) |>
          filter(year_week_index >= current_month_index) |>
          select(-year_week_index)
      } else if (input$toggle_current_month == FALSE) {
        breeding_calendar_long
      }

      x <- if (input$toggle_show_priority_column) {
        x
      } else {
        select(x, -priority_species)
      }

      #pivot wider at the end
      x <- x |>
        pivot_wider(names_from = week, values_from = breeding_calendar_code)

      #filter with input$toggle_exclude_na_code
      x <- if (input$toggle_exclude_na_code == TRUE) {
        first_col <- x |>
          select(3) |>
          names() |>
          sym()

        x |>
          filter(!!first_col != "")
      } else if (input$toggle_exclude_na_code == FALSE) {
        x
      }
    }
  )

  breeding_table_formatting <- reactive({
    #formatting for calendar
    month_cols <- breeding_calendar() |>
      select(-any_of(c("common_name", "priority_species"))) |>
      names()

    breeding_col_styles <- map(month_cols, breeding_color_formatting)

    names(breeding_col_styles) <- month_cols

    other_cols <- list(
      common_name = colDef(
        "Common Name",
        filterable = TRUE,
        sticky = "left",
        style = list(borderRight = "2px solid #eee"),
        headerStyle = list(borderRight = "1px solid #eee")
      )
    )

    other_cols <- if (input$toggle_show_priority_column) {
      priority_col <- list(
        priority_species = colDef("Priority", filterable = TRUE)
      )

      c(other_cols, priority_col)
    } else {
      other_cols
    }

    c(other_cols, breeding_col_styles)
  })

  output$calendar <- renderReactable({
    breeding_calendar() |>
      reactable(
        columns = breeding_table_formatting(),
        pagination = FALSE
      )
  })

  output$dates_table <- renderReactable({
    reactable(
      safe_dates,
      columns = list(
        common_name = colDef(
          "Common Name",
          filterable = TRUE,
          sticky = "left",
          style = list(borderRight = "2px solid #eee"),
          headerStyle = list(borderRight = "1px solid #eee")
        ),
        date_probable_start = colDef(
          name = "Probable start",
          cell = function(value) strftime(value, "%b %e")
        ),
        date_probable_end = colDef(
          name = "Probable end",
          cell = function(value) strftime(value, "%b %e")
        ),
        date_possible_start = colDef(
          name = "Possible start",
          cell = function(value) strftime(value, "%b %e")
        ),
        date_possible_end = colDef(
          name = "Possible end",
          cell = function(value) strftime(value, "%b %e")
        )
      ),
      pagination = FALSE
    )
  })

  output$breeding_season_glossary_table <- renderReactable({
    breeding_season_glossary_table |>
      reactable(
        columns = season_glossary_col_styles,
        defaultColDef = colDef(maxWidth = 250)
      )
  })

  #block effort map
  output$block_effort_map <- renderMaplibre({
    block_summary <- block_summary |>
      filter(season == input$season_variable) |>
      collect()

    maplibre_view(
      block_summary,
      column = input$block_variable,
      legend_positon = "top-right"
    )
  })

  output$block_progress_table <- renderReactable({
    x <- block_summary |>
      st_drop_geometry() |>
      filter(season == input$season_variable_table) |>
      select(-season) |>
      arrange(block_region, block_county, block_name) |>
      collect()

    reactable(
      x,
      resizable = TRUE,
      columns = list(
        pba3_block = colDef(
          name = "Block ID",
          filterable = TRUE,
          minWidth = 150,
          cell = function(value) {
            url <- paste0("https://ebird.org/atlaspa/block/", value)
            tags$a(href = url, target = "_blank", value)
          }
        ),
        block_name = colDef(
          name = "Block name",
          filterable = TRUE,
          minWidth = 220
        ),
        block_region = colDef(
          name = "Block region",
          filterable = TRUE,
          minWidth = 180
        ),
        block_county = colDef(
          name = "Block county",
          filterable = TRUE,
          minWidth = 160
        ),
        checklist_count = colDef(name = "Checklists", minWidth = 100),
        species_observed = colDef(name = "Total species", minWidth = 120),
        birders = colDef(name = "Atlasers", minWidth = 100),
        duration_hours_total = colDef(
          name = "Total",
          format = colFormat(digits = 2),
          minWidth = 150
        ),
        duration_hours_diurnal = colDef(
          name = "Diurnal",
          format = colFormat(digits = 2),
          minWidth = 150
        ),
        duration_hours_nocturnal = colDef(
          name = "Nocturnal",
          format = colFormat(digits = 2),
          minWidth = 150
        ),
        duration_hours_unknown = colDef(
          name = "Unknown",
          format = colFormat(digits = 2),
          minWidth = 150
        ),
        effort_distance_km = colDef(
          name = "Effort distance (km)",
          minWidth = 175
        ),
        pct_missing_pba2_confirmations = colDef(
          name = "% of PBA2 confirmations missing",
          format = colFormat(percent = TRUE, digits = 0),
          minWidth = 300
        ),
        pct_coded_atlas_comparison = colDef(
          name = "% of species coded PBA3 >= PBA2",
          format = colFormat(percent = TRUE, digits = 0),
          minWidth = 300
        ),
        pba3_pba2_coded_count_compare_pct = colDef(
          name = "Coded species: PBA3 vs. PBA2",
          format = colFormat(percent = TRUE, digits = 0),
          minWidth = 300
        ),
        breeding_season_months_covered = colDef(
          name = "Breeding season months covered",
          minWidth = 300
        ),
        nocturnal_species_coded = colDef(
          name = "Nocturnal species coded",
          minWidth = 300
        )
      ),
      defaultPageSize = 15,
      columnGroups = list(
        colGroup(
          name = "Coding",
          columns = c(
            "species_observed",
            "Observed",
            "Possible",
            "Probable",
            "Confirmed",
            "nocturnal_species_coded"
          )
        ),
        colGroup(
          name = "Effort",
          columns = c(
            "duration_hours_total",
            "duration_hours_diurnal",
            "duration_hours_nocturnal",
            "duration_hours_unknown",
            "effort_distance_km",
            "breeding_season_months_covered"
          )
        ),
        colGroup(
          name = "Atlas comparison metrics",
          columns = c(
            "pct_missing_pba2_confirmations",
            "pct_coded_atlas_comparison",
            "pba3_pba2_coded_count_compare_pct"
          )
        )
      )
    )
  })

  atlas_comparison_missing <- reactive({
    req(input$report_block_id)

    missing_pba2_breeding_category_obs |>
      filter(pba3_block == input$report_block_id) |>
      mutate(atlas_diff = pba2_breeding_rank_max - pba3_breeding_rank_max) |>
      arrange(desc(atlas_diff)) |>
      left_join(focal_species, by = "common_name") |>
      mutate(is_focal = coalesce(is_focal, FALSE)) |>
      select(
        pba3_block,
        block_name,
        common_name,
        is_focal,
        pba3_breeding_category_max,
        pba2_breeding_category_max
      )
  })

  output$block_atlas_comparison_missing_table <- renderReactable({
    req(nrow(atlas_comparison_missing()) > 0)

    atlas_comparison_missing() |>
      reactable(
        resizable = TRUE,
        columns = list(
          pba3_block = colDef(show = FALSE),
          block_name = colDef(
            name = "Block name",
            cell = function(value, index) {
              url <- paste0(
                "https://ebird.org/atlaspa/block/",
                atlas_comparison_missing()$pba3_block[index]
              )
              tags$a(href = url, target = "_blank", value)
            },
            maxWidth = 220
          ),
          common_name = colDef(name = "Common Name"),
          pba3_breeding_category_max = colDef(
            name = "Max PBA3 Breeding Category"
          ),
          pba2_breeding_category_max = colDef(
            name = "Max PBA2 Breeding Category"
          ),
          is_focal = colDef(name = "Focal species", filterable = TRUE)
        ),
        pagination = FALSE,
        height = 600
      )
  })

  output$readme <- renderUI({
    tags$iframe(
      seamless = "seamless",
      src = "readme.html",
      width = "100%",
      height = "100%"
    )
  })

  output$ebird_release <- renderUI({
    str_c("Data includes checklists from Jan-2024 to", ebird_release, sep = " ")
  })

  #logic for block report export
  #not in scope right now due to memory contraint in free tier of Posit Connect Cloud
  # report_filename <- reactive({
  #   paste0(
  #     "pba3_atlas_block_report_",
  #     input$report_block_id,
  #     "_",
  #     input$report_season,
  #     ".",
  #     input$report_format
  #   ) |>
  #     str_replace_all(" ", "_")
  # })

  # output$download_report <- downloadHandler(
  #   filename = \() {
  #     report_filename()
  #   },
  #   content = \(file) {
  #     project_dir <- normalizePath(".")
  #     report_path <- file.path(project_dir, "reports", "block_report.qmd")
  #     report_dir <- dirname(report_path)
  #     out_name <- report_filename()
  #     out_path <- file.path(report_dir, out_name)

  #     old <- setwd(project_dir)
  #     on.exit(setwd(old), add = TRUE)

  #     quarto::quarto_render(
  #       input = report_path,
  #       output_format = input$report_format,
  #       output_file = out_name,
  #       execute_params = list(block_id = input$report_block_id, season = input$report_season),
  #       execute_dir = project_dir,
  #       quiet = FALSE
  #     )

  #     file.copy(out_path, file, overwrite = TRUE)

  #     if (dir.exists(file.path(report_dir, "block_report_files"))) {
  #       unlink(file.path(report_dir, "block_report_files"), recursive = TRUE)
  #     }

  #     if (file.exists(out_path)) {
  #       unlink(out_path)
  #     }
  #   }
  # )

  observeEvent(1, {
    new_choices <- pba3_region_block_hierarchy

    # Can also set the label and select items
    updateSelectizeInput(
      session,
      "report_block_id",
      choices = new_choices,
      selected = pba3_region_block_hierarchy |>
        pluck(1, 1)
    )
  })

  bird_df_summary <- reactive({
    block_summary |>
      filter(
        pba3_block == input$report_block_id,
        season == input$report_season
      ) |>
      collect() |>
      st_drop_geometry()
  })

  ebd_df_summary <- reactive({
    seasons_filtered <- seasons |>
      filter(season == input$report_season)

    ebd_df |>
      filter(pba3_block == input$report_block_id) |>
      collect() |>
      semi_join(seasons_filtered, by = c("observation_month" = "month"))
  })

  output$summary_effort <- render_gt({
    summarize_effort(bird_df_summary())
  })

  output$summary_breeding_codes <- render_gt({
    summarize_breeding_codes(bird_df_summary())
  })

  output$summary_checklist_map <- renderMaplibre({
    req(input$report_block_id, input$report_season)

    ebd_df_summary() |>
      distinct(checklist_id, longitude, latitude) |>
      count(longitude, latitude, name = "checklist_count") |>
      st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
      map_checklist_count()
  })

  output$effort_breakdown <- renderPlot({
    req(input$report_block_id, input$report_season)

    x <- effort_breakdown |>
      filter(pba3_block == "40080D1SE") |>
      mutate(
        obs_ym = yearmonth(observation_date),
        obs_year = year(observation_date),
        obs_month = month(observation_date, label = TRUE, abbr = TRUE)
      ) |>
      rowwise() |>
      mutate(
        duration_hours_total = sum(
          duration_hours_diurnal,
          duration_hours_nocturnal,
          duration_hours_unknown,
          na.rm = TRUE
        ),
      ) |>
      ungroup() |>
      select(pba3_block, season, starts_with("obs"), starts_with("duration"))

    facet_label <- "Year"

    x |>
      summarize(
        across(starts_with("duration"), sum),
        .by = c(obs_year, obs_month)
      ) |>
      ggplot(aes(x = obs_month, y = duration_hours_total, group = 1)) +
      geom_line() +
      geom_point() +
      facet_wrap(vars(str_c(facet_label, ": ", obs_year, sep = "")), ncol = 1) +
      scale_y_continuous(expand = expansion(mult = .2)) +
      labs(title = "Effort hours across time", x = "Date", y = "Effort") +
      theme_bw(base_size = 18)
  })

  output$block_completion_table <- renderReactable({
    grouped_col_style <- list(fontSize = ".8rem")

    reactable(
      completion_table,
      resizable = TRUE,
      columns = list(
        pba3_block = colDef(
          name = "Block ID",
          filterable = TRUE,
          minWidth = 110,
          cell = function(value) {
            url <- paste0("https://ebird.org/atlaspa/block/", value)
            tags$a(href = url, target = "_blank", value)
          }
        ),
        block_name = colDef(
          name = "Block name",
          filterable = TRUE,
          minWidth = 150
        ),
        block_county = colDef(
          name = "County",
          filterable = TRUE,
          minWidth = 100
        ),
        all_criteria_complete = colDef(
          name = "All criteria complete",
          filterable = TRUE,
          minWidth = 100
        ),
        flag_coded_species = colDef(
          name = "Coded species >= 70",
          filterable = TRUE,
          headerStyle = grouped_col_style
        ),
        flag_confirmed_pct = colDef(
          name = "Confirmed % >= 25%",
          filterable = TRUE,
          headerStyle = grouped_col_style
        ),
        flag_possible_pct = colDef(
          "Possible % <= 50%",
          filterable = TRUE,
          headerStyle = grouped_col_style
        ),
        flag_coded_atlas_comparison = colDef(
          "Coded species >= 80% of PBA2",
          filterable = TRUE,
          headerStyle = grouped_col_style
        ),
        flag_10_effort_hours = colDef(
          name = ">= 10 effort hours",
          filterable = TRUE,
          headerStyle = grouped_col_style
        ),
        flag_breeeding_season_coverage = colDef(
          name = ">= 3 months with checklists in breeding season",
          filterable = TRUE,
          headerStyle = grouped_col_style
        ),
        flag_nocturnal_coverage = colDef(
          name = ">= 2 nocturnal hours OR >= 2 nocturnal species coded",
          filterable = TRUE,
          headerStyle = grouped_col_style
        )
      ),
      columnGroups = list(
        colGroup(
          name = "Ideal Completion Criteria",
          columns = c(
            "flag_coded_species",
            "flag_confirmed_pct",
            "flag_possible_pct",
            "flag_coded_atlas_comparison",
            "flag_10_effort_hours",
            "flag_breeeding_season_coverage",
            "flag_nocturnal_coverage"
          )
        )
      ),
      pagination = TRUE,
      defaultPageSize = 20
    )
  })
}
