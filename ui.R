library(shiny)
library(bslib)
library(bsicons)
library(reactable)
library(shinyWidgets)
library(mapgl)
library(gt)

csvDownloadButton <- function(
  id,
  filename = NULL,
  label = "Download as CSV"
) {
  div(
    style = "text-align: left;",
    tags$button(
      tagList(icon("download"), label),
      type = "button",
      class = "btn btn-sm btn-outline-secondary",
      style = "width: auto; display: inline-block;",
      onclick = sprintf(
        "Reactable.downloadDataCSV('%s', '%s')",
        id,
        filename
      )
    )
  )
}

completion_criteria_value_boxes <- list(
  value_box(title = "Completion criterion 1", value = 1),
  value_box(title = "Completion criterion 2", value = 2),
  value_box(title = "Completion criterion 3", value = 3),
  value_box(title = "Completion criterion 4", value = 4),
  value_box(title = "Completion criterion 5", value = 5)
)

ui <- page_navbar(
  fillable_mobile = TRUE,

  title = "PBA3 Tools",

  window_title = "PBA3 Tools",

  nav_spacer(),

  nav_menu(
    "Breeding season calendar",
    nav_panel(
      "Calendar",

      card(reactableOutput(outputId = "calendar"), full_screen = TRUE)
    ),

    nav_panel(
      "Safe Dates",

      card(reactableOutput("dates_table"), full_screen = TRUE)
    ),

    nav_panel(
      "Season glossary",

      reactableOutput("breeding_season_glossary_table")
    )
  ),

  nav_menu(
    "Block Progress",
    nav_panel(
      "Map",

      layout_columns(
        radioGroupButtons(
          inputId = "season_variable",
          label = "Select season",
          choices = c(
            "All seasons" = "All seasons",
            "Breeding" = "Breeding",
            "Winter" = "Winter"
          ),
          selected = "Breeding"
        ),
        radioGroupButtons(
          inputId = "block_variable",
          label = "Variable",
          choices = c(
            "Effort hours (total)" = "duration_hours_total",
            "Effort hours (diurnal)" = "duration_hours_diurnal",
            "Effort hours (nocturnal)" = "duration_hours_nocturnal",
            "Confirmed species" = "Confirmed"
          )
        )
      ),
      maplibreOutput("block_effort_map")
    ),

    nav_panel(
      "Table",
      radioGroupButtons(
        inputId = "season_variable_table",
        label = "Select season",
        choices = c(
          "All seasons" = "All seasons",
          "Breeding" = "Breeding",
          "Winter" = "Winter"
        ),
        selected = "Breeding"
      ),
      csvDownloadButton(
        "block_progress_table",
        filename = "pba3_block_progress_table.csv"
      ),
      reactableOutput("block_progress_table")
    ),

    nav_panel(
      "Block progress report",
      layout_sidebar(
        # sidebar + main content
        sidebar = sidebar(
          open = TRUE,
          width = 300,
          title = "Select a block and season",

          selectizeInput(
            inputId = "report_block_id",
            label = "Block ID",
            choices = c("40080D1SE", "39077G6SW")
          ),

          selectInput(
            inputId = "report_season",
            label = "Season",
            choices = c("All seasons", "Breeding", "Winter"),
            selected = "Breeding"
          ),

          # selectInput(
          #   "report_format",
          #   "Format",
          #   choices = c("html", "pdf"),
          #   selected = "html"
          # ),
          # downloadButton("download_report", "Download report"),
        ),
        accordion(
          accordion_panel(
            "Effort",
            layout_columns(
              col_widths = c(6, 6, 12),
              gt_output("summary_effort"),
              gt_output("summary_breeding_codes")
            )
          ),

          accordion_panel(
            "Atlas Comparison",
            card(
              csvDownloadButton(
                id = "block_atlas_comparison_missing_table",
                filename = "atlas_comparison_table.csv"
              ),
              reactableOutput("block_atlas_comparison_missing_table"),
              max_height = 700,
              full_screen = TRUE
            )
          ),

          accordion_panel(
            "Checklist map",
            card(
              maplibreOutput("summary_checklist_map"),
              full_screen = TRUE
            )
          ),

          accordion_panel(
            "Effort breakdown",
            card(plotOutput("effort_breakdown"), full_screen = TRUE)
          )
        )
      )
    ),

    nav_panel(
      "Block completion",
      csvDownloadButton(
        "block_completion_table",
        filename = "pba3_block_completion_table.csv"
      ),
      reactableOutput("block_completion_table")
    ),
  ),

  nav_panel(
    "About",

    # card(
    #   card_image(
    #     file = "input/pba3_logo.svg",
    #     href = "https://ebird.org/atlaspa/home",
    #     fill = TRUE
    #   ),
    #   max_height = 100
    # ),

    card(
      uiOutput("readme")
    ),

    card_footer(
      "App developed by Conor Tompkins with assistance from Amber Wiewel and Joe Gyekis.",
      popover(
        a(
          "eBird data extracted from the eBird Basic Dataset.",
          href = "https://ebird.org/data/download"
        ),
        markdown(
          "eBird Basic Dataset. Version: EBD_relNov-2025. Cornell Lab of Ornithology, Ithaca, New York. Nov 2025."
        )
      )
    )
  ),

  nav_panel(
    "Settings",

    accordion(
      accordion_panel(
        value = "accordion_calendar",
        title = "Calendar",
        materialSwitch(
          inputId = "toggle_current_month",
          label = "Start on current month",
          value = TRUE
        ),
        materialSwitch(
          inputId = "toggle_exclude_na_code",
          label = "Exclude birds with no code in first month",
          value = TRUE
        ),
        materialSwitch(
          inputId = "toggle_show_priority_column",
          label = "Show priority column",
          value = FALSE
        )
      ),
      accordion_panel(
        value = "accordion_map",
        title = "Block map"
      )
    ),
  ),
  nav_item(
    "Release",
    tooltip(
      bs_icon("info-circle"),
      uiOutput("ebird_release")
    )
  )
)
