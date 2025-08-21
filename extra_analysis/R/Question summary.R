# Question summary

# SUMMARY FUNCTION -------------------------------------------------------

qsummary <- function(data, groups = NULL, qq, include_month = TRUE) {
  # proper control/exposed by ad source
  match_control <- switch(
    names(data$allprob),
    "weights_xmedia" = "matched_control_xmedia",
    "weights_digital" = "matched_control_digital",
    "weights_social" = 'matched_control_social',
    "weights_tv" = "matched_control_tv",
    "weights_you_tube" = "matched_control_you_tube",
    "weights_ooh" = "matched_control_ooh",
    "probs" = NULL
  )

  # renaming for variable as 'svy_q'
  tmp <- data |>
    dplyr::mutate(month = lubridate::floor_date(date, "month")) |>
    dplyr::mutate(
      month = case_when(
        month == as.Date("2025-01-01") ~ as.Date("2024-12-01"),
        TRUE ~ month
      )
    ) |>
    dplyr::rename(svy_q = !!rlang::sym(qq))

  # if brand momentum, convert to top 2 / bottom 2 box
  if (stringr::str_detect(qq, "opn_br|momentum_br")) {
    tmp <- tmp |>
      dplyr::mutate(
        svy_q = dplyr::case_when(
          svy_q %in%
            c(
              "On its way up, a lot going for it",
              "On its way up, a little going for it"
            ) ~
            "On its way up - Top 2 Box",
          svy_q %in%
            c(
              "On its way down, losing a little",
              "On its way down, nothing going for it"
            ) ~
            "On its way down - Bottom 2 Box",
          svy_q == "It's holding its ground" ~ "It's holding its ground"
        ),
        svy_q = factor(
          svy_q,
          levels = c(
            "On its way up - Top 2 Box",
            "It's holding its ground",
            "On its way down - Bottom 2 Box"
          )
        )
      )
  }

  # if not unaided awareness, drop NULL (NULL are unaware of BMW; don't want them in our denominator)
  if (!str_detect(qq, "unaided_awareness")) {
    tmp <- tmp |>
      dplyr::filter(svy_q != "NULL")
  }

  # set up groupings for the data (if groups are there)
  group_vars <- groups # Start with user-supplied groups

  if (include_month) {
    group_vars <- c(group_vars, "month")
  }

  group_vars <- c(group_vars, match_control, "svy_q") # Always group by svy_q

  tmp <- tmp |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars)))

  # process for proportion and n count
  tmp <- tmp |>
    srvyr::summarise(
      proportion = srvyr::survey_mean(),
      n = srvyr::survey_total()
    )

  if (!is.null(match_control)) {
    tmp <- tmp |>
      filter(!!sym(match_control) == "test")
  }

  # adding totals for easier testing later on
  if (!is.null(groups)) {
    tmp |>
      # dplyr::group_by(dplyr::across(dplyr::all_of(groups))) |>
      dplyr::mutate(
        total = sum(n),
        var = qq,
        svy_q = str_remove_all(svy_q, "\r|\n")
      ) |>
      dplyr::filter(svy_q != "NULL") |> #dropping null from unaided awareness
      dplyr::filter(svy_q != 0) |>
      dplyr::select(-c(proportion_se, n_se))
  } else {
    tmp |>
      # dplyr::group_by(svy_q) |>
      dplyr::mutate(
        total = sum(n),
        var = qq,
        svy_q = str_remove_all(svy_q, "\r|\n")
      ) |>
      dplyr::filter(svy_q != "NULL") |> # dropping null from unaided awareness
      dplyr::filter(svy_q != 0) |>
      dplyr::select(-c(proportion_se, n_se))
  }
}

# helper cleaning function
cleaner <- function(tbl, brand) {
  tbl |>
    mutate(svy_q = case_when(svy_q == brand ~ q, TRUE ~ svy_q)) |>
    filter(
      !svy_q %in% c("It's holding its ground", "On its way down - Bottom 2 Box")
    ) |>
    select(-q) |>
    rename_with(
      .fn = ~ paste0(brand, "_", .x),
      .cols = c("proportion", "n", "total")
    )
}


# TO RUN -----------------------------------------------------------------

# 1. Read in the data
file_location <- "~/R/bmw/brand_tracker_v2/cint_data/bmw_brandtracker_july2025.xlsx"
source(here::here("R", "read_cint.R"))
source(here::here("R", "brand_choice.R"))
read_cint(file_location)

# 2. Get questions for a brand
bmw_qs <- brand_choice("bmw")

# 3. Run qs through the summary

# Overall
brand_metrics_bmw <- bmw_qs_2025$brand_vars$var |>
  map_dfr(
    ~ qsummary(data = unweighted, groups = NULL, qq = .x, include_month = TRUE)
  ) |>
  left_join(bmw_qs_2025$brand_vars) |>
  select(-var) |>
  cleaner(brand = "BMW")

# Or for a specific group
# Women
brand_metrics_bmw_female <- bmw_qs_2025$brand_vars$var |>
  map_dfr(
    ~ qsummary(
      data = unweighted,
      groups = "demo_gender",
      qq = .x,
      include_month = TRUE
    )
  ) |>
  left_join(bmw_qs_2025$brand_vars) |>
  select(-var) |>
  filter(demo_gender == "Female") |>
  cleaner(brand = "BMW")
