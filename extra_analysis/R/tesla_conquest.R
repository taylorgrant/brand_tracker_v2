# Tesla Conquest Data #

pacman::p_load(tidyverse, janitor, here, glue)


# HELPER FUNCTION TO SUMMARISE DATA ---------------------------------------

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

# READ IN 2025 TRACKER DATA -----------------------------------------------

file_location <- "~/R/bmw/brand_tracker_v2/cint_data/bmw_brandtracker_july2025.xlsx"
source(here::here("R", "read_cint.R"))
source(here::here("R", "brand_choice.R"))
read_cint(file_location)
brand_choice("bmw")

tesla_qs_2025 <- brand_choice("tesla")
bmw_qs_2025 <- brand_choice("bmw")


# BRAND METRICS - TESLA ---------------------------------------------------

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

# Overall
brand_metrics_tesla <- tesla_qs_2025$brand_vars$var |>
  map_dfr(
    ~ qsummary(data = unweighted, groups = NULL, qq = .x, include_month = TRUE)
  ) |>
  left_join(tesla_qs_2025$brand_vars) |>
  select(-var) |>
  cleaner(brand = "Tesla")

brand_metrics_tesla |> tp()

# Women
brand_metrics_tesla_female <- tesla_qs_2025$brand_vars$var |>
  map_dfr(
    ~ qsummary(
      data = unweighted,
      groups = "demo_gender",
      qq = .x,
      include_month = TRUE
    )
  ) |>
  left_join(tesla_qs_2025$brand_vars) |>
  select(-var) |>
  filter(demo_gender == "Female") |>
  cleaner(brand = "Tesla")

brand_metrics_tesla_female |> tp()

# AAPI
brand_metrics_tesla_aapi <- tesla_qs_2025$brand_vars$var |>
  map_dfr(
    ~ qsummary(
      data = unweighted,
      groups = "demo_race_ethnicity",
      qq = .x,
      include_month = TRUE
    )
  ) |>
  left_join(tesla_qs_2025$brand_vars) |>
  select(-var) |>
  filter(demo_race_ethnicity == "Asian or Pacific Islander") |>
  cleaner(brand = "Tesla")

brand_metrics_tesla_aapi |> tp()

# Gen Z / Millennial
brand_metrics_tesla_genz <- tesla_qs_2025$brand_vars$var |>
  map_dfr(
    ~ qsummary(
      data = unweighted,
      groups = "genz_millen",
      qq = .x,
      include_month = TRUE
    )
  ) |>
  left_join(tesla_qs_2025$brand_vars) |>
  select(-var) |>
  filter(genz_millen == "Gen Z/Millennial") |>
  cleaner(brand = "Tesla")

brand_metrics_tesla_genz |> tp()


# BRAND METRICS - BMW -----------------------------------------------------

# Overall
brand_metrics_bmw <- bmw_qs_2025$brand_vars$var |>
  map_dfr(
    ~ qsummary(data = unweighted, groups = NULL, qq = .x, include_month = TRUE)
  ) |>
  left_join(bmw_qs_2025$brand_vars) |>
  select(-var) |>
  cleaner(brand = "BMW")

brand_metrics_bmw |>
  left_join(brand_metrics_tesla) |>
  tp()

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

brand_metrics_bmw_female |>
  left_join(brand_metrics_tesla_female) |>
  tp()

# AAPI
brand_metrics_bmw_aapi <- bmw_qs_2025$brand_vars$var |>
  map_dfr(
    ~ qsummary(
      data = unweighted,
      groups = "demo_race_ethnicity",
      qq = .x,
      include_month = TRUE
    )
  ) |>
  left_join(bmw_qs_2025$brand_vars) |>
  select(-var) |>
  filter(demo_race_ethnicity == "Asian or Pacific Islander") |>
  cleaner(brand = "BMW")

brand_metrics_bmw_aapi |>
  left_join(brand_metrics_tesla_aapi) |>
  tp()

# Gen Z / Millennials
brand_metrics_bmw_genz <- bmw_qs_2025$brand_vars$var |>
  map_dfr(
    ~ qsummary(
      data = unweighted,
      groups = "genz_millen",
      qq = .x,
      include_month = TRUE
    )
  ) |>
  left_join(bmw_qs_2025$brand_vars) |>
  select(-var) |>
  filter(genz_millen == "Gen Z/Millennial") |>
  cleaner(brand = "BMW")

brand_metrics_bmw_genz |>
  left_join(brand_metrics_tesla_genz) |>
  tp()


# PATCHWORK PLOT ---------------------------------------------------------
library(patchwork)
bmw_metrics <- brand_metrics_bmw |> tp()
filter(
  svy_q %in%
    c("Aided Ad Awareness", "Purchase Intent", "On its way up - Top 2 Box")
) |>
  mutate(
    svy_q = case_when(
      svy_q == "On its way up - Top 2 Box" ~ "Brand Momentum - Top 2 Box",
      TRUE ~ svy_q
    )
  ) |>
  mutate(
    labs = ifelse(
      month == as.Date("2025-07-01"),
      paste0(scales::percent(BMW_proportion, accuracy = 1), "*"),
      scales::percent(BMW_proportion, accuracy = 1)
    )
  )

p1 <- ggplot(
  filter(bmw_metrics, svy_q == "Aided Ad Awareness"),
  aes(x = month, y = BMW_proportion)
) +
  geom_line(linewidth = .7, color = "#073399") +
  geom_point(size = 1, color = "#073399") +
  geom_text(
    aes(
      label = labs, # format as %
    ),
    vjust = -1,
    size = 3
  ) +
  scale_y_continuous(
    labels = scales::percent,
    breaks = seq(0.40, 0.60, 0.1),
    limits = c(0.40, 0.60)
  ) +
  scale_x_date(
    date_breaks = "months",
    date_labels = "%b %y"
  ) +
  labs(
    title = "Aided Ad Awareness",
    y = "BMW Percentage",
    x = NULL
  ) +
  theme(legend.position = "none") +
  theme_bmw(grid = FALSE)

p2 <- ggplot(
  filter(bmw_metrics, svy_q == "Purchase Intent"),
  aes(x = month, y = BMW_proportion)
) +
  geom_line(linewidth = .7, color = "#073399") +
  geom_point(size = 1, color = "#073399") +
  geom_text(
    aes(label = labs), # format as %
    vjust = -1, # move label slightly above point
    size = 3
  ) +
  #scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") + don't need dates on all graphs
  scale_y_continuous(
    labels = scales::percent,
    breaks = seq(0.30, 0.47, 0.05),
    limits = c(0.30, 0.40)
  ) +
  scale_x_date(
    date_breaks = "months",
    date_labels = "%b %y"
  ) +
  labs(
    title = "Purchase Intent",
    y = NULL,
    x = NULL
  ) +
  theme_bmw(grid = FALSE)

p3 <- ggplot(
  filter(bmw_metrics, svy_q == "Brand Momentum - Top 2 Box"),
  aes(x = month, y = BMW_proportion)
) +
  geom_line(linewidth = .7, color = "#073399") +
  geom_point(size = 1, color = "#073399") +
  geom_text(
    aes(label = labs), # format as %
    vjust = -1, # move label slightly above point
    size = 3
  ) +
  #scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") + don't need dates on all graphs
  scale_y_continuous(
    labels = scales::percent,
    breaks = seq(0.50, 0.70, 0.1),
    limits = c(0.50, 0.70)
  ) +
  scale_x_date(
    date_breaks = "months",
    date_labels = "%b %y"
  ) +
  labs(
    title = "Brand Momentum - Top 2 Box",
    y = NULL,
    x = NULL
  ) +
  theme_bmw(grid = FALSE)

combined <- ((p1 / p2 / p3) +
  theme_bmw(grid = FALSE) &
  theme(
    plot.title = element_text(size = 12),
    plot.margin = margin(t = 6, b = 14)
  )) +
  plot_annotation(
    title = "Significant lifts during SSE",
    subtitle = "March 2025 – July 2025",
    caption = "Total Respondents Aware of BMW, 3/15-7/30, N = 9,119\n*statistically significant lift in July vs. June and vs. Mar–Jun average (95% CI).",
    theme = theme_bmw(
      grid = FALSE,
      plot_title_size = 18,
      subtitle_size = 12,
      caption_size = 9
    )
  )
combined

ggsave("~/Desktop/tmppng.png")


# SSE versus other months ------------------------------------------------

brand_metrics_bmw_genz |>
  mutate(sse = ifelse(month == as.Date("2025-07-01"), 1, 0)) |>
  group_by(svy_q, sse) |>
  summarise(n = sum(BMW_n), total = sum(BMW_total), frac = n / total)

brand_metrics_bmw_female |>
  mutate(sse = ifelse(month == as.Date("2025-07-01"), 1, 0)) |>
  group_by(svy_q, sse) |>
  summarise(n = sum(BMW_n), total = sum(BMW_total), frac = n / total)


# READ IN 2024 TRACKER DATA -----------------------------------------------

file_location <- "~/R/bmw/brand_tracker/cint_data/BMW Dashboard Data Through Dec 2024.csv"
source("~/R/bmw/brand_tracker/R/read_cint.R")
read_cint(file_location)
source("~/R/bmw/brand_tracker/R/brand_choice.R")
tesla_qs_2024 <- brand_choice("tesla")$brand_vars[3:4, ]
bmw_qs_2024 <- brand_choice("bmw")$brand_vars[4:5, ]

# Consideration 2024 - Tesla
tesla_cons_24 <- qsummary(
  unweighted,
  groups = NULL,
  qq = tesla_qs_2024$var[1],
  include_month = TRUE
)
tesla_cons_women_24 <- qsummary(
  campaign,
  groups = "demo_gender",
  qq = tesla_qs_2024$var[1],
  include_month = TRUE
)
tesla_cons_aapi_24 <- qsummary(
  unweighted,
  groups = "demo_race_ethnicity",
  qq = tesla_qs_2024$var[1],
  include_month = TRUE
)
tesla_cons_genzmill_24 <- qsummary(
  unweighted,
  groups = "genz_millen",
  qq = tesla_qs_2024$var[1],
  include_month = TRUE
)

# Consideration 2024 - BMW
bmw_consider_24 <- qsummary(
  unweighted,
  groups = NULL,
  qq = bmw_qs_2024$var[1],
  include_month = TRUE
)
bmw_cons_women_24 <- qsummary(
  unweighted,
  groups = "demo_gender",
  qq = bmw_qs_2024$var[1],
  include_month = TRUE
)
bmw_cons_aapi_24 <- qsummary(
  unweighted,
  groups = "demo_race_ethnicity",
  qq = bmw_qs_2024$var[1],
  include_month = TRUE
)
bmw_cons_genzmill_24 <- qsummary(
  unweighted,
  groups = "genz_millen",
  qq = bmw_qs_2024$var[1],
  include_month = TRUE
)

# Momentum 2024 - Tesla
tesla_mom_24 <- qsummary(
  unweighted,
  groups = NULL,
  qq = tesla_qs_2024$var[2],
  include_month = TRUE
)
tesla_mom_women_24 <- qsummary(
  unweighted,
  groups = "demo_gender",
  qq = tesla_qs_2024$var[2],
  include_month = TRUE
)
tesla_mom_aapi_24 <- qsummary(
  unweighted,
  groups = "demo_race_ethnicity",
  qq = tesla_qs_2024$var[2],
  include_month = TRUE
)
tesla_mom_genzmill_24 <- qsummary(
  unweighted,
  groups = "genz_millen",
  qq = tesla_qs_2024$var[2],
  include_month = TRUE
)

# Momentum 2024 - BMW
bmw_mom_24 <- qsummary(
  unweighted,
  groups = NULL,
  qq = bmw_qs_2024$var[2],
  include_month = TRUE
)
bmw_mom_women_24 <- qsummary(
  unweighted,
  groups = "demo_gender",
  qq = bmw_qs_2024$var[2],
  include_month = TRUE
)
bmw_mom_aapi_24 <- qsummary(
  unweighted,
  groups = "demo_race_ethnicity",
  qq = bmw_qs_2024$var[2],
  include_month = TRUE
)
bmw_mom_genzmill_24 <- qsummary(
  unweighted,
  groups = "genz_millen",
  qq = bmw_qs_2024$var[2],
  include_month = TRUE
)
