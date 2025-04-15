question_summary_all_brands <- function(data, groups = NULL, qq) {
  
  # no control/exposed when looking at all brands 
  # total <- data |> dplyr::summarise(srvyr::survey_total()) |> dplyr::pull(coef)
  
  # renaming for variable as 'svy_q'
  tmp <- data |> dplyr::rename(svy_q = !!rlang::sym(qq)) |> 
    mutate(svy_q = stringr::str_remove_all(svy_q, "\r|\n"))
  
  # if brand momentum, convert to top 2 / bottom 2 box
  if (stringr::str_detect(qq, "momentum_br")) {
    tmp <- tmp |> 
      dplyr::mutate(svy_q = dplyr::case_when(
        svy_q %in%  c("On its way up, a lot going for it", "On its way up, a little going for it") ~ "On its way up - Top 2 Box",
        svy_q %in%  c("On its way down, losing a little", "On its way down, nothing going for it") ~ "On its way down - Bottom 2 Box",
        svy_q == "It's holding its ground" ~ "It's holding its ground"
      ), 
      svy_q = factor(svy_q, levels = c("On its way up - Top 2 Box", "It's holding its ground", "On its way down - Bottom 2 Box"))) 
  }
  
  # if not unaided awareness, drop NULL (NULL are unaware of BMW; don't want them in our denominator)
  if (!str_detect(qq, "unaided_awareness")) {
    tmp <- tmp |>
      dplyr::filter(svy_q != "NULL") 
  }
  # set up groupings for the data (if groups are there)
  if (!is.null(groups)) {
    tmp <- tmp |> 
      dplyr::group_by(dplyr::across(dplyr::all_of(groups)), svy_q)
    
  } else {
    tmp <- tmp |> 
      dplyr::group_by(dplyr::across(svy_q))
    
  }
  
  # process for proportion and n count
  tmp <- tmp |> 
    srvyr::summarise(proportion = srvyr::survey_mean(),
                     n = srvyr::survey_total())
  
  # adding totals for easier testing later on
  if (!is.null(groups)) {
    tmp |>
      dplyr::group_by(dplyr::across(dplyr::all_of(groups))) |>
      dplyr::mutate(total = sum(n)) |> 
      dplyr::filter(svy_q != "NULL") |> #dropping null from unaided awareness
      dplyr::filter(svy_q != 0) |> 
      dplyr::select(-c(proportion_se, n_se))
    
  } else {
    
    tmp |> 
      # dplyr::group_by(svy_q) |> 
      dplyr::mutate(total = sum(n)) |> 
      dplyr::filter(svy_q != "NULL") |> # dropping null from unaided awareness
      dplyr::filter(svy_q != 0) |> 
      dplyr::select(-c(proportion_se, n_se))
  }
}
