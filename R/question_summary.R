#' Summarize a question in the survey
#' 
#' Summarizes a given question; if the data is cut by a group, specify groups 
#'
#' @param data Survey dataset, weighted using the `srvyr` package
#' @param groups Crosstab group or groups, names from columns in survey data
#' @param qq Question to summarize
#'
#' @return Tidy dataframe with control/exposed, any groups, and questions along with results
#' @export
#'
#' @examples
#' \dontrun{
#' ## single question 
#' 
#' brand_vars <- dplyr::tibble(var = c("unaided_awareness_coded","awr_a_1", "awr_aad_1", 
#' "con_br_5", "opn_br_x1"), 
#' q = c("Unaided Awareness","Aided Awareness", "Aided Ad Awareness", 
#' "Purchase Consideration", "Brand Momentum"))
#' 
#' question_summary(data = social, qq = brand_vars$var[2]) 
#' 
#' question_summary(data = social, groups = "demo_gender", qq = brand_vars$var[2])
#' 
#' ## multiple questions 
#' 
#' results <- purrr::map(brand_vars$var, ~question_summary(data = social, groups = NULL, qq = .x)) 
#' names(results) <- brand_vars$q
#' }
question_summary <- function(data, groups = NULL, qq) {
  
  # proper control/exposed by ad source
  match_control <- switch(names(data$allprob), "weights_xmedia" = "matched_control_xmedia", 
                          "weights_digital" = "matched_control_digital", 
                          "weights_social" = 'matched_control_social',
                          "weights_tv" = "matched_control_tv",
                          "weights_you_tube" = "matched_control_you_tube",
                          "weights_ooh" = "matched_control_ooh")
  
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
  if (!stringr::str_detect(qq, "unaided_awareness")) {
    tmp <- tmp |>
      dplyr::filter(svy_q != "NULL") 
  }
  # set up groupings for the data (if groups are there)
  if (!is.null(groups)) {
    tmp <- tmp |> 
      dplyr::group_by(dplyr::across(dplyr::all_of(match_control)), 
                      dplyr::across(dplyr::all_of(groups)), svy_q)
    
  } else {
    tmp <- tmp |> 
      dplyr::group_by(dplyr::across(dplyr::all_of(match_control)), svy_q)
    
  }
  
  # process for proportion and n count
  tmp <- tmp |> 
    srvyr::summarise(proportion = srvyr::survey_mean(),
                     n = srvyr::survey_total()) |> 
    dplyr::filter(!!rlang::sym(match_control) != "unclassified") |> 
    dplyr::filter(!!rlang::sym(match_control) != "NULL") # TV is `NULL` rather than `unclassified`
  
  # adding totals for easier testing later on
  if (!is.null(groups)) {
    tmp |>
      dplyr::group_by(dplyr::across(dplyr::all_of(match_control)), 
                      dplyr::across(dplyr::all_of(groups))) |>
      dplyr::mutate(total = sum(n)) |> 
      dplyr::filter(svy_q != "NULL") |> #dropping null from unaided awareness
      dplyr::filter(svy_q != 0) |> 
      dplyr::select(-c(proportion_se, n_se))
    
  } else {
    
    tmp |> dplyr::group_by(dplyr::across(dplyr::all_of(match_control)), svy_q) |> 
      dplyr::group_by(dplyr::across(dplyr::all_of(match_control))) |> 
      dplyr::mutate(total = round(sum(n))) |> 
      dplyr::filter(svy_q != "NULL") |> # dropping null from unaided awareness
      dplyr::filter(svy_q != 0) |> 
      dplyr::select(-c(proportion_se, n_se))
  }
}