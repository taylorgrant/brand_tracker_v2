# BMW Brand Tracker from Cint # 

pacman::p_load(tidyverse, janitor, here, glue, srvyr)

cint_wrapper <- function(file_location, brand, sig_thresh, my_groups = NULL) {
  source(here::here("R", "read_cint.R"))
  source(here::here("R", "brand_choice.R"))
  source(here::here("R", "question_summary.R"))
  source(here::here("R", "ce_prop_test.R"))
  source(here::here("R", "table_prep.R"))
  source(here::here("R", "create_directory.R"))
  # source(here::here("R", "tracker_table.R"))
  source(here::here("R", "tracker_dumbell.R"))
  source(here::here("R", "brand_choice_all_brands.R"))
  source(here::here("R", "question_summary_all_brands.R"))
  source(here::here("R", "proptest_dataframe.R"))
  source(here::here("R", "process_list.R"))
  source(here::here("R", "sig_table.R"))
  # source(here::here("R", "mental_advantage.R"))
  source(here::here("R", "process_all_brands.R"))
  source(here::here("R", "raw_tables.R"))
  
  # 1. READ IN THE SURVEY DATA FROM CINT ------------------------------------
  read_cint(file_location)
  
  # 2. LOAD TRACKER QUESTIONS FOR BMW ---------------------------------------
  tracker_qs <- brand_choice(brand)
  
  # Helper function to apply question_summary and result_prop_test
  process_tracker <- function(tracker_section_data, tracker_qs) {
    brand_vars <- purrr::map(tracker_qs$brand_vars$var, ~question_summary(data = tracker_section_data, groups = my_groups, qq = .x)) |> 
      purrr::set_names(nm = tracker_qs$brand_vars$q)
    brand_traits <- purrr::map(tracker_qs$brand_traits$var, ~question_summary(data = tracker_section_data, groups = my_groups, qq = .x)) |>
      purrr::set_names(nm = tracker_qs$brand_traits$q)
    brand_attrs <- purrr::map(tracker_qs$brand_attrs$var, ~question_summary(data = tracker_section_data, groups = my_groups, qq = .x)) |>
      purrr::set_names(nm = tracker_qs$brand_attrs$q)

    # Apply result_prop_test
    brand_vars_result <- purrr::map(brand_vars, ce_prop_test)
    brand_traits_result <- purrr::map(brand_traits, ce_prop_test)
    brand_attrs_result <- purrr::map(brand_attrs, ce_prop_test)

    list(
      brand_vars_result = brand_vars_result,
      brand_traits_result = brand_traits_result,
      brand_attrs_result = brand_attrs_result
    )
  }
  
  # 3. PROCESS EACH SECTION (CAMPAIGN, SOCIAL, DIGITAL) -----------------------
  if ("weights_xmedia" %in% names(unweighted$variables)) {
    campaign_results <- process_tracker(campaign, tracker_qs)
  }
  if ("weights_social" %in% names(unweighted$variables)) {
    social_results <- process_tracker(social, tracker_qs)
  }
  if ("weights_digital" %in% names(unweighted$variables)) {
    digital_results <- process_tracker(digital, tracker_qs)
  }
  if ("weights_tv" %in% names(unweighted$variables)) {
    tv_results <- process_tracker(tv, tracker_qs)
  }
  if ("weights_you_tube" %in% names(unweighted$variables)) {
    youtube_results <- process_tracker(youtube, tracker_qs)
  }
  if ("weights_ooh" %in% names(unweighted$variables)) {
    ooh_results <- process_tracker(ooh, tracker_qs)
  }
  
  # 4. APPLY TABLE PREP TO ALLOW USER TO SELECT FILTERS ---------------------
  group_filter <- table_prep(campaign_results$brand_vars_result[[1]])
  f <- group_filter
  

  # 5. PASS DATA TO DUMBBELL PLOTTER ----------------------------------------
  if ("weights_xmedia" %in% names(unweighted$variables)) {
    purrr::map(campaign_results, ~tracker_dumbbell(.x, brand, sig = sig_thresh, group_filter, "Campaign"))
    raw_tables(campaign_results, filters = f, name = "campaign")
  }
  if ("weights_social" %in% names(unweighted$variables)) {
    purrr::map(social_results, ~tracker_dumbbell(.x, brand, sig = sig_thresh, group_filter, "Social"))
    raw_tables(social_results, filters = f, name = "social")
  }
  if ("weights_digital" %in% names(unweighted$variables)) {
    purrr::map(digital_results, ~tracker_dumbbell(.x, brand, sig = sig_thresh, group_filter, "Digital"))
    raw_tables(digital_results, filters = f, name = "digital")
  }
  if ("weights_tv" %in% names(unweighted$variables)) {
    purrr::map(tv_results, ~tracker_dumbbell(.x, brand, sig = sig_thresh, group_filter, "TV"))
    raw_tables(tv_results, filters = f, name = "tv")
  }
  if ("weights_you_tube" %in% names(unweighted$variables)) {
    purrr::map(youtube_results, ~tracker_dumbbell(.x, brand, sig = sig_thresh, group_filter, "YouTube"))
    raw_tables(youtube_results, filters = f, name = "youtube")
  }
  if ("weights_ooh" %in% names(unweighted$variables)) {
    purrr::map(ooh_results, ~tracker_dumbbell(.x, brand, sig = sig_thresh, group_filter, "OOH"))
    raw_tables(ooh_results, filters = f, name = "ooh")
  }

  # # run full competitive to get entire table
  process_all_brands(f, sig_thresh)
}









  


