# BMW Tracker V2 - Creative Diagnostics # 
# This has to be updated as more creative gets added to the survey # 

creative_summary_wrapper <- function(qq, group = NULL, filters) {
  if (length(filters) == 3) {
    sub3 <- glue::glue("{filters[1]} & {filters[2] & filters[3]}")
  } else if (length(filters) == 2) {
    sub3 <- glue::glue("{filters[1]} & {filters[2]}")
  } else if (length(filters) == 1) {
    sub3 <- glue::glue("{filters[1]}")
  } else {
    sub3 <- glue::glue("Overall")
  }
  brand <- "BMW"
  path <- here::here("processed",glue::glue("{brand}-{stringr::str_replace(sub3, '/', '-')}_{Sys.Date()}"))
  
  creative_summary <- function(qq, group = NULL) {
    
    group_vars <- c("creative_assignment", group) |> discard(is.null)
    
    df_filtered <- df |> 
      dplyr::group_by(across(all_of(group_vars)), !!sym(qq)) |>
      dplyr::summarise(n = n(), .groups = "drop") |> 
      dplyr::filter(!!sym(qq) != "NULL") 
    
    if (qq == "ad_rec") {
      tmp <- df_filtered |> 
        dplyr::group_by(across(all_of(group_vars))) |> 
        dplyr::mutate(total = sum(n),
                      frac = n / total) |> 
        dplyr::filter(!!sym(qq) == "Yes") |> 
        dplyr::ungroup() |> 
        select(-ad_rec)
      
    } else {
      box_label <- dplyr::case_when(
        qq == "ad_opn" ~ "positive",
        qq == "ad_pi" ~ "Somewhat likely|Very likely",
        TRUE ~ "Somewhat Agree|Strongly Agree"
      )
      
      tmp <- df_filtered |> 
        dplyr::mutate(box = dplyr::case_when(
          stringr::str_detect(!!sym(qq), box_label) ~ "T2B",
          TRUE ~ "not"
        )) |> 
        dplyr::group_by(across(all_of(group_vars)), box) |> 
        dplyr::summarise(n = sum(n), .groups = "drop") |> 
        dplyr::group_by(across(all_of(group_vars))) |> 
        dplyr::mutate(total = sum(n), frac = n / total) |> 
        dplyr::filter(box == "T2B") |> 
        dplyr::ungroup() |> 
        dplyr::select(-box)
    }
    
    tmp |> 
      dplyr::rename(creative = creative_assignment) |> 
      dplyr::mutate(
        qq = qq,
        creative = dplyr::case_when(
          stringr::str_detect(creative, "Cacophony") ~ "Cacophony vs Calm i5",
          stringr::str_detect(creative, "LCI") ~ "Never Say Never i4",
          stringr::str_detect(creative, "SMIF") ~ "Phone Wallet Keys i5",
          stringr::str_detect(creative, "Compromises") ~ "No Compromises iX",
          TRUE ~ creative
        ),
        creative = factor(
          creative,
          levels = c("Cacophony vs Calm i5", "Never Say Never i4", 
                     "Phone Wallet Keys i5", "No Compromises iX", 
                     "Service Messaging Ultimate Care")
        )
      ) |> 
      dplyr::relocate(n, .after = total)
    
  }
  creative_questions <- df |> select(starts_with("ad_") & !contains("ad_type")) |> names()
  
  # run through the creative summary
  tmpout <- map_dfr(questions, ~creative_summary(qq = .x, group = my_groups, filters = f))
}




creative_summary_wrapper <- function(group = NULL, filters) {
  library(tidyverse)
  library(openxlsx)
  library(glue)
  library(here)
  
  # Build the subtitle string from filters
  if (length(filters) == 3) {
    sub3 <- glue("{filters[1]} & {filters[2]} & {filters[3]}")
  } else if (length(filters) == 2) {
    sub3 <- glue("{filters[1]} & {filters[2]}")
  } else if (length(filters) == 1) {
    sub3 <- glue("{filters[1]}")
  } else {
    sub3 <- "Overall"
  }
  
  brand <- "BMW"
  path <- here("processed", glue("{brand}-{stringr::str_replace_all(sub3, '/', '-')}_{Sys.Date()}"))
  if (!dir.exists(path)) dir.create(path, recursive = TRUE)
  
  # Inner function that does the work
  creative_summary <- function(qq, group = NULL) {
    group_vars <- c("creative_assignment", group) |> discard(is.null)
    
    df_filtered <- df |> 
      group_by(across(all_of(group_vars)), !!sym(qq)) |>
      summarise(n = n(), .groups = "drop") |> 
      filter(!!sym(qq) != "NULL") 
    
    if (qq == "ad_rec") {
      tmp <- df_filtered |> 
        group_by(across(all_of(group_vars))) |> 
        mutate(total = sum(n), frac = n / total) |> 
        filter(!!sym(qq) == "Yes") |> 
        ungroup() |> 
        select(-!!sym(qq))
    } else {
      box_label <- case_when(
        qq == "ad_opn" ~ "positive",
        qq == "ad_pi" ~ "Somewhat likely|Very likely",
        TRUE ~ "Somewhat Agree|Strongly Agree"
      )
      
      tmp <- df_filtered |> 
        mutate(box = case_when(
          str_detect(!!sym(qq), box_label) ~ "T2B",
          TRUE ~ "not"
        )) |> 
        group_by(across(all_of(group_vars)), box) |> 
        summarise(n = sum(n), .groups = "drop") |> 
        group_by(across(all_of(group_vars))) |> 
        mutate(total = sum(n), frac = n / total) |> 
        filter(box == "T2B") |> 
        ungroup() |> 
        select(-box)
    }
    
    tmp |> 
      rename(creative = creative_assignment) |> 
      mutate(
        qq = qq,
        creative = case_when(
          str_detect(creative, "Cacophony") ~ "Cacophony vs Calm i5",
          str_detect(creative, "LCI") ~ "Never Say Never i4",
          str_detect(creative, "SMIF") ~ "Phone Wallet Keys i5",
          str_detect(creative, "Compromises") ~ "No Compromises iX",
          TRUE ~ creative
        ),
        creative = factor(
          creative,
          levels = c("Cacophony vs Calm i5", "Never Say Never i4", 
                     "Phone Wallet Keys i5", "No Compromises iX", 
                     "Service Messaging Ultimate Care")
        )
      ) |> 
      relocate(n, .after = total)
  }
  
  # Get all ad diagnostic questions
  questions <- names(df) |> 
    keep(~ str_starts(.x, "ad_") && !str_detect(.x, "ad_type"))
  
  diags <- tibble(qq = c("ad_rec", "ad_opn", "ad_pi", paste0("ad_diag_", 1:12)),
                  statement = c("Seen ad past 2 weeks", "Ad change opinion of BMW (T2B Better)", "Ad change purchase consideration (T2B Likely)","Is unique and different", "Is believable", "Is enjoyable",
                                 "Is relevant to me", 
                                "Makes me feel good",
                                "Fits the way I feel about the brand",
                                "Makes me think about the brand in a new way", 
                                "I was emotionally moved by the ad", 
                                "Makes me want to learn more about the product/brand", 
                                "Is confusing", "Is irritating",
                                "Makes me think this brand is for me"))
  # Run all and combine
  tmpout <- map_dfr(questions, ~creative_summary(qq = .x, group = group)) |> 
    left_join(diags, by = "qq")
  
  # Save to Excel
  out_file <- file.path(path, "creative", glue("creative_diagnostics_{Sys.Date()}.xlsx"))
  openxlsx::write.xlsx(tmpout, out_file)

}


