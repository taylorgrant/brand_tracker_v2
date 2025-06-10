# have to process all brands at the same time # 

process_all_brands <- function(group_filter, sig_thresh){
  
  # get all of the questions with proper ID variables
  tracker_qs <- brand_choice_all_brands()
  
  key_trait_drivers <- c("Practical", "Adventurous", "Exciting", "Leader",
                         "Innovative", "Trusted", "Youthful", "Passionate")
  
  key_attr_drivers <- c("Good value for the money", "Environmentally friendly", "Lasts a long time", 
                        "Reliable", "Fun to drive", "Customer-oriented dealerships", "Attractive styling")
  
  # read in reference data 
  reference_data <- readxl::read_excel(here::here("data", "bmw_ref_values.xlsx"))
  group_name <- if (length(group_filter) == 0) "Overall" else group_filter
  
  # filter reference data down to a specific group (if group isn't in data, reference_filtered is 0 rows)
  reference_filtered <- reference_data |> 
    dplyr::filter(Group == group_name) |> 
    select(-Group) |> 
    mutate(Ref_2024 = scales::percent(Ref_2024, accuracy = 1))
  
  # process three levels of data across all brands 
  process_tracker <- function(tracker_section_data, tracker_qs) {
    brand_vars <- purrr::map(tracker_qs$brand_vars$var, ~question_summary_all_brands(data = tracker_section_data, groups = my_groups, qq = .x)) |> 
      purrr::set_names(nm = tracker_qs$brand_vars$q)
    brand_traits <- purrr::map(tracker_qs$brand_traits$var, ~question_summary_all_brands(data = tracker_section_data, groups = my_groups, qq = .x)) |> 
      purrr::set_names(nm = tracker_qs$brand_traits$q)
    brand_attrs <- purrr::map(tracker_qs$brand_attrs$var, ~question_summary_all_brands(data = tracker_section_data, groups = my_groups, qq = .x)) |> 
      purrr::set_names(nm = tracker_qs$brand_attrs$q)
    
    list(
      brand_vars = brand_vars,
      brand_traits = brand_traits,
      brand_attrs = brand_attrs
    )
  }
  
  # run using the unweighted srvry survey 
  total_results <- process_tracker(unweighted, tracker_qs)
  
  # rename any grouping variables for eventual filter
  rename_groups <- function(data) {
    # Find the position of the 'svy_q' column
    svy_pos <- which(names(data) == "svy_q")
    # If 'svy_q' is not the first column, rename the previous columns
    if (svy_pos > 1) {
      colnames(data)[1:(svy_pos - 1)] <- paste0("group_", seq_len(svy_pos - 1))
    }
    return(data)  # Return the modified tibble
  }
  
  renamed_total_results <- purrr::map(total_results, ~ purrr::map(.x, rename_groups))
  
  # get each list and put into tibble for easy filtering if necessary
  brand_vars <- renamed_total_results$brand_vars
  brand_traits <- renamed_total_results$brand_traits
  brand_attrs <- renamed_total_results$brand_attrs
    
  brand_vars_tbl <- data.table::rbindlist(brand_vars, idcol = "Category") |> 
    dplyr::tibble() |> 
    dplyr::mutate(Category = ifelse(Category == "Brand Momentum" & svy_q == "On its way up - Top 2 Box", paste(Category, "- Top 2 Box"), Category)) |> 
    dplyr::mutate(svy_q = as.character(svy_q)) |> 
    dplyr::filter(Category != "Brand Momentum") |>
    dplyr::mutate(svy_q = ifelse(svy_q == "On its way up - Top 2 Box", NA, svy_q)) |> 
    tidyr::fill(svy_q, .direction = "down") |> 
    dplyr::mutate(cat = "Brand Metrics") |> 
    dplyr::mutate(Category = factor(Category, levels = c("Unaided Awareness","Aided Awareness", "Aided Ad Awareness",
                                                  "Purchase Consideration", "Purchase Intent", "Brand Momentum - Top 2 Box"))) |> 
    dplyr::mutate(svy_q = factor(svy_q, levels = c("BMW", "Audi", "Lexus", "Mercedes Benz", "Tesla"))) |> 
    dplyr::arrange(Category, svy_q) 
  
  brand_traits_tbl <- data.table::rbindlist(brand_traits, idcol = "Category") |> 
    dplyr::tibble() |> 
    dplyr::arrange(Category) |> 
    dplyr::mutate(cat = "Brand Traits") |> 
    dplyr::mutate(Category = ifelse(Category %in% key_trait_drivers, paste0(Category, "†"), Category)) |> 
    dplyr::mutate(Category = factor(Category,
                                    levels = c("Practical†", "Adventurous†", "Exciting†", "Leader†",
                                               "Innovative†", "Trusted†", "Youthful†", "Passionate†",
                                               "Responsible", "Confident", "Distinctive", "Classy",
                                               "Aggressive", "Arrogant", "Traditional"))) |> 
    dplyr::mutate(svy_q = factor(svy_q, levels = c("BMW", "Audi", "Lexus", "Mercedes Benz", "Tesla"))) |> 
    dplyr::arrange(Category, svy_q)
  
  brand_attrs_tbl <- data.table::rbindlist(brand_attrs, idcol = "Category") |> 
    dplyr::tibble() |> 
    dplyr::mutate(cat = "Brand Attributes") |> 
    dplyr::mutate(Category = ifelse(Category %in% key_attr_drivers, paste0(Category, "†"), Category)) |> 
    dplyr::mutate(Category = factor(Category,
                                    levels = c("Good value for the money†", "Environmentally friendly†", "Lasts a long time†", 
                                               "Reliable†", "Fun to drive†", "Customer-oriented dealerships†", "Attractive styling†",
                                               "Comfortable", "Advanced tech features", "Advanced safety features",
                                               "Responsive handling", "Quality materials, fit, and finish",
                                               "Customizable", "Prestigious", "Strong brand heritage")),
                  svy_q = factor(svy_q, levels = c("BMW", "Audi", "Lexus", "Mercedes Benz", "Tesla"))) |> 
    dplyr::arrange(Category, svy_q)
  
  # put into single tibble
  tmp <- rbind(brand_vars_tbl, brand_traits_tbl, brand_attrs_tbl)
  
  # check length of filters to figure out subtitles
  if (length(group_filter) == 3) {
    sub3 <- glue::glue("{group_filter[1]} & {group_filter[2] & group_filter[3]}")
    tmp <- tmp |> 
      dplyr::filter(group_1 %in% group_filter[1] & 
                      group_2 %in% group_filter[2] & 
                      group_3 %in% group_filter[3]) |>  
      dplyr::select(-c(group_1, group_2, group_3))
    
  } else if (length(group_filter) == 2) {
    sub3 <- glue::glue("{group_filter[1]} & {group_filter[2]}")
    tmp <- tmp |> 
      dplyr::filter(group_1 %in% group_filter[1] & 
                      group_2 %in% group_filter[2])  |> 
      dplyr::select(-c(group_1, group_2))
    
  } else if (length(group_filter) == 1) {
    
    sub3 <- glue::glue("{group_filter[1]}")
    tmp <- tmp |> 
      dplyr::filter(group_1 %in% group_filter[1]) |>  
      dplyr::select(-group_1)
  } else {
    # sub3 <- glue::glue("No filters")
    sub3 <- glue::glue("Overall")
  }
  
  # now split the dataframe into a series of lists by question category 
  all_results <- split(tmp, tmp$Category)
  
  base_note <- if (sub3 == "Overall") {
    glue::glue("Total Sample; N: {scales::comma(unique(all_results[[1]]$total))}; (A,B,C,D,E) indicate significant difference at {scales::percent(sig_thresh, accuracy = 1)} confidence interval")
  } else {
    glue::glue("{sub3} subsample; N: {scales::comma(unique(all_results[[1]]$total))}; (A,B,C,D,E) indicate significant difference at {scales::percent(sig_thresh, accuracy = 1)} confidence interval")
  }
  
  # Add this special note for traits and attributes
  extra_note <- "†: Key Drivers ordered by Relative Importance on BMW Purchase Consideration"
  
  # Build separate footnotes for the three categories
  footnote_vars <- base_note
  footnote_traits <- glue::glue("{base_note}<br>{extra_note}")
  footnote_attrs <- glue::glue("{base_note}<br>{extra_note}")
  
  # run everything through prop.test and formatting for gt
  combined_results <- process_list(all_results, sig_thresh)
  
  # split again into lists 
  results_list <- split(combined_results, combined_results$cat)
  
  # to save - need the path 
  path <- create_directory(brand, filters = gsub(" & ", "-", sub3))
  
  # build gt tables  
  # conditionally add in reference metrics 
  if (group_name %in% reference_data$Group) {
    results_list$`Brand Metrics` <- results_list$`Brand Metrics` |> 
      dplyr::left_join(reference_filtered, by = "Category") |> 
      relocate(Ref_2024, .after = "Category") |> 
      rename(`BMW 2024` = Ref_2024)
    
    sig_table(results_list$`Brand Metrics`[,-2], footnote_vars) |> 
      gt::gtsave(file.path(path, "competitive", paste0("no_ref-sig-", sig_thresh,"-competitive_brand_metrics.png")), expand = 10)
    
    sig_table(results_list$`Brand Metrics`, footnote_vars) |> 
      gt::gtsave(file.path(path, "competitive", paste0("w_ref-sig-", sig_thresh,"-competitive_brand_metrics.png")), expand = 10)
    
  } else {
    
    sig_table(results_list$`Brand Metrics`, footnote_vars) |> 
      gt::gtsave(file.path(path, "competitive", paste0("sig-", sig_thresh,"-competitive_brand_metrics.png")), expand = 10)
    
  }
  
  sig_table(results_list$`Brand Attributes`, footnote_attrs) |> 
    gt::gtsave(file.path(path, "competitive", paste0("sig-", sig_thresh,"-competitive_brand_attributes.png")), expand = 10)
  
  sig_table(results_list$`Brand Traits`, footnote_traits) |> 
    gt::gtsave(file.path(path, "competitive", paste0("sig-", sig_thresh,"-competitive_brand_traits.png")), expand = 10)
  
  # mental advantage for the attributes 
  # if (length(group_filter) < 2) {
  #   mental_advantage(key_attrs_tbl, N = nrow(df), filters = group_filter, note = footnote2) |>
  #     gt::gtsave(file.path(path, "mental_advantage", "key_attributes.png"), expand = 10)
  # 
  #   mental_advantage(brand_attrs_tbl, N = nrow(df), filters = group_filter, note = footnote2) |>
  #     gt::gtsave(file.path(path, "mental_advantage", "brand_attributes.png"), expand = 10)
  # }
  
  }
  









