# prop.test each tibble within a list 
proptest_dataframe <- function(df, significance_level = 0.05) {
  df <- df |> 
    dplyr::mutate(id = LETTERS[1:dplyr::n()])  # Assign capital letters as IDs
  
  run_prop_test_significant <- function(i, j, df) {
    prop1 <- df$proportion[i]
    prop2 <- df$proportion[j]
    
    n1 <- df$n[i]
    n2 <- df$n[j]
    
    total1 <- df$total[i]
    total2 <- df$total[j]
    
    counts <- c(n1, n2)
    totals <- c(total1, total2)
    
    test_result <- prop.test(counts, totals)
    
    return(test_result$p.value < significance_level && prop1 > prop2)
  }
  
  df$greater_than <- NA
  
  for (i in 1:nrow(df)) {
    greater_ids <- c()
    for (j in 1:nrow(df)) {
      if (i != j && run_prop_test_significant(i, j, df)) {
        greater_ids <- c(greater_ids, df$id[j])
      }
    }
    df$greater_than[i] <- paste(greater_ids, collapse = ", ")
  }
  
  df <- df |> 
    dplyr::mutate(
      brand_id = glue::glue("{svy_q} {id}"),  
      proportion_with_sub = ifelse(
        greater_than != "", 
        glue::glue("{round(proportion * 100)}% <sub>({greater_than})</sub>"),  
        glue::glue("{round(proportion * 100)}%")
      )
    )
  
  df_wide <- df |> 
    dplyr::select(Category, cat, brand_id, proportion_with_sub) |> 
    tidyr::pivot_wider(
      names_from = brand_id, 
      values_from = proportion_with_sub,
      values_fn = list(proportion_with_sub = function(x) paste(x, collapse = ""))
    )
  
  return(df_wide)
}
