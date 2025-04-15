# Function to process list of dataframes
process_list <- function(list_of_dataframes, significance_level = 0.95) {
  # Map over each tibble, passing the additional argument
  results <- list_of_dataframes |> 
    purrr::map(~proptest_dataframe(.x, significance_level = 1 - significance_level))
  
  # Combine all results into a single tibble
  combined_result <- results |> 
    data.table::rbindlist(fill = TRUE) |> 
    tibble::as_tibble() |> 
    dplyr::mutate(Category = as.character(Category))
  
  return(combined_result)
}
