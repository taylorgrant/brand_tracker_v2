brand_choice_all_brands <- function() {
  # Define the brands to iterate over
  brands <- c("Audi", "BMW", "Lexus", "Mercedes Benz", "Tesla")
  
  # Initialize empty tibbles for each category
  all_brand_vars <- dplyr::tibble(var = character(), q = character(), brand = character())
  all_brand_traits <- dplyr::tibble(var = character(), q = character(), brand = character())
  all_brand_attrs <- dplyr::tibble(var = character(), q = character(), brand = character())
  
  # Iterate over each brand and accumulate the results
  for (brand in brands) {
    # Get the individual results for each brand
    result <- brand_choice(brand)
    
    # Add a brand column to each tibble and accumulate
    all_brand_vars <- dplyr::bind_rows(
      all_brand_vars,
      result$brand_vars |>  
        dplyr::mutate(brand = brand)
    )
    
    all_brand_traits <- dplyr::bind_rows(
      all_brand_traits,
      result$brand_traits |>  
        dplyr::mutate(brand = brand)
    )
    
    all_brand_attrs <- dplyr::bind_rows(
      all_brand_attrs,
      result$brand_attrs |>  
        dplyr::mutate(brand = brand)
    )
  }
  
  # Return a list of the three accumulated tibbles
  list(
    brand_vars = all_brand_vars,
    brand_traits = all_brand_traits,
    brand_attrs = all_brand_attrs
  )
}


