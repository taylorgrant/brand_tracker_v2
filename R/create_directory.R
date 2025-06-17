create_directory <- function(brand, filters, main_subfolders = c("figures", "competitive", "raw_data", "creative"), subfolders = c("Campaign", "Social", "Digital", "TV", "YouTube")) {

    main_path <- here::here("processed",glue::glue("{brand}-{stringr::str_replace(filters, '/', '-')}_{Sys.Date()}"))
    if (!dir.exists(main_path)) {
      dir.create(main_path, recursive = TRUE)
    } 
  # }
    
  # Create the main subfolders ("tables", "figures", and "competitive")
  purrr::walk(main_subfolders, function(main_subfolder) {
    main_subfolder_path <- file.path(main_path, main_subfolder)
    if (!dir.exists(main_subfolder_path)) {
      dir.create(main_subfolder_path)
    } 
    
    # Only create subfolders for "tables" and "figures"
    if (main_subfolder %in% c("tables", "figures")) {
      purrr::walk(subfolders, function(subfolder) {
        subfolder_path <- file.path(main_subfolder_path, subfolder)
        if (!dir.exists(subfolder_path)) {
          dir.create(subfolder_path)
        }
      })
    }
  })
  return(main_path)
  }



