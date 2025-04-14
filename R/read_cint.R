#' Read in tracker data from Cint
#'
#' This function reads in the raw data and weights the data using the weights provided by Cint in the file. All are then put into the global environment
#' 
#' @param file_loc Give the location of the tracker file
#'
#' @return No object is returned. Side effect: multiple survey objects are created in the global environment.
#' @export
#'
#' @examples
#' \dontrun{
#' file_loc <- "/folder/director/file.csv"
#' read_cint(file_loc)
#' }
read_cint <- function(file_loc) {
  gen_labels = c("Gen Alpha", "Gen Z", "Millennials", "Gen X",
                 "Boomers", "Silent", "Greatest")
  premium_makes <- c("car_make_2", "car_make_3","car_make_10", "car_make_13", 
                     "car_make_14","car_make_16", "car_make_20", "car_make_21", "car_make_24") 
  # premium - audi, bmw, infiniti, lexus, lucid, mercedes-benz, porsche, range rover, tesla
  
  df <- readxl::read_excel(file_loc) |> 
    janitor::clean_names() |> 
    filter(end_date != "End Date") |> 
    dplyr::distinct(response_id, .keep_all = TRUE) |> 
    dplyr::mutate(
      across(c(age, freq_total_digital, freq_total_tv, starts_with("weights")), as.numeric),
      yob = lubridate::year(Sys.Date()) - age,
      generations = dplyr::case_when(
        yob < 2013 & yob > 1996 ~ "Gen Z",
        yob < 1997 & yob > 1980 ~ "Millennials",
        yob < 1981 & yob > 1964 ~ "Gen X",
        yob < 1965 & yob > 1945 ~ "Boomers",
        yob < 1946 & yob > 1927 ~ "Silent",
        yob < 1928 ~ "Greatest",
        yob > 2012 ~ "Gen Alpha"
      ),
      demo_region = dplyr::case_when(
        demo_state %in% c("ALABAMA", "ARKANSAS", "DISTRICT OF COLUMBIA", 
                          "FLORIDA", "GEORGIA", "KENTUCKY", "LOUISIANA", "MISSISSIPPI", 
                          "NORTH CAROLINA", "OKLAHOMA", "SOUTH CAROLINA", "TENNESSEE", 
                          "TEXAS", "VIRGINIA", "WEST VIRGINIA", "DELAWARE") ~ "South",
        demo_state %in% c("ILLINOIS", "INDIANA", "IOWA", "KANSAS", "MICHIGAN",
                          "MINNESOTA", "MISSOURI", "NEBRASKA", "NORTH DAKOTA", "OHIO", 
                          "SOUTH DAKOTA", "WISCONSIN") ~ "Central",
        demo_state %in% c("CONNECTICUT", "MAINE", "MASSACHUSETTS", "MARYLAND", 
                          "NEW HAMPSHIRE", "NEW JERSEY", "NEW YORK", "PENNSYLVANIA", 
                          "RHODE ISLAND", "VERMONT") ~ "East",
        TRUE ~ "West"
      ),
      genz_millen = dplyr::if_else(stringr::str_detect(generations, "Z|Mill"), "Gen Z/Millennial", "Gen X/Boomer"),
      generations = factor(generations, levels = gen_labels), 
      demo_premium = dplyr::case_when(
        demo_income != "$100k-$139,999k" & 
          rowSums(dplyr::across(dplyr::any_of(premium_makes), ~ as.integer(. != "0"))) > 0 ~ 'Premium',
        TRUE ~ "Non-Premium"
      ),
      date = as.Date(end_date, format = "%m/%d/%Y"),
      month = lubridate::month(date, label = TRUE),
      across(starts_with("weights"), ~ dplyr::if_else(is.na(.), 0, .))
    )
  
  if ("weights_digital" %in% names(df)) {
    assign("digital", df |> srvyr::as_survey_design(ids = 1, weight = weights_digital), envir = .GlobalEnv)
  }
  if ("weights_social" %in% names(df)) {
    assign("social", df |> srvyr::as_survey_design(ids = 1, weight = weights_social), envir = .GlobalEnv)
  }
  if ("weights_xmedia" %in% names(df)) {
    assign("campaign", df |> srvyr::as_survey_design(ids = 1, weight = weights_xmedia), envir = .GlobalEnv)
  }
  if ("weights_tv" %in% names(df)) {
    assign("tv", df |> srvyr::as_survey_design(ids = 1, weight = weights_tv), envir = .GlobalEnv)
  }
  if ("weights_you_tube" %in% names(df)) {
    assign("youtube", df |> srvyr::as_survey_design(ids = 1, weight = weights_you_tube), envir = .GlobalEnv)
  }
  if ("weights_ooh" %in% names(df)) {
    assign("ooh", df |> srvyr::as_survey_design(ids = 1, weight = weights_ooh), envir = .GlobalEnv)
  }
  assign("unweighted", df |> srvyr::as_survey_design(ids = NULL), envir = .GlobalEnv)
  assign("df", df, envir = .GlobalEnv)
}
