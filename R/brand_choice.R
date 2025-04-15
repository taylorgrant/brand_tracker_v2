#' Retrieve tracker variable names for a specific brand
#'
#' Based on the input brand name, this function returns the survey variable names used
#' to measure brand awareness, key attributes, and brand perceptions in the tracker data.
#' 
#' Supported brands include: Audi, BMW, Lexus, Mercedes Benz, Tesla, and None of the Above.
#' This function is case-insensitive and includes alias handling (e.g., "merc" → "Mercedes Benz").
#'
#' @param brand Character. Name of the brand of interest (e.g., "BMW").
#'
#' @return A named list of three tibbles:
#' \describe{
#'   \item{brand_vars}{Variables and question labels for awareness and momentum.}
#'   \item{key_attrs}{Variables and question labels for key model attributes.}
#'   \item{brand_attrs}{Variables and question labels for brand perceptions.}
#' }
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' brand_choice("BMW")
#' brand_choice("merc")
#' }
brand_choice <- function(brand) {
  brand_clean <- tolower(brand)
  
  if (!stringr::str_detect(brand_clean, "mercedes|tesla|bmw|audi|lexus|none")) {
    stop("Select a make included in the tracker")
  }
  
  if (stringr::str_detect(brand_clean, "merc")) {
    brand <- "Mercedes Benz"
  } else if (stringr::str_detect(brand_clean, "none")) {
    brand <- "None of the Above"
  }
  
  brand_clean <- tolower(brand)
  
  bv_n <- switch(brand_clean, 
                 "none of the above" = c("6", "7", "7", "7", "x6"),
                 "audi" = c("audi","4", "4", "4", "4", "x4"),
                 "bmw" = c("client","1", "1", "1", "1", "x1"),
                 "lexus" = c("lexus","5", "5", "5", "5", "x5"),
                 "mercedes benz" = c("mercedes_benz","2", "2", "2", "2", "x2"),
                 "tesla" = c("tesla","3", "3", "3","3", "x3"))
  
  bpt_n <- switch(brand_clean, 
                 "none of these" = rep("99", 13),
                 "audi" = rep("4", 15),
                 "bmw" = rep("1", 15),
                 "lexus" = rep("5", 15),
                 "mercedes benz" = rep("2", 15),
                 "tesla" = rep("3", 15))
  
  fba_n <- switch(brand_clean, 
                 "none of the above" = rep("7", 15),
                 "audi" = rep("4", 15),
                 "bmw" = rep("1", 15),
                 "lexus" = rep("5", 15),
                 "mercedes benz" = rep("2", 15),
                 "tesla" = rep("3", 15))
  
  bv_vec <- c("unaided_awareness_","awr_a_", "awr_aad_", "con_br_", "pi_br_", "momentum_br_"
              )
  bpt_vec <- c("att_brpersonality_1_", "att_brpersonality_2_", "att_brpersonality_3_", 
              "att_brpersonality_4_", "att_brpersonality_5_", "att_brpersonality_6_", 
              "att_brpersonality_7_", "att_brpersonality_8_", "att_brpersonality_9_", 
              "att_brpersonality_10_", "att_brpersonality_11_", "att_brpersonality_12_", 
              "att_brpersonality_13_", "att_brpersonality_14_", "att_brpersonality_15_"
              )
  fba_vec <- c("att_brfunctional_1_", "att_brfunctional_2_", "att_brfunctional_3_", 
              "att_brfunctional_4_", "att_brfunctional_5_", "att_brfunctional_6_", 
              "att_brfunctional_7_", "att_brfunctional_8_", "att_brfunctional_9_", 
              "att_brfunctional_10_", "att_brfunctional_11_", "att_brfunctional_12_", 
              "att_brfunctional_13_", "att_brfunctional_14_", "att_brfunctional_15_"
              )

  brand_vars <- dplyr::tibble(
      var = glue::glue("{bv_vec}{bv_n}"),
      q = c("Unaided Awareness","Aided Awareness", "Aided Ad Awareness", 
            "Purchase Consideration", "Purchase Intent", "Brand Momentum")
    )
  
  brand_traits <- dplyr::tibble(
    var = glue::glue("{bpt_vec}{bpt_n}"),
    q = c("Adventurous", "Aggressive", "Arrogant", "Confident", "Distinctive",
          "Exciting", "Innovative", "Passionate", "Practical", "Responsible",
          "Trusted", "Leader", "Youthful", "Classy", "Traditional")
  )
  
  brand_attrs <- dplyr::tibble(
    var = glue::glue("{fba_vec}{fba_n}"),
    q = c("Advanced safety features", "Advanced tech features", "Attractive styling",
          "Comfortable", "Customer-oriented dealerships", "Customizable", "Reliable",
          "Environmentally friendly", "Fun to drive", "Lasts a long time", 
          "Prestigious", "Quality materials, fit, and finish", "Responsive handling",
          "Strong brand heritage", "Good value for the money")
  )
  
  list(brand_vars = brand_vars, brand_traits = brand_traits, brand_attrs = brand_attrs)
}
