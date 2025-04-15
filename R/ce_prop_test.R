#' Significance test across control / exposed
#' 
#' Take the tidy dataframe and spread it for easy prop.test
#'
#' @param res Tidy dataframe returned by the `question_summary()` function
#'
#' @return Wide dataframe with response proportions, n, total for each group, p.value and chi-square statistic
#' @export
#'
#' @examples
#' \dontrun{
#' # assuming `question_summary()` returned a list of multiple questions
#' # run across all dataframes in my list 
#' result_list <- purrr::map(results, result_prop_test)
#' }
ce_prop_test <- function(res) {
  tmpout <- res |> 
    tidyr::pivot_wider(
      names_from = !!rlang::sym(names(res)[1]), 
      values_from = c(proportion, n, total)
    ) |> 
    dplyr::filter(!is.na(proportion_control)) |> 
    dplyr::filter(!is.na(proportion_test)) |> 
    dplyr::rowwise() |> 
    dplyr::mutate(
      prop_test = list(prop.test(c(n_control, n_test), c(total_control, total_test))),
      p_value = prop_test$p.value,  # Extract the p-value
      statistic = prop_test$statistic # Extract the chi-squared statistic
    ) |> 
    dplyr::mutate(lift = round((round(proportion_test, 2) - round(proportion_control, 2))*100), 
                  sig_level = dplyr::case_when(p_value <= .05 ~ .95,
                                 p_value <= .1 & p_value >.05 ~ .90,
                                 p_value < .2 & p_value > .1 ~ .80)) 
  
  # rename any groups 
  svy_pos <- which(names(tmpout) == "svy_q")
  if (svy_pos > 1) {
    colnames(tmpout)[1:(svy_pos - 1)] <- paste0("group_", seq_len(svy_pos - 1))
  }
  return(tmpout)
  
}









