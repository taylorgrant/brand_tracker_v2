#' Prep for GT table
#'
#' Need to get the crosstab groups that are going to stay in the data table.
#'
#' @param dat Single dataframe of results from the list
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' filters <- table_prep(result_list[[1]])
#' }
table_prep <- function(dat) {
  if (any(stringr::str_detect(colnames(dat), "group_2"))) {
    # 2 groups could be a demo and campaign source
    grp2 <- select.list(
      title = "\nWhich groups would you like to keep in the table?",
      choices = unique(dat$group_2),
      multiple = TRUE,
      graphics = FALSE
    )
    grp1 <- select.list(
      title = "\nWhich groups would you like to keep in the table?",
      choices = unique(dat$group_1),
      multiple = TRUE,
      graphics = FALSE
    )
    group_filter <- c(grp1, grp2)
  } else if (
    !any(stringr::str_detect(colnames(dat), "group_2")) &
      any(stringr::str_detect(colnames(dat), "group_1"))
  ) {
    grp1 <- select.list(
      title = "\nWhich groups would you like to keep in the table?",
      c(unique(dat$group_1)),
      multiple = TRUE,
      graphics = FALSE
    )
    group_filter <- c(grp1, grp2 = NULL)
  } else {
    group_filter <- c(grp1 = NULL, grp2 = NULL)
  }
  return(group_filter)
}
