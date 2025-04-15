# create tables with percentages and significance 
sig_table <- function(data, note) {
  library(gt)
  
  # Programmatically generate the labels for the columns (with <br>)
  col_labels <- data |> 
    colnames() |> 
    purrr::set_names() |>   # Keeps the original column names as-is
    purrr::map(~ html(glue::glue("{gsub(' ', '<br>', .)}")))  # Programmatically replace space with <br> in labels
  
  # set groups depending on the table to make 
  if (any(names(data) == "bucket")) {
    data <- data |> 
      group_by(bucket)
  } 
  # Create the gt table with programmatically generated column labels and subscripting
  data |> 
    gt() |> 
    # hide the cat column from the table
    cols_hide(columns = "cat") |> 
    tab_header(
      title = glue::glue("{unique(data$cat)} - Competitive summary"),
      # subtitle = glue::glue("{sub3}")
      ) |> 
    cols_label(.list = col_labels) |> 
    fmt_markdown(columns = everything()) |> 
    # bolding groups and columns 
    tab_style(
      style = cell_text(weight = 'bold',
                        size = px(13)),
      locations = list(
        cells_column_labels(
          columns = everything()),
        cells_row_groups(groups = TRUE))
    ) |> 
    gt::tab_footnote(
      footnote = note,
      locations = gt::cells_title(groups = "title")
    ) |> 
    # aligning the title left
    gt::tab_style(
      style = gt::cell_text(align = 'left',
                            weight = 'bold',
                            size = gt::px(16)),
      locations = gt::cells_title(c("title"))
    ) |> 
    # aligning the subtitle left
    gt::tab_style(
      style = gt::cell_text(align = 'left',
                            weight = 'bold',
                            size = gt::px(14)),
      locations = gt::cells_title(c("subtitle")) 
    ) |> 
    # centering column labels (not first column)
    gt::tab_style(
      style = gt::cell_text(align = 'center'),
      locations = gt::cells_column_labels(
        columns = -1  # Exclude the first column
      )
    ) |>
    # font size in table
    gt::tab_style(
      style = gt::cell_text(size = px(12)),
      locations = cells_body(
        columns = dplyr::everything())
    ) |> 
    # centering all variables in table (not first column)
    gt::tab_style(
      style = gt::cell_text(align = "center"),
      locations = gt::cells_body(columns = -1)
    ) |> 
    cols_label(
      Category = ""
    ) |> 
    opt_table_font(
      # font = list(
      #   google_font("Open Sans")
      # )
      font = c(
        "BMW"
      )
    ) |> 
    # final options
    gt::tab_options(
      data_row.padding = gt::px(4),
      row_group.padding = gt::px(4),
      source_notes.font.size = gt::px(10),
      footnotes.font.size = gt::px(10),
      footnotes.marks = "" # empty footnote mark
    ) 
}
