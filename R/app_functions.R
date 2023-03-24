#'
#' do_data_sample
#'
#' @param sdata: dataset
#' @param sel_scen: selected scenario/s
#' @param sel_years: selected year/s
#' @param sel_cols: selected column/s
#' @param sel_tree: selected variables in tree
#' @importFrom magrittr %>%
do_data_sample <- function(sdata,sel_scen,sel_years,sel_cols,sel_tree) {
  # subset dataset to the desired user's input
  data_sample = sdata %>%
    dplyr::filter(Scenario %in% sel_scen) %>%
    dplyr::filter(Variable %in% sel_tree) %>%
    dplyr::select(c(sel_cols, sel_years)) %>%
    data.table::as.data.table()

  return(data_sample)
}


do_mount_tree <- function(df, column_names, current_column = 1) {
  # filter the data frame to include only rows with the current level
  filtered_df <- df[!is.na(df[[column_names[current_column]]]), ]

  if (nrow(filtered_df) == 0) {
    # base case: if there are no more rows, return an empty list
    return("")
  } else {
    # create a list for the current level
    current_list <- list()

    # loop over the unique values in the current column
    for (value in unique(filtered_df[[column_names[current_column]]])) {
      # create a nested list for the next level
      filtered_df_tmp = filtered_df[filtered_df[[column_names[current_column]]] == value,]
      next_list <- create_nested_list(filtered_df_tmp, column_names, current_column = current_column + 1)

      # add the nested list to the current level with the appropriate attributes
      current_list[[value]] <- structure(next_list,
                                         sttype="default",
                                         stopened=FALSE,
                                         sticon="glyphicon glyphicon-plus",
                                         stselected=TRUE)
    }

    # add the current level to the list with the appropriate attributes
    structure(current_list,
              sttype="default",
              stopened=FALSE,
              sticon="glyphicon glyphicon-plus",
              stselected=TRUE)
  }
}

do_unmount_tree <- function(a) {

  if (length(a) > 0) {
    # transform dataset to list of items with delimiter |
    ll = rrapply(
      a,
      classes = "numeric",
      how = "flatten",
      options = list(namesep = "|", simplify = FALSE)
    )
    ll = names(ll)

    # keep only the the string after the first delimiter appearance
    extract_string <- function(x) {
      split_string <- strsplit(x, "\\|")[[1]]
      paste(split_string[2:length(split_string)], collapse = "|")
    }
    extracted_list <- c(unlist(lapply(ll, extract_string)))

    return(extracted_list)
  }
  return(a)
}
