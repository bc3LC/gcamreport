#########################################################################
#                         APP ANCILLARY FUNCTIONS                       #
#########################################################################

#' do_data_sample
#'
#' Function to subset a dataset with the user's choices
#' @param sdata: dataset
#' @param sel_scen: selected scenario/s
#' @param sel_years: selected year/s
#' @param sel_cols: selected column/s
#' @param sel_vars: selected variables in tree
#' @param sel_reg: selected regions in tree
#' @importFrom magrittr %>%
#' @return subseted dataset
do_data_sample <- function(sdata,sel_scen,sel_years,sel_cols,sel_vars,sel_reg) {
  # create dataframes from the nested variables and regions lists
  sel_vars = do_unmount_tree(sel_vars, 'variables')
  sel_reg = do_unmount_tree(sel_reg, 'regions')

  # subset dataset to the desired user's input
  data_sample = sdata %>%
    dplyr::filter(Scenario %in% sel_scen) %>%
    dplyr::filter(Variable %in% sel_vars) %>%
    dplyr::filter(Region %in% sel_reg) %>%
    dplyr::select(c(sel_cols, sel_years)) %>%
    data.table::as.data.table()

  return(data_sample)
}


#' do_mount_tree
#'
#' Recursive function to create a nested list from a dataframe.
#' @param df: dataset
#' @param column_names: names of the columns of the original dataset
#' @param current_column: number of the current column
#' @importFrom magrittr %>%
#' @return nested list
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
      next_list <- do_mount_tree(filtered_df_tmp, column_names, current_column = current_column + 1)

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


#' do_unmount_tree
#'
#' Create a dataframe from a nested list.
#' @param base_tree: nested list
#' @param type: 'variables' if nested list is refereed to variables, 'regions'
#' if it refers to regional  aggregation
#' @importFrom magrittr %>%
#' @return dataframe
do_unmount_tree <- function(base_tree, type) {

  if (length(base_tree) > 0) {
    # transform dataset to list of items with delimiter |
    ll = rrapply::rrapply(
        base_tree,
        classes = "numeric",
        how = "flatten",
        options = list(namesep = "|", simplify = FALSE)
    )
    ll = names(ll)

    # keep only the the string after the first delimiter appearance
    extract_string <- function(x) {
      split_string <- strsplit(x, "\\|")[[1]]
      if (type == 'variables') {
        paste(split_string[2:length(split_string)], collapse = "|")
      } else if (type == 'regions') {
        split_string <- strsplit(x, "\\|")[[1]][3]
        split_string <- na.omit(split_string)
      }
    }
    extracted_list <- c(unlist(lapply(ll, extract_string)))

    return(extracted_list)
  }
  return(a)
}
