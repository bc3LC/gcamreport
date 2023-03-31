#########################################################################
#                         APP ANCILLARY FUNCTIONS                       #
#########################################################################

do_codes <- function(data) {
  n = ncol(data)
  for (i in 1:n) {
    # create codes
    codes <- formatC(1:length(data[,i]), width = 3, flag = "0")

    # add a new column with the code for each region
    data$codes <- codes
    data$codes <- as.numeric(data$codes) + i*100
    names(data)[n + i] <- paste0('code_',names(data)[i])
  }
  return(data)
}

do_codes <- function(data) {
  n = ncol(data)
  for (i in 1:n) {
    data$code <- tidyr::unite(data, col = 'codes', names(data)[1]:names(data)[i], sep = '|')[1]
    names(data)[n + i] <- paste0('code_',names(data)[i])
  }
  data_clean <- data[,(n+1):(n+n)]
  data_clean <- apply(data_clean, MARGIN = c(1,2), function(x)
    ifelse(grepl("\\|NA", x), NA, x))

  data[,(n+1):(n+n)] = data_clean
  return(data)
}

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
#' @export
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
#' @export
do_mount_tree <- function(df, column_names, current_column = 1, selec = TRUE) {
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
      next_list <- do_mount_tree(filtered_df_tmp, column_names, current_column = current_column + 1, selec)

      # add the nested list to the current level with the appropriate attributes
      current_list[[value]] <- structure(next_list,
                                         sttype="default",
                                         stopened=FALSE,
                                         sticon="glyphicon glyphicon-plus",
                                         id="a",
                                         stselected=selec)
    }

    # add the current level to the list with the appropriate attributes
    structure(current_list,
              sttype="default",
              stopened=FALSE,
              id = 'a',
              sticon="glyphicon glyphicon-plus",
              stselected=selec)
  }
}

#' do_mount_db
#'
#' Build a database by iterating through a list of items using the values of
#' previously-recorded items to determine which column to place it in.
#' @param data: list of items
#' @param current_row: number of the current row
#' @param current_column: number of the current column
#' @param db: final dataframe
#' @param ref: reference dataframe
#' @importFrom magrittr %>%
#' @return itemized dataframe
#' @export
do_mount_db <- function(data, current_row = 1, current_col = 1, db = NULL, ref) {

  # if no database is provided, initialize one with a single row and 7 columns.
  if (is.null(db)) {
    db = data.frame(matrix(ncol = 7, nrow = 1))
  }

  if (length(data) > 0) {
    # if there are still items in the data list:

    if (current_col == 1) {
      # if this is the first item being added, set the reference dataframe for column selection to include all columns.
      sub_ref = ref
    } else {
      # otherwise, subset the reference dataframe to only include rows that match the values of previously-recorded items.
      sub_ref = ref
      for (i in 1:(current_col-1)) {
        sub_ref = subset(sub_ref, sub_ref[, i] == db[current_row, i])
      }
    }

    if (grepl(data[1], unique(sub_ref[current_col]))) {
      # if the item matches the expected value for the current column:

      # tecord the item in the database.
      db[current_row, current_col] = data[1]

      # advance to the next column and fill in the remaining columns with NA.
      current_col = current_col + 1
      db[current_row, current_col : 7] = NA

      # copy the current row to a new row in the database.
      # this way, the new item can be added to a new row while preserving the values of previously-recorded items.
      db[current_row + 1,] = db[current_row,]

      # recursively call the function on the remaining items in the list.
      do_mount_db(data[-1], current_row = current_row + 1, current_col, db, ref)

    } else {
      # if the item does not match the expected value for the current column:

      # copy the current row to a new row in the database.
      # this is necessary because the current row may be modified in a recursive call.
      db[current_row + 1,] = db[current_row,]

      # recursively call the function with a lower column index.
      # this effectively "undoes" the previous item recorded at the current column.
      do_mount_db(data, current_row = current_row + 1, current_col = current_col - 1, db, ref)
    }
  } else {
    # when there are no more items in the list, return the unique rows of the database.
    return(unique(db))
  }
}



#' do_list
#'
#' Create a list of items with separator to match the style of the 'Variables' column
#' @param db: dataframe
#' @importFrom magrittr %>%
#' @return itemized list
#' @export
do_list <- function(db) {
  ## idea: to create a function that from a dataframe creates this list to later see the desired varaibles
  list = db %>%
    tidyr::unite(X0, 1:7, sep = "|", remove = FALSE)
  list = unlist(list[,1])
  list_clean = lapply(list, function(x) stringr::str_remove_all(x, "\\|NA"))

  return(unlist(list_clean))
}


#' do_unmount_tree
#'
#' Create a list of "nested" items witha separator from a nested list.
#' @param base_tree: nested list
#' @param type: 'variables' if nested list is refereed to variables, 'regions'
#' if it refers to regional  aggregation
#' @importFrom magrittr %>%
#' @return list of "nested" items
#' @export
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
