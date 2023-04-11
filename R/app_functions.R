#########################################################################
#                         APP ANCILLARY FUNCTIONS                       #
#########################################################################

update_user_choices_plot <- function(selected_scen, selected_years, selected_cols,
                                     tree_regions, tree_variables, sidebarItemExpanded,
                                     aim) {
  sel_reg_ini = shinyTree::get_selected(tree_regions, format = 'slices')
  sel_vars_ini = shinyTree::get_selected(tree_variables, format = 'slices')

  # read user's selection
  basic_reg = 0
  basic_vars = 0
  if (firstReg && ((!is.null(sidebarItemExpanded) && sidebarItemExpanded != "Regions") || is.null(sidebarItemExpanded))) {
    sel_reg_ini = reg_cont$region
    basic_reg = 1
  }
  if (firstVars && ((!is.null(sidebarItemExpanded) && sidebarItemExpanded != "Variables") || is.null(sidebarItemExpanded))) {
    sel_vars_ini = unique(cols$col1)
    basic_vars = 1
  }
  if (noReg) {
    noReg <<- FALSE
    sel_reg_ini = c()
    basic_reg = 2
  }
  if (noVars) {
    noVars <<- FALSE
    sel_vars_ini = c()
    basic_vars = 2
  }
  firstVars <<- ifelse(!firstVars || (firstVars && !is.null(sidebarItemExpanded) && sidebarItemExpanded == "Variables"), FALSE, TRUE)
  firstReg <<- ifelse(!firstReg || (firstReg && !is.null(sidebarItemExpanded) && sidebarItemExpanded == "Regions"), FALSE, TRUE)

  if (aim == 'data') {
    sel_cols = selected_cols
  } else {
    sel_cols = c('Model', 'Scenario', 'Region', 'Variable', 'Unit')
  }

  toret = list(
    'scen' = selected_scen,
    'years' = selected_years,
    'cols' = sel_cols,
    'vars' = sel_vars_ini,
    'reg' = sel_reg_ini,
    'basic_reg' = basic_reg,
    'basic_vars' = basic_vars
  )
  return(toret)
}



#' reset_first_load
#'
#' Initialize variables to run the app
#' @importFrom magrittr %>%
#' @export
reset_first_load <- function() {
  reg_cont <<- read.csv(paste0(here::here(), "/inst/extdata/mappings", "/regions_continents_map.csv"), skip = 1)
  tree_reg <<- do_mount_tree(reg_cont,names(reg_cont),selec=TRUE)
  cols <<- unique(sdata[, grepl('col', names(sdata))])
  tree_vars <<- do_mount_tree(cols,names(cols),selec=TRUE)
  firstLoad <<- TRUE
  firstReg <<- TRUE
  firstVars <<- TRUE
  noReg <<- FALSE
  noVars <<- FALSE
}


#' do_codes
#'
#' Create new column for each column collapsing all the previous columns
#' @param data: dataset
#' @importFrom magrittr %>%
#' @export
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
do_data_sample <- function(sdata,sel_scen,sel_years,sel_cols,sel_vars,sel_reg,
                           basic_reg, basic_vars) {
  print('do_data_sample')
  if (basic_reg == 1) {
    reg = unique(sdata$Region)
  } else if (basic_reg == 2) {
    reg = c()
  } else {
    reg = do_unmount_tree(sel_reg, 'regions')
  }

  if (basic_vars == 1) {
    vars = unique(sdata$Variable)
  } else if (basic_vars == 2) {
    vars = c()
  } else{
    vars = do_unmount_tree(sel_vars, 'variables')
  }

  data_sample = sdata %>%
    dplyr::filter(Scenario %in% sel_scen) %>%
    dplyr::filter(Variable %in% vars) %>%
    dplyr::filter(Region %in% reg) %>%
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
                                         stselected=selec)
    }

    # add the current level to the list with the appropriate attributes
    structure(current_list,
              sttype="default",
              stopened=FALSE,
              sticon="glyphicon glyphicon-plus",
              stselected=selec)
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
#' @export
do_unmount_tree <- function(base_tree, type) {

  print('unmount tree')

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
        split_string <- strsplit(x, "\\|")[[1]][length(split_string)]
        split_string <- na.omit(split_string)
      }
    }
    extracted_list <- c(unlist(lapply(ll, extract_string)))

    return(extracted_list)
  }
  return(a)
}
