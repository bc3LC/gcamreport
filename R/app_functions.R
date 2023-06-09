#########################################################################
#                         APP ANCILLARY FUNCTIONS                       #
#########################################################################


#' compute_height
#'
#' Return perfect heigh of rendered plots with ungrouped regions
#' @param reg_all: regions' vector
#' @keywords internal
#' @importFrom magrittr %>%
#' @export
compute_height <- function(reg_all) {
  reg_clean = setdiff(reg_all, c('MAF','LAM','OECD90','REF','ASIA'))
  h = dplyr::if_else(length(reg_clean) < 3, 325, 160*ceiling(sqrt(length(reg_clean))))

  return(h)
}


#' is_leaf
#'
#' Return TRUE if it is a leaf node; FALSE otherwise.
#' @param tree: tree node to be checked
#' @keywords internal
#' @importFrom magrittr %>%
#' @export
is_leaf <- function(tree) {
  if (length(tree) > 1 || length(tree[[1]]) > 1 || length(tree[[1]][[1]]) > 1 || length(tree[[1]][[1]][[1]]) > 1
      || length(tree[[1]][[1]][[1]][[1]]) > 1 || length(tree[[1]][[1]][[1]][[1]][[1]]) > 1 || length(tree[[1]][[1]][[1]][[1]][[1]][[1]]) > 1) {
    return(FALSE)
  }

  return(TRUE)
}


#' change_style
#'
#' Change the style of the tree nodes: 'dis' if needs to be disabled, 'basic' otherwise.
#' @param tree: base tree to change the style on
#' @param type: either regions or variables
#' @param tmp_vars: variables which require 'dis' style
#' @keywords internal
#' @importFrom magrittr %>%
#' @export
change_style <- function(tree, type, tmp_vars = NULL) {
  n = length(tree)
  for (i in 1:n) {
    # get the attributes for the current node
    attrs <- attributes(tree[[i]])

    # check if the current node has children
    if (!is_leaf(tree[[i]])) {
      # Recursively call the function on the children of the current node
      tree[[i]] <- change_style(tree[[i]], type, tmp_vars)
    }

    # change the style for the current node. If it's in the list of variables to be disabled, set
    # it to 'dis'. To 'basic' otherwise
    if (type == 'variables' && length(tmp_vars) > 0 && attrs$my_id %in% tmp_vars) {
      attrs$sttype = 'dis'
    } else {
      attrs$sttype = 'basic'
    }
    attributes(tree[[i]]) <- attrs
  }

  return(tree)
}


#' check_user_choices_plot
#'
#' Check user's choices to do the plot: at least one scenario, variable, year, and region
#' must be chosen. In case of 'grouped' plot, all variables must be from the same category.
#' @param scen: user's selected scenarios
#' @param years: user's selected years
#' @param reg: user's selected regions
#' @param vars: user's selected variables
#' @param grouped: if TRUE, aim to display grouped plot; ungrouped plot otherwise
#' @keywords internal
#' @importFrom magrittr %>%
#' @export
check_user_choices_plot <- function(vars, scen, years, reg, grouped) {

  # errors' vector
  error_message = c()

  # study the category of the selected variables
  if (length(vars) > 0) {
    check_vars = sub("\\|.*", "", stringr::str_extract(vars, "(.*?)(\\||$)"))
  } else {
    check_vars = NULL
  }

  # check that at least one scenario has been choosen
  if (length(unique(scen)) < 1) {
    error_message <- c(error_message,"ERROR: Select at least one scenario please.")
  }
  # check that at least one year has been choosen
  if (length(unique(years)) < 1) {
    error_message <- c(error_message,"ERROR: Select at least one year please.")
  }
  # check that at least one region has been choosen
  if (length(unique(reg)) < 1) {
    error_message <- c(error_message,"ERROR: Select at least one region please.")
  }
  # check that at least one variable has been choosen
  if (length(unique(check_vars)) < 1) {
    error_message <- c(error_message,"ERROR: Select at least one variable please.")
  }
  # in case of grouped-variables' plot, check that at only variables from the same category have been choosen
  if (grouped & length(unique(check_vars)) > 1) {
    error_message <- c(error_message,"ERROR: Select only variables from the same category please.")
  }

  return(error_message)
}

#' update_user_choices_plot
#'
#' Update dataframe to display plots with the user's choices
#' @param selected_scen: user's selected scenarios
#' @param selected_years: user's selected years
#' @param tree_regions: user's selected regions
#' @param tree_variables: user's selected variables
#' @param sidebarItemExpanded: input sidebar expanded item
#' @keywords internal
#' @importFrom magrittr %>%
#' @export
update_user_choices_plot <- function(selected_scen, selected_years,
                                     tree_regions, tree_variables, sidebarItemExpanded) {
  # get selected regions and variables from input
  sel_reg_ini = shinyTree::get_selected(tree_regions, format = 'slices')
  sel_vars_ini = shinyTree::get_selected(tree_variables, format = 'slices')

  basic_reg = 0
  basic_vars = 0
  # if it's the first time loading regions and there is a sidebarItem expanded different than regions, choose all possible regions
  if (firstReg && ((!is.null(sidebarItemExpanded) && sidebarItemExpanded != "Regions") || is.null(sidebarItemExpanded))) {
    sel_reg_ini = reg_cont$region
    basic_reg = 1
  }

  # if it's the first time loading variables and there is a sidebarItem expanded different than variables, choose all possible variables
  if (firstVars && ((!is.null(sidebarItemExpanded) && sidebarItemExpanded != "Variables") || is.null(sidebarItemExpanded))) {
    sel_vars_ini = unique(cols$col1)
    basic_vars = 1
  }

  # if there are no selected regions
  if (noReg) {
    noReg <<- FALSE
    sel_reg_ini = c()
    basic_reg = 2
  }

  # if there are no selected variables
  if (noVars) {
    noVars <<- FALSE
    sel_vars_ini = c()
    basic_vars = 2
  }

  # set firstVars and/or firstReg to FALSE if it's not the first time loading them or if their sidebarItem is expanded
  firstVars <<- ifelse(!firstVars || (firstVars && !is.null(sidebarItemExpanded) && sidebarItemExpanded == "Variables"), FALSE, TRUE)
  firstReg <<- ifelse(!firstReg || (firstReg && !is.null(sidebarItemExpanded) && sidebarItemExpanded == "Regions"), FALSE, TRUE)

  # transform the regions and variables' structures to lists
  if (is.list(sel_vars_ini) & length(sel_vars_ini) > 0) {
    sel_vars = do_unmount_tree(sel_vars_ini, 'variables')
  } else {
    sel_vars = sel_vars_ini
  }
  if (is.list(sel_reg_ini) & length(sel_reg_ini) > 0) {
    sel_reg = do_unmount_tree(sel_reg_ini, 'regions')
  } else {
    sel_reg = sel_reg_ini
  }

  # consider all possible columns
  sel_cols = c('Model', 'Scenario', 'Region', 'Variable', 'Unit')

  toret = list(
    'scen' = selected_scen,
    'years' = selected_years,
    'cols' = sel_cols,
    'vars_ini' = sel_vars_ini,
    'reg_ini' = sel_reg_ini,
    'vars' = sel_vars,
    'reg' = sel_reg,
    'basic_reg' = basic_reg,
    'basic_vars' = basic_vars
  )
  return(toret)
}


#' reset_first_load
#'
#' Initialize variables to run the app
#' @keywords internal
#' @importFrom magrittr %>%
#' @export
reset_first_load <- function() {
  tree_reg <<- do_mount_tree(reg_cont,names(reg_cont),selec=TRUE)
  cols <<- unique(sdata[, grepl('col', names(sdata))])
  tree_vars <<- do_mount_tree(cols,names(cols),selec=TRUE)
  firstLoad <<- TRUE
  firstReg <<- TRUE
  firstVars <<- TRUE
  noReg <<- FALSE
  noVars <<- FALSE
  updatedVars <<- FALSE
  all_vars <<- do_collapse_df(cols)
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
#' @keywords internal
#' @importFrom magrittr %>%
#' @return subseted dataset
#' @export
do_data_sample <- function(sdata,sel_scen,sel_years,sel_cols,sel_vars,sel_reg,
                           basic_reg, basic_vars) {
  # obtain the region's list
  if (basic_reg == 1) {
    reg = unique(sdata$Region)
  } else if (basic_reg == 2 || (is.list(sel_reg) && length(sel_reg) == 0)) {
    reg = c()
  } else {
    reg = do_unmount_tree(sel_reg, 'regions')
  }

  # obtain the variables's list
  if (basic_vars == 1) {
    vars = unique(sdata$Variable)
  } else if (basic_vars == 2 || (is.list(sel_vars) && length(sel_vars) == 0)) {
    vars = c()
  } else{
    vars = do_unmount_tree(sel_vars, 'variables')
  }

  # subset the data
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
#' @keywords internal
#' @importFrom magrittr %>%
#' @return nested list
#' @export
do_mount_tree <- function(df, column_names, current_column = 1, selec = TRUE, iid = NULL) {
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
      if (!is.null(iid)) {
        tmp_id = paste(iid, value, sep = '|')
      } else {
        tmp_id = value
      }
      next_list <- do_mount_tree(filtered_df_tmp, column_names, current_column = current_column + 1, selec, tmp_id)

      # add the nested list to the current level with the appropriate attributes
      current_list[[value]] <- structure(next_list,
                                         sttype="basic",
                                         stopened=FALSE,
                                         sticon="glyphicon glyphicon-plus",
                                         stselected=selec,
                                         my_id=tmp_id)
    }

    # add the current level to the list with the appropriate attributes
    structure(current_list,
              sttype="basic",
              stopened=FALSE,
              sticon="glyphicon glyphicon-plus",
              stselected=selec,
              my_id=tmp_id)
  }
}


#' do_unmount_tree
#'
#' Create a dataframe from a nested list.
#' @param base_tree: nested list
#' @param type: 'variables' if nested list is refereed to variables, 'regions'
#' if it refers to regional  aggregation
#' @keywords internal
#' @importFrom magrittr %>%
#' @return dataframe
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
        split_string <- strsplit(x, "\\|")[[1]][length(split_string)]
        split_string <- na.omit(split_string)
      }
    }
    extracted_list <- c(unlist(lapply(ll, extract_string)))

    return(extracted_list)
  }
  return(a)
}


#' do_collapse_df
#'
#' Collapse all columns by row of the dataframe and remove NA
#' @param basic_data: dataframe/datatable to be collapsed
#' @return Vector with the collapsed data
#' @keywords internal
#' @importFrom magrittr %>%
#' @export
do_collapse_df <- function(basic_data) {
  # collapse all columns by row
  df_collapsed <- data.frame(collapsed = apply(basic_data, 1, paste, collapse = "|"))

  # remove 'NA' pattern
  df_clean <- apply(df_collapsed, c(1), function(x) gsub("\\|NA", "", x)) %>%
    as.vector()

  return(df_clean)
}
