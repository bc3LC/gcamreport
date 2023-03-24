
#' do_data_sample
#'
#' @param sdata: dataset
#' @param sel_scen: selected scenario/s
#' @param sel_years: selected year/s
#' @param sel_cols: selected column/s
#' @param sel_tree: selected variables in tree
#' @importFrom magrittr %>%
do_data_sample <- function(sdata,sel_scen,sel_years,sel_cols,sel_tree) {
  data_sample = sdata %>%
    dplyr::filter(Scenario %in% sel_scen) %>%
    dplyr::filter(Variable %in% sel_tree) %>%
    dplyr::select(c(sel_cols, sel_years)) %>%
    data.table::as.data.table()

  # if (sel_type_vars == 'See all options') {
  #   data_sample = data_sample %>%
  #     filter(Variable %in% sel_col0)
  # } else {
  #   data_sample = data_sample %>%
  #     filter(col1 %in% sel_col1)
  # }

  # data_sample = data_sample %>%
  #   dplyr::select(c(sel_cols, sel_years)) %>%
  #   data.table::as.data.table()

  return(data_sample)
}


do_mount_tree <- function(a) {

  # defining tree as list
  tree <- list()

  # Condition to identifly if selected column is not last column
  if (!class(a) %in% c("character","factor") && length(a) > 1) {

    # getting list of unique names from the columns to create folder
    b <- na.omit(unique(a[,1]))

    # running file name in loop
    for (i in b) {
      # check if the file name i not blank
      if (i != "") {
        # subset data for content of folder
        subdata <- a[which(a[,1] == i),]

        # if there is only one element for that item change icon as file
        if (length(subdata[,-1])==1) {
          tree[[i]] <- structure("", sticon = "file", stdisabled = FALSE, stopened=FALSE)
        }
        else {
          # call the function recursively
          tree[[i]] <- gettree(subdata[,-1])
        }
      }
    }
  }

  # Change icon of last columns as file
  if ((class(a) == "factor" || length(a) == 1) || (class(a) == "character")) {
    for (i in a) {
      tree[[i]] = structure("", sticon = "glyphicon glyphicon-plus", stdisabled = TRUE, stopened=FALSE)
    }
  }

  return(tree)
}

do_unmount_tree <- function(a) {

  for (i in 0:6) {
    tmp <- paste0("(t(sapply(a,function(x) names(x",
                  paste(rep("[[1]]", i), collapse = ""), "))))")
    db.tmp = as.data.frame(eval(parse(text = tmp)))
    if (!exists('db')) {
      db = t(db.tmp)
    } else {
      db = cbind(db,t(db.tmp))
    }
  }
  db = as.data.table(db)
  data = apply(db, 1, paste, collapse = "|")
  data = lapply(data, function(x) sub("\\|NULL.*", "", x))

  data = unlist(data,recursive=F)
  return(data)
}
