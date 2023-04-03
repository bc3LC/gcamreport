#########################################################################
#                         APP ANCILLARY FUNCTIONS                       #
#########################################################################

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
do_data_sample <- function(sdata,sel_scen,sel_years,sel_cols,sel_vars,sel_reg) {
  reg <- c(unlist(lapply(sel_reg, function(x) na.omit(strsplit(x, "\\|")[[1]][2]))))

  data_sample = sdata %>%
    dplyr::filter(Scenario %in% sel_scen) %>%
    dplyr::filter(Variable %in% sel_vars) %>%
    dplyr::filter(Region %in% reg) %>%
    dplyr::select(c(sel_cols, sel_years)) %>%
    data.table::as.data.table()

  return(data_sample)
}
