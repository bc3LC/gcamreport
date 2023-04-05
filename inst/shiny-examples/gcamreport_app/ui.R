library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinyTree)
library(magrittr)
library(shinyjs)
library(dashboardthemes)

# Define some variables --------------------------------------------------------

reg_cont <<- read.csv(paste0(here::here(), "/inst/extdata/mappings", "/regions_continents_map.csv"), skip = 1)
tree_reg <<- do_mount_tree(reg_cont,names(reg_cont),selec=TRUE)
cols <<- unique(sdata[, grepl('col', names(sdata))])
tree_vars <<- do_mount_tree(cols,names(cols),selec=TRUE)
firstLoad <<- TRUE
firstReg <<- TRUE
firstVars <<- TRUE
printi <<- 1

# Define UI --------------------------------------------------------------------

ui <- dashboardPage(
  dashboardHeader(title = "gcamreport"),
  dashboardSidebar(sidebarMenu(


    ## -- Scenarios
    menuItem(
      text = "Scenarios",
      icon = NULL,
      startExpanded = FALSE,
      menuItem(
        awesomeCheckboxGroup(
          inputId = "selected_scen",
          label = "Select scenarios",
          choices = unique(sdata$Scenario),
          selected = unique(sdata$Scenario)
        )
      )
    ),


    ## -- Regions
    menuItem(
      text = "Regions",
      icon = NULL,
      startExpanded = FALSE,
      menuItem(
        shinyTree("tree_regions",
                  checkbox = TRUE)
      ),
      menuItem(
        actionBttn(
          inputId = "select_all_regions",
          label = "Select all",
          style = "minimal",
          size = 'xs'
        )
      ),
      menuItem(
        actionBttn(
          inputId = "select_none_regions",
          label = "Select none",
          style = "minimal",
          size = 'xs'
        )
      ),
      br()
    ),


    ## -- Variables
    menuItem(
      text = "Variables",
      icon = NULL,
      startExpanded = FALSE,
      menuItem(
        shinyTree("tree_variables",
                  checkbox = TRUE)
      ),
      menuItem(
        actionBttn(
          inputId = "select_all_variables",
          label = "Select all",
          style = "minimal",
          size = 'xs'
        )
      ),
      menuItem(
        actionBttn(
          inputId = "select_none_variables",
          label = "Select none",
          style = "minimal",
          size = 'xs'
        )
      ),
      br()
    ),


    ## -- Years
    menuItem(
      text = "Years",
      icon = NULL,
      startExpanded = FALSE,
      menuItem(
        awesomeCheckboxGroup(
          inputId = "selected_years",
          label = "Select years",
          choices = available_years,
          selected = available_years
        )
      )
    ),


    ## -- Columns
    menuItem(
      text = "Columns",
      icon = NULL,
      startExpanded = FALSE,
      menuItem(
        awesomeCheckboxGroup(
          inputId = "selected_cols",
          label = "Select columns",
          choices = c('Model',
                      'Scenario',
                      'Region',
                      'Variable',
                      'Unit'),
          selected = c('Model',
                       'Scenario',
                       'Region',
                       'Variable',
                       'Unit')
        )
      )
    ),


    ## -- Download button
    downloadBttn(
      outputId = "downloadData",
      style = "simple",
      color = "default",
      size = 'sm'
    )
  )),


  dashboardBody(
    # includeCSS("C:/Users/claudia.rodes/Documents/IAM_COMPACT/gcamreport/R/www/style.css"),
    fluidRow(
      tabBox(
        width = 12,
        id = "tab_box",
        tabPanel("Data",
                 verbatimTextOutput("res3"),
                 DT::dataTableOutput(outputId = "datatable")
        ),
        tabPanel("Plot",
                 radioGroupButtons(
                   inputId = "graph_grouping",
                   label = "Choose how the graph variables should be displayed: ",
                   choices = c("Grouped", "Ungrouped")
                 ),
                 br(),
                 # dynamic UI for the plots
                 uiOutput("plots"))
      )
    )
  )
)

