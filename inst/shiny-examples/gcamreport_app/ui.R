library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinyTree)
library(magrittr)
library(shinyjs)
library(dashboardthemes)

# Define some variables --------------------------------------------------------

reset_first_load()

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
      ),
      menuItem(
        actionBttn(
          inputId = "select_all_scen",
          label = "Select all",
          style = "minimal",
          size = 'xs'
        )
      ),
      menuItem(
        actionBttn(
          inputId = "select_none_scen",
          label = "Select none",
          style = "minimal",
          size = 'xs'
        )
      ),
      br()
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
      ),
      menuItem(
        actionBttn(
          inputId = "select_all_years",
          label = "Select all",
          style = "minimal",
          size = 'xs'
        )
      ),
      menuItem(
        actionBttn(
          inputId = "select_none_years",
          label = "Select none",
          style = "minimal",
          size = 'xs'
        )
      ),
      br()
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
          choices = c('Model', 'Scenario', 'Region',
                      'Variable', 'Unit'),
          selected = c('Model', 'Scenario', 'Region',
                       'Variable', 'Unit')
        )
      ),
      menuItem(
        actionBttn(
          inputId = "select_all_cols",
          label = "Select all",
          style = "minimal",
          size = 'xs'
        )
      ),
      menuItem(
        actionBttn(
          inputId = "select_none_cols",
          label = "Select none",
          style = "minimal",
          size = 'xs'
        )
      ),
      br()
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
                 shiny::dataTableOutput(outputId = "datatable")
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

