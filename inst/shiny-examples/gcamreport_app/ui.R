library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinyTree)
library(magrittr)
library(shinyjs)
library(dashboardthemes)

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
        treeInput(
          inputId = "tree_regions",
          label = "Select regions:",
          choices = shinyWidgets::create_tree(reg_cont,
                                              levels = c('continent','region'),
                                              levels_id = c('code_continent','code_region')),
          selected = "Africa",
          returnValue = "id",
          closeDepth = 0
        )
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
        treeInput(
          inputId = "tree_variables",
          label = "Select variables:",
          choices = shinyWidgets::create_tree(cols,
                                              levels = c('col1','col2','col3','col4','col5','col6','col7'),
                                              levels_id = c('code_col1','code_col2','code_col3','code_col4','code_col5','code_col6','code_col7')),
          selected = "Agricultural Demand",
          returnValue = "id",
          closeDepth = 0
        )
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
    fluidRow(
      tabBox(
        width = 12,
        id = "tab_box",
        tabPanel("Data",
                 # verbatimTextOutput("res3"),
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

