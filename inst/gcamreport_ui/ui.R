library(magrittr)
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinyTree)
library(shinyjs)

# Define some variables --------------------------------------------------------

reset_first_load()

# Define UI --------------------------------------------------------------------

ui <- dashboardPage(
  # header
  dashboardHeader(title = "gcamreport"),

  # sidebar
  dashboardSidebar(sidebarMenu(
    shinyjs::useShinyjs(),

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
        ) %>%
          tagAppendAttributes(class = 'names_split')
      ),
      # select none/all buttons
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
                  checkbox = TRUE,
                  types = "{ 'basic': {'a_attr' : { 'style' : 'background-color: #2c3b41; color: inherit; cursor: inherit; pointer-events: inherit; opacity: inherit'}}}")
      ),
      # select none/all buttons
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
                  checkbox = TRUE,
                  types = "{ 'basic': {'a_attr' : { 'style' : 'background-color: #2c3b41; color: inherit; cursor: inherit; pointer-events: inherit; opacity: inherit'}},
                  'dis': {'a_attr' : { 'style' : 'opacity: 0.3; cursor: not-allowed; pointer-events: none' } }}")
      ),
      # select none/all buttons
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
        ) %>%
          tagAppendAttributes(class = 'names_split')
      ),
      # select none/all buttons
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
      id = 'columns_id',
      text = "Columns",
      icon = NULL,
      startExpanded = FALSE,
      class = 'enabled_cols',
      menuItem(
        awesomeCheckboxGroup(
          inputId = "selected_cols",
          label = "Select columns",
          choices = c('Model', 'Scenario', 'Region',
                      'Variable', 'Unit'),
          selected = c('Model', 'Scenario', 'Region',
                       'Variable', 'Unit')
        ) %>%
          tagAppendAttributes(class = 'names_split')
      ),
      # select none/all buttons
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
    downloadButton(
      outputId = "downloadData",
      label = "Download",
      color = "default",
      size = 'sm',
      class = 'dwnbutton'
    )

  )),

  # body
  dashboardBody(
    # css file
    includeCSS(paste0(here::here(),"/R/www/style.css")),

    # dashboard items
    fluidRow(
      tabBox(
        width = 12,
        id = "tab_box",
        tabPanel("Data",
                 shiny::dataTableOutput(outputId = "datatable")
        ),
        tabPanel("Plot",
                 # buttons to chose how to display the regions and variables
                 fluidRow(style='padding-left:25px;',
                          column(width = 5,
                                 radioGroupButtons(
                                   inputId = "vars_grouping",
                                   label = "Choose how the variables and regions should be displayed: ",
                                   choices = c("Grouped Variables", "Ungrouped Variables"),
                                   direction = "vertical"
                                 )),
                          column(width = 5,
                                 radioGroupButtons(
                                   inputId = "reg_grouping",
                                   label = "  ",
                                   choices = c("Grouped Regions", "Ungrouped Regions"),
                                   direction = "vertical"
                                 ))
                 ),
                 br(),
                 # dynamic UI for the plots
                 uiOutput("plots"))
      )
    )
  )
)
