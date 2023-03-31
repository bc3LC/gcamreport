library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinyTree)
library(magrittr)
library(shinyjs)
library(dashboardthemes)

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
          label = "Select scenarios to account in the table",
          choices = unique(sdata$Scenario),
          selected = unique(sdata$Scenario)
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
          label = "Select columns to appear in the table",
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
          selected = "Argentina",
          returnValue = "id",
          closeDepth = 0
        )
      )
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
      )
    ),


    ## -- Years
    menuItem(
      text = "Years",
      icon = NULL,
      startExpanded = FALSE,
      menuItem(
        awesomeCheckboxGroup(
          inputId = "selected_years",
          label = "Select years to appear in the table",
          choices = available_years,
          selected = available_years
        )
      )
    )
  )),


  dashboardBody(
    fluidRow(
      tabBox(
        width = 12,
        id = "tab_box",
        tabPanel("Data",
                 DT::dataTableOutput(outputId = "datatable")
                 ),
        tabPanel("Plot", "TODO")

        # title = "Data",
        #   #   "Box content here", br(), "More box content",
        #   # verbatimTextOutput("res3"),
        # DT::dataTableOutput(outputId = "datatable")
        #   #   # downloadButton("download_data", "Download data")
        #   # )
      )
    )
  )
)

server <- function(input, output) {
 # useShinyjs()
  ## -- debug
  # output$res3 <- renderPrint(input$tree_variables)


  ## -- function: select variables chosen by the user

  # doo_data_sample <- reactive({
  #   do_data_sample(sdata,input$selected_scen,input$selected_years,input$selected_cols,
  #                  shinyTree::get_selected(input$tree_variables, format = 'slices'),
  #                  shinyTree::get_selected(input$tree_regions, format = 'slices'))
  # })



  ## -- data table
  output$datatable <- DT::renderDataTable({
    reg <- c(unlist(lapply(input$tree_regions, function(x) na.omit(strsplit(x, "\\|")[[1]][2]))))

    data_sample = sdata %>%
      dplyr::filter(Scenario %in% input$selected_scen) %>%
      dplyr::filter(Variable %in% input$tree_variables) %>%
      dplyr::filter(Region %in% reg) %>%
      dplyr::select(c(input$selected_cols, input$selected_years)) %>%
      data.table::as.data.table()

    # DT::datatable(data = doo_data_sample(),
    DT::datatable(data = data_sample,
                  options = list(pageLength = 10, scrollX = TRUE),
                  rownames = FALSE)

  })

}

shinyApp(ui = ui, server = server)
