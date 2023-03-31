library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinyTree)
library(magrittr)
library(shinyjs)

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
        # shinyTree("tree_regions",
        #           checkbox = TRUE)
        # actionLink("select_all_regions","Select All"),
        # actionLink("select_none_regions","Select None")
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
          # choices = create_tree(cols),
          # choices = shinyWidgets::create_tree(cols,
          #                       levels = c('col1','col2'),
          #                       levels_id = c('code_col1','code_col2')),
          choices = shinyWidgets::create_tree(cols,
                                levels = c('col1','col2','col3','col4','col5','col6','col7'),
                                levels_id = c('code_col1','code_col2','code_col3','code_col4','code_col5','code_col6','code_col7')),
          selected = "Agricultural Demand",
          returnValue = "id",
          closeDepth = 0
        )
        # shinyTree("tree_variables",
        #           checkbox = TRUE)
        # actionLink("select_all_variables","Select All"),
        # actionLink("select_none_variables","Select None"),
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


  dashboardBody(fluidRow(
    # box(
    #   title = "Data", status = "warning",
    #   "Box content here", br(), "More box content",
    # verbatimTextOutput("res3"),
      DT::dataTableOutput(outputId = "datatable")
    #   # downloadButton("download_data", "Download data")
    # )
  ))
)

server <- function(input, output) {
 useShinyjs()
  ## -- debug
  # output$res3 <- renderPrint(input$tree_variables)


  # ## -- regions tree
  # output$tree_regions <- shinyTree::renderTree({
  #   tree_reg
  # })
  #
  # ## -- variables tree
  # output$tree_variables <- shinyTree::renderTree({
  #   tree_vars
  # })
  #

  ## -- function: select variables chosen by the user

  # doo_data_sample <- reactive({
  #   do_data_sample(sdata,input$selected_scen,input$selected_years,input$selected_cols,
  #                  shinyTree::get_selected(input$tree_variables, format = 'slices'),
  #                  shinyTree::get_selected(input$tree_regions, format = 'slices'))
  # })



  ## -- data table
  output$datatable <- DT::renderDataTable({
    # sel_varss = shinyTree::get_selected(input$tree_variables, format = 'slices')
    # sel_regg = shinyTree::get_selected(input$tree_regions, format = 'slices')
    #
    # sel_vars = do_unmount_tree(sel_varss, 'variables')
    # sel_reg = do_unmount_tree(sel_regg, 'regions')
    # vars = do_list(do_mount_db(data = input$tree_variables, ref = cols))
    # save(vars, file = file.path('C:\\Users\\claudia.rodes\\Documents\\IAM_COMPACT\\gcamreport\\vars.RData'))

    reg <- c(unlist(lapply(input$tree_regions, function(x) na.omit(strsplit(x, "\\|")[[1]][2]))))
    print(reg)


    data_sample = sdata %>%
      dplyr::filter(Scenario %in% input$selected_scen) %>%
      dplyr::filter(Variable %in% input$tree_variables) %>%
      dplyr::filter(Region %in% reg) %>%
      dplyr::select(c(input$selected_cols, input$selected_years)) %>%
      data.table::as.data.table()

    # DT::datatable(data = doo_data_sample(),
    DT::datatable(data = data_sample,
                  options = list(pageLength = 10),
                  rownames = FALSE)
  })

}

shinyApp(ui = ui, server = server)
