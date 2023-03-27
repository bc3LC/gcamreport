# User friendly interface to interact with.

library(usethis)
library(magrittr)
library(shiny)
library(shinyTree)

# Define app functions ---------------------------------------------------------

source(paste0(here::here(),'/R/app_functions.R'))


# Load data --------------------------------------------------------------------

if (!exists('final_data')) {
  gcamreport::load_project('gas_fin_updated')
  gcamreport::read_queries(final_db_year = 2050)
}

if (!exists('sdata')) {
  # split variables into different subgroups
  sdata = final_data %>%
    tidyr::separate(Variable, into = c('col1','col2','col3','col4','col5','col6','col7'), sep = "([\\|])", extra = 'merge', remove = FALSE)

  # create vector of available years
  available_years = as.numeric(names(sdata)[13:length(names(sdata))])

  # develop a nested list for the variables
  cols = unique(sdata[, grepl('col', names(sdata))])
  tree_vars <- do_mount_tree(cols,names(cols),selec=TRUE)

  # reg_cont <<- read.csv(paste0(map_dir, "/regions_continents_map.csv"), header = TRUE, sep = ",", encoding = "UTF-8")
  reg_cont <<- read.csv(paste0(map_dir, "/regions_continents_map.csv"), skip = 1)
  tree_reg <- do_mount_tree(reg_cont,names(reg_cont),selec=TRUE)
}

# Define UI --------------------------------------------------------------------

ui <- fluidPage(
  titlePanel("GCAMREPORT user interface",
             windowTitle = 'gcamreport'),

  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(inputId = "selected_scen",
                         label = "Select scenarios:",
                         choices = unique(sdata$Scenario),
                         selected = unique(sdata$Scenario)),

      br(),
      tags$div(style="font-weight: bold", checked=NA,
               tags$p("Select variables")
      ),
      shinyTree("tree_variables",
                 checkbox = TRUE,
                 search = TRUE,
                 searchtime = 500),
      actionLink("select_all_variables","Select All"),
      actionLink("select_none_variables","Select None"),

      br(),
      tags$div(style="font-weight: bold", checked=NA,
               tags$p("Select regions")
      ),
      # tags$div(class="header", checked=NA,
      #          tags$p("Select regions")
      # ),

      shinyTree("tree_regions",
                 checkbox = TRUE),
      actionLink("select_all_regions","Select All"),
      actionLink("select_none_regions","Select None"),

      checkboxGroupInput(inputId = "selected_years",
                         label = "Select years:",
                         choices = available_years,
                         selected = available_years),

      checkboxGroupInput(inputId = "selected_cols",
                         label = "Select columns:",
                         choices = c('Model',
                                     'Scenario',
                                     'Region',
                                     'Variable',
                                     'Unit'),
                         selected = c('Model',
                                      'Scenario',
                                      'Region',
                                      'Variable',
                                      'Unit')),
    ),

    mainPanel(
      tabsetPanel(
        type = "tabs",
        tabPanel("Data",
                 # textOutput(outputId = "debug"),
                 # textOutput(outputId = "debug2"),
                 DT::dataTableOutput(outputId = "datatable"),
                 downloadButton("download_data", "Download data")
          ),
        tabPanel("Plot",
                 plotOutput("plot"))
      )
    )
  )
)

# Define server ----------------------------------------------------------------

server <- function(input, output, session) {

  # Debugging
  #########
  # output$debug <- renderPrint({
  #   sel_tree = shinyTree::get_selected(input$tree, format = 'slices')
  #   save(sel_tree, file = file.path('C:\\Users\\claudia.rodes\\Documents\\IAM_COMPACT\\gcamreport\\tt.RData'))
  #
  #   print(sel_tree)
  # })
  # output$debug2 <- renderPrint({
  #   sel_tree_reg = shinyTree::get_selected(input$tree_regions, format = 'slices')
  #   sel_tree_reg = do_unmount_tree(sel_tree_reg, 'regions')
  #   save(sel_tree_reg, file = file.path('C:\\Users\\claudia.rodes\\Documents\\IAM_COMPACT\\gcamreport\\reg.RData'))
  #
  #   print(sel_tree_reg)
  # })
  ############

  # Select all/none variables
  observe({
    if(input$select_all_variables == 0) return(NULL)
    else {
      updateTree(session, treeId = "tree_variables", data = treeDataVar_sel())
      # data_sample()
    }
  })
  observe({
    if(input$select_none_variables == 0) return(NULL)
    else {
      updateTree(session, treeId = "tree_variables", data = treeDataVar_unsel())
      # data_sample()
    }
  })

  treeDataVar_sel <- reactive({
    tree_vars <- do_mount_tree(cols ,names(cols), selec = TRUE)
  })
  treeDataVar_unsel <- reactive({
    tree_vars <- do_mount_tree(cols ,names(cols), selec = FALSE)
  })

  # Select all/none regions
  observe({
    if(input$select_all_regions == 0) return(NULL)
    else {
      updateTree(session, treeId = "tree_regions", data = treeDataReg_sel())
      # data_sample()
    }
  })
  observe({
    if(input$select_none_regions == 0) return(NULL)
    else {
      updateTree(session, treeId = "tree_regions", data = treeDataReg_unsel())
      # data_sample()
    }
  })

  treeDataReg_sel <- reactive({
    tree_reg <- do_mount_tree(reg_cont ,names(reg_cont), selec = TRUE)
  })
  treeDataReg_unsel <- reactive({
    tree_reg <- do_mount_tree(reg_cont ,names(reg_cont), selec = FALSE)
  })

  # observeEvent(input$updateTree,{
  #   updateTree(session, treeId = "tree_variables", data = treeData())
  # })


  # Variables tree
  output$tree_variables <- shinyTree::renderTree({
    tree_vars
  })

  # Regions tree
  output$tree_regions <- shinyTree::renderTree({
    tree_reg
  })

  # # Subset selected by the user
  # data_sample <- observe({
  #   do_data_sample(sdata,input$selected_scen,input$selected_years,input$selected_cols,
  #                  shinyTree::get_selected(input$tree_variables, format = 'slices'),
  #                  shinyTree::get_selected(input$tree_regions, format = 'slices'))
  # })

  # Plot
  output$plot <- renderPlot({

    sel_tree_vars = shinyTree::get_selected(input$tree_variables, format = 'slices')
    sel_tree_reg = shinyTree::get_selected(input$tree_regions, format = 'slices')
    data_sample = do_data_sample(sdata,input$selected_scen,input$selected_years,input$selected_cols,
                                 sel_tree_vars,sel_tree_reg)
    data_sample = tidyr::pivot_longer(data_sample, cols = 6:ncol(data_sample), names_to = 'year', values_to = 'values') %>%
      dplyr::mutate(values = as.numeric(as.character(values)))
    save(data_sample, file = file.path('C:\\Users\\claudia.rodes\\Documents\\IAM_COMPACT\\gcamreport\\data_sample.RData'))
    data_sample = data_sample %>%
      dplyr::mutate(year = as.numeric(as.character(year))) %>%
      dplyr::mutate(values = as.numeric(as.character(values)))
    ggplot2::ggplot(data = data_sample, ggplot2::aes(x = year, y = values, color = Scenario, linetype = Variable, group = interaction(Scenario,Region,Variable))) +
      ggplot2::geom_point(ggplot2::aes(shape = Region)) +
      ggplot2::geom_line() +
      ggplot2::guides(color = ggplot2::guide_legend(title = 'Scenario')) +
      ggplot2::labs(title = paste0('Evolution of ', unique(data_sample$Variable)), y = unique(data_sample$Unit), x = 'Year')
  })

  # Data table
  output$datatable <- DT::renderDataTable({
    sel_tree_vars = shinyTree::get_selected(input$tree_variables, format = 'slices')
    sel_tree_reg = shinyTree::get_selected(input$tree_regions, format = 'slices')
    data_sample = do_data_sample(sdata,input$selected_scen,input$selected_years,input$selected_cols,
                                 sel_tree_vars,sel_tree_reg)

    DT::datatable(data = data_sample,
                  options = list(pageLength = 10),
                  rownames = FALSE)
  })

  # # Download file
  # output$download_data <- downloadHandler(
  #   filename = function() {
  #     paste0("gcamreport.csv")
  #   },
  #   content = function(file) {
  #     sel_tree_vars = shinyTree::get_selected(input$tree_variables, format = 'slices')
  #     sel_tree_reg = shinyTree::get_selected(input$tree_regions, format = 'slices')
  #     data_sample = do_data_sample(sdata,input$selected_scen,input$selected_years,input$selected_cols,
  #                                  sel_tree_vars,sel_tree_reg)
  #     write.csv(data_sample, file)
  #   }
  # )

}

# Create the Shiny app object --------------------------------------------------

shinyApp(ui = ui, server = server)
