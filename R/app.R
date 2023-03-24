# User friendly interface to interact with.

library(usethis)
library(magrittr)
library(shinyTree)

# Load data --------------------------------------------------------------------

if (!exists('final_data')) {
  gcamreport::load_project('gas_fin_updated')
  gcamreport::read_queries(final_db_year = 2050)
}

if (!exists('sdata')) {
  # split variables into different subgroups
  sdata = final_data %>%
    tidyr::separate(Variable, into = c('col1','col2','col3','col4','col5','col6','col7'), sep = "([\\|])", extra = 'merge', remove = FALSE)

  available_years = as.numeric(names(sdata)[13:length(names(sdata))])

  cols = unique(sdata[, grepl('col', names(sdata))])

  tree_cols <- do_mount_tree(cols)


}

# Define app functions ---------------------------------------------------------


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

      # radioButtons(inputId = "select_type_var",
      #              label = "Select variables:",
      #              choices = c('By type',
      #                          'See all options'),
      #              selected = 'By type'),

      # conditionalPanel(condition = "input.select_type_var == 'See all options'",
      #                  checkboxGroupInput(inputId = "selected_col0",
      #                                     label = "Select variables:",
      #                                     choices = unique(sdata$Variable),
      #                                     selected = unique(sdata$Variable))
      #                  ),

      # conditionalPanel(condition = "input.select_type_var == 'By type'",
      #                  shinyTree("tree_cols",
      #                            checkbox = TRUE,
      #                            search = TRUE)
      #                  ),
      shinyTree("tree",
                 checkbox = TRUE,
                 search = TRUE,
                 searchtime = 1000),

      # conditionalPanel(condition = "input.select_type_var == 'By type'",
      #                  shinyTree("tree",
      #                            checkbox = TRUE,
      #                            search = TRUE,
      #                            searchtime = 1000)
      #                  ),

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
      # textOutput(outputId = "debug"),
      # textOutput(outputId = "debug2"),
      DT::dataTableOutput(outputId = "datatable"),
      downloadButton("download_data", "Download data")
    )
  )
)

# Define server ----------------------------------------------------------------

server <- function(input, output, session) {

  # Text to debug
  output$debug <- renderPrint({
    # print(as.matrix.data.frame(a))
    tree_data = shinyTree::get_selected(input$tree, format = 'slices')
    print(do_unmount_tree(tree_data))
    # print(str(tree_data[[1]][1]))
    # print(as.matrix.data.frame(as.data.frame(do.call(cbind,tree_data))))


    # dd  <-  as.data.frame(matrix(unlist(tree_data), nrow=length(unlist(tree_data[3]))))
    # print(dd)
  })
  output$debug2 <- renderPrint({
    sel_tree = shinyTree::get_selected(input$tree, format = 'slices')
    sel_tree = do_unmount_tree(sel_tree)

    print(sel_tree)
  })


  output$tree <- shinyTree::renderTree({
    tree_cols
  })


  # Data table
  output$datatable <- DT::renderDataTable({
    sel_tree = shinyTree::get_selected(input$tree, format = 'slices')
    sel_tree = do_unmount_tree(sel_tree)
    data_sample = do_data_sample(sdata,input$selected_scen,input$selected_years,input$selected_cols,sel_tree)
    DT::datatable(data = data_sample,
                  options = list(pageLength = 10),
                  rownames = FALSE)
  })

  # Download file
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("gcamreport.csv")
    },
    content = function(file) {
      data_sample = do_data_sample(input$selected_scen,input$selected_years,input$selected_cols)
      write.csv(data_sample, file)
    }
  )

}

# Create the Shiny app object --------------------------------------------------

shinyApp(ui = ui, server = server)
