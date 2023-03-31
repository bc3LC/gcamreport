library(usethis)
library(magrittr)
library(shiny)
library(shinyTree)

# server <- shinyServer(function(input, output, session) {
#   addClass(selector = "body", class = "sidebar-collapse")
# })
#

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
  # sel_tree_reg = shinyTree::get_selected(input$tree_regions, format = 'slices')
  # sel_tree_reg = do_unmount_tree(sel_tree_reg, 'regions')
  # save(sel_tree_reg, file = file.path('C:\\Users\\claudia.rodes\\Documents\\IAM_COMPACT\\gcamreport\\reg.RData'))

  # print(unique(data_sample$Regions))
  # })
  ############

  # Select all/none variables
  treeDataVar_sel <- reactive({
    tree_vars <- do_mount_tree(cols, names(cols), selec = TRUE)
  })
  treeDataVar_unsel <- reactive({
    tree_vars <- do_mount_tree(cols, names(cols), selec = FALSE)
  })
  observeEvent(input$select_all_variables, {
    updateTree(session, treeId = "tree_variables", data = treeDataVar_sel())
  })
  observeEvent(input$select_none_variables, {
    updateTree(session, treeId = "tree_variables", data = treeDataVar_unsel())
  })


  # Select all/none regions
  treeDataReg_sel <- reactive({
    tree_reg <- do_mount_tree(reg_cont, names(reg_cont), selec = TRUE)
  })
  treeDataReg_unsel <- reactive({
    tree_reg <- do_mount_tree(reg_cont, names(reg_cont), selec = FALSE)

  })
  observeEvent(input$select_all_regions, {
    updateTree(session, treeId = "tree_regions", data = treeDataReg_sel())
  })
  observeEvent(input$select_none_regions, {
    updateTree(session, treeId = "tree_regions", data = treeDataReg_unsel())
  })


  # Variables tree
  output$tree_variables <- shinyTree::renderTree({
    tree_vars
  })
  # Regions tree
  output$tree_regions <- shinyTree::renderTree({
    tree_reg
  })


  # Subset selected by the user
  doo_data_sample <- reactive({
    do_data_sample(sdata,input$selected_scen,input$selected_years,input$selected_cols,
                   shinyTree::get_selected(input$tree_variables, format = 'slices'),
                   shinyTree::get_selected(input$tree_regions, format = 'slices'))
  })

  # Plot
  output$plot <- renderPlot({
    data_sample = doo_data_sample()
    data_sample = tidyr::pivot_longer(data_sample, cols = 6:ncol(data_sample), names_to = 'year', values_to = 'values') %>%
      dplyr::mutate(values = as.numeric(as.character(values))) %>%
      dplyr::mutate(year = as.numeric(as.character(year)))

    ggplot2::ggplot(data = data_sample, ggplot2::aes(x = year, y = values, color = Scenario, linetype = Variable, group = interaction(Scenario,Region,Variable))) +
      ggplot2::geom_point(ggplot2::aes(shape = Region)) +
      ggplot2::geom_line() +
      ggplot2::scale_color_manual('Scenario', values = viridis::magma(length(unique(data_sample$Scenario)))) +
      ggplot2::scale_linetype_manual('Variables', values = rep(c(1:9), times = ceiling(length(unique(data_sample$Variable))/9))) +
      ggplot2::labs(title = paste0('Evolution of ', unique(data_sample$Variable)), y = unique(data_sample$Unit), x = 'Year')
  })

  # Data table
  output$datatable <- DT::renderDataTable({
    DT::datatable(data = doo_data_sample(),
                  options = list(pageLength = 10),
                  rownames = FALSE)
  })

  # Download file
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("gcamreport.csv")
    },
    content = function(file) {
      write.csv(doo_data_sample(), file)
    }
  )

}

