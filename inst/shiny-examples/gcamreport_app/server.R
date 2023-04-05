library(usethis)
library(magrittr)
library(shiny)
library(shinyTree)

# Define server ----------------------------------------------------------------

server <- function(input, output) {
  ## -- debug
  # output$res3 <- renderPrint(shinyTree::get_selected(input$tree_regions, format = 'slices'))

  output$res3 <- renderPrint({
  sel_tree_reg = shinyTree::get_selected(input$tree_regions, format = 'slices')
  reg = do_unmount_tree(sel_tree_reg, 'regions')
  # print(reg)
  # save(reg, file = file.path('C:\\Users\\claudia.rodes\\Documents\\IAM_COMPACT\\gcamreport\\reg.RData'))
  #
  # print(reg)
  })

  # ## -- select all/none variables
  # observeEvent(input$select_all_variables, {
  #   print(input$tree_variables)
  #   updateTreeInput(inputId = "tree_variables", selected = c("Primary Energy",c("Agricultural Demand",
  #            "Capacity Additions","Capacity","Capital Cost","Carbon Sequestration","Concentration",
  #            "Emissions","Energy Service","Final Energy","Forcing","GDP","Investment","Land Cover",
  #            "Population","Price","Production","Temperature","Trade","Secondary Energy")))
  #   # updateTreeInput(inputId = "tree_variables", selected = c("Primary Energy"))
  # })
  # observeEvent(input$select_none_variables, {
  #   updateTreeInput(inputId = "tree_variables", selected = character(0))
  # })
  #
  #
  # ## -- select all/none regions
  # observeEvent(input$select_all_regions, {
  #   updateTreeInput(inputId = "tree_regions", selected = c(unique(reg_cont$region)))
  # })
  # observeEvent(input$select_none_regions, {
  #   updateTreeInput(inputId = "tree_regions", selected = character(0))
  # })

  ## -- render regions tree
  output$tree_regions <- shinyTree::renderTree({
    tree_reg
  })
  observeEvent(input$tree_regions, {
    updateTreeInput(session = getDefaultReactiveDomain(), "tree_regions", input$tree_regions)
    tree_reg <<- input$tree_regions
  })
  observeEvent(input$sidebarItemExpanded, {
    if (input$sidebarItemExpanded == "Regions") {
      print('rerender reg tree')
      output$tree_regions <- shinyTree::renderTree({
        tree_reg
      })
    }
  })


  ## -- render variables tree
  output$tree_variables <- shinyTree::renderTree({
    tree_vars
  })
  observeEvent(input$tree_variables, {
    updateTreeInput(session = getDefaultReactiveDomain(), "tree_variables", input$tree_variables)
    tree_vars <<- input$tree_variables
  })
  observeEvent(input$sidebarItemExpanded, {
    if (input$sidebarItemExpanded == "Variables") {
      print('rerender vars tree')
      output$tree_variables <- shinyTree::renderTree({
        tree_vars
      })
    }
  })


  # observeEvent(input, {
    output$datatable <- DT::renderDataTable({
      sel_reg = shinyTree::get_selected(input$tree_regions, format = 'slices')
      sel_vars = shinyTree::get_selected(input$tree_variables, format = 'slices')
      if (length(sel_reg) == 0) {
        print('display basic tree')
        DT::datatable(data = do_data_sample(sdata,
                                            input$selected_scen,input$selected_years,
                                            input$selected_cols,unique(cols$col1),
                                            reg_cont$region, first_display = TRUE),
                      options = list(pageLength = 10, scrollX = TRUE),
                      rownames = FALSE)
      } else {
        print('display chosen tree')
        DT::datatable(data = do_data_sample(sdata,
                                            input$selected_scen,input$selected_years,
                                            input$selected_cols,sel_vars,
                                            sel_reg),
                      options = list(pageLength = 10, scrollX = TRUE),
                      rownames = FALSE)
      }
    })

  # })

  ## -- data table
  # output$datatable <- DT::renderDataTable({
  #   print(paste0('my tree = ',shinyTree::get_selected(input$tree_regions, format = 'slices')))
  #   DT::datatable(data = do_data_sample(sdata,
  #                                       input$selected_scen,input$selected_years,
  #                                       input$selected_cols,input$tree_variables,
  #                                       tree_reg),
  #                 options = list(pageLength = 10, scrollX = TRUE),
  #                 rownames = FALSE)
  # })


  # ## -- plot
  # observe({
  #   if (input$graph_grouping == 'Grouped'){
  #     # display one single plot with all selected variables
  #
  #     # check that only variables from the same family are selected, and that at least
  #     # one scenario, one region, and one year are selected
  #     check_vars = sub("\\|.*", "", stringr::str_extract(input$tree_variables, "(.*?)(\\||$)"))
  #     scen_reg_year_ok = TRUE
  #     error_message = c()
  #
  #     if (length(unique(input$selected_scen)) < 1) {
  #       error_message <- c(error_message,"ERROR: Select at least one scenario please.")
  #       scen_reg_year_ok = FALSE
  #     }
  #     if (length(unique(input$selected_years)) < 1) {
  #       error_message <- c(error_message,"ERROR: Select at least one year please.")
  #       scen_reg_year_ok = FALSE
  #     }
  #     if (length(unique(input$tree_regions)) < 1) {
  #       error_message <- c(error_message,"ERROR: Select at least one region please.")
  #       scen_reg_year_ok = FALSE
  #     }
  #
  #     if (scen_reg_year_ok && length(unique(check_vars)) == 1) {
  #       # insert the right number of plot output objects into the web page
  #       output$plots <- renderUI({
  #         plot_output_list <- lapply(1:1, function(i) {
  #           plotname <- paste("plot", i, sep="")
  #           tagList(
  #             plotOutput(plotname, height = 450, width = 1000),
  #             downloadButton(paste0("download", i), label = "Download")
  #           )
  #         })
  #
  #         # Convert the list to a tagList - this is necessary for the list of items
  #         # to display properly.
  #         do.call(tagList, plot_output_list)
  #       })
  #
  #       # do plot
  #       data_sample = do_data_sample(sdata,
  #                                    input$selected_scen,input$selected_years,
  #                                    input$selected_cols,input$tree_variables,
  #                                    input$tree_regions)
  #       data_sample = tidyr::pivot_longer(data_sample, cols = 6:ncol(data_sample), names_to = 'year', values_to = 'values') %>%
  #         dplyr::mutate(values = as.numeric(as.character(values))) %>%
  #         dplyr::mutate(year = as.numeric(as.character(year)))
  #
  #       assign(paste0('fig_',unique(data_sample$Variable)[1]),
  #              ggplot2::ggplot(data = data_sample, ggplot2::aes(x = year, y = values, color = Scenario, linetype = Variable, group = interaction(Scenario,Region,Variable))) +
  #                ggplot2::geom_point(ggplot2::aes(shape = Region)) +
  #                ggplot2::geom_line() +
  #                ggplot2::scale_shape_manual('Region', values = rep(c(1:25), times = ceiling(length(unique(data_sample$Region))/6))) +
  #                ggplot2::scale_linetype_manual('Variables', values = rep(c(1:9), times = ceiling(length(unique(data_sample$Variable))/9))) +
  #                ggplot2::scale_color_manual('Scenario', values = rainbow(length(unique(data_sample$Scenario)))) +
  #                ggplot2::labs(title = paste0('Evolution of ', unique(data_sample$Variable)), y = unique(data_sample$Unit), x = 'Year') +
  #                ggplot2::guides(color = ggplot2::guide_legend(ncol = floor(length(unique(data_sample$Scenario))/6))) +
  #                ggplot2::guides(shape = ggplot2::guide_legend(ncol = floor(length(unique(data_sample$Region))/7))) +
  #                ggplot2::guides(linetype = ggplot2::guide_legend(ncol = floor(length(unique(data_sample$Variable))/8))) +
  #                ggplot2::theme_bw() +
  #                ggplot2::theme(legend.text = ggplot2::element_text(size = 8),legend.title = ggplot2::element_text(size = 10),
  #                               legend.key.size = ggplot2::unit(0.5, "cm"))
  #       )
  #
  #       local({
  #         my_i <- 1
  #         plotname <- paste("plot", my_i, sep="")
  #
  #         # display plot
  #         output[[plotname]] <- renderPlot({
  #           get(paste0('fig_',unique(data_sample$Variable)[1]))
  #         })
  #         # display download button
  #         output[[paste0("download", my_i)]] <- downloadHandler(
  #           filename = function() { paste0('fig_',unique(data_sample$Variable)[1],'.png') },
  #           content = function(file) {
  #             assign(paste0('fig_',unique(data_sample$Variable)[1]),
  #                    get(paste0('fig_',unique(data_sample$Variable)[1])) +
  #                      ggplot2::theme(legend.key.size = ggplot2::unit(0.1, "cm")))
  #
  #             # compute width
  #             w = 5*(max(floor(length(unique(data_sample$Scenario))/6),floor(length(unique(data_sample$Region))/7),floor(length(unique(data_sample$Variable))/8))-1)
  #             ggplot2::ggsave(file, plot = get(paste0('fig_',unique(data_sample$Variable)[1])), device = "png",
  #                             height = 10, width = 20+w, units = 'cm', limitsize = FALSE)
  #           })
  #       })
  #     } else if (length(unique(check_vars)) < 1) {
  #       error_message <- c(error_message,"ERROR: Select at least one variable please.")
  #       scen_reg_year_ok = FALSE
  #     } else if (length(unique(check_vars)) > 1) {
  #       error_message <- c(error_message,"ERROR: Select only variables from the same group please.")
  #       scen_reg_year_ok = FALSE
  #     }
  #
  #     if (!scen_reg_year_ok) {
  #       output$plots <- renderUI({
  #         HTML(paste(error_message, collapse = '<br/>'))
  #       })
  #     }
  #   }
  #   else if (input$graph_grouping == 'Ungrouped') {
  #     # display one plot for each variable
  #
  #     # insert the right number of plot output objects into the web page
  #     output$plots <- renderUI({
  #       plot_output_list <- lapply(1:length(input$tree_variables), function(i) {
  #         plotname <- paste("plot", i, sep="")
  #         tagList(
  #           plotOutput(plotname, height = 400, width = 1000),
  #           downloadButton(paste0("download", i), label = "Download"),
  #           br(),br(),br()
  #         )
  #       })
  #
  #       # Convert the list to a tagList - this is necessary for the list of items
  #       # to display properly.
  #       do.call(tagList, plot_output_list)
  #     })
  #
  #
  #     n = length(input$tree_variables)
  #     for (i in 1:n) {
  #       # Need local so that each item gets its own number. Without it, the value
  #       # of i in the renderPlot() will be the same across all instances, because
  #       # of when the expression is evaluated.
  #       local({
  #         my_i <- i
  #         plotname <- paste("plot", my_i, sep="")
  #
  #         # create plot
  #         data_sample = do_data_sample(sdata,
  #                                      input$selected_scen,input$selected_years,
  #                                      input$selected_cols,input$tree_variables[my_i],
  #                                      input$tree_regions)
  #         data_sample = tidyr::pivot_longer(data_sample, cols = 6:ncol(data_sample), names_to = 'year', values_to = 'values') %>%
  #           dplyr::mutate(values = as.numeric(as.character(values))) %>%
  #           dplyr::mutate(year = as.numeric(as.character(year)))
  #
  #         assign(paste0('fig_',unique(data_sample$Variable)[1]),
  #                ggplot2::ggplot(data = data_sample, ggplot2::aes(x = year, y = values, color = Scenario, linetype = Variable, group = interaction(Scenario,Region,Variable))) +
  #                  ggplot2::geom_point(ggplot2::aes(shape = Region)) +
  #                  ggplot2::geom_line() +
  #                  ggplot2::guides(linetype = 'none') +
  #                  ggplot2::scale_shape_manual('Region', values = rep(c(1:25), times = ceiling(length(unique(data_sample$Variable))/6))) +
  #                  ggplot2::scale_color_manual('Scenario', values = rainbow(length(unique(data_sample$Scenario)))) +
  #                  ggplot2::labs(title = paste0('Evolution of ', unique(data_sample$Variable)), y = unique(data_sample$Unit), x = 'Year') +
  #                  ggplot2::guides(color = ggplot2::guide_legend(ncol = floor(length(unique(data_sample$Scenario))/6))) +
  #                  ggplot2::guides(shape = ggplot2::guide_legend(ncol = floor(length(unique(data_sample$Region))/7))) +
  #                  ggplot2::guides(linetype = ggplot2::guide_legend(ncol = floor(length(unique(data_sample$Variable))/8))) +
  #                  ggplot2::theme_bw() +
  #                  ggplot2::theme(legend.text = ggplot2::element_text(size = 8),legend.title = ggplot2::element_text(size = 10),
  #                                 legend.key.size = ggplot2::unit(0.7, "cm"))
  #         )
  #
  #         # display plot
  #         output[[plotname]] <- renderPlot({
  #           get(paste0('fig_',unique(data_sample$Variable)[1]))
  #         })
  #
  #         # display download button
  #         output[[paste0("download", my_i)]] <- downloadHandler(
  #           filename = function() { paste0('fig_',unique(data_sample$Variable)[1],'.png') },
  #           content = function(file) {
  #             assign(paste0('fig_',unique(data_sample$Variable)[1]),
  #                    get(paste0('fig_',unique(data_sample$Variable)[1])) +
  #                      ggplot2::theme(legend.key.size = ggplot2::unit(0.25, "cm")))
  #
  #             # compute width
  #             w = 5*(max(floor(length(unique(data_sample$Scenario))/6),floor(length(unique(data_sample$Region))/7),floor(length(unique(data_sample$Variable))/8))-1)
  #             ggplot2::ggsave(file, plot = get(paste0('fig_',unique(data_sample$Variable)[1])), device = "png",
  #                             height = 10, width = 20+w, units = 'cm', limitsize = FALSE)
  #           })
  #       })
  #     }
  #   }
  # })


  ## -- download button
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('data-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(do_data_sample(sdata,
                               input$selected_scen,input$selected_years,
                               input$selected_cols,input$tree_variables,
                               input$tree_regions),
                con)
    }
  )

}
