library(usethis)
library(magrittr)
library(shiny)
library(shinyTree)

# Define server ----------------------------------------------------------------

server <- function(input, output) {
  ## -- debug
  output$res3 <- renderPrint(input$tree_regions)


  ## -- select all/none variables
  observeEvent(input$select_all_variables, {
    updateTreeInput(inputId = "tree_variables", selected = c(unique(cols[,1])))
  })
  observeEvent(input$select_none_variables, {
    updateTreeInput(inputId = "tree_variables", selected = character(0))
  })


  ## -- select all/none variables
  observeEvent(input$select_all_regions, {
    updateTreeInput(inputId = "tree_regions", selected = c(unique(reg_cont$region)))
  })
  observeEvent(input$select_none_regions, {
    updateTreeInput(inputId = "tree_regions", selected = character(0))
  })


  ## -- data table
  output$datatable <- DT::renderDataTable({
    DT::datatable(data = do_data_sample(sdata,
                                        input$selected_scen,input$selected_years,
                                        input$selected_cols,input$tree_variables,
                                        input$tree_regions),
                  options = list(pageLength = 10, scrollX = TRUE),
                  rownames = FALSE)
  })


  ## -- plot
  observe({
    if (input$graph_grouping == 'Grouped'){
      # display one single plot with all selected variables

      # insert the right number of plot output objects into the web page
      output$plots <- renderUI({
        plot_output_list <- lapply(1:1, function(i) {
          plotname <- paste("plot", i, sep="")
          tagList(
            plotOutput(plotname, height = 400, width = 1000),
            downloadButton(paste0("download", i), label = "Download")
          )
        })

        # Convert the list to a tagList - this is necessary for the list of items
        # to display properly.
        do.call(tagList, plot_output_list)
      })

      data_sample = do_data_sample(sdata,
                                   input$selected_scen,input$selected_years,
                                   input$selected_cols,input$tree_variables,
                                   input$tree_regions)
      data_sample = tidyr::pivot_longer(data_sample, cols = 6:ncol(data_sample), names_to = 'year', values_to = 'values') %>%
        dplyr::mutate(values = as.numeric(as.character(values))) %>%
        dplyr::mutate(year = as.numeric(as.character(year)))
      assign(paste0('fig_',unique(data_sample$Variable)[1]),
             ggplot2::ggplot(data = data_sample, ggplot2::aes(x = year, y = values, color = Scenario, linetype = Variable, group = interaction(Scenario,Region,Variable))) +
               ggplot2::geom_point(ggplot2::aes(shape = Region)) +
               ggplot2::geom_line() +
               ggplot2::scale_shape_manual('Region', values = rep(c(1:25), times = ceiling(length(unique(data_sample$Variable))/6))) +
               ggplot2::scale_linetype_manual('Variables', values = rep(c(1:9), times = ceiling(length(unique(data_sample$Variable))/9))) +
               ggplot2::scale_color_manual('Scenario', values = rainbow(length(unique(data_sample$Scenario)))) +
               ggplot2::labs(title = paste0('Evolution of ', unique(data_sample$Variable)), y = unique(data_sample$Unit), x = 'Year') +
               ggplot2::theme_bw() +
               ggplot2::theme(legend.text = ggplot2::element_text(size = 8),legend.title = ggplot2::element_text(size = 10),
                              legend.key.size = ggplot2::unit(0.5, "cm"))
      )

      local({
        my_i <- 1
        plotname <- paste("plot", my_i, sep="")

        # display plot
        output[[plotname]] <- renderPlot({
          get(paste0('fig_',unique(data_sample$Variable)[1]))
        })
        # display download button
        output[[paste0("download", my_i)]] <- downloadHandler(
          filename = function() { paste0('fig_',unique(data_sample$Variable)[1],'.png') },
          content = function(file) {
            assign(paste0('fig_',unique(data_sample$Variable)[1]),
                   get(paste0('fig_',unique(data_sample$Variable)[1])) +
                     ggplot2::theme(legend.key.size = ggplot2::unit(0.1, "cm")))

            ggplot2::ggsave(file, plot = get(paste0('fig_',unique(data_sample$Variable)[1])), device = "png",
                            height = 10, width = 20, units = 'cm', limitsize = FALSE)
          })
      })
    }
    else if (input$graph_grouping == 'Ungrouped') {
      # display one plot for each variable

      # insert the right number of plot output objects into the web page
      output$plots <- renderUI({
        plot_output_list <- lapply(1:length(input$tree_variables), function(i) {
          plotname <- paste("plot", i, sep="")
          tagList(
            plotOutput(plotname, height = 400, width = 1000),
            downloadButton(paste0("download", i), label = "Download")
          )

        })

        # Convert the list to a tagList - this is necessary for the list of items
        # to display properly.
        do.call(tagList, plot_output_list)
      })


      n = length(input$tree_variables)
      for (i in 1:n) {
        # Need local so that each item gets its own number. Without it, the value
        # of i in the renderPlot() will be the same across all instances, because
        # of when the expression is evaluated.
        local({
          my_i <- i
          plotname <- paste("plot", my_i, sep="")

          # create plot
          data_sample = do_data_sample(sdata,
                                       input$selected_scen,input$selected_years,
                                       input$selected_cols,input$tree_variables[my_i],
                                       input$tree_regions)
          data_sample = tidyr::pivot_longer(data_sample, cols = 6:ncol(data_sample), names_to = 'year', values_to = 'values') %>%
            dplyr::mutate(values = as.numeric(as.character(values))) %>%
            dplyr::mutate(year = as.numeric(as.character(year)))

          assign(paste0('fig_',unique(data_sample$Variable)[1]),
                 ggplot2::ggplot(data = data_sample, ggplot2::aes(x = year, y = values, color = Scenario, linetype = Variable, group = interaction(Scenario,Region,Variable))) +
                   ggplot2::geom_point(ggplot2::aes(shape = Region)) +
                   ggplot2::geom_line() +
                   ggplot2::guides(linetype = 'none') +
                   ggplot2::scale_shape_manual('Region', values = rep(c(1:25), times = ceiling(length(unique(data_sample$Variable))/6))) +
                   ggplot2::scale_color_manual('Scenario', values = rainbow(length(unique(data_sample$Scenario)))) +
                   ggplot2::labs(title = paste0('Evolution of ', unique(data_sample$Variable)), y = unique(data_sample$Unit), x = 'Year') +
                   ggplot2::theme_bw() +
                   ggplot2::theme(legend.text = ggplot2::element_text(size = 8),legend.title = ggplot2::element_text(size = 10),
                                  legend.key.size = ggplot2::unit(0.7, "cm"))
          )

          # display plot
          output[[plotname]] <- renderPlot({
            get(paste0('fig_',unique(data_sample$Variable)[1]))
          })
          print(plotname)

          # display download button
          output[[paste0("download", my_i)]] <- downloadHandler(
            filename = function() { paste0('fig_',unique(data_sample$Variable)[1],'.png') },
            content = function(file) {
              assign(paste0('fig_',unique(data_sample$Variable)[1]),
                     get(paste0('fig_',unique(data_sample$Variable)[1])) +
                       ggplot2::theme(legend.key.size = ggplot2::unit(0.25, "cm")))

              ggplot2::ggsave(file, plot = get(paste0('fig_',unique(data_sample$Variable)[1])), device = "png",
                              height = 10, width = 20, units = 'cm', limitsize = FALSE)
            })
        })
      }
    }
  })


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
