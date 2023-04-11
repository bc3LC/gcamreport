library(usethis)
library(magrittr)
library(shiny)
library(shinyTree)

# Define server ----------------------------------------------------------------

server <- function(input, output, session) {

  ## -- debug
  # output$res3 <- renderPrint(shinyTree::get_selected(input$tree_variables, format = 'slices'))

  # output$res3 <- renderPrint({
  # sel_tree_reg = shinyTree::get_selected(input$tree_regions, format = 'slices')
  # reg = do_unmount_tree(sel_tree_reg, 'regions')
  # # print(reg)
  # # save(reg, file = file.path('C:\\Users\\claudia.rodes\\Documents\\IAM_COMPACT\\gcamreport\\reg.RData'))
  # #
  # # print(reg)
  # })

  ## -- select all/none variables
  treeDataVar_sel <- reactive({
    tree_vars <<- do_mount_tree(cols, names(cols), selec = TRUE)
  })
  treeDataVar_unsel <- reactive({
    tree_vars <<- do_mount_tree(cols, names(cols), selec = FALSE)
  })
  observeEvent(input$select_all_variables, {
    updateTree(session = getDefaultReactiveDomain(), treeId = "tree_variables", data = treeDataVar_sel())
  })
  observeEvent(input$select_none_variables, {
    updateTree(session = getDefaultReactiveDomain(), treeId = "tree_variables", data = treeDataVar_unsel())
    noVars <<- TRUE
  })


  ## -- select all/none regions
  treeDataReg_sel <- reactive({
    tree_reg <<- do_mount_tree(reg_cont, names(reg_cont), selec = TRUE)
  })
  treeDataReg_unsel <- reactive({
    tree_reg <<- do_mount_tree(reg_cont, names(reg_cont), selec = FALSE)
  })
  observeEvent(input$select_all_regions, {
    updateTree(session = getDefaultReactiveDomain(), treeId = "tree_regions", data = treeDataReg_sel())
  })
  observeEvent(input$select_none_regions, {
    updateTree(session = getDefaultReactiveDomain(), treeId = "tree_regions", data = treeDataReg_unsel())
    noReg <<- TRUE
  })

  ## -- select all/none scenarios
  observeEvent(input$select_all_scen, {
    updateAwesomeCheckboxGroup(session = getDefaultReactiveDomain(), inputId = 'selected_scen',
                               label = "Select scenarios",
                               choices = unique(sdata$Scenario),
                               selected = unique(sdata$Scenario))
  })
  observeEvent(input$select_none_scen, {
    updateAwesomeCheckboxGroup(session = getDefaultReactiveDomain(), inputId = 'selected_scen',
                               label = "Select scenarios",
                               choices = unique(sdata$Scenario),
                               selected = NULL)
  })

  ## -- select all/none years
  observeEvent(input$select_all_years, {
    updateAwesomeCheckboxGroup(session = getDefaultReactiveDomain(), inputId = 'selected_years',
                               label = "Select years",
                               choices = available_years,
                               selected = available_years)
  })
  observeEvent(input$select_none_years, {
    updateAwesomeCheckboxGroup(session = getDefaultReactiveDomain(), inputId = 'selected_years',
                               label = "Select years",
                               choices = available_years,
                               selected = NULL)
  })

  ## -- select all/none columns
  observeEvent(input$select_all_cols, {
    updateAwesomeCheckboxGroup(session = getDefaultReactiveDomain(), inputId = 'selected_cols',
                               label = "Select columns",
                               choices = c('Model', 'Scenario', 'Region','Variable', 'Unit'),
                               selected = c('Model', 'Scenario', 'Region', 'Variable', 'Unit'))
  })
  observeEvent(input$select_none_cols, {
    updateAwesomeCheckboxGroup(session = getDefaultReactiveDomain(), inputId = 'selected_cols',
                               label = "Select columns",
                               choices = c('Model', 'Scenario', 'Region','Variable', 'Unit'),
                               selected = NULL)
  })


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
      output$tree_variables <- shinyTree::renderTree({
        tree_vars
      })
    }
  })


  ## -- listen to 'select_none' buttons
  observeEvent(c(input$tab_box, input,
                 input$tree_regions, input$select_none_regions,
                 input$tree_variables, input$select_none_variables), {

   if (input$tab_box == 'Data') {
   # data table
     # sel = update_user_choices_plot(selected_scen = input$selected_scen,
     #                          selected_years = input$selected_years,
     #                          selected_cols = input$selected_cols,
     #                          tree_regions = input$tree_regions,
     #                          tree_variables = input$tree_variables,
     #                          sidebarItemExpanded = input$sidebarItemExpanded,
     #                          aim = 'data')

      sel_reg <<- shinyTree::get_selected(input$tree_regions, format = 'slices')
      sel_vars <<- shinyTree::get_selected(input$tree_variables, format = 'slices')
      if (firstLoad) {
        firstLoad <<- FALSE
        sel_vars = unique(cols$col1)
        sel_reg = reg_cont$region
        basic_reg = TRUE
        basic_vars = TRUE
        print('a1')
      } else {
        basic_reg = 0
        basic_vars = 0
        if (firstReg && ((!is.null(input$sidebarItemExpanded) && input$sidebarItemExpanded != "Regions") || is.null(input$sidebarItemExpanded))) {
          sel_reg = reg_cont$region
          basic_reg = 1
        }
        if (firstVars && ((!is.null(input$sidebarItemExpanded) && input$sidebarItemExpanded != "Variables") || is.null(input$sidebarItemExpanded))) {
          sel_vars = unique(cols$col1)
          basic_vars = 1
        }
        if (noReg) {
          noReg <<- FALSE
          sel_reg = c()
          basic_reg = 2
        }
        if (noVars) {
          noVars <<- FALSE
          sel_vars = c()
          basic_vars = 2
        }
        firstVars <<- ifelse(!firstVars || (firstVars && !is.null(input$sidebarItemExpanded) && input$sidebarItemExpanded == "Variables"), FALSE, TRUE)
        firstReg <<- ifelse(!firstReg || (firstReg && !is.null(input$sidebarItemExpanded) && input$sidebarItemExpanded == "Regions"), FALSE, TRUE)
        print('a2')
      }

      output$datatable <- shiny::renderDataTable(
        # do_data_sample(sdata,
        #                sel$scen,sel$years,
        #                sel$cols,sel$vars,
        #                sel$reg, sel$basic_reg, sel$basic_vars),
        do_data_sample(sdata,
                       input$selected_scen,input$selected_years,
                       input$selected_cols,sel_vars,
                       sel_reg, basic_reg, basic_vars),
        options = list(pageLength = 10,
                       scrollX = TRUE,
                       rownames = FALSE)
      )
   } else if (input$tab_box == 'Plot') {
     # plot
     print('plot')

     sel = update_user_choices_plot(selected_scen = input$selected_scen,
                              selected_years = input$selected_years,
                              selected_cols = input$selected_cols,
                              tree_regions = input$tree_regions,
                              tree_variables = input$tree_variables,
                              sidebarItemExpanded = input$sidebarItemExpanded)

     if (input$graph_grouping == 'Grouped'){
      # single plot since 'grouped' selected

       errors = check_user_choices_plot(vars = sel$vars,
                                        scen = sel$scen,
                                        years = sel$years,
                                        reg = sel$reg,
                                        grouped = TRUE)
       # display one single plot with all selected variables

       # if (scen_reg_year_ok && length(unique(check_vars)) == 1) {
       if (length(errors) < 1) {
         print('start plotting')
         # insert the right number of plot output objects into the web page
         output$plots <- renderUI({
           plot_output_list <- lapply(1:1, function(i) {
             plotname <- paste("plot", i, sep="")
             tagList(
               plotOutput(plotname, height = 450, width = 1000),
               downloadButton(paste0("download", i), label = "Download")
             )
           })

           # Convert the list to a tagList - this is necessary for the list of items
           # to display properly.
           do.call(tagList, plot_output_list)
         })

         # do plot
         print('a8')
         data_sample = do_data_sample(sdata,
                                      sel$scen, sel$years,
                                      sel$cols, sel$vars_ini,
                                      sel$reg_ini, sel$basic_reg, sel$basic_vars)
         data_sample = tidyr::pivot_longer(data_sample, cols = 6:ncol(data_sample), names_to = 'year', values_to = 'values') %>%
           dplyr::mutate(values = as.numeric(as.character(values))) %>%
           dplyr::mutate(year = as.numeric(as.character(year)))

         assign(paste0('fig_',unique(data_sample$Variable)[1]),
                ggplot2::ggplot(data = data_sample, ggplot2::aes(x = year, y = values, color = Scenario, linetype = Variable, group = interaction(Scenario,Region,Variable))) +
                  ggplot2::geom_point(ggplot2::aes(shape = Region)) +
                  ggplot2::geom_line() +
                  ggplot2::scale_shape_manual('Region', values = rep(c(1:25), times = ceiling(length(unique(data_sample$Region))/6))) +
                  ggplot2::scale_linetype_manual('Variables', values = rep(c(1:9), times = ceiling(length(unique(data_sample$Variable))/9))) +
                  ggplot2::scale_color_manual('Scenario', values = rainbow(length(unique(data_sample$Scenario)))) +
                  ggplot2::labs(title = paste0('Evolution of ', unique(data_sample$Variable)), y = unique(data_sample$Unit), x = 'Year') +
                  # ggplot2::guides(color = ggplot2::guide_legend(ncol = floor(length(unique(data_sample$Scenario))/6))) +
                  # ggplot2::guides(shape = ggplot2::guide_legend(ncol = floor(length(unique(data_sample$Region))/7))) +
                  # ggplot2::guides(linetype = ggplot2::guide_legend(ncol = floor(length(unique(data_sample$Variable))/8))) +
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

               # compute width
               w = 5*(max(floor(length(unique(data_sample$Scenario))/6),floor(length(unique(data_sample$Region))/7),floor(length(unique(data_sample$Variable))/8))-1)
               ggplot2::ggsave(file, plot = get(paste0('fig_',unique(data_sample$Variable)[1])), device = "png",
                               height = 10, width = 20+w, units = 'cm', limitsize = FALSE)
             })
         })
       } else {
         output$plots <- renderUI({
           HTML(paste(errors, collapse = '<br/>'))
         })
       }
     }
     else if (input$graph_grouping == 'Ungrouped') {
     # multiple plots since 'ungrouped' selected
       print('ungrouped')
       errors = check_user_choices_plot(vars = sel$vars,
                                        scen = sel$scen,
                                        years = sel$years,
                                        reg = sel$reg,
                                        grouped = FALSE)
       if (length(errors) < 1) {
           # display one plot for each variable
           n = length(sel$vars)
           print(n)
           # insert the right number of plot output objects into the web page
           output$plots <- renderUI({
             plot_output_list <- lapply(1:n, function(i) {
               plotname <- paste("plot", i, sep="")
               tagList(
                 plotOutput(plotname, height = 400, width = 1000),
                 downloadButton(paste0("download", i), label = "Download"),
                 br(),br(),br()
               )
             })

             # Convert the list to a tagList - this is necessary for the list of items
             # to display properly.
             do.call(tagList, plot_output_list)
           })

           for (i in 1:n) {
             # Need local so that each item gets its own number. Without it, the value
             # of i in the renderPlot() will be the same across all instances, because
             # of when the expression is evaluated.
             local({
               my_i <- i
               plotname <- paste("plot", my_i, sep="")

               # create plot
               data_sample = do_data_sample(sdata,
                                            sel$scen, sel$years,
                                            sel$cols, sel$vars_ini,
                                            sel$reg_ini, sel$basic_reg, sel$basic_vars)
               data_sample = tidyr::pivot_longer(data_sample, cols = 6:ncol(data_sample), names_to = 'year', values_to = 'values') %>%
                 dplyr::mutate(values = as.numeric(as.character(values))) %>%
                 dplyr::mutate(year = as.numeric(as.character(year)))

               print('b1')
               assign(paste0('fig_',unique(data_sample$Variable)[my_i]),
                      ggplot2::ggplot(data = data_sample %>% dplyr::filter(Variable == unique(data_sample$Variable)[my_i]), ggplot2::aes(x = year, y = values, color = Scenario, linetype = Variable, group = interaction(Scenario,Region,Variable))) +
                        ggplot2::geom_point(ggplot2::aes(shape = Region)) +
                        ggplot2::geom_line() +
                        ggplot2::guides(linetype = 'none') +
                        ggplot2::scale_shape_manual('Region', values = rep(c(1:25), times = ceiling(length(unique(data_sample$Variable))/6))) +
                        ggplot2::scale_color_manual('Scenario', values = rainbow(length(unique(data_sample$Scenario)))) +
                        ggplot2::labs(title = paste0('Evolution of ', unique(data_sample$Variable)[my_i]), y = unique(data_sample$Unit), x = 'Year') +
                        # ggplot2::guides(color = ggplot2::guide_legend(ncol = floor(length(unique(data_sample$Scenario))/6))) +
                        # ggplot2::guides(shape = ggplot2::guide_legend(ncol = floor(length(unique(data_sample$Region))/7))) +
                        # ggplot2::guides(linetype = ggplot2::guide_legend(ncol = floor(length(unique(data_sample$Variable))/8))) +
                        ggplot2::theme_bw() +
                        ggplot2::theme(legend.text = ggplot2::element_text(size = 8),legend.title = ggplot2::element_text(size = 10),
                                       legend.key.size = ggplot2::unit(0.7, "cm"))
               )

               # display plot
               output[[plotname]] <- renderPlot({
                 get(paste0('fig_',unique(data_sample$Variable)[my_i]))
               })

               # display download button
               output[[paste0("download", my_i)]] <- downloadHandler(
                 filename = function() { paste0('fig_',unique(data_sample$Variable)[my_i],'.png') },
                 content = function(file) {
                   assign(paste0('fig_',unique(data_sample$Variable)[my_i]),
                          get(paste0('fig_',unique(data_sample$Variable)[my_i])) +
                            ggplot2::theme(legend.key.size = ggplot2::unit(0.25, "cm")))

                   # compute width
                   w = 5*(max(floor(length(unique(data_sample$Scenario))/6),floor(length(unique(data_sample$Region))/7),floor(length(unique(data_sample$Variable))/8))-1)
                   ggplot2::ggsave(file, plot = get(paste0('fig_',unique(data_sample$Variable)[my_i])), device = "png",
                                   height = 10, width = 20+w, units = 'cm', limitsize = FALSE)
                 })
             })
           }
       } else {
         output$plots <- renderUI({
           HTML(paste(errors, collapse = '<br/>'))
         })

       }
     }
   }

  })


  tableData <- reactive({
    sel_reg <<- shinyTree::get_selected(input$tree_regions, format = 'slices')
    sel_vars <<- shinyTree::get_selected(input$tree_variables, format = 'slices')
    if (firstLoad) {
      firstLoad <<- FALSE
      print('a3')
      tableData <- do_data_sample(sdata,
                                  input$selected_scen,input$selected_years,
                                  input$selected_cols,unique(cols$col1),
                                  reg_cont$region, TRUE, TRUE)
    } else {
      basic_reg = 0
      basic_vars = 0
      if (firstReg && ((!is.null(input$sidebarItemExpanded) && input$sidebarItemExpanded != "Regions") || is.null(input$sidebarItemExpanded))) {
        sel_reg = reg_cont$region
        basic_reg = 1
      }
      if (firstVars && ((!is.null(input$sidebarItemExpanded) && input$sidebarItemExpanded != "Variables") || is.null(input$sidebarItemExpanded))) {
        sel_vars = unique(cols$col1)
        basic_vars = 1
      }
      if (noReg) {
        noReg <<- FALSE
        sel_reg = c()
        basic_reg = 2
      }
      if (noVars) {
        noVars <<- FALSE
        sel_vars = c()
        basic_vars = 2
      }
      firstVars <<- ifelse(!firstVars || (firstVars && !is.null(input$sidebarItemExpanded) && input$sidebarItemExpanded == "Variables"), FALSE, TRUE)
      firstReg <<- ifelse(!firstReg || (firstReg && !is.null(input$sidebarItemExpanded) && input$sidebarItemExpanded == "Regions"), FALSE, TRUE)
      print('a4')
      tableData <- do_data_sample(sdata,
                                 input$selected_scen,input$selected_years,
                                 input$selected_cols,sel_vars,
                                 sel_reg, basic_reg, basic_vars)
    }

  })


  ## -- plot
  observe({
    print('observeEvent')

    if (input$tab_box == 'Plot') {
      print('plot')
      sel = update_user_choices_plot(selected_scen = input$selected_scen,
                                     selected_years = input$selected_years,
                                     selected_cols = input$selected_cols,
                                     tree_regions = input$tree_regions,
                                     tree_variables = input$tree_variables,
                                     sidebarItemExpanded = input$sidebarItemExpanded)

      if (input$graph_grouping == 'Grouped'){
        errors = check_user_choices_plot(vars = sel$vars,
                                         scen = sel$scen,
                                         years = sel$years,
                                         reg = sel$reg,
                                         grouped = TRUE)

        # display one single plot with all selected variables

        # check that only variables from the same family are selected, and that at least
        # one scenario, one region, and one year are selected

        if (length(errors) < 1) {
          print('start plotting')
          # insert the right number of plot output objects into the web page
          output$plots <- renderUI({
            plot_output_list <- lapply(1:1, function(i) {
              plotname <- paste("plot", i, sep="")
              tagList(
                plotOutput(plotname, height = 450, width = 1000),
                downloadButton(paste0("download", i), label = "Download")
              )
            })

            # Convert the list to a tagList - this is necessary for the list of items
            # to display properly.
            do.call(tagList, plot_output_list)
          })

          # do plot
          print('a5')
          data_sample = do_data_sample(sdata,
                                       sel$scen, sel$years,
                                       sel$cols, sel$vars_ini,
                                       sel$reg_ini, sel$basic_reg, sel$basic_vars)
          data_sample = tidyr::pivot_longer(data_sample, cols = 6:ncol(data_sample), names_to = 'year', values_to = 'values') %>%
            dplyr::mutate(values = as.numeric(as.character(values))) %>%
            dplyr::mutate(year = as.numeric(as.character(year)))

          print('b2')
          assign(paste0('fig_',unique(data_sample$Variable)[1]),
                 ggplot2::ggplot(data = data_sample, ggplot2::aes(x = year, y = values, color = Scenario, linetype = Variable, group = interaction(Scenario,Region,Variable))) +
                   ggplot2::geom_point(ggplot2::aes(shape = Region)) +
                   ggplot2::geom_line() +
                   ggplot2::scale_shape_manual('Region', values = rep(c(1:25), times = ceiling(length(unique(data_sample$Region))/6))) +
                   ggplot2::scale_linetype_manual('Variables', values = rep(c(1:9), times = ceiling(length(unique(data_sample$Variable))/9))) +
                   ggplot2::scale_color_manual('Scenario', values = rainbow(length(unique(data_sample$Scenario)))) +
                   ggplot2::labs(title = paste0('Evolution of ', unique(data_sample$Variable)), y = unique(data_sample$Unit), x = 'Year') +
                   # ggplot2::guides(color = ggplot2::guide_legend(ncol = floor(length(unique(data_sample$Scenario))/6))) +
                   # ggplot2::guides(shape = ggplot2::guide_legend(ncol = floor(length(unique(data_sample$Region))/7))) +
                   # ggplot2::guides(linetype = ggplot2::guide_legend(ncol = floor(length(unique(data_sample$Variable))/8))) +
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

                # compute width
                w = 5*(max(floor(length(unique(data_sample$Scenario))/6),floor(length(unique(data_sample$Region))/7),floor(length(unique(data_sample$Variable))/8))-1)
                ggplot2::ggsave(file, plot = get(paste0('fig_',unique(data_sample$Variable)[1])), device = "png",
                                height = 10, width = 20+w, units = 'cm', limitsize = FALSE)
              })
          })
        } else {
          output$plots <- renderUI({
            HTML(paste(errors, collapse = '<br/>'))
          })
        }
      }
      else if (input$graph_grouping == 'Ungrouped') {

        errors = check_user_choices_plot(vars = sel$vars,
                                         scen = sel$scen,
                                         years = sel$years,
                                         reg = sel$reg,
                                         grouped = FALSE)

        print('ungrouped')
        if (length(errors) < 1) {

              # display one plot for each variable
              n = length(sel$vars)
              print(paste0('n = ',n))
              # insert the right number of plot output objects into the web page
              output$plots <- renderUI({
                plot_output_list <- lapply(1:n, function(i) {
                  plotname <- paste("plot", i, sep="")
                  tagList(
                    plotOutput(plotname, height = 400, width = 1000),
                    downloadButton(paste0("download", i), label = "Download"),
                    br(),br(),br()
                  )
                })

                # Convert the list to a tagList - this is necessary for the list of items
                # to display properly.
                do.call(tagList, plot_output_list)
              })


              # n = length(input$tree_variables)
              # print(n)
              for (i in 1:n) {
                # Need local so that each item gets its own number. Without it, the value
                # of i in the renderPlot() will be the same across all instances, because
                # of when the expression is evaluated.
                local({
                  my_i <- i
                  plotname <- paste("plot", my_i, sep="")

                  # create plot
                  data_sample = do_data_sample(sdata,
                                               sel$scen, sel$years,
                                               sel$cols, sel$vars_ini,
                                               sel$reg_ini, sel$basic_reg, sel$basic_vars)
                  data_sample = tidyr::pivot_longer(data_sample, cols = 6:ncol(data_sample), names_to = 'year', values_to = 'values') %>%
                    dplyr::mutate(values = as.numeric(as.character(values))) %>%
                    dplyr::mutate(year = as.numeric(as.character(year)))

                  assign(paste0('fig_',unique(data_sample$Variable)[my_i]),
                         ggplot2::ggplot(data = data_sample %>% dplyr::filter(Variable == unique(data_sample$Variable)[my_i]), ggplot2::aes(x = year, y = values, color = Scenario, linetype = Variable, group = interaction(Scenario,Region,Variable))) +
                           ggplot2::geom_point(ggplot2::aes(shape = Region)) +
                           ggplot2::geom_line() +
                           ggplot2::guides(linetype = 'none') +
                           ggplot2::scale_shape_manual('Region', values = rep(c(1:25), times = ceiling(length(unique(data_sample$Variable))/6))) +
                           ggplot2::scale_color_manual('Scenario', values = rainbow(length(unique(data_sample$Scenario)))) +
                           ggplot2::labs(title = paste0('Evolution of ', unique(data_sample$Variable)[my_i]), y = unique(data_sample$Unit), x = 'Year') +
                           # ggplot2::guides(color = ggplot2::guide_legend(ncol = floor(length(unique(data_sample$Scenario))/6))) +
                           # ggplot2::guides(shape = ggplot2::guide_legend(ncol = floor(length(unique(data_sample$Region))/7))) +
                           # ggplot2::guides(linetype = ggplot2::guide_legend(ncol = floor(length(unique(data_sample$Variable))/8))) +
                           ggplot2::theme_bw() +
                           ggplot2::theme(legend.text = ggplot2::element_text(size = 8),legend.title = ggplot2::element_text(size = 10),
                                          legend.key.size = ggplot2::unit(0.7, "cm"))
                  )

                  # display plot
                  output[[plotname]] <- renderPlot({
                    get(paste0('fig_',unique(data_sample$Variable)[my_i]))
                  })

                  # display download button
                  output[[paste0("download", my_i)]] <- downloadHandler(
                    filename = function() { paste0('fig_',unique(data_sample$Variable)[my_i],'.png') },
                    content = function(file) {
                      assign(paste0('fig_',unique(data_sample$Variable)[my_i]),
                             get(paste0('fig_',unique(data_sample$Variable)[my_i])) +
                               ggplot2::theme(legend.key.size = ggplot2::unit(0.25, "cm")))

                      # compute width
                      w = 5*(max(floor(length(unique(data_sample$Scenario))/6),floor(length(unique(data_sample$Region))/7),floor(length(unique(data_sample$Variable))/8))-1)
                      ggplot2::ggsave(file, plot = get(paste0('fig_',unique(data_sample$Variable)[my_i])), device = "png",
                                      height = 10, width = 20+w, units = 'cm', limitsize = FALSE)
                    })
                })
              }
        } else {
            output$plots <- renderUI({
            HTML(paste(errors, collapse = '<br/>'))
          })
        }
      }
    }
  })


  # ## -- download button
  # output$downloadData <- downloadHandler(
  #   filename = function() {
  #     paste('data-', Sys.Date(), '.csv', sep='')
  #   },
  #   content = function(con) {
  #     write.csv(tableData(),
  #               con)
  #   }
  # )

  session$onSessionEnded(reset_first_load)


}
