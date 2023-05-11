library(usethis)
library(magrittr)

# Define server ----------------------------------------------------------------

server <- function(input, output, session) {


  ## -- select all/none variables
  observeEvent(input$select_all_variables, {
    tree_vars <<- do_mount_tree(cols, names(cols), selec = TRUE)
    updateTree(session = getDefaultReactiveDomain(), treeId = "tree_variables", data = tree_vars)
    noVars <<- FALSE
  })
  observeEvent(input$select_none_variables, {
    tree_vars <<- do_mount_tree(cols, names(cols), selec = FALSE)
    updateTree(session = getDefaultReactiveDomain(), treeId = "tree_variables", data = tree_vars)
    noVars <<- TRUE
  })


  ## -- select all/none regions
  observeEvent(input$select_all_regions, {
    tree_reg <<- do_mount_tree(reg_cont, names(reg_cont), selec = TRUE)
    updateTree(session = getDefaultReactiveDomain(), treeId = "tree_regions", data = tree_reg)
    noReg <<- FALSE
  })
  observeEvent(input$select_none_regions, {
    tree_reg <<- do_mount_tree(reg_cont, names(reg_cont), selec = FALSE)
    updateTree(session = getDefaultReactiveDomain(), treeId = "tree_regions", data = tree_reg)
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
    tree_reg <<- change_style(input$tree_regions, 'regions')
    noReg <<- FALSE
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
    if (firstReg) {
      sel_reg_vec = reg_cont$region %>%
        tidyr::replace_na('World')
    } else {
      sel_reg = shinyTree::get_selected(input$tree_regions, format = 'slices')
      if (length(sel_reg) > 0) {
        sel_reg_vec = do_unmount_tree(sel_reg, 'regions')
        # pull the variables of the whole data with the regions restricted to the user's selection
        tmp_vars = sdata %>%
          dplyr::filter(Region %in% sel_reg_vec) %>%
          dplyr::distinct(Variable) %>%
          dplyr::pull()
        tmp_vars = all_vars[!(all_vars %in% tmp_vars)]
        tree_vars <<- change_style(input$tree_variables, 'variables', tmp_vars)
      } else {
        tree_vars <<- change_style(input$tree_variables, 'regions')
      }
    }
    noVars <<- FALSE

    if (!updatedVars) {
      # re-render tree if style modified
      output$tree_variables <- shinyTree::renderTree({
        tree_vars
      })
      updatedVars <<- TRUE
    }
  })
  observeEvent(input$sidebarItemExpanded, {
    if (input$sidebarItemExpanded == "Variables") {
      output$tree_variables <- shinyTree::renderTree({
        tree_vars
      })
    } else {
      updatedVars <<- FALSE
    }
  })


  ## -- listen to 'select_none' buttons
  observeEvent(c(input$tab_box, input,
                 input$tree_regions, input$select_none_regions,
                 input$tree_variables, input$select_none_variables), {

                   # if the selected tab is 'Data'
                   if (input$tab_box == 'Data') {

                     # enable columns dropdown and set class 'enabled_cols'
                     shinyjs::enable("columns_id")
                     shinyjs::addClass(selector = "#columns_id", class = "enabled_cols")

                     # get selected regions and variables from input
                     sel_reg <<- shinyTree::get_selected(input$tree_regions, format = 'slices')
                     sel_vars <<- shinyTree::get_selected(input$tree_variables, format = 'slices')

                     # if it's the first load
                     if (firstLoad) {
                       firstLoad <<- FALSE
                       # set selected variables and regions to all possiblities
                       sel_vars = unique(cols$col1)
                       sel_reg = reg_cont$region
                       # set basic_reg and basic_vars to TRUE
                       basic_reg = TRUE
                       basic_vars = TRUE
                     } else {
                       basic_reg = 0
                       basic_vars = 0

                       # if it's the first time loading regions and there is a sidebarItem expanded different than regions, choose all possible regions
                       if (firstReg && ((!is.null(input$sidebarItemExpanded) && input$sidebarItemExpanded != "Regions") || is.null(input$sidebarItemExpanded))) {
                         sel_reg = reg_cont$region
                         basic_reg = 1
                       }

                       # if it's the first time loading variables and there is a sidebarItem expanded different than variables, choose all possible variables
                       if (firstVars && ((!is.null(input$sidebarItemExpanded) && input$sidebarItemExpanded != "Variables") || is.null(input$sidebarItemExpanded))) {
                         sel_vars = unique(cols$col1)
                         basic_vars = 1
                       }

                       # if there are no selected regions
                       if (noReg) {
                         sel_reg = c()
                         basic_reg = 2
                       }

                       # if there are no selected variables
                       if (noVars) {
                         sel_vars = c()
                         basic_vars = 2
                       }

                       # set firstVars and/or firstReg to FALSE if it's not the first time loading them or if their sidebarItem is expanded
                       firstVars <<- ifelse(!firstVars || (firstVars && !is.null(input$sidebarItemExpanded) && input$sidebarItemExpanded == "Variables"), FALSE, TRUE)
                       firstReg <<- ifelse(!firstReg || (firstReg && !is.null(input$sidebarItemExpanded) && input$sidebarItemExpanded == "Regions"), FALSE, TRUE)
                     }

                     # render data table
                     output$datatable <- shiny::renderDataTable(
                       do_data_sample(sdata,
                                      input$selected_scen,input$selected_years,
                                      input$selected_cols,sel_vars,
                                      sel_reg, basic_reg, basic_vars),
                       options = list(pageLength = 10,
                                      scrollX = TRUE,
                                      rownames = FALSE)
                     )
                   } else if (input$tab_box == 'Plot') {
                     # if the selected tab is 'Plot'

                     # disable columns dropdown and set class 'disabled_cols'
                     shinyjs::disable("columns_id")
                     shinyjs::addClass(selector = "#columns_id", class = "disabled_cols")
                     shinyjs::removeClass(selector = "#columns_id", class = "enabled_cols")

                     # get selected regions, variables, years, and scenarios from input
                     sel = update_user_choices_plot(selected_scen = input$selected_scen,
                                                    selected_years = input$selected_years,
                                                    tree_regions = input$tree_regions,
                                                    tree_variables = input$tree_variables,
                                                    sidebarItemExpanded = input$sidebarItemExpanded)
                     # if no errors
                     if (input$vars_grouping == 'Grouped Variables'){
                       # if the variables must be displayed all in one plot

                       # check if the user's choice contains errors
                       errors = check_user_choices_plot(vars = sel$vars,
                                                        scen = sel$scen,
                                                        years = sel$years,
                                                        reg = sel$reg,
                                                        grouped = TRUE)

                       if (length(errors) < 1) {
                         # insert a single plot output object into the web page

                         # compute the display and download height
                         if (input$reg_grouping == 'Grouped Regions') {
                           hh_disp = 450
                           hh_dwn = 15
                         } else {
                           hh_disp = 40*(length(sel$reg))
                           hh_dwn = 15 + length(sel$reg)/2
                         }

                         # render the plot and the corresponding download button
                         output$plots <- renderUI({
                           plot_output_list <- lapply(1:1, function(i) {
                             plotname <- paste("plot", i, sep="")
                             tagList(
                               plotOutput(plotname, height = hh_disp, width = 1000),
                               downloadButton(paste0("download", i), label = "Download")
                             )
                           })

                           # convert the list to a tagList to display properly the list of items
                           do.call(tagList, plot_output_list)
                         })

                         # restrict the dataset to the user's choices
                         data_sample = do_data_sample(sdata,
                                                      sel$scen, sel$years,
                                                      sel$cols, sel$vars_ini,
                                                      sel$reg_ini, sel$basic_reg, sel$basic_vars)
                         data_sample = tidyr::pivot_longer(data_sample, cols = 6:ncol(data_sample), names_to = 'year', values_to = 'values') %>%
                           dplyr::mutate(values = as.numeric(as.character(values))) %>%
                           dplyr::mutate(year = as.numeric(as.character(year)))

                         # title of the plot
                         tt = check_vars = sub("\\|.*", "", stringr::str_extract(unique(data_sample$Variable)[1], "(.*?)(\\||$)"))

                         # do plot
                         if (input$reg_grouping == 'Grouped Regions') {
                           # consider regions in the legend
                           assign(paste0('fig_',tt),
                                  ggplot2::ggplot(data = data_sample, ggplot2::aes(x = year, y = values, color = Scenario, linetype = Variable, group = interaction(Scenario,Region,Variable))) +
                                    ggplot2::geom_point(ggplot2::aes(shape = Region)) +
                                    ggplot2::geom_line() +
                                    ggplot2::scale_shape_manual('Regions', values = rep(c(1:20), times = ceiling(length(unique(data_sample$Region))/20))) +
                                    ggplot2::scale_linetype_manual('Variables', values = rep(c(1:6), times = ceiling(length(unique(data_sample$Variable))/6))) +
                                    ggplot2::scale_color_manual('Scenarios', values = rainbow(length(unique(data_sample$Scenario)))) +
                                    ggplot2::labs(title = paste0('Evolution of ', tt), y = unique(data_sample$Unit), x = 'Year') +
                                    ggplot2::guides(color = ggplot2::guide_legend(ncol = ceiling(length(unique(data_sample$Scenario))/6))) +
                                    ggplot2::guides(shape = ggplot2::guide_legend(ncol = ceiling(length(unique(data_sample$Region))/7))) +
                                    ggplot2::guides(linetype = ggplot2::guide_legend(ncol = ceiling(length(unique(data_sample$Variable))/8))) +
                                    ggplot2::theme_bw() +
                                    ggplot2::theme(legend.text = ggplot2::element_text(size = 8),legend.title = ggplot2::element_text(size = 10),
                                                   legend.key.size = ggplot2::unit(0.5, "cm"))
                           )
                         } else {
                           # facet the plot by regions
                           assign(paste0('fig_',tt),
                                  ggplot2::ggplot(data = data_sample, ggplot2::aes(x = year, y = values, color = Scenario, linetype = Variable, group = interaction(Scenario,Region,Variable))) +
                                    ggplot2::geom_point() +
                                    ggplot2::geom_line() +
                                    ggplot2::facet_wrap(. ~ Region) +
                                    ggplot2::scale_linetype_manual('Variables', values = rep(c(1:6), times = ceiling(length(unique(data_sample$Variable))/6))) +
                                    ggplot2::scale_color_manual('Scenarios', values = rainbow(length(unique(data_sample$Scenario)))) +
                                    ggplot2::labs(title = paste0('Evolution of ', tt), y = unique(data_sample$Unit), x = 'Year') +
                                    ggplot2::guides(color = ggplot2::guide_legend(ncol = ceiling(length(unique(data_sample$Scenario))/8))) +
                                    ggplot2::guides(linetype = ggplot2::guide_legend(ncol = ceiling(length(unique(data_sample$Variable))/5))) +
                                    ggplot2::theme_bw() +
                                    ggplot2::theme(legend.text = ggplot2::element_text(size = 8),legend.title = ggplot2::element_text(size = 10),
                                                   legend.key.size = ggplot2::unit(0.5, "cm"), legend.position = 'bottom')
                           )
                         }

                         local({
                           my_i <- 1
                           plotname <- paste("plot", my_i, sep="")

                           # display plot
                           output[[plotname]] <- renderPlot({
                             get(paste0('fig_',tt))
                           })

                           # display download button
                           output[[paste0("download", my_i)]] <- downloadHandler(
                             filename = function() { paste0('fig_',tt,'.png') },
                             content = function(file) {
                               assign(paste0('fig_',tt),
                                      get(paste0('fig_',tt)) +
                                        ggplot2::theme(legend.key.size = ggplot2::unit(0.1, "cm")))

                               # compute width
                               w = 5*(max(floor(length(unique(data_sample$Scenario))/6),floor(length(unique(data_sample$Region))/7),floor(length(unique(data_sample$Variable))/8))-1)

                               # save plot
                               ggplot2::ggsave(file, plot = get(paste0('fig_',tt)), device = "png",
                                               height = hh_dwn, width = 20+w, units = 'cm', limitsize = FALSE)
                             })
                         })
                       } else {
                         # display errors to the user
                         output$plots <- renderUI({
                           HTML(paste(errors, collapse = '<br/>'))
                         })
                       }
                     }
                     else if (input$vars_grouping == 'Ungrouped Variables') {
                       # multiple plots since 'ungrouped' selected, ie., display one plot for each variable

                       # check if the user's choice contains errors
                       errors = check_user_choices_plot(vars = sel$vars,
                                                        scen = sel$scen,
                                                        years = sel$years,
                                                        reg = sel$reg,
                                                        grouped = FALSE)
                       # if no errors
                       if (length(errors) < 1) {
                         # if the variables must be displayed in different plots

                         # restrict the dataset to the user's choices
                         data_sample = do_data_sample(sdata,
                                                      sel$scen, sel$years,
                                                      sel$cols, sel$vars_ini,
                                                      sel$reg_ini, sel$basic_reg, sel$basic_vars)
                         data_sample = tidyr::pivot_longer(data_sample, cols = 6:ncol(data_sample), names_to = 'year', values_to = 'values') %>%
                           dplyr::mutate(values = as.numeric(as.character(values))) %>%
                           dplyr::mutate(year = as.numeric(as.character(year)))

                         # number of plots to render
                         n = length(unique(data_sample$Variable))

                         # compute the display and download height
                         if (input$reg_grouping == 'Grouped Regions') {
                           hh_disp = 400
                           hh_dwn = 10
                         } else {
                           hh_disp = 30*(length(sel$reg))
                           hh_dwn = 10 + length(sel$reg)/2
                         }

                         # render the plot and the corresponding download button
                         output$plots <- renderUI({
                           plot_output_list <- lapply(1:n, function(i) {
                             plotname <- paste("plot", i, sep="")
                             tagList(
                               plotOutput(plotname, height = hh_disp, width = 1000),
                               downloadButton(paste0("download", i), label = "Download"),
                               br(),br(),br()
                             )
                           })

                           # convert the list to a tagListto display properly the list of items
                           do.call(tagList, plot_output_list)
                         })

                         for (i in 1:n) {
                           local({
                             my_i <- i
                             plotname <- paste("plot", my_i, sep="")

                             # do plot
                             if (input$reg_grouping == 'Grouped Regions') {
                               # consider regions in the legend
                               assign(paste0('fig_',unique(data_sample$Variable)[my_i]),
                                      ggplot2::ggplot(data = data_sample %>% dplyr::filter(Variable == unique(data_sample$Variable)[my_i]), ggplot2::aes(x = year, y = values, color = Scenario, linetype = Variable, group = interaction(Scenario,Region,Variable))) +
                                        ggplot2::geom_point(ggplot2::aes(shape = Region)) +
                                        ggplot2::geom_line() +
                                        ggplot2::guides(linetype = 'none') +
                                        ggplot2::scale_shape_manual('Regions', values = rep(c(1:20), times = ceiling(length(unique(data_sample$Region))/20))) +
                                        ggplot2::scale_color_manual('Scenarios', values = rainbow(length(unique(data_sample$Scenario)))) +
                                        ggplot2::labs(title = paste0('Evolution of ', unique(data_sample$Variable)[my_i]), y = unique(data_sample$Unit), x = 'Year') +
                                        ggplot2::guides(color = ggplot2::guide_legend(ncol = ceiling(length(unique(data_sample$Scenario))/6))) +
                                        ggplot2::guides(shape = ggplot2::guide_legend(ncol = ceiling(length(unique(data_sample$Region))/7))) +
                                        ggplot2::guides(linetype = 'none') +
                                        ggplot2::theme_bw() +
                                        ggplot2::theme(legend.text = ggplot2::element_text(size = 8),legend.title = ggplot2::element_text(size = 10),
                                                       legend.key.size = ggplot2::unit(0.7, "cm"))
                               )
                             } else {
                               # facet the plot by regions
                               assign(paste0('fig_',unique(data_sample$Variable)[my_i]),
                                      ggplot2::ggplot(data = data_sample %>% dplyr::filter(Variable == unique(data_sample$Variable)[my_i]), ggplot2::aes(x = year, y = values, color = Scenario, linetype = Variable, group = interaction(Scenario,Region,Variable))) +
                                        ggplot2::geom_point() +
                                        ggplot2::geom_line() +
                                        ggplot2::facet_wrap(. ~ Region) +
                                        ggplot2::scale_color_manual('Scenarios', values = rainbow(length(unique(data_sample$Scenario)))) +
                                        ggplot2::labs(title = paste0('Evolution of ', unique(data_sample$Variable)[my_i]), y = unique(data_sample$Unit), x = 'Year') +
                                        ggplot2::guides(color = ggplot2::guide_legend(ncol = ceiling(length(unique(data_sample$Scenario))/3))) +
                                        ggplot2::guides(linetype = 'none') +
                                        ggplot2::guides(shape = 'none') +
                                        ggplot2::theme_bw() +
                                        ggplot2::theme(legend.text = ggplot2::element_text(size = 8),legend.title = ggplot2::element_text(size = 10),
                                                       legend.key.size = ggplot2::unit(0.5, "cm"), legend.position = 'bottom')
                               )
                             }

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

                                 # save plot
                                 ggplot2::ggsave(file, plot = get(paste0('fig_',unique(data_sample$Variable)[my_i])), device = "png",
                                                 height = hh_dwn, width = 20+w, units = 'cm', limitsize = FALSE)
                               })
                           })
                         }
                       } else {
                         # display errors messages to the user
                         output$plots <- renderUI({
                           HTML(paste(errors, collapse = '<br/>'))
                         })

                       }
                     }
                   }

                 })


  ## -- plot
  observe({
    # if the selected tab is 'Plot'
    if (input$tab_box == 'Plot') {

      # disable columns dropdown and set class 'disabled_cols'
      shinyjs::disable("columns_id")
      shinyjs::addClass(selector = "#columns_id", class = "disabled_cols")
      shinyjs::removeClass(selector = "#columns_id", class = "enabled_cols")

      # get selected regions, variables, years, and scenarios from input
      sel = update_user_choices_plot(selected_scen = input$selected_scen,
                                     selected_years = input$selected_years,
                                     tree_regions = input$tree_regions,
                                     tree_variables = input$tree_variables,
                                     sidebarItemExpanded = input$sidebarItemExpanded)
      # if no errors
      if (input$vars_grouping == 'Grouped Variables'){
        # if the variables must be displayed all in one plot

        # check if the user's choice contains errors
        errors = check_user_choices_plot(vars = sel$vars,
                                         scen = sel$scen,
                                         years = sel$years,
                                         reg = sel$reg,
                                         grouped = TRUE)

        if (length(errors) < 1) {
          # insert a single plot output object into the web page

          # compute the display and download height
          if (input$reg_grouping == 'Grouped Regions') {
            hh_disp = 450
            hh_dwn = 15
          } else {
            hh_disp = 900
            hh_dwn = 20
          }

          # render the plot and the corresponding download button
          output$plots <- renderUI({
            plot_output_list <- lapply(1:1, function(i) {
              plotname <- paste("plot", i, sep="")
              tagList(
                plotOutput(plotname, height = hh_disp, width = 1000),
                downloadButton(paste0("download", i), label = "Download")
              )
            })

            # convert the list to a tagListto display properly the list of items
            do.call(tagList, plot_output_list)
          })

          # restrict the dataset to the user's choices
          data_sample = do_data_sample(sdata,
                                       sel$scen, sel$years,
                                       sel$cols, sel$vars_ini,
                                       sel$reg_ini, sel$basic_reg, sel$basic_vars)
          data_sample = tidyr::pivot_longer(data_sample, cols = 6:ncol(data_sample), names_to = 'year', values_to = 'values') %>%
            dplyr::mutate(values = as.numeric(as.character(values))) %>%
            dplyr::mutate(year = as.numeric(as.character(year)))

          # title of the plot
          tt = check_vars = sub("\\|.*", "", stringr::str_extract(unique(data_sample$Variable)[1], "(.*?)(\\||$)"))

          # do plot
          if (input$reg_grouping == 'Grouped Regions') {
            # consider regions in the legend
            assign(paste0('fig_',tt),
                   ggplot2::ggplot(data = data_sample, ggplot2::aes(x = year, y = values, color = Scenario, linetype = Variable, group = interaction(Scenario,Region,Variable))) +
                     ggplot2::geom_point(ggplot2::aes(shape = Region)) +
                     ggplot2::geom_line() +
                     ggplot2::scale_shape_manual('Regions', values = rep(c(1:20), times = ceiling(length(unique(data_sample$Region))/20))) +
                     ggplot2::scale_linetype_manual('Variables', values = rep(c(1:6), times = ceiling(length(unique(data_sample$Variable))/6))) +
                     ggplot2::scale_color_manual('Scenarios', values = rainbow(length(unique(data_sample$Scenario)))) +
                     ggplot2::labs(title = paste0('Evolution of ', tt), y = unique(data_sample$Unit), x = 'Year') +
                     ggplot2::guides(color = ggplot2::guide_legend(ncol = ceiling(length(unique(data_sample$Scenario))/6))) +
                     ggplot2::guides(shape = ggplot2::guide_legend(ncol = ceiling(length(unique(data_sample$Region))/7))) +
                     ggplot2::guides(linetype = ggplot2::guide_legend(ncol = ceiling(length(unique(data_sample$Variable))/8))) +
                     ggplot2::theme_bw() +
                     ggplot2::theme(legend.text = ggplot2::element_text(size = 8),legend.title = ggplot2::element_text(size = 10),
                                    legend.key.size = ggplot2::unit(0.5, "cm"))
            )
          } else {
            # facet the plot by regions
            assign(paste0('fig_',tt),
                   ggplot2::ggplot(data = data_sample, ggplot2::aes(x = year, y = values, color = Scenario, linetype = Variable, group = interaction(Scenario,Region,Variable))) +
                     ggplot2::geom_point() +
                     ggplot2::geom_line() +
                     ggplot2::facet_wrap(. ~ Region) +
                     ggplot2::scale_linetype_manual('Variables', values = rep(c(1:6), times = ceiling(length(unique(data_sample$Variable))/6))) +
                     ggplot2::scale_color_manual('Scenarios', values = rainbow(length(unique(data_sample$Scenario)))) +
                     ggplot2::labs(title = paste0('Evolution of ', tt), y = unique(data_sample$Unit), x = 'Year') +
                     ggplot2::guides(color = ggplot2::guide_legend(ncol = ceiling(length(unique(data_sample$Scenario))/8))) +
                     ggplot2::guides(linetype = ggplot2::guide_legend(ncol = ceiling(length(unique(data_sample$Variable))/5))) +
                     ggplot2::theme_bw() +
                     ggplot2::theme(legend.text = ggplot2::element_text(size = 8),legend.title = ggplot2::element_text(size = 10),
                                    legend.key.size = ggplot2::unit(0.5, "cm"), legend.position = 'bottom')
            )
          }

          local({
            my_i <- 1
            plotname <- paste("plot", my_i, sep="")

            # display plot
            output[[plotname]] <- renderPlot({
              get(paste0('fig_',tt))
            })

            # display download button
            output[[paste0("download", my_i)]] <- downloadHandler(
              filename = function() { paste0('fig_',tt,'.png') },
              content = function(file) {
                assign(paste0('fig_',tt),
                       get(paste0('fig_',tt)) +
                         ggplot2::theme(legend.key.size = ggplot2::unit(0.1, "cm")))

                # compute width
                w = 5*(max(floor(length(unique(data_sample$Scenario))/6),floor(length(unique(data_sample$Region))/7),floor(length(unique(data_sample$Variable))/8))-1)

                # save plot
                ggplot2::ggsave(file, plot = get(paste0('fig_',tt)), device = "png",
                                height = hh_dwn, width = 20+w, units = 'cm', limitsize = FALSE)
              })
          })
        } else {
          # display errors messages to the user
          output$plots <- renderUI({
            HTML(paste(errors, collapse = '<br/>'))
          })
        }
      }
      else if (input$vars_grouping == 'Ungrouped Variables') {
        # multiple plots since 'ungrouped' selected, ie., display one plot for each variable

        # check if the user's choice contains errors
        errors = check_user_choices_plot(vars = sel$vars,
                                         scen = sel$scen,
                                         years = sel$years,
                                         reg = sel$reg,
                                         grouped = FALSE)
        # if no errors
        if (length(errors) < 1) {
          # if the variables must be displayed in different plots

          # restrict the dataset to the user's choices
          data_sample = do_data_sample(sdata,
                                       sel$scen, sel$years,
                                       sel$cols, sel$vars_ini,
                                       sel$reg_ini, sel$basic_reg, sel$basic_vars)
          data_sample = tidyr::pivot_longer(data_sample, cols = 6:ncol(data_sample), names_to = 'year', values_to = 'values') %>%
            dplyr::mutate(values = as.numeric(as.character(values))) %>%
            dplyr::mutate(year = as.numeric(as.character(year)))

          # number of plots to render
          n = length(unique(data_sample$Variable))

          # compute the display and download height
          if (input$reg_grouping == 'Grouped Regions') {
            hh_disp = 400
            hh_dwn = 15
          } else {
            hh_disp = 30*(length(sel$reg))
            hh_dwn = 10 + length(sel$reg)/2
          }

          # render the plot and the corresponding download button
          output$plots <- renderUI({
            plot_output_list <- lapply(1:n, function(i) {
              plotname <- paste("plot", i, sep="")
              tagList(
                plotOutput(plotname, height = hh_disp, width = 1000),
                downloadButton(paste0("download", i), label = "Download"),
                br(),br(),br()
              )
            })

            # convert the list to a tagList to display properly the list of items
            do.call(tagList, plot_output_list)
          })

          for (i in 1:n) {
            local({
              my_i <- i
              plotname <- paste("plot", my_i, sep="")

              # do plot
              if (input$reg_grouping == 'Grouped Regions') {
                # consider regions in the legend
                assign(paste0('fig_',unique(data_sample$Variable)[my_i]),
                       ggplot2::ggplot(data = data_sample %>% dplyr::filter(Variable == unique(data_sample$Variable)[my_i]), ggplot2::aes(x = year, y = values, color = Scenario, linetype = Variable, group = interaction(Scenario,Region,Variable))) +
                         ggplot2::geom_point(ggplot2::aes(shape = Region)) +
                         ggplot2::geom_line() +
                         ggplot2::guides(linetype = 'none') +
                         ggplot2::scale_shape_manual('Regions', values = rep(c(1:20), times = ceiling(length(unique(data_sample$Region))/20))) +
                         ggplot2::scale_color_manual('Scenarios', values = rainbow(length(unique(data_sample$Scenario)))) +
                         ggplot2::labs(title = paste0('Evolution of ', unique(data_sample$Variable)[my_i]), y = unique(data_sample$Unit), x = 'Year') +
                         ggplot2::guides(color = ggplot2::guide_legend(ncol = ceiling(length(unique(data_sample$Scenario))/6))) +
                         ggplot2::guides(shape = ggplot2::guide_legend(ncol = ceiling(length(unique(data_sample$Region))/7))) +
                         ggplot2::guides(linetype = 'none') +
                         ggplot2::theme_bw() +
                         ggplot2::theme(legend.text = ggplot2::element_text(size = 8),legend.title = ggplot2::element_text(size = 10),
                                        legend.key.size = ggplot2::unit(0.7, "cm"))
                )
              } else {
                # facet the plot by regions
                assign(paste0('fig_',unique(data_sample$Variable)[my_i]),
                       ggplot2::ggplot(data = data_sample %>% dplyr::filter(Variable == unique(data_sample$Variable)[my_i]), ggplot2::aes(x = year, y = values, color = Scenario, linetype = Variable, group = interaction(Scenario,Region,Variable))) +
                         ggplot2::geom_point() +
                         ggplot2::geom_line() +
                         ggplot2::facet_wrap(. ~ Region) +
                         ggplot2::scale_color_manual('Scenarios', values = rainbow(length(unique(data_sample$Scenario)))) +
                         ggplot2::labs(title = paste0('Evolution of ', unique(data_sample$Variable)[my_i]), y = unique(data_sample$Unit), x = 'Year') +
                         ggplot2::guides(color = ggplot2::guide_legend(ncol = ceiling(length(unique(data_sample$Scenario))/3))) +
                         ggplot2::guides(linetype = 'none') +
                         ggplot2::guides(shape = 'none') +
                         ggplot2::theme_bw() +
                         ggplot2::theme(legend.text = ggplot2::element_text(size = 8),legend.title = ggplot2::element_text(size = 10),
                                        legend.key.size = ggplot2::unit(0.5, "cm"), legend.position = 'bottom')
                )
              }

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

                  # save plot
                  ggplot2::ggsave(file, plot = get(paste0('fig_',unique(data_sample$Variable)[my_i])), device = "png",
                                  height = hh_dwn, width = 20+w, units = 'cm', limitsize = FALSE)
                })
            })
          }
        } else {
          # display errors messages to the user
          output$plots <- renderUI({
            HTML(paste(errors, collapse = '<br/>'))
          })
        }
      }
    } else {
      # enable columns dropdown and set class 'enabled_cols'
      shinyjs::enable("columns_id")
      shinyjs::addClass(selector = "#columns_id", class = "enabled_cols")
      shinyjs::removeClass(selector = "#columns_id", class = "disabled_cols")
    }
  })


  # -- self-actualized data table with the user's choices
  tableData <- reactive({
    # get selected regions and variables from input
    sel_reg <<- shinyTree::get_selected(input$tree_regions, format = 'slices')
    sel_vars <<- shinyTree::get_selected(input$tree_variables, format = 'slices')

    # if it's the first load
    if (firstLoad) {
      firstLoad <<- FALSE
      # update the tableData with the user's choices but considering all possible variables and regions
      tableData <- do_data_sample(sdata,
                                  input$selected_scen,input$selected_years,
                                  input$selected_cols,unique(cols$col1),
                                  reg_cont$region, TRUE, TRUE)
    } else {
      basic_reg = 0
      basic_vars = 0

      # if it's the first time loading regions and there is a sidebarItem expanded different than regions, choose all possible regions
      if (firstReg && ((!is.null(input$sidebarItemExpanded) && input$sidebarItemExpanded != "Regions") || is.null(input$sidebarItemExpanded))) {
        sel_reg = reg_cont$region
        basic_reg = 1
      }

      # if it's the first time loading variables and there is a sidebarItem expanded different than variables, choose all possible variables
      if (firstVars && ((!is.null(input$sidebarItemExpanded) && input$sidebarItemExpanded != "Variables") || is.null(input$sidebarItemExpanded))) {
        sel_vars = unique(cols$col1)
        basic_vars = 1
      }

      # if there are no selected regions
      if (noReg) {
        sel_reg = c()
        basic_reg = 2
      }

      # if there are no selected variables
      if (noVars) {
        sel_vars = c()
        basic_vars = 2
      }

      # set firstVars and/or firstReg to FALSE if it's not the first time loading them or if their sidebarItem is expanded
      firstVars <<- ifelse(!firstVars || (firstVars && !is.null(input$sidebarItemExpanded) && input$sidebarItemExpanded == "Variables"), FALSE, TRUE)
      firstReg <<- ifelse(!firstReg || (firstReg && !is.null(input$sidebarItemExpanded) && input$sidebarItemExpanded == "Regions"), FALSE, TRUE)

      # update the tableData with the user's choices
      tableData <- do_data_sample(sdata,
                                  input$selected_scen,input$selected_years,
                                  input$selected_cols,sel_vars,
                                  sel_reg, basic_reg, basic_vars)
    }

  })


  ## -- download button
  # disable download button if "select_none" buttons are pressed
  observeEvent(c(input, input$select_none_regions,
                 input$select_none_variables), {
                   if (input$select_none_variables | input$select_none_regions) {
                     # enable the download button
                     shinyjs::disable("downloadData")
                     # change the html of the download button
                     shinyjs::html("downloadData",
                                   sprintf("<button class='btn btn-default btn-sm'>
                    <i class='fa fa-download'></i> Download </button>")
                     )
                   }
                 })

  # enable/disable download button depending on the dataset size
  observe({
    if (nrow(tableData()) == 0) {
      # if dataset empty, disable button

      # disable the download button
      shinyjs::disable("downloadData")
      # change the html of the download button
      shinyjs::html("downloadData",
                    sprintf("<button class='btn btn-default btn-sm'>
                    <i class='fa fa-download'></i> Download </button>")
      )
    } else {
      # if dataset no-empty, enable button

      # enable the download button
      shinyjs::enable("downloadData")
      # change the html of the download button
      shinyjs::html("downloadData",
                    sprintf("<button class='btn btn-default btn-sm'>
                    <i class='fa fa-download'></i> Download </button>")
      )
    }
  })

  # download data when download button clicked
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('data-', Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      write.csv(tableData(), file)
    }
  )

  # enable the downdload button on page load
  shinyjs::enable("downloadData")

  # enable Column's dropdown on page load
  shinyjs::enable("columns_id")
  shinyjs::addClass(selector = "#columns_id", class = "enabled_cols")
  shinyjs::removeClass(selector = "#columns_id", class = "disabled_cols")

  # when page refreshed, pick initial variables' values
  session$onSessionEnded(reset_first_load)

}
library(usethis)
library(magrittr)

# Define server ----------------------------------------------------------------

server <- function(input, output, session) {


  ## -- select all/none variables
  observeEvent(input$select_all_variables, {
    tree_vars <<- do_mount_tree(cols, names(cols), selec = TRUE)
    updateTree(session = getDefaultReactiveDomain(), treeId = "tree_variables", data = tree_vars)
    noVars <<- FALSE
  })
  observeEvent(input$select_none_variables, {
    tree_vars <<- do_mount_tree(cols, names(cols), selec = FALSE)
    updateTree(session = getDefaultReactiveDomain(), treeId = "tree_variables", data = tree_vars)
    noVars <<- TRUE
  })


  ## -- select all/none regions
  observeEvent(input$select_all_regions, {
    tree_reg <<- do_mount_tree(reg_cont, names(reg_cont), selec = TRUE)
    updateTree(session = getDefaultReactiveDomain(), treeId = "tree_regions", data = tree_reg)
    noReg <<- FALSE
  })
  observeEvent(input$select_none_regions, {
    tree_reg <<- do_mount_tree(reg_cont, names(reg_cont), selec = FALSE)
    updateTree(session = getDefaultReactiveDomain(), treeId = "tree_regions", data = tree_reg)
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
    tree_reg <<- change_style(input$tree_regions, 'regions')
    noReg <<- FALSE
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
    if (firstReg) {
      sel_reg_vec = reg_cont$region %>%
        tidyr::replace_na('World')
    } else {
      sel_reg = shinyTree::get_selected(input$tree_regions, format = 'slices')
      if (length(sel_reg) > 0) {
        sel_reg_vec = do_unmount_tree(sel_reg, 'regions')
        # pull the variables of the whole data with the regions restricted to the user's selection
        tmp_vars = sdata %>%
          dplyr::filter(Region %in% sel_reg_vec) %>%
          dplyr::distinct(Variable) %>%
          dplyr::pull()
        tmp_vars = all_vars[!(all_vars %in% tmp_vars)]
        tree_vars <<- change_style(input$tree_variables, 'variables', tmp_vars)
      } else {
        tree_vars <<- change_style(input$tree_variables, 'regions')
      }
    }
    noVars <<- FALSE

    if (!updatedVars) {
      # re-render tree if style modified
      output$tree_variables <- shinyTree::renderTree({
        tree_vars
      })
      updatedVars <<- TRUE
    }
  })
  observeEvent(input$sidebarItemExpanded, {
    if (input$sidebarItemExpanded == "Variables") {
      output$tree_variables <- shinyTree::renderTree({
        tree_vars
      })
    } else {
      updatedVars <<- FALSE
    }
  })


  ## -- listen to 'select_none' buttons
  observeEvent(c(input$tab_box, input,
                 input$tree_regions, input$select_none_regions,
                 input$tree_variables, input$select_none_variables), {

                   # if the selected tab is 'Data'
                   if (input$tab_box == 'Data') {

                     # enable columns dropdown and set class 'enabled_cols'
                     shinyjs::enable("columns_id")
                     shinyjs::addClass(selector = "#columns_id", class = "enabled_cols")

                     # get selected regions and variables from input
                     sel_reg <<- shinyTree::get_selected(input$tree_regions, format = 'slices')
                     sel_vars <<- shinyTree::get_selected(input$tree_variables, format = 'slices')

                     # if it's the first load
                     if (firstLoad) {
                       firstLoad <<- FALSE
                       # set selected variables and regions to all possiblities
                       sel_vars = unique(cols$col1)
                       sel_reg = reg_cont$region
                       # set basic_reg and basic_vars to TRUE
                       basic_reg = TRUE
                       basic_vars = TRUE
                     } else {
                       basic_reg = 0
                       basic_vars = 0

                       # if it's the first time loading regions and there is a sidebarItem expanded different than regions, choose all possible regions
                       if (firstReg && ((!is.null(input$sidebarItemExpanded) && input$sidebarItemExpanded != "Regions") || is.null(input$sidebarItemExpanded))) {
                         sel_reg = reg_cont$region
                         basic_reg = 1
                       }

                       # if it's the first time loading variables and there is a sidebarItem expanded different than variables, choose all possible variables
                       if (firstVars && ((!is.null(input$sidebarItemExpanded) && input$sidebarItemExpanded != "Variables") || is.null(input$sidebarItemExpanded))) {
                         sel_vars = unique(cols$col1)
                         basic_vars = 1
                       }

                       # if there are no selected regions
                       if (noReg) {
                         sel_reg = c()
                         basic_reg = 2
                       }

                       # if there are no selected variables
                       if (noVars) {
                         sel_vars = c()
                         basic_vars = 2
                       }

                       # set firstVars and/or firstReg to FALSE if it's not the first time loading them or if their sidebarItem is expanded
                       firstVars <<- ifelse(!firstVars || (firstVars && !is.null(input$sidebarItemExpanded) && input$sidebarItemExpanded == "Variables"), FALSE, TRUE)
                       firstReg <<- ifelse(!firstReg || (firstReg && !is.null(input$sidebarItemExpanded) && input$sidebarItemExpanded == "Regions"), FALSE, TRUE)
                     }

                     # render data table
                     output$datatable <- shiny::renderDataTable(
                       do_data_sample(sdata,
                                      input$selected_scen,input$selected_years,
                                      input$selected_cols,sel_vars,
                                      sel_reg, basic_reg, basic_vars),
                       options = list(pageLength = 10,
                                      scrollX = TRUE,
                                      rownames = FALSE)
                     )
                   } else if (input$tab_box == 'Plot') {
                     # if the selected tab is 'Plot'

                     # disable columns dropdown and set class 'disabled_cols'
                     shinyjs::disable("columns_id")
                     shinyjs::addClass(selector = "#columns_id", class = "disabled_cols")
                     shinyjs::removeClass(selector = "#columns_id", class = "enabled_cols")

                     # get selected regions, variables, years, and scenarios from input
                     sel = update_user_choices_plot(selected_scen = input$selected_scen,
                                                    selected_years = input$selected_years,
                                                    tree_regions = input$tree_regions,
                                                    tree_variables = input$tree_variables,
                                                    sidebarItemExpanded = input$sidebarItemExpanded)
                     # if no errors
                     if (input$vars_grouping == 'Grouped Variables'){
                       # if the variables must be displayed all in one plot

                       # check if the user's choice contains errors
                       errors = check_user_choices_plot(vars = sel$vars,
                                                        scen = sel$scen,
                                                        years = sel$years,
                                                        reg = sel$reg,
                                                        grouped = TRUE)

                       if (length(errors) < 1) {
                         # insert a single plot output object into the web page

                         # restrict the dataset to the user's choices
                         data_sample = do_data_sample(sdata,
                                                      sel$scen, sel$years,
                                                      sel$cols, sel$vars_ini,
                                                      sel$reg_ini, sel$basic_reg, sel$basic_vars)
                         data_sample = tidyr::pivot_longer(data_sample, cols = 6:ncol(data_sample), names_to = 'year', values_to = 'values') %>%
                           dplyr::mutate(values = as.numeric(as.character(values))) %>%
                           dplyr::mutate(year = as.numeric(as.character(year)))

                         # compute the display and download height
                         if (input$reg_grouping == 'Grouped Regions') {
                           hh_disp = 450
                           hh_dwn = 15
                         } else {
                           hh_disp = compute_height(sel$reg[sel$reg %in% unique(data_sample$Region)])
                           hh_dwn = hh_disp/20
                         }

                         # render the plot and the corresponding download button
                         output$plots <- renderUI({
                           plot_output_list <- lapply(1:1, function(i) {
                             plotname <- paste("plot", i, sep="")
                             tagList(
                               plotOutput(plotname, height = hh_disp, width = 1000),
                               downloadButton(paste0("download", i), label = "Download")
                             )
                           })

                           # convert the list to a tagList to display properly the list of items
                           do.call(tagList, plot_output_list)
                         })

                         # title of the plot
                         tt = check_vars = sub("\\|.*", "", stringr::str_extract(unique(data_sample$Variable)[1], "(.*?)(\\||$)"))

                         # do plot
                         if (input$reg_grouping == 'Grouped Regions') {
                           # consider regions in the legend
                           assign(paste0('fig_',tt),
                                  ggplot2::ggplot(data = data_sample, ggplot2::aes(x = year, y = values, color = Scenario, linetype = Variable, group = interaction(Scenario,Region,Variable))) +
                                    ggplot2::geom_point(ggplot2::aes(shape = Region)) +
                                    ggplot2::geom_line() +
                                    ggplot2::scale_shape_manual('Regions', values = rep(c(1:20), times = ceiling(length(unique(data_sample$Region))/20))) +
                                    ggplot2::scale_linetype_manual('Variables', values = rep(c(1:6), times = ceiling(length(unique(data_sample$Variable))/6))) +
                                    ggplot2::scale_color_manual('Scenarios', values = rainbow(length(unique(data_sample$Scenario)))) +
                                    ggplot2::labs(title = paste0('Evolution of ', tt), y = unique(data_sample$Unit), x = 'Year') +
                                    ggplot2::guides(color = ggplot2::guide_legend(ncol = ceiling(length(unique(data_sample$Scenario))/6))) +
                                    ggplot2::guides(shape = ggplot2::guide_legend(ncol = ceiling(length(unique(data_sample$Region))/7))) +
                                    ggplot2::guides(linetype = ggplot2::guide_legend(ncol = ceiling(length(unique(data_sample$Variable))/8))) +
                                    ggplot2::theme_bw() +
                                    ggplot2::theme(legend.text = ggplot2::element_text(size = 8),legend.title = ggplot2::element_text(size = 10),
                                                   legend.key.size = ggplot2::unit(0.5, "cm"))
                           )
                         } else {
                           # facet the plot by regions
                           assign(paste0('fig_',tt),
                                  ggplot2::ggplot(data = data_sample, ggplot2::aes(x = year, y = values, color = Scenario, linetype = Variable, group = interaction(Scenario,Region,Variable))) +
                                    ggplot2::geom_point() +
                                    ggplot2::geom_line() +
                                    ggplot2::facet_wrap(. ~ Region) +
                                    ggplot2::scale_linetype_manual('Variables', values = rep(c(1:6), times = ceiling(length(unique(data_sample$Variable))/6))) +
                                    ggplot2::scale_color_manual('Scenarios', values = rainbow(length(unique(data_sample$Scenario)))) +
                                    ggplot2::labs(title = paste0('Evolution of ', tt), y = unique(data_sample$Unit), x = 'Year') +
                                    ggplot2::guides(color = ggplot2::guide_legend(ncol = ceiling(length(unique(data_sample$Scenario))/8))) +
                                    ggplot2::guides(linetype = ggplot2::guide_legend(ncol = ceiling(length(unique(data_sample$Variable))/5))) +
                                    ggplot2::theme_bw() +
                                    ggplot2::theme(legend.text = ggplot2::element_text(size = 8),legend.title = ggplot2::element_text(size = 10),
                                                   legend.key.size = ggplot2::unit(0.5, "cm"), legend.position = 'bottom')
                           )
                         }

                         local({
                           my_i <- 1
                           plotname <- paste("plot", my_i, sep="")

                           # display plot
                           output[[plotname]] <- renderPlot({
                             get(paste0('fig_',tt))
                           })

                           # display download button
                           output[[paste0("download", my_i)]] <- downloadHandler(
                             filename = function() { paste0('fig_',tt,'.png') },
                             content = function(file) {
                               assign(paste0('fig_',tt),
                                      get(paste0('fig_',tt)) +
                                        ggplot2::theme(legend.key.size = ggplot2::unit(0.1, "cm")))

                               # compute width
                               w = 5*(max(floor(length(unique(data_sample$Scenario))/6),floor(length(unique(data_sample$Region))/7),floor(length(unique(data_sample$Variable))/8))-1)

                               # save plot
                               ggplot2::ggsave(file, plot = get(paste0('fig_',tt)), device = "png",
                                               height = hh_dwn, width = 20+w, units = 'cm', limitsize = FALSE)
                             })
                         })
                       } else {
                         # display errors to the user
                         output$plots <- renderUI({
                           HTML(paste(errors, collapse = '<br/>'))
                         })
                       }
                     }
                     else if (input$vars_grouping == 'Ungrouped Variables') {
                       # multiple plots since 'ungrouped' selected, ie., display one plot for each variable

                       # check if the user's choice contains errors
                       errors = check_user_choices_plot(vars = sel$vars,
                                                        scen = sel$scen,
                                                        years = sel$years,
                                                        reg = sel$reg,
                                                        grouped = FALSE)
                       # if no errors
                       if (length(errors) < 1) {
                         # if the variables must be displayed in different plots

                         # restrict the dataset to the user's choices
                         data_sample = do_data_sample(sdata,
                                                      sel$scen, sel$years,
                                                      sel$cols, sel$vars_ini,
                                                      sel$reg_ini, sel$basic_reg, sel$basic_vars)
                         data_sample = tidyr::pivot_longer(data_sample, cols = 6:ncol(data_sample), names_to = 'year', values_to = 'values') %>%
                           dplyr::mutate(values = as.numeric(as.character(values))) %>%
                           dplyr::mutate(year = as.numeric(as.character(year)))

                         # number of plots to render
                         n = length(unique(data_sample$Variable))

                         # compute the display and download height
                         if (input$reg_grouping == 'Grouped Regions') {
                           hh_disp = 450
                           hh_dwn = 15
                         } else {
                           hh_disp = compute_height(sel$reg[sel$reg %in% unique(data_sample$Region)])
                           hh_dwn = hh_disp/20
                         }

                         # render the plot and the corresponding download button
                         output$plots <- renderUI({
                           plot_output_list <- lapply(1:n, function(i) {
                             plotname <- paste("plot", i, sep="")
                             tagList(
                               plotOutput(plotname, height = hh_disp, width = 1000),
                               downloadButton(paste0("download", i), label = "Download"),
                               br(),br(),br()
                             )
                           })

                           # convert the list to a tagListto display properly the list of items
                           do.call(tagList, plot_output_list)
                         })

                         for (i in 1:n) {
                           local({
                             my_i <- i
                             plotname <- paste("plot", my_i, sep="")

                             # do plot
                             if (input$reg_grouping == 'Grouped Regions') {
                               # consider regions in the legend
                               assign(paste0('fig_',unique(data_sample$Variable)[my_i]),
                                      ggplot2::ggplot(data = data_sample %>% dplyr::filter(Variable == unique(data_sample$Variable)[my_i]), ggplot2::aes(x = year, y = values, color = Scenario, linetype = Variable, group = interaction(Scenario,Region,Variable))) +
                                        ggplot2::geom_point(ggplot2::aes(shape = Region)) +
                                        ggplot2::geom_line() +
                                        ggplot2::guides(linetype = 'none') +
                                        ggplot2::scale_shape_manual('Regions', values = rep(c(1:20), times = ceiling(length(unique(data_sample$Region))/20))) +
                                        ggplot2::scale_color_manual('Scenarios', values = rainbow(length(unique(data_sample$Scenario)))) +
                                        ggplot2::labs(title = paste0('Evolution of ', unique(data_sample$Variable)[my_i]), y = unique(data_sample$Unit), x = 'Year') +
                                        ggplot2::guides(color = ggplot2::guide_legend(ncol = ceiling(length(unique(data_sample$Scenario))/6))) +
                                        ggplot2::guides(shape = ggplot2::guide_legend(ncol = ceiling(length(unique(data_sample$Region))/7))) +
                                        ggplot2::guides(linetype = 'none') +
                                        ggplot2::theme_bw() +
                                        ggplot2::theme(legend.text = ggplot2::element_text(size = 8),legend.title = ggplot2::element_text(size = 10),
                                                       legend.key.size = ggplot2::unit(0.7, "cm"))
                               )
                             } else {
                               # facet the plot by regions
                               assign(paste0('fig_',unique(data_sample$Variable)[my_i]),
                                      ggplot2::ggplot(data = data_sample %>% dplyr::filter(Variable == unique(data_sample$Variable)[my_i]), ggplot2::aes(x = year, y = values, color = Scenario, linetype = Variable, group = interaction(Scenario,Region,Variable))) +
                                        ggplot2::geom_point() +
                                        ggplot2::geom_line() +
                                        ggplot2::facet_wrap(. ~ Region) +
                                        ggplot2::scale_color_manual('Scenarios', values = rainbow(length(unique(data_sample$Scenario)))) +
                                        ggplot2::labs(title = paste0('Evolution of ', unique(data_sample$Variable)[my_i]), y = unique(data_sample$Unit), x = 'Year') +
                                        ggplot2::guides(color = ggplot2::guide_legend(ncol = ceiling(length(unique(data_sample$Scenario))/3))) +
                                        ggplot2::guides(linetype = 'none') +
                                        ggplot2::guides(shape = 'none') +
                                        ggplot2::theme_bw() +
                                        ggplot2::theme(legend.text = ggplot2::element_text(size = 8),legend.title = ggplot2::element_text(size = 10),
                                                       legend.key.size = ggplot2::unit(0.5, "cm"), legend.position = 'bottom')
                               )
                             }

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

                                 # save plot
                                 ggplot2::ggsave(file, plot = get(paste0('fig_',unique(data_sample$Variable)[my_i])), device = "png",
                                                 height = hh_dwn, width = 20+w, units = 'cm', limitsize = FALSE)
                               })
                           })
                         }
                       } else {
                         # display errors messages to the user
                         output$plots <- renderUI({
                           HTML(paste(errors, collapse = '<br/>'))
                         })

                       }
                     }
                   }

                 })


  ## -- plot
  observe({
    # if the selected tab is 'Plot'
    if (input$tab_box == 'Plot') {

      # disable columns dropdown and set class 'disabled_cols'
      shinyjs::disable("columns_id")
      shinyjs::addClass(selector = "#columns_id", class = "disabled_cols")
      shinyjs::removeClass(selector = "#columns_id", class = "enabled_cols")

      # get selected regions, variables, years, and scenarios from input
      sel = update_user_choices_plot(selected_scen = input$selected_scen,
                                     selected_years = input$selected_years,
                                     tree_regions = input$tree_regions,
                                     tree_variables = input$tree_variables,
                                     sidebarItemExpanded = input$sidebarItemExpanded)
      # if no errors
      if (input$vars_grouping == 'Grouped Variables'){
        # if the variables must be displayed all in one plot

        # check if the user's choice contains errors
        errors = check_user_choices_plot(vars = sel$vars,
                                         scen = sel$scen,
                                         years = sel$years,
                                         reg = sel$reg,
                                         grouped = TRUE)

        if (length(errors) < 1) {
          # insert a single plot output object into the web page

          # restrict the dataset to the user's choices
          data_sample = do_data_sample(sdata,
                                       sel$scen, sel$years,
                                       sel$cols, sel$vars_ini,
                                       sel$reg_ini, sel$basic_reg, sel$basic_vars)
          data_sample = tidyr::pivot_longer(data_sample, cols = 6:ncol(data_sample), names_to = 'year', values_to = 'values') %>%
            dplyr::mutate(values = as.numeric(as.character(values))) %>%
            dplyr::mutate(year = as.numeric(as.character(year)))

          # compute the display and download height
          if (input$reg_grouping == 'Grouped Regions') {
            hh_disp = 450
            hh_dwn = 15
          } else {
            hh_disp = compute_height(sel$reg[sel$reg %in% unique(data_sample$Region)])
            hh_dwn = hh_disp/20
          }

          # render the plot and the corresponding download button
          output$plots <- renderUI({
            plot_output_list <- lapply(1:1, function(i) {
              plotname <- paste("plot", i, sep="")
              tagList(
                plotOutput(plotname, height = hh_disp, width = 1000),
                downloadButton(paste0("download", i), label = "Download")
              )
            })

            # convert the list to a tagListto display properly the list of items
            do.call(tagList, plot_output_list)
          })

          # title of the plot
          tt = check_vars = sub("\\|.*", "", stringr::str_extract(unique(data_sample$Variable)[1], "(.*?)(\\||$)"))

          # do plot
          if (input$reg_grouping == 'Grouped Regions') {
            # consider regions in the legend
            assign(paste0('fig_',tt),
                   ggplot2::ggplot(data = data_sample, ggplot2::aes(x = year, y = values, color = Scenario, linetype = Variable, group = interaction(Scenario,Region,Variable))) +
                     ggplot2::geom_point(ggplot2::aes(shape = Region)) +
                     ggplot2::geom_line() +
                     ggplot2::scale_shape_manual('Regions', values = rep(c(1:20), times = ceiling(length(unique(data_sample$Region))/20))) +
                     ggplot2::scale_linetype_manual('Variables', values = rep(c(1:6), times = ceiling(length(unique(data_sample$Variable))/6))) +
                     ggplot2::scale_color_manual('Scenarios', values = rainbow(length(unique(data_sample$Scenario)))) +
                     ggplot2::labs(title = paste0('Evolution of ', tt), y = unique(data_sample$Unit), x = 'Year') +
                     ggplot2::guides(color = ggplot2::guide_legend(ncol = ceiling(length(unique(data_sample$Scenario))/6))) +
                     ggplot2::guides(shape = ggplot2::guide_legend(ncol = ceiling(length(unique(data_sample$Region))/7))) +
                     ggplot2::guides(linetype = ggplot2::guide_legend(ncol = ceiling(length(unique(data_sample$Variable))/8))) +
                     ggplot2::theme_bw() +
                     ggplot2::theme(legend.text = ggplot2::element_text(size = 8),legend.title = ggplot2::element_text(size = 10),
                                    legend.key.size = ggplot2::unit(0.5, "cm"))
            )
          } else {
            # facet the plot by regions
            assign(paste0('fig_',tt),
                   ggplot2::ggplot(data = data_sample, ggplot2::aes(x = year, y = values, color = Scenario, linetype = Variable, group = interaction(Scenario,Region,Variable))) +
                     ggplot2::geom_point() +
                     ggplot2::geom_line() +
                     ggplot2::facet_wrap(. ~ Region) +
                     ggplot2::scale_linetype_manual('Variables', values = rep(c(1:6), times = ceiling(length(unique(data_sample$Variable))/6))) +
                     ggplot2::scale_color_manual('Scenarios', values = rainbow(length(unique(data_sample$Scenario)))) +
                     ggplot2::labs(title = paste0('Evolution of ', tt), y = unique(data_sample$Unit), x = 'Year') +
                     ggplot2::guides(color = ggplot2::guide_legend(ncol = ceiling(length(unique(data_sample$Scenario))/8))) +
                     ggplot2::guides(linetype = ggplot2::guide_legend(ncol = ceiling(length(unique(data_sample$Variable))/5))) +
                     ggplot2::theme_bw() +
                     ggplot2::theme(legend.text = ggplot2::element_text(size = 8),legend.title = ggplot2::element_text(size = 10),
                                    legend.key.size = ggplot2::unit(0.5, "cm"), legend.position = 'bottom')
            )
          }

          local({
            my_i <- 1
            plotname <- paste("plot", my_i, sep="")

            # display plot
            output[[plotname]] <- renderPlot({
              get(paste0('fig_',tt))
            })

            # display download button
            output[[paste0("download", my_i)]] <- downloadHandler(
              filename = function() { paste0('fig_',tt,'.png') },
              content = function(file) {
                assign(paste0('fig_',tt),
                       get(paste0('fig_',tt)) +
                         ggplot2::theme(legend.key.size = ggplot2::unit(0.1, "cm")))

                # compute width
                w = 5*(max(floor(length(unique(data_sample$Scenario))/6),floor(length(unique(data_sample$Region))/7),floor(length(unique(data_sample$Variable))/8))-1)

                # save plot
                ggplot2::ggsave(file, plot = get(paste0('fig_',tt)), device = "png",
                                height = hh_dwn, width = 20+w, units = 'cm', limitsize = FALSE)
              })
          })
        } else {
          # display errors messages to the user
          output$plots <- renderUI({
            HTML(paste(errors, collapse = '<br/>'))
          })
        }
      }
      else if (input$vars_grouping == 'Ungrouped Variables') {
        # multiple plots since 'ungrouped' selected, ie., display one plot for each variable

        # check if the user's choice contains errors
        errors = check_user_choices_plot(vars = sel$vars,
                                         scen = sel$scen,
                                         years = sel$years,
                                         reg = sel$reg,
                                         grouped = FALSE)
        # if no errors
        if (length(errors) < 1) {
          # if the variables must be displayed in different plots

          # restrict the dataset to the user's choices
          data_sample = do_data_sample(sdata,
                                       sel$scen, sel$years,
                                       sel$cols, sel$vars_ini,
                                       sel$reg_ini, sel$basic_reg, sel$basic_vars)
          data_sample = tidyr::pivot_longer(data_sample, cols = 6:ncol(data_sample), names_to = 'year', values_to = 'values') %>%
            dplyr::mutate(values = as.numeric(as.character(values))) %>%
            dplyr::mutate(year = as.numeric(as.character(year)))

          # number of plots to render
          n = length(unique(data_sample$Variable))

          # compute the display and download height
          if (input$reg_grouping == 'Grouped Regions') {
            hh_disp = 400
            hh_dwn = 15
          } else {
            hh_disp = compute_height(sel$reg[sel$reg %in% unique(data_sample$Region)])
            hh_dwn = hh_disp/20
          }

          # render the plot and the corresponding download button
          output$plots <- renderUI({
            plot_output_list <- lapply(1:n, function(i) {
              plotname <- paste("plot", i, sep="")
              tagList(
                plotOutput(plotname, height = hh_disp, width = 1000),
                downloadButton(paste0("download", i), label = "Download"),
                br(),br(),br()
              )
            })

            # convert the list to a tagList to display properly the list of items
            do.call(tagList, plot_output_list)
          })

          for (i in 1:n) {
            local({
              my_i <- i
              plotname <- paste("plot", my_i, sep="")

              # do plot
              if (input$reg_grouping == 'Grouped Regions') {
                # consider regions in the legend
                assign(paste0('fig_',unique(data_sample$Variable)[my_i]),
                       ggplot2::ggplot(data = data_sample %>% dplyr::filter(Variable == unique(data_sample$Variable)[my_i]), ggplot2::aes(x = year, y = values, color = Scenario, linetype = Variable, group = interaction(Scenario,Region,Variable))) +
                         ggplot2::geom_point(ggplot2::aes(shape = Region)) +
                         ggplot2::geom_line() +
                         ggplot2::guides(linetype = 'none') +
                         ggplot2::scale_shape_manual('Regions', values = rep(c(1:20), times = ceiling(length(unique(data_sample$Region))/20))) +
                         ggplot2::scale_color_manual('Scenarios', values = rainbow(length(unique(data_sample$Scenario)))) +
                         ggplot2::labs(title = paste0('Evolution of ', unique(data_sample$Variable)[my_i]), y = unique(data_sample$Unit), x = 'Year') +
                         ggplot2::guides(color = ggplot2::guide_legend(ncol = ceiling(length(unique(data_sample$Scenario))/6))) +
                         ggplot2::guides(shape = ggplot2::guide_legend(ncol = ceiling(length(unique(data_sample$Region))/7))) +
                         ggplot2::guides(linetype = 'none') +
                         ggplot2::theme_bw() +
                         ggplot2::theme(legend.text = ggplot2::element_text(size = 8),legend.title = ggplot2::element_text(size = 10),
                                        legend.key.size = ggplot2::unit(0.7, "cm"))
                )
              } else {
                # facet the plot by regions
                assign(paste0('fig_',unique(data_sample$Variable)[my_i]),
                       ggplot2::ggplot(data = data_sample %>% dplyr::filter(Variable == unique(data_sample$Variable)[my_i]), ggplot2::aes(x = year, y = values, color = Scenario, linetype = Variable, group = interaction(Scenario,Region,Variable))) +
                         ggplot2::geom_point() +
                         ggplot2::geom_line() +
                         ggplot2::facet_wrap(. ~ Region) +
                         ggplot2::scale_color_manual('Scenarios', values = rainbow(length(unique(data_sample$Scenario)))) +
                         ggplot2::labs(title = paste0('Evolution of ', unique(data_sample$Variable)[my_i]), y = unique(data_sample$Unit), x = 'Year') +
                         ggplot2::guides(color = ggplot2::guide_legend(ncol = ceiling(length(unique(data_sample$Scenario))/3))) +
                         ggplot2::guides(linetype = 'none') +
                         ggplot2::guides(shape = 'none') +
                         ggplot2::theme_bw() +
                         ggplot2::theme(legend.text = ggplot2::element_text(size = 8),legend.title = ggplot2::element_text(size = 10),
                                        legend.key.size = ggplot2::unit(0.5, "cm"), legend.position = 'bottom')
                )
              }

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

                  # save plot
                  ggplot2::ggsave(file, plot = get(paste0('fig_',unique(data_sample$Variable)[my_i])), device = "png",
                                  height = hh_dwn, width = 20+w, units = 'cm', limitsize = FALSE)
                })
            })
          }
        } else {
          # display errors messages to the user
          output$plots <- renderUI({
            HTML(paste(errors, collapse = '<br/>'))
          })
        }
      }
    } else {
      # enable columns dropdown and set class 'enabled_cols'
      shinyjs::enable("columns_id")
      shinyjs::addClass(selector = "#columns_id", class = "enabled_cols")
      shinyjs::removeClass(selector = "#columns_id", class = "disabled_cols")
    }
  })


  # -- self-actualized data table with the user's choices
  tableData <- reactive({
    # get selected regions and variables from input
    sel_reg <<- shinyTree::get_selected(input$tree_regions, format = 'slices')
    sel_vars <<- shinyTree::get_selected(input$tree_variables, format = 'slices')

    # if it's the first load
    if (firstLoad) {
      firstLoad <<- FALSE
      # update the tableData with the user's choices but considering all possible variables and regions
      tableData <- do_data_sample(sdata,
                                  input$selected_scen,input$selected_years,
                                  input$selected_cols,unique(cols$col1),
                                  reg_cont$region, TRUE, TRUE)
    } else {
      basic_reg = 0
      basic_vars = 0

      # if it's the first time loading regions and there is a sidebarItem expanded different than regions, choose all possible regions
      if (firstReg && ((!is.null(input$sidebarItemExpanded) && input$sidebarItemExpanded != "Regions") || is.null(input$sidebarItemExpanded))) {
        sel_reg = reg_cont$region
        basic_reg = 1
      }

      # if it's the first time loading variables and there is a sidebarItem expanded different than variables, choose all possible variables
      if (firstVars && ((!is.null(input$sidebarItemExpanded) && input$sidebarItemExpanded != "Variables") || is.null(input$sidebarItemExpanded))) {
        sel_vars = unique(cols$col1)
        basic_vars = 1
      }

      # if there are no selected regions
      if (noReg) {
        sel_reg = c()
        basic_reg = 2
      }

      # if there are no selected variables
      if (noVars) {
        sel_vars = c()
        basic_vars = 2
      }

      # set firstVars and/or firstReg to FALSE if it's not the first time loading them or if their sidebarItem is expanded
      firstVars <<- ifelse(!firstVars || (firstVars && !is.null(input$sidebarItemExpanded) && input$sidebarItemExpanded == "Variables"), FALSE, TRUE)
      firstReg <<- ifelse(!firstReg || (firstReg && !is.null(input$sidebarItemExpanded) && input$sidebarItemExpanded == "Regions"), FALSE, TRUE)

      # update the tableData with the user's choices
      tableData <- do_data_sample(sdata,
                                  input$selected_scen,input$selected_years,
                                  input$selected_cols,sel_vars,
                                  sel_reg, basic_reg, basic_vars)
    }

  })


  ## -- download button
  # disable download button if "select_none" buttons are pressed
  observeEvent(c(input, input$select_none_regions,
                 input$select_none_variables), {
                   if (input$select_none_variables | input$select_none_regions) {
                     # enable the download button
                     shinyjs::disable("downloadData")
                     # change the html of the download button
                     shinyjs::html("downloadData",
                                   sprintf("<button class='btn btn-default btn-sm'>
                    <i class='fa fa-download'></i> Download </button>")
                     )
                   }
                 })

  # enable/disable download button depending on the dataset size
  observe({
    if (nrow(tableData()) == 0) {
      # if dataset empty, disable button

      # disable the download button
      shinyjs::disable("downloadData")
      # change the html of the download button
      shinyjs::html("downloadData",
                    sprintf("<button class='btn btn-default btn-sm'>
                    <i class='fa fa-download'></i> Download </button>")
      )
    } else {
      # if dataset no-empty, enable button

      # enable the download button
      shinyjs::enable("downloadData")
      # change the html of the download button
      shinyjs::html("downloadData",
                    sprintf("<button class='btn btn-default btn-sm'>
                    <i class='fa fa-download'></i> Download </button>")
      )
    }
  })

  # download data when download button clicked
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('data-', Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      write.csv(tableData(), file)
    }
  )

  # enable the downdload button on page load
  shinyjs::enable("downloadData")

  # enable Column's dropdown on page load
  shinyjs::enable("columns_id")
  shinyjs::addClass(selector = "#columns_id", class = "enabled_cols")
  shinyjs::removeClass(selector = "#columns_id", class = "disabled_cols")

  # when page refreshed, pick initial variables' values
  session$onSessionEnded(reset_first_load)

}
