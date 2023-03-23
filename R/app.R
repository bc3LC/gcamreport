# User friendly interface to interact with.

library(usethis)
library(magrittr)

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
  tree_cols = dput(cols)

  # tree.data <-    list(
  #   'Recreational - Fishing' = structure(list(
  #     'Boat' = structure(list(
  #       'Cray pot'= structure("",sttype="default",sticon="glyphicon glyphicon-record"),
  #       'Hand/rod & line' = structure("",sttype="default",sticon="glyphicon glyphicon-record"),
  #       'Cray loop' = structure("",sttype="default",sticon="glyphicon glyphicon-record"),
  #       'Drop net' = structure("",sttype="default",sticon="glyphicon glyphicon-record"),
  #       'Spear' = structure("",sttype="default",sticon="glyphicon glyphicon-record"),
  #       'Other' = structure("",sttype="default",sticon="glyphicon glyphicon-record")),
  #       sttype="default",stopened=FALSE,sticon="glyphicon glyphicon-plus", stdisabled=TRUE)),
  #     sttype="default",stopened=FALSE,sticon="glyphicon glyphicon-plus")
  # )

  # tree_cols <- list(
  #   'Agricultural Demand' = structure(list(
  #     'Crops' = structure(list(
  #       'Energy'= structure("",sttype="default",sticon="glyphicon glyphicon-record"),
  #       'Food' = structure("",sttype="default",sticon="glyphicon glyphicon-record"),
  #       'Feed' = structure("",sttype="default",sticon="glyphicon glyphicon-record"),
  #       'Other' = structure("",sttype="default",sticon="glyphicon glyphicon-record")),
  #       sttype="default",stopened=FALSE,sticon="glyphicon glyphicon-plus", stdisabled=TRUE)),
  #     sttype="default",stopened=FALSE,sticon="glyphicon glyphicon-plus")
  # )


}

# Define extra functions -------------------------------------------------------

do_data_sample <- function(sel_scen,sel_years,sel_cols,sel_type_vars,
                           sel_col0,sel_col1) {
  data_sample = sdata %>%
    filter(Scenario %in% sel_scen)

  if (sel_type_vars == 'See all options') {
    data_sample = data_sample %>%
      filter(Variable %in% sel_col0)
  } else {
    data_sample = data_sample %>%
      filter(col1 %in% sel_col1)
  }

  data_sample = data_sample[, colnames(data_sample) %in% c(sel_years, sel_cols)]

  return(data_sample)
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

      radioButtons(inputId = "select_type_var",
                   label = "Select variables:",
                   choices = c('By type',
                               'See all options'),
                   selected = 'By type'),

      conditionalPanel(condition = "input.select_type_var == 'See all options'",
                       checkboxGroupInput(inputId = "selected_col0",
                                          label = "Select variables:",
                                          choices = unique(sdata$Variable),
                                          selected = unique(sdata$Variable))
                       ),

      # conditionalPanel(condition = "input.select_type_var == 'By type'",
      #                  shinyTree("tree_cols",
      #                            checkbox = TRUE,
      #                            search = TRUE)
      #                  ),

      conditionalPanel(condition = "input.select_type_var == 'By type'",
                       checkboxGroupInput(inputId = "selected_col1",
                                          label = "Select variables:",
                                          choices = unique(sdata$col1),
                                          selected = unique(sdata$col1))
                       ),

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
      textOutput(outputId = "debug"),
      DT::dataTableOutput(outputId = "datatable"),
      downloadButton("download_data", "Download data")
    )
  )
)

# Define server ----------------------------------------------------------------

server <- function(input, output, session) {

  # Text to debug
  output$debug <- renderText({
    # paste("Debug: sel_varss = ", input$selected_col0, "\n","\n",
    #       "sel_col1 = ", input$selected_col1, "\n","\n",
    #       "length sel_col0 = ", length(input$selected_col0))
  })


  # Data table
  output$datatable <- DT::renderDataTable({
    data_sample = do_data_sample(input$selected_scen,input$selected_years,input$selected_cols,input$select_type_var,
                                 input$selected_col0,input$selected_col1)
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
