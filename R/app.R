# User friendly interface to interact with.

library(usethis)
library(magrittr)

# Load data --------------------------------------------------------------------

if (!exists('final_data')) {
  gcamreport::load_project('gas_fin_updated')
  gcamreport::read_queries(final_db_year = 2050)
}

# Define UI --------------------------------------------------------------------

ui <- fluidPage(
  sidebarLayout(

    sidebarPanel(

      checkboxGroupInput(inputId = "selected_var",
                         label = "Select variables:",
                         choices = unique(final_data$Variable),
                         selected = unique(final_data$Variable))

    ),

    mainPanel(
      DT::dataTableOutput(outputId = "datatable"),
      downloadButton("download_data", "Download data")
    )
  )
)

# Define server ----------------------------------------------------------------

server <- function(input, output, session) {

  # Data table
  output$datatable <- DT::renderDataTable({
    data_sample <- final_data
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
      write.csv(final_data, file)
    }
  )

}

# Create the Shiny app object --------------------------------------------------

shinyApp(ui = ui, server = server)
