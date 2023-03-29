library(usethis)
library(magrittr)
library(shiny)
library(shinyTree)

# Define UI --------------------------------------------------------------------

ui <- fluidPage(
  titlePanel("GCAMREPORT user interface",
             windowTitle = 'gcamreport'),

  sidebarLayout(
    sidebarPanel(
      actionButton(inputId = 'hidePanel',
                   label = 'Hide panel'),

      conditionalPanel(
        condition = "input.hidePanel%2 == 0",


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

        br(),br(),
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

        br(),br(),
        checkboxGroupInput(inputId = "selected_years",
                           label = "Select years:",
                           inline = TRUE,
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
      )
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

