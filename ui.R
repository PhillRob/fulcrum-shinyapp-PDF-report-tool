library(ggplot2); source("helper.R")

fluidPage(theme="styles.css",
    titlePanel("Wadi Al Sulai: Construction Management"),
    # Create a new Row in the UI for selectInputs
    fluidRow(
        column(4,
               selectInput("issue",
                           "Issue Type",
                           c("All",
                             na.omit(unique(as.character(loadData()$Issue)))))
        ),
        column(4,
               selectInput("status",
                           "Status",
                           c("All",
                             unique(as.character(loadData()$Status))))
        ),
        column(4,
               selectInput("responsible",
                           "Responsible",
                           c("All",
                             unique(as.character(loadData()$Responsible))))
        ),
        column(4,
               dateRangeInput('dateRange',
                       label = 'Date created:',format = "dd/mm/yy",
                       start = min(loadData()$Created), end = max(loadData()$Created)
                       )
        ),
        # radioButtons('format', 'Document format', c('PDF', 'HTML', 'Word'),
        #              inline = TRUE),
        column(4,
               downloadButton('downloadListReport', label = 'List Report')
        ),
        column(4,
               downloadButton('downloadImgReport',label = 'Image Report')
        ),
        column(4,
               p("Last updated: "),
               textOutput("timenow")
        )
    ),
    # Create a new row for the table.
    fluidRow(
        DT::dataTableOutput("table")
    )
)

