library(DT)
source("helper.R")

function(input, output, session) {
    # Get Data
    updatefreqencyinsec = 600000
    apidata <- reactivePoll(updatefreqencyinsec, session,
                              checkFunc = function() {max(loadData()$Updated)},
                              valueFunc = function() {loadData()}
    )
    time.now <- reactivePoll(updatefreqencyinsec, session,
                             checkFunc = function() {max(loadData()$Updated)},
                             valueFunc = function() {return(format(Sys.time()))}
    )
    output$timenow <-renderText( time.now())
    # Filter data based on selections
    datain <- reactive({
        data <- apidata()
        if (input$issue != "All") {
            data <- (data[data$Issue %in% input$issue,])
        }
        if (input$status != "All") {
            data <- data[data$Status == input$status,]
        }
        if (input$responsible != "All") {
            data <- data[data$Responsible == input$responsible,]
        }
        if (!is.na(input$dateRange[1])){
            data <- data[(data$Created >= input$dateRange[1]) & (data$Created <= input$dateRange[2]),]
        }
        data
    })
    
    output$table <- DT::renderDataTable(DT::datatable({
        datain()[,1:8]}, rownames = FALSE
    ))
    
    # report generation
    ## list report
    output$downloadListReport <- downloadHandler(
        filename = function() {
            paste0('WS-CM-list-report-',Sys.Date(), '.pdf')
        },
        
        content = function(file) {
            src <- normalizePath('report.Rmd')
            src1 <- normalizePath('WS-logo.png')
            src2 <- normalizePath('bpla-logo-220.png')
            owd <- setwd(tempdir())
            on.exit(setwd(owd))
            file.copy(src, 'report.Rmd', overwrite = TRUE)
            file.copy(src1,'WS-logo.png')
            file.copy(src2,'bpla-logo-220.png')
            library(rmarkdown)
            out <- render('report.Rmd',output_format=pdf_document())
            file.rename(out, file)
        }
    )
    ## Image Report
    output$downloadImgReport <- downloadHandler(
        filename = function() {
            paste0('WS-CM-image-report-',Sys.Date(), '.pdf')
        },
        
        content = function(file) {
            src <- normalizePath('img-report.Rmd')
            src1 <- normalizePath('WS-logo.png')
            src2 <- normalizePath('bpla-logo-220.png')
            owd <- setwd(tempdir())
            on.exit(setwd(owd))
            file.copy(src, 'img-report.Rmd', overwrite = TRUE)
            file.copy(src1,'WS-logo.png')
            file.copy(src2,'bpla-logo-220.png')
            library(rmarkdown)
            out <- render('img-report.Rmd',output_format=pdf_document())
            file.rename(out, file)
        }
    )
}
