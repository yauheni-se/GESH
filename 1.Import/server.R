MdImportScreenServer <- function(id) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
      
      MdImportScreenDatasetVar <- reactive({
        req(input$MdImportScreenImportDataBtn)
        FnImportScreenFileImport(input$MdImportScreenImportFileSlct$datapath,
                                 tools::file_ext(input$MdImportScreenImportFileSlct$datapath),
                                 input$MdImportScreenImportFileFormatSlct,
                                 input$MdImportScreenImportFileSeparatorSlct,
                                 input$MdImportScreenImportFileDecimalSeparatorSlct,
                                 input$MdImportScreenImportFileSheetSlct,
                                 input$MdImportScreenImportFileRangeCellSlct)
      })
      
      observeEvent(input$MdImportScreenImportFileRangeCellBtn,{
        updateTextAreaInput(session, inputId = "MdImportScreenImportFileRangeCellSlct", value = "")
      })
      output$ImportScreenServerDatasetVisualizationTbl <- renderDataTable({
        req(input$MdImportScreenImportDataBtn)
        DT::datatable(isolate(MdImportScreenDatasetVar()),
                      options = list(autoWidth = FALSE, scrollX = TRUE, scrollY = TRUE, pageLength = 6, lengthChange = FALSE))
      })
    }
  )    
}