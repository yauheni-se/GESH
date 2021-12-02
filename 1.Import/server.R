MdImportScreenServer <- function(id) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
      
      GlobalReactiveLst <<- reactiveValues()
      
      GlobalReactiveLstDatasetIndicatorVar <- 1
      
      # Condition if user clicks button before uploading anything
      if (GlobalReactiveLstDatasetIndicatorVar < 1) {
        GlobalReactiveLstDatasetIndicatorVar <- 1
      }
      
      observeEvent(input$MdImportScreenImportDataBtn, {
        
        req(input$MdImportScreenImportFileSlct)
        cat(GlobalReactiveLstDatasetIndicatorVar)
        MdImportScreenCurrentDatasetNameVar <- file_path_sans_ext(input$MdImportScreenImportFileSlct$name)
        
        # In case of any error in data import, current dataset will be set to NULL and toast message arises
        MdImportScreenCurrentDataset <- FnImportScreenFileImport(input$MdImportScreenImportFileSlct$datapath,
                                                                 file_ext(input$MdImportScreenImportFileSlct$datapath),
                                                                 input$MdImportScreenImportFileFormatSlct,
                                                                 input$MdImportScreenImportFileSeparatorSlct,
                                                                 input$MdImportScreenImportFileDecimalSeparatorSlct,
                                                                 input$MdImportScreenImportFileSheetSlct,
                                                                 input$MdImportScreenImportFileRangeCellSlct)
        print(MdImportScreenCurrentDataset)
          
        # Condition not to add current dataset to the datasets list if it was NULL
        if (!is.null(MdImportScreenCurrentDataset)) {
          if (MdImportScreenCurrentDatasetNameVar %chin% names(GlobalReactiveLst$ImportedDatasets)) {
            GlobalReactiveLst$ImportedDatasets[[MdImportScreenCurrentDatasetNameVar]] <<- MdImportScreenCurrentDataset
            
            show_toast("Dataset with such a name has been already imported. Rewriting",
                         type = "warning",
                         position = "top-end",
                         timer = 6000)
          } else {
            GlobalReactiveLst$ImportedDatasets[[GlobalReactiveLstDatasetIndicatorVar]] <<- MdImportScreenCurrentDataset
            
            names(GlobalReactiveLst$ImportedDatasets)[GlobalReactiveLstDatasetIndicatorVar] <- MdImportScreenCurrentDatasetNameVar

            output$ImportScreenServerDatasetVisualizationTbl <- renderDataTable({
              req(input$MdImportScreenImportDataBtn)
              DT::datatable(isolate(MdImportScreenCurrentDataset),
              options = list(autoWidth = FALSE, scrollX = TRUE, scrollY = TRUE, pageLength = 6, lengthChange = FALSE))
            })
            GlobalReactiveLstDatasetIndicatorVar <<- GlobalReactiveLstDatasetIndicatorVar + 1
          }
        }
      })
      
      observeEvent(input$MdImportScreenImportFileRangeCellBtn, {
        updateTextAreaInput(session, inputId = "MdImportScreenImportFileRangeCellSlct", value = "")
      })
    }
  )    
}