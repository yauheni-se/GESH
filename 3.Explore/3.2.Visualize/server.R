MdVisualizeScreenServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
     MdVisualizeScreenGlobalReactiveLstImportedDatasetNames <- reactive({names(GlobalReactiveLst$ImportedDatasets)})

     GlobalReactiveLstImportedDatasetCurrentName <- reactive({MdVisualizeScreenGlobalReactiveLstImportedDatasetNames()[1]})
     
     MdVisualizeScreenCurrentDataset <<- reactive({GlobalReactiveLst$ImportedDatasets[[GlobalReactiveLstImportedDatasetCurrentName()]]})
     
     MdVisualizeScreenCurrentDatasetColumnNames <- reactive({names(MdVisualizeScreenCurrentDataset())})
     
     MdVisualizeScreenCurrentDatasetColumnNameSelected <- reactive({MdVisualizeScreenCurrentDatasetColumnNames()[1]})
     
     #####
     # UPDATE INPUT SETTINGS WHEN NEW DATASET IS UPLOADED
     #####
     
     observe({
        updateSelectInput(session,
                          inputId = "MdVisualizeScreenSelectDataset",
                          choices = MdVisualizeScreenGlobalReactiveLstImportedDatasetNames(),
                          selected = GlobalReactiveLstImportedDatasetCurrentName())

        updateSelectInput(session,
                          inputId = "MdVisualizeScreenPlotAxisX",
                          choices = MdVisualizeScreenCurrentDatasetColumnNames(),
                          selected = MdVisualizeScreenCurrentDatasetColumnNameSelected())
        
        updateSelectInput(session,
                          inputId = "MdVisualizeScreenPlotAxisY",
                          choices = MdVisualizeScreenCurrentDatasetColumnNames(),
                          selected = "")
        
        updateTextInput(session,
                        inputId = "MdVisualizeScreenPlotAxisXName",
                        value = input$MdVisualizeScreenPlotAxisX)
        
        updateTextInput(session,
                        inputId = "MdVisualizeScreenPlotAxisYName",
                        value = input$MdVisualizeScreenPlotAxisY)
        
        updateSelectInput(session,
                          inputId = "MdVisualizeScreenPlotGroupColorAxis",
                          choices = MdVisualizeScreenCurrentDatasetColumnNames(),
                          selected = "")
        
        updateSelectInput(session,
                          inputId = "MdVisualizeScreenPlotGroupSizeAxis",
                          choices = MdVisualizeScreenCurrentDatasetColumnNames(),
                          selected = "")
        
        updateSelectInput(session,
                          inputId = "MdVisualizeScreenPlotGroupGridRowAxis",
                          choices = MdVisualizeScreenCurrentDatasetColumnNames(),
                          selected = "")
        
        updateSelectInput(session,
                          inputId = "MdVisualizeScreenPlotGroupGridColAxis",
                          choices = MdVisualizeScreenCurrentDatasetColumnNames(),
                          selected = "")
        })
        
        #####
        # UPDATE WHEN RESET BUTTON IS TRIGGERED
        #####
        
        #####
        # ADD BOX WITH PLOT WHEN CREATE BUTTON IS TRIGGERED
        #####
        
        #####
        # CHANGE PLOT
        #####
        
    }
  )
}