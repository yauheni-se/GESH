MdVisualizeScreenServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {

     #####
     # UPDATE INPUT SETTINGS WHEN NEW DATASET IS UPLOADED / NEW DATASET IS SELECTED
     #####
      
     MdVisualizeScreenTriggerDataset <- reactive({GlobalReactiveLst$ImportedDatasets[[names(GlobalReactiveLst$ImportedDatasets)[1]]]}) 
      
     observeEvent(MdVisualizeScreenTriggerDataset(),{
        updateSelectInput(session,
                          inputId = "MdVisualizeScreenSelectDataset",
                          choices = names(GlobalReactiveLst$ImportedDatasets),
                          selected = names(GlobalReactiveLst$ImportedDatasets)[length(GlobalReactiveLst$ImportedDatasets)])
     })
     
     MdVisualizeScreenCurrentDataset <- reactive({GlobalReactiveLst$ImportedDatasets[[input$MdVisualizeScreenSelectDataset]]})
     
     MdVisualizeScreenCurrentDatasetColumnNames <- reactive({names(MdVisualizeScreenCurrentDataset())})
     
     MdVisualizeScreenCurrentDatasetColumnNameSelected <- reactive({MdVisualizeScreenCurrentDatasetColumnNames()[1]})
     
     
     observeEvent(MdVisualizeScreenCurrentDataset(), {
     
        updateSelectInput(session,
                          inputId = "MdVisualizeScreenPlotAxisX",
                          choices = MdVisualizeScreenCurrentDatasetColumnNames(),
                          selected = MdVisualizeScreenCurrentDatasetColumnNameSelected())
        
        updateSelectInput(session,
                          inputId = "MdVisualizeScreenPlotAxisY",
                          choices = MdVisualizeScreenCurrentDatasetColumnNames(),
                          selected = "")
        
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
        
        observeEvent(input$MdVisualizeScreenPlotAxisX, {
          updateTextInput(session,
                          inputId = "MdVisualizeScreenPlotAxisXName",
                          value = input$MdVisualizeScreenPlotAxisX)
        })
        
        observeEvent(input$MdVisualizeScreenPlotAxisY, {
          updateTextInput(session,
                          inputId = "MdVisualizeScreenPlotAxisYName",
                          value = input$MdVisualizeScreenPlotAxisY)
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