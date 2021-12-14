MdVisualizeScreenServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      #####
      # UPDATE INPUT SETTINGS WHEN NEW DATASET IS UPLOADED / SELECTED
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
      
      observeEvent(input$MdVisualizeScreenPlotType, {
        
        updateSelectInput(session,
                          inputId = "MdVisualizeScreenPlotGroupColorAxis",
                          choices = MdVisualizeScreenCurrentDatasetColumnNames(),
                          selected = "")
        
        updateSelectInput(session,
                          inputId = "MdVisualizeScreenPlotGroupSizeAxis",
                          choices = MdVisualizeScreenCurrentDatasetColumnNames(),
                          selected = "")
        
        if (input$MdVisualizeScreenPlotType %chin% c("density", "histogram", "dotplot")) {
          updateSelectInput(session,
                            inputId = "MdVisualizeScreenPlotAxisY",
                            choices = MdVisualizeScreenCurrentDatasetColumnNames(),
                            selected = "")
        }
      })
        
      #####
      # UPDATE WHEN RESET BUTTON IS TRIGGERED
      #####
        
      #####
      # ADD BOX WITH PLOT WHEN CREATE BUTTON IS TRIGGERED
      #####
        
      MdVisualizeScreenReactiveLstPlotIndicatorVar <- 1
        
      # Condition if user clicks button before uploading anything
      if (MdVisualizeScreenReactiveLstPlotIndicatorVar < 1) {
        MdVisualizeScreenReactiveLstPlotIndicatorVar <- 1
      }
      
      MdVisualizeScreenPlotReactiveLst <- reactiveValues()
      
      observeEvent(input$MdVisualizeScreenCreatePlotBtn, {
        MdVisualizeScreenCurrentPlot <- FnVisualizeScreenBuildPlot(MdVisualizeScreenCurrentDataset(),
                                                                   input$MdVisualizeScreenPlotType,
                                                                   input$MdVisualizeScreenPlotAxisX,
                                                                   input$MdVisualizeScreenPlotAxisY,
                                                                   input$MdVisualizeScreenPlotColor,
                                                                   input$MdVisualizeScreenPlotSize,
                                                                   input$MdVisualizeScreenPlotOpacity,
                                                                   input$MdVisualizeScreenPlotTheme,
                                                                   input$MdVisualizeScreenPlotPointShape,
                                                                   input$MdVisualizeScreenPlotLineType,
                                                                   input$MdVisualizeScreenPlotAxisXName,
                                                                   input$MdVisualizeScreenPlotAxisYName,
                                                                   input$MdVisualizeScreenPlotTitle,
                                                                   input$MdVisualizeScreenPlotFontSize,
                                                                   input$MdVisualizeScreenPlotSecondaryLine,
                                                                   input$MdVisualizeScreenPlotSecondaryLineQuantileProb,
                                                                   input$MdVisualizeScreenPlotGroupColorAxis,
                                                                   input$MdVisualizeScreenPlotGroupSizeAxis,
                                                                   input$MdVisualizeScreenPlotGroupGridRowAxis,
                                                                   input$MdVisualizeScreenPlotGroupGridColAxis,
                                                                   input$MdVisualizeScreenPlotColorBrew)
        if (!is.null(MdVisualizeScreenCurrentPlot)) {
          
          MdVisualizeScreenReactiveLstPlotIndicatorName <- paste0("MdVisualizeScreenPlot", MdVisualizeScreenReactiveLstPlotIndicatorVar)
          
          output[[MdVisualizeScreenReactiveLstPlotIndicatorName]] <- renderPlotly(MdVisualizeScreenCurrentPlot)
            
          MdVisualizeScreenPlotReactiveLst$Plot[[MdVisualizeScreenReactiveLstPlotIndicatorVar]] <- MdVisualizeScreenCurrentPlot
            
          MdVisualizeScreenPlotReactiveLst$Configuration[[MdVisualizeScreenReactiveLstPlotIndicatorVar]] <- data.table(
            Dataset = MdVisualizeScreenCurrentDataset(),
            PlotType = input$MdVisualizeScreenPlotType,
            PlotAxisX = input$MdVisualizeScreenPlotAxisX,
            PlotAxisY = input$MdVisualizeScreenPlotAxisY,
            PlotColor = input$MdVisualizeScreenPlotColor,
            PlotSize = input$MdVisualizeScreenPlotSize,
            PlotOpacity = input$MdVisualizeScreenPlotOpacity,
            PlotTheme = input$MdVisualizeScreenPlotTheme,
            PlotPointShape = input$MdVisualizeScreenPlotPointShape,
            PlotLineType = input$MdVisualizeScreenPlotLineType,
            PlotAxisXName = input$MdVisualizeScreenPlotAxisXName,
            PlotAxisYName = input$MdVisualizeScreenPlotAxisYName,
            PlotTitle = input$MdVisualizeScreenPlotTitle,
            PlotFontSize = input$MdVisualizeScreenPlotFontSize,
            PlotSecondaryLines = input$MdVisualizeScreenPlotSecondaryLine,
            PlotSecondaryLineQuantileProbs = input$MdVisualizeScreenPlotSecondaryLineQuantileProb,
            PlotGroupColorAxis = input$MdVisualizeScreenPlotGroupColorAxis,
            PlotGroupSizeAxis = input$MdVisualizeScreenPlotGroupSizeAxis,
            PlotGroupGridRowAxis = input$MdVisualizeScreenPlotGroupGridRowAxis,
            PlotGroupGridColAxis = input$MdVisualizeScreenPlotGroupGridColAxis,
            PlotColorBrew = input$MdVisualizeScreenPlotColorBrew
          )
            
          MdVisualizeScreenReactiveLstPlotIndicatorVar <<- MdVisualizeScreenReactiveLstPlotIndicatorVar + 1
          
          
          MdVisualizeScreenPlotBoxWidth <- switch(input$MdVisualizeScreenPlotBoxSize, "large" = 12, "big" = 9, "normal" = 6, "small" = 3)
          
          insertUI(selector = "#MdVisualizeScreenBoxPlotPlaceholder",
                   where = "afterBegin",
                   session = session,
                   ui = box(title = "",
                            status = "primary",
                            solidHeader = TRUE,
                            collapsible = TRUE,
                            width = MdVisualizeScreenPlotBoxWidth,
                            fluidRow(
                              column(width = 12,
                                     plotlyOutput(ns(MdVisualizeScreenReactiveLstPlotIndicatorName)))
                              )
                            )
                        )
            
          }
          
        })
        
        #####
        # CHANGE PLOT
        #####
        
    }
  )
}