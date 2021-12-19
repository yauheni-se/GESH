MdVisualizeScreenServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      hide("MdVisualizeScreenEditPlotBtn")
      hide("MdVisualizeScreenCreateUndoEditBtn")

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
      observeEvent(input$MdVisualizeScreenResetPlotBtn, {
        
        updateSelectInput(session,
                          inputId = "MdVisualizeScreenPlotType",
                          selected = "point")
        
        updateSelectInput(session,
                          inputId = "MdVisualizeScreenPlotBoxSize",
                          selected = "normal")
        
        updateSelectInput(session,
                          inputId = "MdVisualizeScreenPlotAxisX",
                          choices = MdVisualizeScreenCurrentDatasetColumnNames(),
                          selected = MdVisualizeScreenCurrentDatasetColumnNameSelected())
        
        updateSelectInput(session,
                          inputId = "MdVisualizeScreenPlotAxisY",
                          choices = MdVisualizeScreenCurrentDatasetColumnNames(),
                          selected = "")
        
        updateColourInput(session,
                          inputId = "MdVisualizeScreenPlotColor",
                          value = "black")
        
        updateSelectInput(session,
                          inputId = "MdVisualizeScreenPlotColorBrew",
                          selected = "Set1")
        
        updateNumericInput(session,
                           inputId = "MdVisualizeScreenPlotSize",
                           value = 1)
        
        updateSliderInput(session,
                          inputId = "MdVisualizeScreenPlotOpacity",
                          value = 1)
        
        updateSelectInput(session,
                          inputId = "MdVisualizeScreenPlotTheme",
                          selected = "gray")
        
        updateSelectInput(session,
                          inputId = "MdVisualizeScreenPlotPointShape",
                          selected = 19)
        
        updateSelectInput(session,
                          inputId = "MdVisualizeScreenPlotLineType",
                          selected = "solid")
        
        updateTextInput(session,
                        inputId = "MdVisualizeScreenPlotAxisXName",
                        value = input$MdVisualizeScreenPlotAxisX)
        
        updateTextInput(session,
                        inputId = "MdVisualizeScreenPlotAxisYName",
                        value = input$MdVisualizeScreenPlotAxisY)
        
        updateTextInput(session,
                        inputId = "MdVisualizeScreenPlotTitle",
                        value = "")
        
        updateNumericInput(session,
                           inputId = "MdVisualizeScreenPlotFontSize",
                           value = 11)
        
        updateSelectInput(session,
                          inputId = "MdVisualizeScreenPlotSecondaryLine",
                          selected = "")
        
        updateSliderInput(session,
                          inputId = "MdVisualizeScreenPlotSecondaryLineQuantileProb",
                          value = c(0.25, 0.75))
        
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
          
          MdVisualizeScreenReactiveLstPlotIndicatorName <- paste0("MdVisualizeScreenPlot",
                                                                  MdVisualizeScreenReactiveLstPlotIndicatorVar)
          
          MdVisualizeScreenReactiveLstEditBtnIndicatorName <- paste0("MdVisualizeScreenEditBtn",
                                                                     MdVisualizeScreenReactiveLstPlotIndicatorVar)
          
          MdVisualizeScreenReactiveLstDeleteBtnIndicatorName <- paste0("MdVisualizeScreenDeleteBtn",
                                                                       MdVisualizeScreenReactiveLstPlotIndicatorVar)
          
          output[[MdVisualizeScreenReactiveLstPlotIndicatorName]] <- renderPlotly(MdVisualizeScreenCurrentPlot)
          
          MdVisualizeScreenPlotReactiveLst$Plot[[paste0("Plot", MdVisualizeScreenReactiveLstPlotIndicatorVar)]] <-
            MdVisualizeScreenCurrentPlot
          
          MdVisualizeScreenPlotReactiveLst$Dataset[[paste0("Dataset", MdVisualizeScreenReactiveLstPlotIndicatorVar)]] <-
            MdVisualizeScreenCurrentDataset()
          
          MdVisualizeScreenPlotReactiveLst$Configuration[[paste0("Config", MdVisualizeScreenReactiveLstPlotIndicatorVar)]] <- data.table(
            PlotDatasetName = input$MdVisualizeScreenSelectDataset,
            PlotType = input$MdVisualizeScreenPlotType,
            PlotBoxSize = input$MdVisualizeScreenPlotBoxSize,
            PlotAxisX = input$MdVisualizeScreenPlotAxisX,
            PlotAxisY = input$MdVisualizeScreenPlotAxisY,
            PlotColor = input$MdVisualizeScreenPlotColor,
            PlotColorBrew = input$MdVisualizeScreenPlotColorBrew,
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
            PlotGroupGridColAxis = input$MdVisualizeScreenPlotGroupGridColAxis
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
                              column(width = 1,
                                     offset = 10,
                                     actionBttn(ns(MdVisualizeScreenReactiveLstEditBtnIndicatorName),
                                                label = "",
                                                icon = icon("cogs"),
                                                style = "minimal",
                                                color = "warning",
                                                block = TRUE)),
                              column(width = 1,
                                     actionBttn(ns(MdVisualizeScreenReactiveLstDeleteBtnIndicatorName),
                                                label = "",
                                                icon = icon("trash-alt"),
                                                style = "minimal",
                                                color = "danger",
                                                block = TRUE)),
                            ),
                            
                            br(),
                            fluidRow(
                              column(width = 12,
                                     plotlyOutput(ns(MdVisualizeScreenReactiveLstPlotIndicatorName)))
                              )
                            )
                        )
            
          }
          
        })
        
      #####
      # EDIT PLOT
      #####
      
      observe({
        
        MdVisualizeScreenPlotEditBtnLogicalInduces <- vapply(names(input),
                                                             grepl,
                                                             pattern = "MdVisualizeScreenEditBtn",
                                                             FUN.VALUE = logical(1))
        # proceed only if any edit btts exist:
        if (any(MdVisualizeScreenPlotEditBtnLogicalInduces)) {
          MdVisualizeScreenPlotEditBtnNames <- names(input)[MdVisualizeScreenPlotEditBtnLogicalInduces]
          
          MdVisualizeScreenPlotDeleteBtnLogicalInduces <- vapply(names(input),
                                                                 grepl,
                                                                 pattern = "MdVisualizeScreenDeleteBtn",
                                                                 FUN.VALUE = logical(1))
          MdVisualizeScreenPlotDeleteBtnNames <- names(input)[MdVisualizeScreenPlotDeleteBtnLogicalInduces]
          
          for (i in MdVisualizeScreenPlotEditBtnNames) {
            onclick(i, {
              
              # hide all edit and delete buttons
              for (j in MdVisualizeScreenPlotEditBtnNames) {
                hide(j)
              }
              for (k in MdVisualizeScreenPlotDeleteBtnNames) {
                hide(k)
              }
              
              hide("MdVisualizeScreenCreatePlotBtn")
              #hide("MdVisualizeScreenPlotBoxSize")
              
              show("MdVisualizeScreenEditPlotBtn")
              show("MdVisualizeScreenCreateUndoEditBtn")
              
              #####
              # UPDATING INPUTS
              #####
              
              MdVisualizeScreenCurrentPlotEditId <- substr(i, nchar(i), nchar(i))
              
              MdVisualizeScreenCurrentPlotConfigurationEdit <-
                MdVisualizeScreenPlotReactiveLst$Configuration[[paste0("Config", MdVisualizeScreenCurrentPlotEditId)]]
              
              MdVisualizeScreenCurrentPlotDatasetEdit <- 
                MdVisualizeScreenPlotReactiveLst$Dataset[[paste0("Dataset", MdVisualizeScreenCurrentPlotEditId)]]
              
              MdVisualizeScreenCurrentPlotDatasetEditColumnNames <- names(MdVisualizeScreenCurrentPlotDatasetEdit)
            
              updateSelectInput(session,
                                inputId = "MdVisualizeScreenSelectDataset",
                                #choices = names(GlobalReactiveLst$ImportedDatasets),
                                selected = MdVisualizeScreenCurrentPlotConfigurationEdit$PlotDatasetName[1])
              
              updateSelectInput(session,
                                inputId = "MdVisualizeScreenPlotType",
                                selected = MdVisualizeScreenCurrentPlotConfigurationEdit$PlotType[1])
              
              updateSelectInput(session,
                                inputId = "MdVisualizeScreenPlotBoxSize",
                                selected = MdVisualizeScreenCurrentPlotConfigurationEdit$PlotBoxSize[1])
              
              updateSelectInput(session,
                                inputId = "MdVisualizeScreenPlotAxisX",
                                choices = MdVisualizeScreenCurrentPlotDatasetEditColumnNames,
                                selected = MdVisualizeScreenCurrentPlotConfigurationEdit$PlotAxisX[1])
              
              updateSelectInput(session,
                                inputId = "MdVisualizeScreenPlotAxisY",
                                choices = MdVisualizeScreenCurrentPlotDatasetEditColumnNames,
                                selected = MdVisualizeScreenCurrentPlotConfigurationEdit$PlotAxisY[1])
              
              updateColourInput(session,
                                inputId = "MdVisualizeScreenPlotColor",
                                value = MdVisualizeScreenCurrentPlotConfigurationEdit$PlotColor[1])
              
              updateSelectInput(session,
                                inputId = "MdVisualizeScreenPlotColorBrew",
                                selected = MdVisualizeScreenCurrentPlotConfigurationEdit$PlotColorBrew[1])
              
              updateNumericInput(session,
                                 inputId = "MdVisualizeScreenPlotSize",
                                 value = MdVisualizeScreenCurrentPlotConfigurationEdit$PlotSize[1])
              
              updateSliderInput(session,
                                inputId = "MdVisualizeScreenPlotOpacity",
                                value = MdVisualizeScreenCurrentPlotConfigurationEdit$PlotOpacity[1])
              
              updateSelectInput(session,
                                inputId = "MdVisualizeScreenPlotTheme",
                                selected = MdVisualizeScreenCurrentPlotConfigurationEdit$PlotTheme[1])
              
              updateSelectInput(session,
                                inputId = "MdVisualizeScreenPlotPointShape",
                                selected = MdVisualizeScreenCurrentPlotConfigurationEdit$PlotPointShape[1])
              
              updateSelectInput(session,
                                inputId = "MdVisualizeScreenPlotLineType",
                                selected = MdVisualizeScreenCurrentPlotConfigurationEdit$PlotLineType[1])
              
              updateTextInput(session,
                              inputId = "MdVisualizeScreenPlotAxisXName",
                              value = MdVisualizeScreenCurrentPlotConfigurationEdit$PlotAxisXName[1])
              
              updateTextInput(session,
                              inputId = "MdVisualizeScreenPlotAxisYName",
                              value = MdVisualizeScreenCurrentPlotConfigurationEdit$PlotAxisYName[1])
              
              updateTextInput(session,
                              inputId = "MdVisualizeScreenPlotTitle",
                              value = MdVisualizeScreenCurrentPlotConfigurationEdit$PlotTitle[1])
              
              updateNumericInput(session,
                                 inputId = "MdVisualizeScreenPlotFontSize",
                                 value = MdVisualizeScreenCurrentPlotConfigurationEdit$PlotFontSize[1])
              
              updateSelectInput(session,
                                inputId = "MdVisualizeScreenPlotSecondaryLine",
                                selected = MdVisualizeScreenCurrentPlotConfigurationEdit$PlotSecondaryLine[1])
              
              updateSliderInput(session,
                                inputId = "MdVisualizeScreenPlotSecondaryLineQuantileProb",
                                value = c(MdVisualizeScreenCurrentPlotConfigurationEdit$PlotSecondaryLineQuantileProb[1],
                                          MdVisualizeScreenCurrentPlotConfigurationEdit$PlotSecondaryLineQuantileProb[2]))
              
              updateSelectInput(session,
                                inputId = "MdVisualizeScreenPlotGroupColorAxis",
                                choices = MdVisualizeScreenCurrentPlotDatasetEditColumnNames,
                                selected = MdVisualizeScreenCurrentPlotConfigurationEdit$PlotGroupColorAxis[1])
              
              updateSelectInput(session,
                                inputId = "MdVisualizeScreenPlotGroupSizeAxis",
                                choices = MdVisualizeScreenCurrentPlotDatasetEditColumnNames,
                                selected = MdVisualizeScreenCurrentPlotConfigurationEdit$GroupSizeAxis[1])
              
              updateSelectInput(session,
                                inputId = "MdVisualizeScreenPlotGroupGridRowAxis",
                                choices = MdVisualizeScreenCurrentPlotDatasetEditColumnNames,
                                selected = MdVisualizeScreenCurrentPlotConfigurationEdit$PlotGroupGridRowAxis[1])
              
              updateSelectInput(session,
                                inputId = "MdVisualizeScreenPlotGroupGridColAxis",
                                choices = MdVisualizeScreenCurrentPlotDatasetEditColumnNames,
                                selected = MdVisualizeScreenCurrentPlotConfigurationEdit$PlotGroupGridColAxis[1])
              print(MdVisualizeScreenCurrentPlotConfigurationEdit)
            })
          }
        }

        # when clicking on any edit button:
        # extract its ID number
        # update configuration in the input$ using observeEvent to that related to that ID in reactive list;
            
        # when clicking undo:
        # show/hide buttons again (both on graphs and on the very plot config screen)
        # load the previous state of inputs
        
        # when clicking edit:
        # update plot on the existing box
        # update reactive list
        # show/hide buttons again (both on graphs and on the very plot config screen)
        # load the previous state of inputs
      })
      
      #####
      # DELETE PLOT
      #####
      
      #####
      # SAVE PLOTS
      #####
    }
  )
}