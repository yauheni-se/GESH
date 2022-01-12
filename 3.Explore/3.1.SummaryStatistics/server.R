MdSummaryStatisticsScreenServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      hide("MdSummaryStatisticsScreenEditTableBtn")
      hide("MdSummaryStatisticsScreenUndoEditBtn")
      
      
      #####
      # UPDATE INPUTS WHEN A NEW DATASET IS UPLOADED / SELECTED
      #####
      
      MdSummaryStatisticsScreenTriggerDataset <- reactive({
        GlobalReactiveLst$ImportedDatasets[[names(GlobalReactiveLst$ImportedDatasets)[1]]]
      })
      
      observeEvent(MdSummaryStatisticsScreenTriggerDataset(), {
        updateSelectInput(session,
                          inputId = "MdSummaryStatisticsScreenSelectDataset",
                          choices = names(GlobalReactiveLst$ImportedDatasets),
                          selected = names(GlobalReactiveLst$ImportedDatasets)[length(GlobalReactiveLst$ImportedDatasets)])
      })
      
      MdSummaryStatisticsScreenCurrentDataset <- reactive({
        GlobalReactiveLst$ImportedDatasets[[input$MdSummaryStatisticsScreenSelectDataset]]
      })
      
      MdSummaryStatisticsScreenCurrentDatasetColumnNames <- reactive({
        names(MdSummaryStatisticsScreenCurrentDataset())
      })
      
      MdSummaryStatisticsScreenCurrentDatasetColumnNameSelected <- reactive({
        MdSummaryStatisticsScreenCurrentDatasetColumnNames()[1]
      })
      
      
      observeEvent(MdSummaryStatisticsScreenCurrentDataset(), {
        
        updateSelectInput(session,
                          inputId = "MdSummaryStatisticsScreenTableColumns",
                          choices = MdSummaryStatisticsScreenCurrentDatasetColumnNames(),
                          selected = MdSummaryStatisticsScreenCurrentDatasetColumnNameSelected())
        
        updateSelectInput(session,
                          inputId = "MdSummaryStatisticsScreenTableGroupingColumns",
                          choices = MdSummaryStatisticsScreenCurrentDatasetColumnNames(),
                          selected = "")
        
        updateSliderInput(session,
                          "MdSummaryStatisticsScreenTableObservations",
                          min = 1,
                          max = nrow(MdSummaryStatisticsScreenCurrentDataset()),
                          value = nrow(MdSummaryStatisticsScreenCurrentDataset()),
                          step = 1)
     })
      
      
      #####
      # CLICKING RESET BUTTON
      #####
      observeEvent(input$MdSummaryStatisticsScreenResetTableBtn, {
       
        updateSelectInput(session,
                          inputId = "MdSummaryStatisticsScreenTableColumns",
                          choices = MdSummaryStatisticsScreenCurrentDatasetColumnNames(),
                          selected = MdSummaryStatisticsScreenCurrentDatasetColumnNameSelected())
       
        updateSelectInput(session,
                          inputId = "MdSummaryStatisticsScreenTableGroupingColumns",
                          choices = MdSummaryStatisticsScreenCurrentDatasetColumnNames(),
                          selected = "")
       
        updateSelectInput(session,
                          "MdSummaryStatisticsScreenTableBoxSize",
                          choices = c("large", "big", "normal", "small"),
                          selected = "normal")
       
        updateSelectInput(session,
                          "MdSummaryStatisticsScreenTableStatitisticsTypes",
                          selected = "")
      
        updateSelectInput(session,
                          "MdSummaryStatisticsScreenTableDescriptivesTypes",
                          selected = "")
       
        updateColorPickr(session,
                         "MdSummaryStatisticsScreenTableColor",
                         value = "white")
       
        updateColorPickr(session,
                         "MdSummaryStatisticsScreenTableTextColor",
                         value = "black")
       
        updateTextInput(session,
                        "MdSummaryStatisticsScreenTableHeader",
                        value = "")
       
        updateSliderInput(session,
                          "MdSummaryStatisticsScreenTableObservations",
                          min = 1,
                          max = nrow(MdSummaryStatisticsScreenCurrentDataset()),
                          value = nrow(MdSummaryStatisticsScreenCurrentDataset()),
                          step = 1)
       
        updateTextAreaInput(session,
                            "MdSummaryStatisticsScreenTableFilter",
                            value = "")
       
        updateTextAreaInput(session,
                            "MdSummaryStatisticsScreenTableOwnStatistic",
                            value = "")
       
        updateSliderInput(session,
                          "MdSummaryStatisticsScreenTableQuantilesVector",
                          min = 0,
                          max = 1,
                          value = c(0.25, 0.75),
                          step = 0.05)
       
        updatePrettyCheckbox(session,
                             "MdSummaryStatisticsScreenTableUseGroupingColumnsWithDescriptives",
                             value = FALSE)
       
      })
      
      
      #####
      # CLICKING CREATE BUTTON
      #####
      
      MdSummaryStatisticsScreenReactiveLstTableIndicatorVar <- 1
      
      # Condition if user clicks button before uploading anything
      if (MdSummaryStatisticsScreenReactiveLstTableIndicatorVar < 1) {
        MdSummaryStatisticsScreenReactiveLstTableIndicatorVar <- 1
      }
      
      MdSummaryStatisticsScreenTableReactiveLst <- reactiveValues()
      
      observeEvent(input$MdSummaryStatisticsScreenCreateTableBtn, {
        
        MdSummaryStatisticsScreenCurrentTable <- FnSummaryStatisticsScreenBuildTable(
          MdSummaryStatisticsScreenCurrentDataset(),
          input$MdSummaryStatisticsScreenTableColumns,
          input$MdSummaryStatisticsScreenTableGroupingColumns,
          input$MdSummaryStatisticsScreenTableStatitisticsTypes,
          input$MdSummaryStatisticsScreenTableDescriptivesTypes,
          input$MdSummaryStatisticsScreenTableObservations,
          nrow(MdSummaryStatisticsScreenCurrentDataset()),
          input$MdSummaryStatisticsScreenTableFilter,
          input$MdSummaryStatisticsScreenTableOwnStatistic,
          input$MdSummaryStatisticsScreenTableUseGroupingColumnsWithDescriptives,
          input$MdSummaryStatisticsScreenTableQuantilesVector,
          input$MdSummaryStatisticsScreenTableColor,
          input$MdSummaryStatisticsScreenTableTextColor,
          input$MdSummaryStatisticsScreenTableHeader)
        
        if (!is.null(MdSummaryStatisticsScreenCurrentTable)) {
          
          MdSummaryStatisticsScreenReactiveLstTableIndicatorName <- paste0("MdSummaryStatisticsScreenTable",
                                                                           MdSummaryStatisticsScreenReactiveLstTableIndicatorVar)
          
          MdSummaryStatisticsScreenReactiveLstEditBtnIndicatorName <- paste0("MdSummaryStatisticsScreenEditBtn",
                                                                             MdSummaryStatisticsScreenReactiveLstTableIndicatorVar)
          
          MdSummaryStatisticsScreenReactiveLstDeleteBtnIndicatorName <- paste0("MdSummaryStatisticsScreenDeleteBtn",
                                                                               MdSummaryStatisticsScreenReactiveLstTableIndicatorVar)
          
          output[[MdSummaryStatisticsScreenReactiveLstTableIndicatorName]] <- renderDT(MdSummaryStatisticsScreenCurrentTable)
          
          MdSummaryStatisticsScreenTableReactiveLst$Table[[paste0("Table", MdSummaryStatisticsScreenReactiveLstTableIndicatorVar)]] <-
            MdSummaryStatisticsScreenCurrentTable
          
          MdSummaryStatisticsScreenTableReactiveLst$Dataset[[paste0("Dataset", MdSummaryStatisticsScreenReactiveLstTableIndicatorVar)]] <-
            MdSummaryStatisticsScreenCurrentDataset()
          
          MdSummaryStatisticsScreenTableReactiveLst$Configuration[[paste0("Config",
                                                                          MdSummaryStatisticsScreenReactiveLstTableIndicatorVar)]] <- 
            data.table(
              TableDatasetName = input$MdSummaryStatisticsScreenSelectDataset,
              TableBoxSize = input$MdSummaryStatisticsScreenTableBoxSize,
              TableColumns = input$MdSummaryStatisticsScreenTableColumns,
              TableGroupingColumns = input$MdSummaryStatisticsScreenTableGroupingColumns,
              TableStatitisticsTypes = input$MdSummaryStatisticsScreenTableStatitisticsTypes,
              TableDescriptivesTypes = input$MdSummaryStatisticsScreenTableDescriptivesTypes,
              TableObservations = input$MdSummaryStatisticsScreenTableObservations,
              TableLength = nrow(MdSummaryStatisticsScreenCurrentDataset()),
              TableFilter = input$MdSummaryStatisticsScreenTableFilter,
              TableOwnStatistic = input$MdSummaryStatisticsScreenTableOwnStatistic,
              TableUseGroupingColumnsWithDescriptives = input$MdSummaryStatisticsScreenTableUseGroupingColumnsWithDescriptives,
              TableQuantilesVector = input$MdSummaryStatisticsScreenTableQuantilesVector,
              TableColor = input$MdSummaryStatisticsScreenTableColor,
              TableTextColor = input$MdSummaryStatisticsScreenTableTextColor,
              TableHeader = input$MdSummaryStatisticsScreenTableHeader)
          
          MdSummaryStatisticsScreenReactiveLstTableIndicatorVar <<- MdSummaryStatisticsScreenReactiveLstTableIndicatorVar + 1
          
          
          MdSummaryStatisticsScreenTableBoxWidth <- switch(input$MdSummaryStatisticsScreenTableBoxSize,
                                                           "large" = 12,
                                                           "big" = 9,
                                                           "normal" = 6,
                                                           "small" = 3)
          
          insertUI(selector = "#MdSummaryStatisticsScreenBoxTablePlaceholder",
                   where = "afterBegin",
                   session = session,
                   ui = box(id = paste0("MdSummaryStatisticsScreenTableBox",
                                        MdSummaryStatisticsScreenReactiveLstTableIndicatorVar-1),
                            title = "",
                            status = "primary",
                            solidHeader = TRUE,
                            collapsible = TRUE,
                            width = MdSummaryStatisticsScreenTableBoxWidth,
                            fluidRow(
                              column(width = 1,
                                     offset = 9,
                                     actionBttn(ns(MdSummaryStatisticsScreenReactiveLstEditBtnIndicatorName),
                                                label = "",
                                                icon = icon("cogs"),
                                                style = "minimal",
                                                color = "warning",
                                                block = FALSE)),
                              column(width = 1,
                                     actionBttn(ns(MdSummaryStatisticsScreenReactiveLstDeleteBtnIndicatorName),
                                                label = "",
                                                icon = icon("trash-alt"),
                                                style = "minimal",
                                                color = "danger",
                                                block = FALSE))
                            ),
                            
                            br(),
                            fluidRow(
                              column(width = 12,
                                     DTOutput(ns(MdSummaryStatisticsScreenReactiveLstTableIndicatorName)))
                            )
                   )
          )
          
        }
        
      })

      #####
      # CLICKING ANY COG / TRASH BUTTON IN THE TableS'S BOX
      #####
      MdSummaryStatisticsScreenOldEditBtnValues <<- list()
      MdSummaryStatisticsScreenOldDeleteBtnValues <<- list()
      
      observe({
        MdSummaryStatisticsScreenTableEditBtnLogicalInduces <- vapply(names(input),
                                                                      grepl,
                                                                      pattern = "MdSummaryStatisticsScreenEditBtn",
                                                                      FUN.VALUE = logical(1))
        # proceed only if any edit btts exist:
        if (any(MdSummaryStatisticsScreenTableEditBtnLogicalInduces)) {
          
          MdSummaryStatisticsScreenTableEditBtnNames <<- names(input)[MdSummaryStatisticsScreenTableEditBtnLogicalInduces]
          
          MdSummaryStatisticsScreenTableDeleteBtnLogicalInduces <- vapply(names(input),
                                                                 grepl,
                                                                 pattern = "MdSummaryStatisticsScreenDeleteBtn",
                                                                 FUN.VALUE = logical(1))
          MdSummaryStatisticsScreenTableDeleteBtnNames <<- names(input)[MdSummaryStatisticsScreenTableDeleteBtnLogicalInduces]
          
          #####
          # WHEN TRASH BUTTON TRIGGERED
          #####
          
          for (i in MdSummaryStatisticsScreenTableDeleteBtnNames) {
            
            MdSummaryStatisticsScreenNewDeleteBtnValue <- input[[i]]
            
            if (is.null(MdSummaryStatisticsScreenOldDeleteBtnValues[[i]])) {
              MdSummaryStatisticsScreenOldDeleteBtnValues[[i]] <<- 0
            }
            
            if (MdSummaryStatisticsScreenNewDeleteBtnValue != MdSummaryStatisticsScreenOldDeleteBtnValues[[i]]) {
              
              MdSummaryStatisticsScreenOldDeleteBtnValues[[i]] <<- MdSummaryStatisticsScreenOldDeleteBtnValues[[i]] +
                as.integer(input[[i]])
              
              MdSummaryStatisticsScreenCurrentTableDeleteId <<- str_extract(i, "MdSummaryStatisticsScreenDeleteBtn[0-9]") %>%
                substr(., nchar(.), nchar(.))
              
              removeUI(paste0("#MdSummaryStatisticsScreenTableBox", MdSummaryStatisticsScreenCurrentTableDeleteId))
              
              MdSummaryStatisticsScreenTableReactiveLst$Table[[paste0("Table",
                                                                      MdSummaryStatisticsScreenCurrentTableDeleteId)]] <- NULL
              
              MdSummaryStatisticsScreenTableReactiveLst$Dataset[[paste0("Dataset",
                                                                        MdSummaryStatisticsScreenCurrentTableDeleteId)]] <- NULL
              
              MdSummaryStatisticsScreenTableReactiveLst$Configuration[[paste0("Config",
                                                                              MdSummaryStatisticsScreenCurrentTableDeleteId)]] <- NULL
              
              
            }
          }
          
          #####
          # WHEN COG BUTTON TRIGGERED
          #####
          
          # save the state of inputs before editing:
          MdSummaryStatisticsScreenInputsBeforeEdit <<- data.table(
            TableDatasetName = isolate(input$MdSummaryStatisticsScreenSelectDataset),
            TableBoxSize = isolate(input$MdSummaryStatisticsScreenTableBoxSize),
            TableColumns = isolate(input$MdSummaryStatisticsScreenTableColumns),
            TableGroupingColumns = isolate(input$MdSummaryStatisticsScreenTableGroupingColumns),
            TableStatitisticsTypes = isolate(input$MdSummaryStatisticsScreenTableStatitisticsTypes),
            TableDescriptivesTypes = isolate(input$MdSummaryStatisticsScreenTableDescriptivesTypes),
            TableObservations = isolate(input$MdSummaryStatisticsScreenTableObservations),
            TableLength = isolate(nrow(MdSummaryStatisticsScreenCurrentDataset())),
            TableFilter = isolate(input$MdSummaryStatisticsScreenTableFilter),
            TableOwnStatistic = isolate(input$MdSummaryStatisticsScreenTableOwnStatistic),
            TableUseGroupingColumnsWithDescriptives = isolate(input$MdSummaryStatisticsScreenTableUseGroupingColumnsWithDescriptives),
            TableQuantilesVector = isolate(input$MdSummaryStatisticsScreenTableQuantilesVector),
            TableColor = isolate(input$MdSummaryStatisticsScreenTableColor),
            TableTextColor = isolate(input$MdSummaryStatisticsScreenTableTextColor),
            TableHeader = isolate(input$MdSummaryStatisticsScreenTableHeader))
          
          for (i in MdSummaryStatisticsScreenTableEditBtnNames) {
            
            MdSummaryStatisticsScreenNewEditBtnValue <- input[[i]]
            
            if (is.null(MdSummaryStatisticsScreenOldEditBtnValues[[i]])) {
              MdSummaryStatisticsScreenOldEditBtnValues[[i]] <<- 0
            }
            
            if (MdSummaryStatisticsScreenNewEditBtnValue > MdSummaryStatisticsScreenOldEditBtnValues[[i]]) {
              
              MdSummaryStatisticsScreenOldEditBtnValues[[i]] <<- MdSummaryStatisticsScreenNewEditBtnValue
              
              observeEvent(input[[i]], {
                
                # hide all edit and delete buttons
                for (j in MdSummaryStatisticsScreenTableEditBtnNames) {
                  hide(j)
                }
                for (k in MdSummaryStatisticsScreenTableDeleteBtnNames) {
                  hide(k)
                }
                
                hide("MdSummaryStatisticsScreenCreateTableBtn")
                hide("MdSummaryStatisticsScreenTableBoxSize")
                
                show("MdSummaryStatisticsScreenEditTableBtn")
                show("MdSummaryStatisticsScreenUndoEditBtn")
                
                #####
                # UPDATING INPUTS
                #####
                
                MdSummaryStatisticsScreenCurrentTableEditId <<- str_extract(i, "MdSummaryStatisticsScreenEditBtn[0-9]") %>% 
                  substr(., nchar(.), nchar(.))
                
                MdSummaryStatisticsScreenCurrentTableConfigurationEdit <-
                  MdSummaryStatisticsScreenTableReactiveLst$Configuration[[paste0("Config", MdSummaryStatisticsScreenCurrentTableEditId)]]
                
                MdSummaryStatisticsScreenCurrentTableDatasetEdit <- 
                  MdSummaryStatisticsScreenTableReactiveLst$Dataset[[paste0("Dataset", MdSummaryStatisticsScreenCurrentTableEditId)]]
                
                MdSummaryStatisticsScreenCurrentTableDatasetEditColumnNames <- names(MdSummaryStatisticsScreenCurrentTableDatasetEdit)
                
                updateSelectInput(session,
                                  inputId = "MdSummaryStatisticsScreenSelectDataset",
                                  selected = MdSummaryStatisticsScreenCurrentTableConfigurationEdit$TableDatasetName[1])
                
                updateSelectInput(session,
                                  inputId = "MdSummaryStatisticsScreenTableBoxSize",
                                  selected = MdSummaryStatisticsScreenCurrentTableConfigurationEdit$TableBoxSize[1])
                
                updateSelectInput(session,
                                  inputId = "MdSummaryStatisticsScreenTableColumns",
                                  choices = MdSummaryStatisticsScreenCurrentTableDatasetEditColumnNames,
                                  selected = MdSummaryStatisticsScreenCurrentTableConfigurationEdit$TableColumns)
                
                updateSelectInput(session,
                                  inputId = "MdSummaryStatisticsScreenTableGroupingColumns",
                                  choices = MdSummaryStatisticsScreenCurrentTableDatasetEditColumnNames,
                                  selected = MdSummaryStatisticsScreenCurrentTableConfigurationEdit$TableGroupingColumns)

                updateSelectInput(session,
                                  "MdSummaryStatisticsScreenTableStatitisticsTypes",
                                  selected = MdSummaryStatisticsScreenCurrentTableConfigurationEdit$TableStatitisticsTypes)
                
                updateSelectInput(session,
                                  "MdSummaryStatisticsScreenTableDescriptivesTypes",
                                  selected = MdSummaryStatisticsScreenCurrentTableConfigurationEdit$TableDescriptivesTypes)
                
                updateColorPickr(session,
                                 "MdSummaryStatisticsScreenTableColor",
                                 value = MdSummaryStatisticsScreenCurrentTableConfigurationEdit$TableColor[1])
                
                updateColorPickr(session,
                                 "MdSummaryStatisticsScreenTableTextColor",
                                 value = MdSummaryStatisticsScreenCurrentTableConfigurationEdit$TableTextColor[1])
                
                updateTextInput(session,
                                "MdSummaryStatisticsScreenTableHeader",
                                value = MdSummaryStatisticsScreenCurrentTableConfigurationEdit$TableHeader[1])
                
                updateSliderInput(session,
                                  "MdSummaryStatisticsScreenTableObservations",
                                  min = 1,
                                  max = MdSummaryStatisticsScreenCurrentTableConfigurationEdit$TableLength[1],
                                  value = MdSummaryStatisticsScreenCurrentTableConfigurationEdit$TableObservations[1],
                                  step = 1)
                
                updateTextAreaInput(session,
                                    "MdSummaryStatisticsScreenTableFilter",
                                    value = MdSummaryStatisticsScreenCurrentTableConfigurationEdit$TableFilter[1])
                
                updateTextAreaInput(session,
                                    "MdSummaryStatisticsScreenTableOwnStatistic",
                                    value = MdSummaryStatisticsScreenCurrentTableConfigurationEdit$TableOwnStatistic[1])
                
                updateSliderInput(session,
                                  "MdSummaryStatisticsScreenTableQuantilesVector",
                                  min = 0,
                                  max = 1,
                                  value = c(MdSummaryStatisticsScreenCurrentTableConfigurationEdit$TableQuantilesVector[1],
                                            MdSummaryStatisticsScreenCurrentTableConfigurationEdit$TableQuantilesVector[2]),
                                  step = 0.05)
                
                updatePrettyCheckbox(
                  session,
                  "MdSummaryStatisticsScreenTableUseGroupingColumnsWithDescriptives",
                  value = MdSummaryStatisticsScreenCurrentTableConfigurationEdit$TableUseGroupingColumnsWithDescriptives[1]
                )
              })
              break
            }
          }
          
        }
      })
      
      
      #####
      # CLICKING CANCEL EDITING BUTTON
      #####
      
      observeEvent(input$MdSummaryStatisticsScreenUndoEditBtn, {
        
        for (j in MdSummaryStatisticsScreenTableEditBtnNames) {
          show(j)
        }
        for (k in MdSummaryStatisticsScreenTableDeleteBtnNames) {
          show(k)
        }
        
        show("MdSummaryStatisticsScreenCreateTableBtn")
        show("MdSummaryStatisticsScreenTableBoxSize")
        
        hide("MdSummaryStatisticsScreenEditTableBtn")
        hide("MdSummaryStatisticsScreenUndoEditBtn")
        
        #####
        # LOAD PREVIOUS STATE OF INPUTS
        #####
        
        updateSelectInput(session,
                          inputId = "MdSummaryStatisticsScreenSelectDataset",
                          selected = MdSummaryStatisticsScreenInputsBeforeEdit$TableDatasetName[1])
        
        updateSelectInput(session,
                          inputId = "MdSummaryStatisticsScreenTableBoxSize",
                          selected = MdSummaryStatisticsScreenInputsBeforeEdit$TableBoxSize[1])
        
        updateSelectInput(session,
                          inputId = "MdSummaryStatisticsScreenTableColumns",
                          selected = MdSummaryStatisticsScreenInputsBeforeEdit$TableColumns)
        
        updateSelectInput(session,
                          inputId = "MdSummaryStatisticsScreenTableGroupingColumns",
                          selected = MdSummaryStatisticsScreenInputsBeforeEdit$TableGroupingColumns)
        
        updateSelectInput(session,
                          "MdSummaryStatisticsScreenTableStatitisticsTypes",
                          selected = MdSummaryStatisticsScreenInputsBeforeEdit$TableStatitisticsTypes)
        
        updateSelectInput(session,
                          "MdSummaryStatisticsScreenTableDescriptivesTypes",
                          selected = MdSummaryStatisticsScreenInputsBeforeEdit$TableDescriptivesTypes)
        
        updateColorPickr(session,
                         "MdSummaryStatisticsScreenTableColor",
                         value = MdSummaryStatisticsScreenInputsBeforeEdit$TableColor[1])
        
        updateColorPickr(session,
                         "MdSummaryStatisticsScreenTableTextColor",
                         value = MdSummaryStatisticsScreenInputsBeforeEdit$TableTextColor[1])
        
        updateTextInput(session,
                        "MdSummaryStatisticsScreenTableHeader",
                        value = MdSummaryStatisticsScreenInputsBeforeEdit$TableHeader[1])
        
        updateSliderInput(session,
                          "MdSummaryStatisticsScreenTableObservations",
                          min = 1,
                          max = MdSummaryStatisticsScreenInputsBeforeEdit$TableLength[1],
                          value = MdSummaryStatisticsScreenInputsBeforeEdit$TableObservations[1],
                          step = 1)
        
        updateTextAreaInput(session,
                            "MdSummaryStatisticsScreenTableFilter",
                            value = MdSummaryStatisticsScreenInputsBeforeEdit$TableFilter[1])
        
        updateTextAreaInput(session,
                            "MdSummaryStatisticsScreenTableOwnStatistic",
                            value = MdSummaryStatisticsScreenInputsBeforeEdit$TableOwnStatistic[1])
        
        updateSliderInput(session,
                          "MdSummaryStatisticsScreenTableQuantilesVector",
                          min = 0,
                          max = 1,
                          value = c(MdSummaryStatisticsScreenInputsBeforeEdit$TableQuantilesVector[1],
                                    MdSummaryStatisticsScreenInputsBeforeEdit$TableQuantilesVector[2]),
                          step = 0.05)
        
        updatePrettyCheckbox(
          session,
          "MdSummaryStatisticsScreenTableUseGroupingColumnsWithDescriptives",
          value = MdSummaryStatisticsScreenInputsBeforeEdit$TableUseGroupingColumnsWithDescriptives[1]
        )
      })
      
      
      #####
      # CLICKING EDIT BUTTON
      #####
      
      observeEvent(input$MdSummaryStatisticsScreenEditTableBtn, {
        
        MdSummaryStatisticsScreenCurrentTable <- FnSummaryStatisticsScreenBuildTable(
          MdSummaryStatisticsScreenCurrentDataset(),
          input$MdSummaryStatisticsScreenTableColumns,
          input$MdSummaryStatisticsScreenTableGroupingColumns,
          input$MdSummaryStatisticsScreenTableStatitisticsTypes,
          input$MdSummaryStatisticsScreenTableDescriptivesTypes,
          input$MdSummaryStatisticsScreenTableObservations,
          nrow(MdSummaryStatisticsScreenCurrentDataset()),
          input$MdSummaryStatisticsScreenTableFilter,
          input$MdSummaryStatisticsScreenTableOwnStatistic,
          input$MdSummaryStatisticsScreenTableUseGroupingColumnsWithDescriptives,
          input$MdSummaryStatisticsScreenTableQuantilesVector,
          input$MdSummaryStatisticsScreenTableColor,
          input$MdSummaryStatisticsScreenTableTextColor,
          input$MdSummaryStatisticsScreenTableHeader)
        
        if (!is.null(MdSummaryStatisticsScreenCurrentTable)) {
          
          #####
          # UPDATE GLOBAL Table LIST AND Table INSIDE THE BOX
          #####
          
          output[[paste0("MdSummaryStatisticsScreenTable",
                         MdSummaryStatisticsScreenCurrentTableEditId)]] <- renderDT(MdSummaryStatisticsScreenCurrentTable)
          
          MdSummaryStatisticsScreenTableReactiveLst$Table[[paste0("Table", MdSummaryStatisticsScreenCurrentTableEditId)]] <-
            MdSummaryStatisticsScreenCurrentTable
          
          MdSummaryStatisticsScreenTableReactiveLst$Dataset[[paste0("Dataset", MdSummaryStatisticsScreenCurrentTableEditId)]] <-
            MdSummaryStatisticsScreenCurrentDataset()
          
          MdSummaryStatisticsScreenTableReactiveLst$Configuration[[paste0("Config",
                                                                          MdSummaryStatisticsScreenCurrentTableEditId)]] <- 
            data.table(
              TableDatasetName = input$MdSummaryStatisticsScreenSelectDataset,
              TableBoxSize = input$MdSummaryStatisticsScreenTableBoxSize,
              TableColumns = input$MdSummaryStatisticsScreenTableColumns,
              TableGroupingColumns = input$MdSummaryStatisticsScreenTableGroupingColumns,
              TableStatitisticsTypes = input$MdSummaryStatisticsScreenTableStatitisticsTypes,
              TableDescriptivesTypes = input$MdSummaryStatisticsScreenTableDescriptivesTypes,
              TableObservations = input$MdSummaryStatisticsScreenTableObservations,
              TableLength = nrow(MdSummaryStatisticsScreenCurrentDataset()),
              TableFilter = input$MdSummaryStatisticsScreenTableFilter,
              TableOwnStatistic = input$MdSummaryStatisticsScreenTableOwnStatistic,
              TableUseGroupingColumnsWithDescriptives = input$MdSummaryStatisticsScreenTableUseGroupingColumnsWithDescriptives,
              TableQuantilesVector = input$MdSummaryStatisticsScreenTableQuantilesVector,
              TableColor = input$MdSummaryStatisticsScreenTableColor,
              TableTextColor = input$MdSummaryStatisticsScreenTableTextColor,
              TableHeader = input$MdSummaryStatisticsScreenTableHeader)
          
          #####
          # SHOW / HIDE BUTTONS
          #####
          
          for (j in MdSummaryStatisticsScreenTableEditBtnNames) {
            show(j)
          }
          for (k in MdSummaryStatisticsScreenTableDeleteBtnNames) {
            show(k)
          }
          
          show("MdSummaryStatisticsScreenCreateTableBtn")
          show("MdSummaryStatisticsScreenTableBoxSize")
          
          hide("MdSummaryStatisticsScreenEditTableBtn")
          hide("MdSummaryStatisticsScreenUndoEditBtn")
          
          #####
          # LOAD PREVIOUS STATE OF INPUTS
          #####
          updateSelectInput(session,
                            inputId = "MdSummaryStatisticsScreenSelectDataset",
                            selected = MdSummaryStatisticsScreenInputsBeforeEdit$TableDatasetName[1])
          
          updateSelectInput(session,
                            inputId = "MdSummaryStatisticsScreenTableBoxSize",
                            selected = MdSummaryStatisticsScreenInputsBeforeEdit$TableBoxSize[1])
          
          updateSelectInput(session,
                            inputId = "MdSummaryStatisticsScreenTableColumns",
                            selected = MdSummaryStatisticsScreenInputsBeforeEdit$TableColumns)
          
          updateSelectInput(session,
                            inputId = "MdSummaryStatisticsScreenTableGroupingColumns",
                            selected = MdSummaryStatisticsScreenInputsBeforeEdit$TableGroupingColumns)
          
          updateSelectInput(session,
                            "MdSummaryStatisticsScreenTableStatitisticsTypes",
                            selected = MdSummaryStatisticsScreenInputsBeforeEdit$TableStatitisticsTypes)
          
          updateSelectInput(session,
                            "MdSummaryStatisticsScreenTableDescriptivesTypes",
                            selected = MdSummaryStatisticsScreenInputsBeforeEdit$TableDescriptivesTypes)
          
          updateColorPickr(session,
                           "MdSummaryStatisticsScreenTableColor",
                           value = MdSummaryStatisticsScreenInputsBeforeEdit$TableColor[1])
          
          updateColorPickr(session,
                           "MdSummaryStatisticsScreenTableTextColor",
                           value = MdSummaryStatisticsScreenInputsBeforeEdit$TableTextColor[1])
          
          updateTextInput(session,
                          "MdSummaryStatisticsScreenTableHeader",
                          value = MdSummaryStatisticsScreenInputsBeforeEdit$TableHeader[1])
          
          updateSliderInput(session,
                            "MdSummaryStatisticsScreenTableObservations",
                            min = 1,
                            max = MdSummaryStatisticsScreenInputsBeforeEdit$TableLength[1],
                            value = MdSummaryStatisticsScreenInputsBeforeEdit$TableObservations[1],
                            step = 1)
          
          updateTextAreaInput(session,
                              "MdSummaryStatisticsScreenTableFilter",
                              value = MdSummaryStatisticsScreenInputsBeforeEdit$TableFilter[1])
          
          updateTextAreaInput(session,
                              "MdSummaryStatisticsScreenTableOwnStatistic",
                              value = MdSummaryStatisticsScreenInputsBeforeEdit$TableOwnStatistic[1])
          
          updateSliderInput(session,
                            "MdSummaryStatisticsScreenTableQuantilesVector",
                            min = 0,
                            max = 1,
                            value = c(MdSummaryStatisticsScreenInputsBeforeEdit$TableQuantilesVector[1],
                                      MdSummaryStatisticsScreenInputsBeforeEdit$TableQuantilesVector[2]),
                            step = 0.05)
          
          updatePrettyCheckbox(
            session,
            "MdSummaryStatisticsScreenTableUseGroupingColumnsWithDescriptives",
            value = MdSummaryStatisticsScreenInputsBeforeEdit$TableUseGroupingColumnsWithDescriptives[1]
          )
          
          show_toast("Table updated", type = "info", timer = 6000, position = "top-end")
        }
      })
      
      
      #####
      # CLICKING EXPORT PLOTS BUTTON
      #####
      output$MdSummaryStatisticsScreenSaveTableBtn <- 
        downloadHandler(
          filename = function() {
            paste0("GESH_summary_statistics_", Sys.Date(), ".html")
          },
          content = function(file) {
            Tables <- MdSummaryStatisticsScreenTableReactiveLst$Table
            Configs <- MdSummaryStatisticsScreenTableReactiveLst$Configuration
            rmarkdown::render("./3.Explore/3.1.SummaryStatistics/rmarkdown.Rmd", output_file = file)
          }
        )
    }
  )
}