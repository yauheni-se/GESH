MdSummaryStatisticsScreenUI <- function(id) {
  ns <- NS(id)
  
  tabItem(
    tabName = "SummaryStatistics",
    box(width = 12,
        title = "Table configuration",
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        box(
          width = 12,
          title = "Required settings",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          fluidRow(
            column(width = 2,
                   selectInput(ns("MdSummaryStatisticsScreenSelectDataset"),
                               "Dataset",
                               choices = "")),
            column(width = 2,
                   selectInput(ns("MdSummaryStatisticsScreenTableBoxSize"),
                               "Table box size",
                               choices = c("large", "big", "normal", "small"),
                               selected = "normal")),
            column(width = 2,
                   selectInput(ns("MdSummaryStatisticsScreenTableColumns"),
                               "Columns",
                               choices = "",
                               selected = "",
                               multiple = TRUE)),
            column(width = 2,
                   selectInput(ns("MdSummaryStatisticsScreenTableGroupingColumns"),
                               "Grouping columns",
                               choices = "",
                               selected = "",
                               multiple = TRUE)),
            column(width = 2,
                   selectInput(ns("MdSummaryStatisticsScreenTableStatitisticsTypes"),
                               "Statistics",
                               choices = c("mean",
                                           "median",
                                           "maximum" = "max",
                                           "minimum" = "min",
                                           "variance" = "var",
                                           "standard deviation" = "sd",
                                           "kurtosis",
                                           "skewness",
                                           "quantiles" = "quantile",
                                           "correlation" = "cor"),
                               multiple = TRUE)),
             column(width = 2,
                    selectInput(ns("MdSummaryStatisticsScreenTableDescriptivesTypes"),
                                "Descriptives",
                                choices = c("number of unique observations" = "uniqueN",
                                            "number of empty observations" = "count_na",
                                            "% rate of empty observations" = "count_na_percentage",
                                            "mode" = "find_mode",
                                            "type of variable" = "class"),
                                multiple = TRUE))
            )
        ),
        
        box(
          width = 12,
          title = "Appearance",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          collapsed = TRUE,
          fluidRow(
            column(width = 2,
                   colorPickr(ns("MdSummaryStatisticsScreenTableColor"),
                              "Background color",
                              selected = "white")),
            column(width = 2,
                   colorPickr(ns("MdSummaryStatisticsScreenTableTextColor"),
                              "Text color",
                              selected = "black")),
            
            column(width = 4,
                   textInput(ns("MdSummaryStatisticsScreenTableHeader"),
                             "Header"))
          )
        ),
        
        box(
          width = 12,
          title = "Advanced",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          collapsed = TRUE,
          fluidRow(
            column(width = 2,
                   sliderInput(ns("MdSummaryStatisticsScreenTableObservations"),
                               "Observations used",
                               min = 1,
                               max = 1,
                               value = 1,
                               step = 1)),
            column(width = 2,
                   textAreaInput(ns("MdSummaryStatisticsScreenTableFilter"),
                                 "Logical filter",
                                 placeholder = "(sex == 'female') & (income >= 10000) | (educ != 'tertiary')",
                                 resize = "vertical")),
            column(width = 2,
                   textAreaInput(ns("MdSummaryStatisticsScreenTableOwnStatistic"),
                                 "Own statistic / descriptive", 
                                 placeholder = "cov, use = 'complete.obs';mass;covariation",
                                 resize = "vertical")),
            column(width = 2,
                   sliderInput(ns("MdSummaryStatisticsScreenTableQuantilesVector"),
                               "Quantile probabilities",
                               min = 0,
                               max = 1,
                               value = c(0.25, 0.75),
                               step = 0.05)),
            column(width = 2,
                   prettyCheckbox(ns("MdSummaryStatisticsScreenTableUseGroupingColumnsWithDescriptives"),
                                  "Use grouping column(s) with descriptives",
                                  value = FALSE, 
                                  shape = "curve",
                                  bigger = TRUE,
                                  animation = "pulse",
                                  icon = icon("check")))
          )
       ),
       fluidRow(
         column(width = 2,
                actionBttn(ns("MdSummaryStatisticsScreenCreateTableBtn"),
                           label = "Create",
                           icon = icon("tools"),
                           style = "jelly",
                           color = "success",
                           block = TRUE),
                actionBttn(ns("MdSummaryStatisticsScreenEditTableBtn"),
                           label = "Edit",
                           icon = icon("edit"),
                           style = "jelly",
                           color = "success",
                           block = TRUE)),
         column(width = 2,
                actionBttn(ns("MdSummaryStatisticsScreenUndoEditBtn"),
                           label = "Cancel editing",
                           icon = icon("undo"),
                           style = "jelly",
                           color = "warning",
                           block = TRUE)),
         
         column(width = 2,
                offset = 6,
                actionBttn(ns("MdSummaryStatisticsScreenResetTableBtn"),
                           label = "Reset",
                           icon = icon("eraser"),
                           style = "jelly",
                           color = "danger",
                           block = TRUE))
       )
   ),
   
   fluidRow(
     column(width = 4,
            offset = 4,
            downloadBttn(ns("MdSummaryStatisticsScreenSaveTableBtn"),
                         label = "Export tables",
                         style = "jelly",
                         color = "primary",
                         block = TRUE))
   ),
   
   br(),
   fluidRow(id = "MdSummaryStatisticsScreenBoxTablePlaceholder")
 )
}