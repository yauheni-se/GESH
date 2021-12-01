MdImportScreenUI <- function(id) {
  ns <- NS(id)
  
  tabItem(
    tabName = "Import",
    box(
      width = 12,
      title = "Upload presets",
      status = "primary",
      solidHeader = TRUE,
      fluidRow(
        column(width = 6,
               selectInput(ns("MdImportScreenImportFileFormatSlct"), "Select file format",
                           choices = c("csv" = "csv",
                                       "Excel" = "xlsx",
                                       "txt" = "txt",
                                       "SPSS" = "sav",
                                       "SAS" = "sas7bdat",
                                       "STATA" = "dta",
                                       "Gretl" = "gdt"))),
        column(width = 6,
               fileInput(ns("MdImportScreenImportFileSlct"),
                         "Select file",
                         accept = c(".csv", ".tsv", ".txt", ".gdt", ".xlsx", ".xls", ".sav", ".dta", ".sas7bdat"), 
                         buttonLabel = "Browse.."))
      ),
      
      fluidRow(
        column(width = 6,
               conditionalPanel("input.MdImportScreenImportFileFormatSlct == 'csv' ||
                                input.MdImportScreenImportFileFormatSlct == 'xlsx' ||
                                input.MdImportScreenImportFileFormatSlct == 'txt'",
                                ns = ns,
                                selectInput(ns("MdImportScreenImportFileDecimalSeparatorSlct"),
                                            "Decimal separator",
                                            choices = c(".", ",")))),
        column(width = 6,
               conditionalPanel("input.MdImportScreenImportFileFormatSlct == 'csv'",
                                ns = ns,
                                selectInput(ns("MdImportScreenImportFileSeparatorSlct"),
                                            "Column separator",
                                            choices = c(",", ";", "Tab" = "\t")))),

    ),
    
    br(),
    fluidRow(
      conditionalPanel("input.MdImportScreenImportFileFormatSlct == 'xlsx'",
                       ns = ns,
                       column(width = 4,
                              prettyCheckbox(ns("MdImportScreenImportFileRangeCellBtn"),
                                             "Data starts from A1 cell",
                                             value = TRUE, 
                                             shape = "curve",
                                             bigger = TRUE,
                                             animation = "pulse",
                                             icon = icon("check"))),
                       column(width = 4,
                              conditionalPanel("input.MdImportScreenImportFileRangeCellBtn != 1",
                                               ns = ns,
                                               textAreaInput(ns("MdImportScreenImportFileRangeCellSlct"),
                                                             "Select range",
                                                             placeholder = "A1:D4",
                                                             resize = "none"))),
                       column(width = 4,
                              sliderInput(ns("MdImportScreenImportFileSheetSlct"),
                                          "Sheet to import from",
                                          min = 1,
                                          max = 255,
                                          value = 1))),
    ),
    
    
    br(),
    fluidRow(
      column(width = 6,
             offset = 3,
             actionBttn(ns("MdImportScreenImportDataBtn"),
                        label = "Import",
                        icon = icon("file-upload"),
                        style = "jelly",
                        color = "primary",block = TRUE))
        )
      ),
    
    box(width = 12,
        title = "View data",
        status = "primary",
        solidHeader = TRUE,
        fluidRow(column(width = 12,
                        dataTableOutput(ns("ImportScreenServerDatasetVisualizationTbl")))))
    )
}