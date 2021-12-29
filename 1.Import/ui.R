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
        column(width = 2,
               selectInput(ns("MdImportScreenImportFileFormatSlct"), "File format",
                           choices = c("csv/txt" = "csv",
                                       "Excel" = "xlsx",
                                       "SPSS" = "sav",
                                       "SAS" = "sas7bdat",
                                       "STATA" = "dta",
                                       "Gretl" = "gdt"))),
        column(width = 2,
               fileInput(ns("MdImportScreenImportFileSlct"),
                         "File",
                         accept = c(".csv", ".txt", ".gdt", ".xlsx", ".xls", ".sav", ".dta", ".sas7bdat"), #, ".tsv"
                         buttonLabel = "Browse.."))
      ),
      
      fluidRow(
        column(width = 2,
               conditionalPanel("input.MdImportScreenImportFileFormatSlct == 'csv' ||
                                input.MdImportScreenImportFileFormatSlct == 'xlsx' ||
                                input.MdImportScreenImportFileFormatSlct == 'txt'",
                                ns = ns,
                                selectInput(ns("MdImportScreenImportFileDecimalSeparatorSlct"),
                                            "Decimal separator",
                                            choices = c(".", ",")))),
        column(width = 2,
               conditionalPanel("input.MdImportScreenImportFileFormatSlct == 'csv'",
                                ns = ns,
                                selectInput(ns("MdImportScreenImportFileSeparatorSlct"),
                                            "CSV column separator",
                                            choices = c(",", ";")))),
        
        conditionalPanel("input.MdImportScreenImportFileFormatSlct == 'xlsx'",
                         ns = ns,
                         column(width = 2,
                                prettyCheckbox(ns("MdImportScreenImportFileRangeCellBtn"),
                                               "Data starts from A1 cell",
                                               value = TRUE, 
                                               shape = "curve",
                                               bigger = TRUE,
                                               animation = "pulse",
                                               icon = icon("check"))),
                         column(width = 2,
                                conditionalPanel("input.MdImportScreenImportFileRangeCellBtn != 1",
                                                 ns = ns,
                                                 textInput(ns("MdImportScreenImportFileRangeCellSlct"),
                                                           "Data range",
                                                           placeholder = "A1:D4"))),
                         column(width = 2,
                                sliderInput(ns("MdImportScreenImportFileSheetSlct"),
                                            "Sheet to import from",
                                            min = 1,
                                            max = 255,
                                            value = 1))
        )

      ),
    
      br(),
      fluidRow(
        column(width = 4,
               offset = 4,
               actionBttn(ns("MdImportScreenImportDataBtn"),
                          label = "Import",
                          icon = icon("file-upload"),
                          style = "jelly",
                          color = "primary",
                          block = TRUE))
      )
    ),
    
    box(width = 12,
        title = "View data",
        status = "primary",
        solidHeader = TRUE,
        fluidRow(
          column(width = 12,
                 dataTableOutput(ns("ImportScreenServerDatasetVisualizationTbl")))
        )
    )
  )
}