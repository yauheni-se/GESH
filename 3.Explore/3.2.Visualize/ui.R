MdVisualizeScreenUI <- function(id) {
  ns <- NS(id)
  
  tabItem(
    tabName = "Visualize",
    box(
      width = 12,
      title = "Plot configuration",
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
                 selectInput(ns("MdVisualizeScreenImportCurrentDataset"), "Select dataset",
                             choices = "")),
          column(width = 2,
                 selectInput(ns("MdVisualizeScreenPlotType"),
                             "Select plot type",
                             choices = c("scatter" = "point",
                                         "area",
                                         "line",
                                         "density",
                                         "histogram",
                                         "dotted histogram" = "dotplot",
                                         "barplot" = "col",
                                         #"pie",
                                         "box" = "boxplot",
                                         "violin"))),
           column(width = 2,
                  selectInput(ns("MdVisualizeScreenPlotBoxSize"),
                              "Plot box size",
                              choices = c("large", "big", "normal", "small"),
                              selected = "normal")),
          column(width = 2,
                 selectInput(ns("MdVisualizeScreenPlotAxisX"), "X axis", "")),
          
          column(width = 2,
                 conditionalPanel("input.MdVisualizeScreenPlotType !='density' &&
                                  input.MdVisualizeScreenPlotType !='histogram' &&
                                  input.MdVisualizeScreenPlotType !='dotplot'",
                                  ns = ns,
                                  selectInput(ns("MdVisualizeScreenPlotAxisY"), "Y axis", "")))
        )
      ),
      
      br(),
      
      box(
        width = 12,
        title = "Appearance",
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        collapsed = TRUE,

        fluidRow(
          column(width = 2,
                 colourInput(ns("MdVisualizeScreenPlotColor"), "Color", value = "default")),
          column(width = 2,
                 numericInput(ns("MdVisualizeScreenPlotSize"), "Size", value = 1, min = 0.1, max = 5, step = 0.1)),
          column(width = 2,
                 sliderInput(ns("MdVisualizeScreenPlotOpacity"), "Opacity", min = 0, max = 1, value = 1)),
          column(width = 2,
                 selectInput(ns("MdVisualizeScreenPlotTheme"),
                             "Overall theme",
                             choices = c("default" = "gray",
                                         "dark",
                                         "minimal",
                                         "extra minimal" = "void",
                                         "excel",
                                         "economist"))),
          column(width = 2,
                 conditionalPanel("input.MdVisualizeScreenPlotType == 'point'",
                                  ns = ns,
                                  selectInput(ns("MdVisualizeScreenPlotPointShape"),
                                              "Point shape",
                                              choices = c("round" = 19,
                                                          "diamond" = 18,
                                                          "square" = 15,
                                                          "triangle" = 17,
                                                          "plus" = 3,
                                                          "cross" = 4)))),
          column(width = 2,
                 conditionalPanel("input.MdVisualizeScreenPlotType == 'line' ||
                                  input.MdVisualizeScreenPlotType == 'violin' ||
                                  input.MdVisualizeScreenPlotType == 'density'",
                                  ns = ns,
                                  selectInput(ns("MdVisualizeScreenPlotLineType"),
                                              "Line type",
                                              choices = c("solid",
                                                          "dashed",
                                                          "dotted",
                                                          "dashes and dots" = "dotdash",
                                                          "long dashes" = "longdash",
                                                          "mixed dashes" = "twodash"))))
        
        )
      ),
      
      box(
        width = 12,
        title = "Labels",
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        collapsed = TRUE,
        
        fluidRow(
          column(width = 6,
                 textInput(ns("MdVisualizeScreenPlotAxisXName"), "X name", value = "")),
          
          column(width = 6,
                 textInput(ns("MdVisualizeScreenPlotAxisYName"), "Y name", value = ""))
        ),
        
        fluidRow(
          column(width = 6,
                 textInput(ns("MdVisualizeScreenPlotTitle"), "Title", value = "")),
          
          column(width = 6,
                 numericInput(ns("MdVisualizeScreenPlotFontSize"), "Font size", value = 11, min = 6, max = 24, step = 1, width = "10%"))
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
          column(width = 3,
                 selectInput(ns("MdVisualizeScreenPlotSecondaryLine"),
                             "Secondary line(s)",
                             choices = c("linear regression" = "lm",
                                         "local regression" = "loess",
                                         "mean",
                                         "minimum" = "min",
                                         "maximum" = "max",
                                         "median",
                                         "quantiles",
                                         "y = 0",
                                         "x = 0"),
                             multiple = TRUE,
                             selected = "")),
          
          column(width = 3,
                 conditionalPanel("input.MdVisualizeScreenPlotSecondaryLine == 'quantiles'",
                                  ns = ns,
                                  sliderInput(ns("MdVisualizeScreenPlotSecondaryLineQuantileProb"),
                                              "Quantile probabilities",
                                              min = 0,
                                              max = 1,
                                              value = c(0.25, 0.75),
                                              step = 0.05)))
        ),
        
        fluidRow(
          column(width = 2,
                 selectInput(ns("MdVisualizeScreenPlotGroupColorAxis"),
                             "Split plot by grouping color",
                             choices = "",
                             selected = "")),
          column(width = 2,
                 selectInput(ns("MdVisualizeScreenPlotGroupSizeAxis"),
                             "Split plot by grouping size",
                             choices = "",
                             selected = "")),
          column(width = 2,
                 selectInput(ns("MdVisualizeScreenPlotGroupGridRowAxis"),
                             "Split plot by group vertically",
                             choices = "",
                             selected = "")),
          column(width = 2,
                 selectInput(ns("MdVisualizeScreenPlotGroupGridColAxis"),
                             "Split plot by group horizontally",
                             choices = "",
                             selected = "")),
        )
      ),

      br(),
      fluidRow(
        column(width = 2,
               actionBttn(ns("MdVisualizeScreenPlotBtn"),
                          label = "Create",
                          icon = icon("tools"),
                          style = "jelly",
                          color = "success",
                          block = TRUE)),
        
        column(width = 2,
               offset = 8,
               actionBttn(ns("MdVisualizeScreenResetPlotBtn"),
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
             actionBttn(ns("MdVisualizeScreenSavePlotBtn"),
                        label = "Export plots",
                        icon = icon("file-download"),
                        style = "jelly",
                        color = "primary",
                        block = TRUE)))
  )
}