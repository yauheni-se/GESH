MdVisualizeScreenUI <- function(id) {
  ns <- NS(id)
  
  tabItem(
    tabName = "Visualize",
    box(
      width = 12,
      title = "Plot configuration",
      status = "primary",
      solidHeader = TRUE,
      fluidRow(
        column(width = 6,
               selectInput(ns("MdVisualizeScreenImportCurrentDataset"), "Select dataset",
                           choices = "")),
        column(width = 6,
               selectInput(ns("MdVisualizeScreenPlotType"),
                           "Select plot type",
                           choices = c("scatter" = "point",
                                       "area",
                                       "line",
                                       "density",
                                       "histogram",
                                       "dotted histogram" = "dotplot",
                                       "barplot" = "col",
                                       "pie",
                                       "box" = "boxplot",
                                       "violin")))
      ),
      
      br(),
      fluidRow(
        column(width = 3,
               actionBttn(ns("MdVisualizeScreenPlotBtn"),
                          label = "Create",
                          icon = icon("tools"),
                          style = "jelly",
                          color = "success",
                          block = TRUE)),
        
        column(width = 3,
               offset = 6,
               actionBttn(ns("MdVisualizeScreenResetPlotBtn"),
                          label = "Reset",
                          icon = icon("eraser"),
                          style = "jelly",
                          color = "danger",
                          block = TRUE))
      )
               
    )
  )
}