Sys.setenv(lang = "en_US")
options(shiny.maxRequestSize = 30 * 1024^2)
rm(list = ls())

Subscripts <- as.list(list.files(path = ".", recursive = TRUE))
for (i in Subscripts) {
  if (!i %in% c("README.md",
                "text.txt",
                "RunShinyApplication.R",
                "3.Explore/3.1.SummaryStatistics/rmarkdown.Rmd",
                "3.Explore/3.1.SummaryStatistics/rmarkdown_child.Rmd",
                "3.Explore/3.2.Visualize/rmarkdown.Rmd",
                "3.Explore/3.2.Visualize/rmarkdown_child.Rmd")) {
    source(i)
  }
}


ui <- tagList(dashboardPage(
  dashboardHeader(title = "GESH"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Import", tabName = "Import", icon = icon("upload")),
      menuItem("Modify", tabName = "Modify", icon = icon("wrench")),
      menuItem("Explore", tabName = "Explore", icon = icon("lightbulb"),
               menuSubItem("Summary Statistics", tabName = "SummaryStatistics", icon = icon("search")),
               menuSubItem("Visualize", tabName = "Visualize", icon = icon("chart-bar"))
      ),
      menuItem("Model", tabName = "Model", icon = icon("layer-group")),
      menuItem("Test", tabName = "Test", icon = icon("vial")),
      menuItem("Predict", tabName = "Predict", icon = icon("chart-line")),
      menuItem("Statistical Tables", tabName = "StatisticalTables", icon = icon("table")),
      menuItem("Statistical Calculator", tabName = "StatisticalCalculator", icon = icon("calculator"))
    )
  ),
  
  dashboardBody(
    useShinyjs(),
    tabItems(
      # here will be 8 ui modules
      MdImportScreenUI("Import"),
      MdSummaryStatisticsScreenUI("SummaryStatistics"),
      MdVisualizeScreenUI("Visualize")
    )
  ),
  
  skin = "black"
))


server <- function(input, output, session) {
  # here will be 8 server modules
  MdImportScreenServer("Import")
  MdSummaryStatisticsServer("SummaryStatistics")
  MdVisualizeScreenServer("Visualize")
}

shinyApp(ui, server)