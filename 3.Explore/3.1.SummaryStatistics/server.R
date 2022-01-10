MdSummaryStatisticsServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      hide("MdSummaryStatisticsEditTableBtn")
      hide("MdSummaryStatisticsUndoEditBtn")
    }
  )
}