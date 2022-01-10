count_na <- function(x) {
  return(sum(is.na(x)))
}
  
count_na_percentage <- function(x) {
  return(sum(is.na(x)) / length(x)  * 100)
}
  
FnSummaryStatisticsScreenBuildTable <- function(Dataset,
                                                TableColumns,
                                                TableGroupingColumns,
                                                TableStatitisticsTypes,
                                                TableDescriptivesTypes,
                                                TableColor,
                                                TableHeatmap,
                                                TableObservations,
                                                TableFilter,
                                                TableOwnStatistic,
                                                TableUseGroupingColumnsWithDescriptives,
                                                TableQuantilesVector) {
  #####
  # INTERNAL VARIABLES CREATION, RECODING, ERROR CHECKING
  #####
  if (TableColumns == "") {
    show_toast("No columns selected",
                type = "error",
                position = "top-end",
                timer = 6000)
    return()
  }
    
  if (any(TableGroupingColumns %chin% TableColumns)) {
    show_toast("Grouping in both grouping and selected lists",
                type = "error",
                position = "top-end",
                timer = 6000)
    return()
  }
  
  if (any(str_detect(TableFilter, TableGroupingColumns))) {
    show_toast("Grouping columns cannot be used as filter", type = "error", position = "top-end", timer = 6000)
    return()
  }
    
  if (TableGroupingColumns != "") {
    if (!all(Dataset[, apply(.SD, 2, class), .SDcols = TableGroupingColumns] %chin% c("character", "factor"))) {
      show_toast("Grouping columns must be of type character or factor", type = "error", position = "top-end", timer = 6000)
      return()
    }
  }
    
  if (TableFilter != "") {
    TableFilterError <- tryCatch({
      Dataset[eval(parse(text = TableFilter)), ] 
      },
      error = function(c) {
        show_toast("Incorrect filter expression provided", type = "error", position = "top-end", timer = 6000)
        return()
      }
    )
      
    if (is.null(TableFilterError)) {
      return()
    }
  }
  
  
  TableOwnStatisticList <- unlist(str_split(TableOwnStatistic, ";"))
  
  TableOwnStatisticColumns <- unlist(str_split(TableOwnStatisticList[2], ","))
  
  if (TableOwnStatistic != "") {
    TableOwnStatisticError <- tryCatch({
        
      if (TableGroupingColumns != "") {
          
        if (TableFilter != "") {
          TableOwnStatistic <- Dataset[eval(parse(text = TableFilter)),
                                       eval(parse(text = TableOwnStatisticList[1])),
                                       .SDcols = TableOwnStatisticColumns,
                                       by = TableGroupingColumns
            ][, t(.SD), .SDcols = TableOwnStatisticColumns]
        } else {
          TableOwnStatistic <- Dataset[, eval(parse(text = TableOwnStatisticList[1])),
                                       .SDcols = TableOwnStatisticColumns,
                                       by = TableGroupingColumns
            ][, t(.SD), .SDcols = TableOwnStatisticColumns]
        }
          
        } else {
          
          if (TableFilter != "") {
            Dataset[eval(parse(text = TableFilter)), eval(parse(text = TableOwnStatisticList[1])), .SDcols = TableOwnStatisticColumns
              ][, t(.SD), .SDcols = TableOwnStatisticColumns]
          } else {
            TableOwnStatistic <- Dataset[, eval(parse(text = TableOwnStatisticList[1])), .SDcols = TableOwnStatisticColumns
              ][, t(.SD), .SDcols = TableOwnStatisticColumns]
          }
        }
        
        colnames(TableOwnStatistic) <- unlist(str_split(TableOwnStatisticList[3], ","))
    },
    error = function(c) {
      show_toast("Incorrect own statistic expression provided", type = "error", position = "top-end", timer = 6000)
      return()
    }
    )
    
    if (is.null(TableOwnStatisticError)) {
      return()
    }
  }
    
  TableColumnsNumeric <- TableColumns[unlist(Dataset[,lapply(.SD, class), .SDcols = TableColumns]) %chin% c("integer", "numeric")]
  TableColumnsCategorical <- TableColumns[unlist(Dataset[,lapply(.SD, class), .SDcols = TableColumns]) %chin% c("character", "factor")]
  TableColumnsComplex <- TableColumns[!TableColumns %chin% c(TableColumnsNumeric, TableColumnsCategorical)]
    
  if (! identical(TableColumnsComplex, character(0))) {
    show_toast(paste0("Columns ",
                      TableColumnsComplex,
                      " are of type complex (list, matrix, etc). Unable to apply statistics / descriptives"),
                type = "warning",
                position = "top-end",
                timer = 6000)
  }
    
  #recode observations
  #think about own statistics
    
  #####
  # TABLE WITH STATISTICS
  #####
    
  # for quantile
  if ("quantile" %chin% TableStatitisticsTypes & ! identical(TableColumnsNumeric, character(0))) {
      
      if (TableGroupingColumns != "") {
        
        if (TableFilter == "") {
          TableQuantile <- Dataset[, lapply(.SD, quantile, probs = TableQuantilesVector, na.rm = TRUE),
                                   .SDcols = TableColumnsNumeric,
                                   by = TableGroupingColumns
          ][, t(.SD), .SDcols = TableColumnsNumeric]
        } else {
          TableQuantile <- Dataset[eval(parse(text = TableFilter)), lapply(.SD, quantile, probs = TableQuantilesVector, na.rm = TRUE),
                                   .SDcols = TableColumnsNumeric,
                                   by = TableGroupingColumns
          ][, t(.SD), .SDcols = TableColumnsNumeric]
        }
        
        TableGroupingColumnsCategories <- Dataset[, distinct(.SD), .SDcols = TableGroupingColumns]
        TableGroupingColumnsCategoriesUnited <- apply(TableGroupingColumnsCategories[, .SD], 1, paste, collapse = ", ")
        
        colnames(TableQuantile) <- paste(rep(c(paste0("Q", TableQuantilesVector[1]*100), paste0("Q", TableQuantilesVector[2]*100)),
                                             each = 1,
                                             length = ncol(TableQuantile)),
                                         "grouped by",
                                         toString(paste(TableGroupingColumns, sep = ", ")),
                                         ifelse(length(TableGroupingColumns) == 1, "for category", "for categories"),
                                         rep(TableGroupingColumnsCategoriesUnited, each = 1))

      } else {
        
        if (TableFilter == "") {
          TableQuantile <- Dataset[, lapply(.SD, quantile, probs = TableQuantilesVector, na.rm = TRUE),
                                   .SDcols = TableColumnsNumeric
                                    ][, t(.SD), .SDcols = TableColumnsNumeric]
        } else {
          TableQuantile <- Dataset[eval(parse(text = TableFilter)), lapply(.SD, quantile, probs = TableQuantilesVector, na.rm = TRUE),
                                   .SDcols = TableColumnsNumeric
                                    ][, t(.SD), .SDcols = TableColumnsNumeric]
        }
        
        colnames(TableQuantile) <- c(paste0("Q", TableQuantilesVector[1]*100), paste0("Q", TableQuantilesVector[2]*100))
      }
      
    } else {
    TableQuantile <- data.table(rn = TableColumns)
  }
    
    
  # for other statistics
  TableStatitisticsTypes <- TableStatitisticsTypes[!TableStatitisticsTypes == "quantile"]
    
  if (TableStatitisticsTypes != "" & ! identical(TableColumnsNumeric, character(0))) {
      
      TableStatitisticsTypesRecoded <- vapply(TableStatitisticsTypes, function(x) {switch(x,
                                                                                          "mean" = "mean",
                                                                                          "median" = "median",
                                                                                          "mode" = "mode",
                                                                                          "max" = "maximum",
                                                                                          "min" = "minimum",
                                                                                          "var" = "variance",
                                                                                          "sd" = "standard deviation",
                                                                                          "kurtosis" = "kurtosis",
                                                                                          "skewness" = "skewness",
                                                                                          "quantile" = "quantiles")},
                                              FUN.VALUE = character(1))
      
      if (TableGroupingColumns != "") {
        
        if (TableFilter == "") {
          TableWithStatistics <- Dataset[, lapply(.SD, function(u) {
            sapply(TableStatitisticsTypes, function(f) as.double(do.call(f, list(u, na.rm = TRUE))))}),
            .SDcols = TableColumnsNumeric,
            by = TableGroupingColumns
              ][, t(.SD), .SDcols = TableColumnsNumeric]
          
        } else {
          TableWithStatistics <- Dataset[eval(parse(text = TableFilter)), lapply(.SD, function(u) {
            sapply(TableStatitisticsTypes, function(f) as.double(do.call(f, list(u, na.rm = TRUE))))}),
            .SDcols = TableColumnsNumeric,
            by = TableGroupingColumns
          ][, t(.SD), .SDcols = TableColumnsNumeric]
        }
        
        TableGroupingColumnsCategories <- Dataset[, distinct(.SD), .SDcols = TableGroupingColumns]
        TableGroupingColumnsCategoriesUnited <- apply(TableGroupingColumnsCategories[, .SD], 1, paste, collapse = ", ")
        
        colnames(TableWithStatistics) <- paste(rep(TableStatitisticsTypesRecoded, each = 1, length = ncol(TableWithStatistics)),
                                               "grouped by",
                                               toString(paste(TableGroupingColumns, sep = ", ")),
                                               ifelse(length(TableGroupingColumns) == 1, "for category", "for categories"),
                                               rep(TableGroupingColumnsCategoriesUnited, each = length(TableStatitisticsTypes)))
      } else {
        
        if (TableFilter == "") {
          TableWithStatistics <- Dataset[, lapply(.SD, function(u) {
            sapply(TableStatitisticsTypes, function(f) as.double(do.call(f, list(u, na.rm = TRUE))))}),
            .SDcols = TableColumnsNumeric
          ][, t(.SD), .SDcols = TableColumnsNumeric]
        } else {
          TableWithStatistics <- Dataset[eval(parse(text = TableFilter)), lapply(.SD, function(u) {
            sapply(TableStatitisticsTypes, function(f) as.double(do.call(f, list(u, na.rm = TRUE))))}),
            .SDcols = TableColumnsNumeric
          ][, t(.SD), .SDcols = TableColumnsNumeric]
        }
        
        
        colnames(TableWithStatistics) <- rep(TableStatitisticsTypesRecoded, each = 1, length = ncol(TableWithStatistics))
      }
      
    } else {
    TableWithStatistics <- data.table(rn = TableColumns)
  }
    
    
  #####
  # TABLE WITH DESCRIPTIVES
  #####
    
    # If 'uniqueN' was in descriptives
    if ("uniqueN" %chin% TableDescriptivesTypes) {
      
      TableUniqueN <- Dataset[, lapply(.SD, uniqueN), .SDcols = TableColumnsCategorical
        ][, t(.SD), .SDcols = TableColumnsCategorical]
      colnames(TableUniqueN) <-  "number of unique observations"
      
    } else {
      TableUniqueN <- data.table(rn = TableColumns)
    }
    
    # If 'class' was in descriptives
    if ("class" %chin% TableDescriptivesTypes) {
      TableClasses <- Dataset[, lapply(.SD, class), .SDcols = TableColumns
                              ][, t(.SD), .SDcols = TableColumns]
      colnames(TableClasses) <- "type of variable"
      
    } else {
      TableClasses <- data.table(rn = TableColumns)
    }
    
    # For other descriptives
    TableDescriptivesTypes <- TableDescriptivesTypes[!TableDescriptivesTypes %chin% c("uniqueN", "class")]
    
    if (TableDescriptivesTypes != "") {
      
      TableDescriptivesTypesRecoded <- vapply(TableDescriptivesTypes, function(x) {
        switch(x,
               "count_na_percentage" = "% rate of empty observations",
               "count_na" = "number of empty observations"
        )
      }, FUN.VALUE = character(1))
      
      if (TableGroupingColumns != "" & TableUseGroupingColumnsWithDescriptives) {
        
        if (TableFilter == "") {
          TableWithDescriptives <- Dataset[, lapply(.SD, function(u) {
            sapply(TableDescriptivesTypes, function(f) do.call(f, list(u)))}),
            .SDcols = TableColumns,
            by = TableGroupingColumns
              ][, t(.SD), .SDcols = TableColumns]
        
        } else {
          TableWithDescriptives <- Dataset[eval(parse(text = TableFilter)), lapply(.SD, function(u) {
            sapply(TableDescriptivesTypes, function(f) do.call(f, list(u)))}),
            .SDcols = TableColumns,
            by = TableGroupingColumns
          ][, t(.SD), .SDcols = TableColumns]
        }
        
        TableGroupingColumnsCategories <- Dataset[, distinct(.SD), .SDcols = TableGroupingColumns]
        TableGroupingColumnsCategoriesUnited <- apply(TableGroupingColumnsCategories[, .SD], 1, paste, collapse = ", ")
        
        colnames(TableWithDescriptives) <- paste(rep(TableDescriptivesTypesRecoded, each = 1, length = ncol(TableWithDescriptives)),
                                                 "grouped by",
                                                 toString(paste(TableGroupingColumns, sep = ", ")),
                                                 ifelse(length(TableGroupingColumns) == 1, "for category", "for categories"),
                                                 rep(TableGroupingColumnsCategoriesUnited, each = length(TableDescriptivesTypes)))
      } else {
        
        if (TableFilter == "") {
          TableWithDescriptives <- Dataset[, lapply(.SD, function(u) {
            sapply(TableDescriptivesTypes, function(f) do.call(f, list(u)))}),
            .SDcols = TableColumns
              ][, t(.SD), .SDcols = TableColumns]
          
        } else {
          TableWithDescriptives <- Dataset[eval(parse(text = TableFilter)), lapply(.SD, function(u) {
            sapply(TableDescriptivesTypes, function(f) do.call(f, list(u)))}),
            .SDcols = TableColumns
          ][, t(.SD), .SDcols = TableColumns]
        }
        
        colnames(TableWithDescriptives) <- rep(TableDescriptivesTypesRecoded, each = 1, length = ncol(TableWithDescriptives))
      }
      
    } else {
      TableWithDescriptives <- data.table(rn = TableColumns)
    }
    
    
  #####
  # OWN STATISTIC
  #####
  if (TableOwnStatistic != "") {
    
    if (TableGroupingColumns != "") {
      
      if (TableFilter != "") {
        TableOwnStatistic <- Dataset[eval(parse(text = TableFilter)),
                                     eval(parse(text = TableOwnStatisticList[1])),
                                     .SDcols = TableOwnStatisticColumns,
                                     by = TableGroupingColumns
          ][, t(.SD), .SDcols = TableOwnStatisticColumns]
      } else {
        TableOwnStatistic <- Dataset[, eval(parse(text = TableOwnStatisticList[1])),
                                     .SDcols = TableOwnStatisticColumns,
                                     by = TableGroupingColumns
          ][, t(.SD), .SDcols = TableOwnStatisticColumns]
      }
      
    } else {
      
      if (TableFilter != "") {
        Dataset[eval(parse(text = TableFilter)), eval(parse(text = TableOwnStatisticList[1])), .SDcols = TableOwnStatisticColumns
          ][, t(.SD), .SDcols = TableOwnStatisticColumns]
      } else {
        TableOwnStatistic <- Dataset[, eval(parse(text = TableOwnStatisticList[1])), .SDcols = TableOwnStatisticColumns
          ][, t(.SD), .SDcols = TableOwnStatisticColumns]
      }
    }
    
    colnames(TableOwnStatistic) <- unlist(str_split(TableOwnStatisticList[3], ","))
    
  } else {
    TableOwnStatistic <- data.table(rn = TableColumns)
  }
    
  #####
  # BIND TABLES
  #####
  TableClasses <- as.data.table(TableClasses, keep.rownames = TRUE)
  TableWithStatistics <- as.data.table(TableWithStatistics, keep.rownames = TRUE)
  TableQuantile <- as.data.table(TableQuantile, keep.rownames = TRUE)
  TableWithDescriptives <- as.data.table(TableWithDescriptives, keep.rownames = TRUE)
  TableUniqueN <- as.data.table(TableUniqueN, keep.rownames = TRUE)
  TableOwnStatistic <- as.data.table(TableOwnStatistic, keep.rownames = TRUE)
    
  TableMerged <- merge(TableClasses,
                        merge(TableWithStatistics,
                              merge(TableQuantile,
                                    merge(TableWithDescriptives,
                                          merge(TableUniqueN,
                                                TableOwnStatistic,
                                                by = "rn",
                                                all = TRUE),
                                          by = "rn",
                                          all = TRUE),
                                    by = "rn",
                                    all = TRUE),
                              by = "rn",
                              all = TRUE),
                        by = "rn",
                        all = TRUE)
    
  colnames(TableMerged) <- str_replace(colnames(TableMerged), "rn", "variable name")

  #####
  # CUSTOMIZE APPEARANCE
  #####
  
  # add table color
  # add heatmap
  # add properties as in Import screen so that fit inside the box
  datatable(TableMerged)
}