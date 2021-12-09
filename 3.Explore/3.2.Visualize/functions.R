FnVisualizeScreenBuildPlot <- function(Dataset,
                                       PlotType,
                                       PlotAxisX,
                                       PlotAxisY = "",
                                       PlotColor,
                                       PlotSize,
                                       PlotOpacity,
                                       PlotTheme,
                                       PlotPointShape,
                                       PlotLineType,
                                       PlotAxisXName,
                                       PlotAxisYName,
                                       PlotTitle,
                                       PlotFontSize,
                                       PlotSecondaryLines,
                                       PlotSecondaryLineQuantileProbs,
                                       PlotGroupColorAxis,
                                       PlotGroupSizeAxis,
                                       PlotGroupGridRowAxis,
                                       PlotGroupGridColAxis) {
  
  #####
  # CATCHING ERRORS, PRESETS CONFIGURATION
  #####
  
  if (is.null(Dataset) || Dataset == "") {
    show_toast("Dataset is not selected", type = "error", position = "top-end", timer = 6000)
    return()
  }
  
  Dataset <- as.data.table(Dataset)
  
  if (PlotAxisY == "" & !(PlotType %chin% c("density", "histogram", "dotplot"))) {
    show_toast(paste("Y varible required for plot type", PlotType), type = "error", position = "top-end", timer = 6000)
    return()
  }
  
  if (PlotAxisX == PlotAxisY) {
    show_toast("Same variable for x and y axes selected", type = "error", position = "top-end", timer = 6000)
    return()
  }
  
  if (PlotAxisX %chin% c(PlotGroupColorAxis, PlotGroupSizeAxis, PlotGroupGridRowAxis, PlotGroupGridColAxis)) {
    show_toast("Same variable for x and grouping axes selected", type = "error", position = "top-end", timer = 6000)
    return()
  }
  
  if (PlotAxisY %chin% c(PlotGroupColorAxis, PlotGroupSizeAxis, PlotGroupGridRowAxis, PlotGroupGridColAxis) & PlotAxisY != "") {
    show_toast("Same variable for y and grouping axes selected", type = "error", position = "top-end", timer = 6000)
    return()
  }
  
  if (PlotGroupGridRowAxis == PlotGroupGridColAxis & PlotGroupGridRowAxis != "") {
    show_toast("Same variable for column and row grouping axes selected", type = "error", position = "top-end", timer = 6000)
    return()
  }
  
  UsedVars <- c(PlotAxisX, PlotAxisY, PlotGroupColorAxis, PlotGroupSizeAxis, PlotGroupGridRowAxis, PlotGroupGridColAxis)
  UsedVars <- UsedVars[!UsedVars %chin% c("", ".")]
  
  if (PlotGroupSizeAxis != "") {
    UsedVars <- c(UsedVars, PlotGroupSizeAxis)
    show_toast("Currently unable to customize size as grouping was used", type = "warning", position = "top-end", timer = 6000)
  } else {
    PlotGroupSizeAxis <- NULL
  }
  
  if (PlotGroupColorAxis != "") {
    UsedVars <- c(UsedVars, PlotGroupColorAxis)
    show_toast("Currently unable to customize color as grouping was used", type = "warning", position = "top-end", timer = 6000)
  } else {
    PlotGroupColorAxis <- NULL
  }

  if (!all(sapply(Dataset[1, .(UsedVars)], class) %chin% c("factor", "numeric", "character", "integer"))) {
    show_toast("Only variables of types factor, numeric, character, integer alloved",
               type = "error",
               position = "top-end",
               timer = 6000)
    return()
  }
  
  if (PlotType %chin% c("point", "line", "violin", "area", "col", "boxplot")) {
    if (!class(pull(Dataset, PlotAxisY)[1]) %chin% c("numeric", "integer")) {
      show_toast("Y variable must be numeric", type = "warning", position = "top-end", timer = 6000)
      return()
    }
  }
  
  if (PlotType %chin% c("point", "density", "dotplot")) {
    if (!class(pull(Dataset, PlotAxisX)[1]) %chin% c("numeric", "integer")) {
      show_toast("X variable must be numeric", type = "warning", position = "top-end", timer = 6000)
      return()
    }
  }
  
  
  PlotGeomFunction <- paste0("geom_", PlotType)
  PlotThemeFunction <- paste0("theme_", PlotTheme)
  
  if (PlotGroupGridRowAxis == "") {
    PlotGroupGridRowAxis <- "."
  }
  
  if (PlotGroupGridColAxis == "") {
    PlotGroupGridColAxis <- "."
  }
  
  PlotFacetGridFormula <- as.formula(paste0("`", PlotGroupGridRowAxis, "`" , " ~ ", "`", PlotGroupGridColAxis, "`"))
  
  #####
  # BUILDING BASIC PLOT, CUSTOMIZING POINT SHAPE AND LINETYPE, GROUPING COLOR, SIZE
  #####
  
  if (PlotType == "point") {
    PlotBasic <- ggplot(Dataset, aes_string(PlotAxisX, PlotAxisY, color = PlotGroupColorAxis)) +
      do.call(PlotGeomFunction, args = list(shape = PlotPointShape)) 
    
  } else if (PlotType == "line") {
    PlotBasic <- ggplot(Dataset, aes_string(PlotAxisX, PlotAxisY)) +
      do.call(PlotGeomFunction, args = list(linetype = PlotPointShape))
    
  } else if (PlotType == "violin") {
    PlotBasic <- ggplot(Dataset, aes_string(PlotAxisX, PlotAxisY)) +
      do.call(PlotGeomFunction, args = list(linetype = PlotPointShape))
  
  } else if (PlotType == "density") {
    PlotBasic <- ggplot(Dataset, aes_string(PlotAxisX)) +
      do.call(PlotGeomFunction, args = list(linetype = PlotPointShape))
      
  } else if(PlotType %chin% c("area", "col", "boxplot")) {
    PlotBasic <- ggplot(Dataset, aes_string(PlotAxisX, PlotAxisY)) +
      do.call(PlotGeomFunction, args = list())
    
  } else if (PlotType == "histogram") {
    
    if (is.numeric(pull(Dataset, PlotAxisX)[1])) {
      PlotBasic <- ggplot(Dataset, aes_string(PlotAxisX)) +
        do.call("geom_histogram", args = list())
    } else {
      PlotBasic <- ggplot(Dataset, aes_string(PlotAxisX)) +
        do.call("geom_bar", args = list())
    }

  } else if (PlotType == "dotplot") {
    PlotBasic <- ggplot(Dataset, aes_string(PlotAxisX)) +
      do.call(PlotGeomFunction, args = list())
      
  } else {
    show_toast("Unknown plot type", type = "error", position = "top-end", timer = 6000)
    return()
  }
  
  #####
  # ADDING LABELS, COLOR, SIZE, ALPHA
  #####
  
  PlotBasicLabeled <- PlotBasic +
    facet_grid(PlotFacetGridFormula) +
    do.call(PlotThemeFunction, args = list()) +
    labs(x = PlotAxisXName, y = PlotAxisYName, title = PlotTitle) +
    theme(text = element_text(size = PlotFontSize))
  
  if (is.null(PlotGroupSizeAxis)) {
    PlotBasicLabeled$layers[[1]]$aes_params$size <- PlotSize
  }
  
  if (is.null(PlotGroupColorAxis)) {
    if (PlotType %chin% c("point", "line", "dotplot")) {
      PlotBasicLabeled$layers[[1]]$aes_params$colour <- PlotColor
    } else {
      PlotBasicLabeled$layers[[1]]$aes_params$fill <- PlotColor
    }
  }
  
  PlotBasicLabeled$layers[[1]]$aes_params$alpha <- PlotOpacity
  
  #####
  # ADDING SECONDARY LINES
  #####
  
  for (i in PlotSecondaryLines) {
    
    if (i %chin% c("lm", "loess")) {
      PlotBasicLabeled <- PlotBasicLabeled +
        geom_smooth(method = i, color = "darkred", se = FALSE, size = 0.5)
    }
    
    if (i %chin% c("mean", "min", "max", "median")) {
      if (PlotType %chin% c("point", "line", "violin", "area", "col", "boxplot")) {
        PlotBasicLabeled <- PlotBasicLabeled +
          geom_hline(yintercept = do.call(i, args = list(pull(Dataset, PlotAxisY), na.rm = TRUE))) +
          annotate(geom = "text",
                   x = 1,
                   y = 1.0001*do.call(i, args = list(pull(Dataset, PlotAxisY), na.rm = TRUE)),
                   angle = "90",
                   label = i)
        
      } else {
        PlotBasicLabeled <- PlotBasicLabeled +
          geom_vline(xintercept = do.call(i, args = list(pull(Dataset, PlotAxisX), na.rm = TRUE))) +
          annotate(geom = "text",
                   y = 0,
                   x = 1.0001*do.call(i, args = list(pull(Dataset, PlotAxisX), na.rm = TRUE)),
                   angle = "90",
                   label = i)
      }
    }
    
    if (i == "quantile") {
      if (PlotType %chin% c("point", "line", "violin", "area", "col", "boxplot")) {
        PlotBasicLabeled <- PlotBasicLabeled +
          geom_hline(yintercept = quantile(pull(Dataset, PlotAxisY), na.rm = TRUE, probs = PlotSecondaryLineQuantileProbs[2]))+
          geom_hline(yintercept = quantile(pull(Dataset, PlotAxisY), na.rm = TRUE, probs = PlotSecondaryLineQuantileProbs[1]))+
          annotate(geom = "text",
                   x = 1,
                   y = 1.0001*quantile(pull(Dataset, PlotAxisY), na.rm = TRUE, probs = PlotSecondaryLineQuantileProbs[2]),
                   angle = "90",
                   label = "Q right")+
          annotate(geom = "text",
                   x = 1,
                   y = 1.0001*quantile(pull(Dataset, PlotAxisY), na.rm = TRUE, probs = PlotSecondaryLineQuantileProbs[1]),
                   angle = "90",
                   label = "Q left")
      } else {
        PlotBasicLabeled <- PlotBasicLabeled +
          geom_vline(xintercept = quantile(pull(Dataset, PlotAxisX), na.rm = TRUE, probs = PlotSecondaryLineQuantileProbs[2]))+
          geom_vline(xintercept = quantile(pull(Dataset, PlotAxisX), na.rm = TRUE, probs = PlotSecondaryLineQuantileProbs[1]))+
          annotate(geom = "text",
                   y = 0,
                   x = 1.0001*quantile(pull(Dataset, PlotAxisX), na.rm = TRUE, probs = PlotSecondaryLineQuantileProbs[2]),
                   angle = "90",
                   label = "Q right")+
          annotate(geom = "text",
                   y = 0,
                   x = 1.0001*quantile(pull(Dataset, PlotAxisX), na.rm = TRUE, probs = PlotSecondaryLineQuantileProbs[1]),
                   angle = "90",
                   label = "Q left")
      }
    }
    
    if (i == "y = 0") {
      PlotBasicLabeled <- PlotBasicLabeled +
        geom_hline(yintercept = 0)
    }
    
    if (i == "x = 0") {
      PlotBasicLabeled <- PlotBasicLabeled +
        geom_vline(xintercept = 0)
    }
  }
  
  PlotFinal <- ggplotly(PlotBasicLabeled)
  
  return(PlotFinal )
}
