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
  
  if (Dataset == "") {
    show_toast("Dataset is not selected", type = "error", position = "top-end", timer = 6000)
    return()
  }
  
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
  
  if (PlotAxisY %chin% c(PlotGroupColorAxis, PlotGroupSizeAxis, PlotGroupGridRowAxis, PlotGroupGridColAxis)) {
    show_toast("Same variable for y and grouping axes selected", type = "error", position = "top-end", timer = 6000)
    return()
  }
  
  if (PlotGroupSizeAxis != "") {
    PlotSize <- PlotGroupSizeAxis
    UsedSizeVar <- PlotGroupSizeAxis
    show_toast("Unable to customize size as grouping was used", type = "warning", position = "top-end", timer = 6000)
  }
  
  if (PlotGroupColorAxis != "") {
    PlotColor <- PlotGroupColorAxis
    UsedColorVar <- PlotGroupColorAxis
    show_toast("Unable to customize color as grouping was used", type = "warning", position = "top-end", timer = 6000)
    
  } else if (PlotColor == "default") {
    if (PlotType %chin% c("violin", "density", "area", "col", "boxplot", "histogram")) {
      PlotColor <- "white"
    } else {
      PlotColor <- "black"
    }
  }
  
  UsedVars <- c(PlotAxisX, PlotAxisY, PlotGroupColorAxis, PlotGroupSizeAxis, PlotGroupGridRowAxis, PlotGroupGridColAxis)
  
  if (exists("UsedSizeVar")) {
    UsedVars <- c(UsedVars, UsedSizeVar)
  }
  
  if (exists("UsedColorVar")) {
    UsedVars <- c(UsedVars, UsedColorVar)
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
      show_toast("Y variable must be numeric")
      return()
    }
  }
  
  if (PlotType %chin% c("point", "density", "histogram", "dotplot")) {
    if (!class(pull(Dataset, PlotAxisX)[1]) %chin% c("numeric", "integer")) {
      show_toast("X variable must be numeric")
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
  
  PlotFacetGridFormula <- paste0(PlotGroupGridRowAxis, "~", PlotGroupGridColAxis)
  
  #####
  # BUILDING BASIC PLOT, CUSTOMIZING APPEARANCE, GROUP COLOR AND SIZE
  #####
  
  if (PlotType == 'point') {
    PlotBasic <- ggplot(Dataset, aes_string(PlotAxisX, PlotAxisY)) +
      do.call(PlotGeomFunction, args = list(colour = PlotColor, shape = PlotPointShape, size = PlotSize, alpha = PlotOpacity))
    
  } else if (PlotType == 'line') {
    PlotBasic <- ggplot(Dataset, aes_string(PlotAxisX, PlotAxisY)) +
      do.call(PlotGeomFunction, args = list(colour = PlotColor, linetype = PlotPointShape, size = PlotSize, alpha = PlotOpacity))
    
  } else if (PlotType == "violin") {
    PlotBasic <- ggplot(Dataset, aes_string(PlotAxisX, PlotAxisY)) +
      do.call(PlotGeomFunction, args = list(fill = PlotColor, linetype = PlotPointShape, size = PlotSize, alpha = PlotOpacity))
  
  } else if (PlotType == "density") {
    PlotBasic <- ggplot(Dataset, aes_string(PlotAxisX)) +
      do.call(PlotGeomFunction, args = list(fill = PlotColor, linetype = PlotPointShape, size = PlotSize, alpha = PlotOpacity))
      
  } else if(PlotType %chin% c("area", "col", "boxplot")) {
    PlotBasic <- ggplot(Dataset, aes_string(PlotAxisX, PlotAxisY)) +
      do.call(PlotGeomFunction, args = list(fill = PlotColor, size = PlotSize, alpha = PlotOpacity))
    
  } else if (PlotType == "histogram") {
    
    if (is.numeric(pull(Dataset, PlotAxisX)[1])) {
      PlotBasic <- ggplot(Dataset, aes_string(PlotAxisX)) +
        do.call("geom_histogram", args = list(fill = PlotColor, size = PlotSize, alpha = PlotOpacity))
    } else {
      PlotBasic <- ggplot(Dataset, aes_string(PlotAxisX)) +
        do.call("geom_bar", args = list(fill = PlotColor, size = PlotSize, alpha = PlotOpacity))
    }

  } else if (PlotType == "dotplot") {
    PlotBasic <- ggplot(Dataset, aes_string(PlotAxisX)) +
      do.call(PlotGeomFunction, args = list(color = PlotColor, size = PlotSize, alpha = PlotOpacity))
      
  } else {
    show_toast("Unknown plot type", type = "error", position = "top-end", timer = 6000)
    return()
  }
  
  #####
  # ADDING LABELS
  #####
  
  PlotBasicLabeled <- PlotBasic +
    facet_grid(PlotFacetGridFormula) +
    do.call(PlotThemeFunction, args = list()) +
    labs(x = PlotAxisXName, y = PlotAxisYName, title = PlotTitle) +
    theme(text = element_text(size = PlotFontSize))
  
  #####
  # ADDING SECONDARY LINES
  #####
  
  for (i in PlotSecondaryLines) {
    
    if (i %chin% c("lm", "loess")) {
      PlotBasicLabeled <- PlotBasicLabeled +
        geom_smooth(method = "lm", color = "darkred", se = FALSE, size = 0.5)
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
          geom_hline(yintercept = quantile(pull(Dataset, PlotAxisY), na.rm = TRUE, probs = PlotSecondaryLineQuantileProbs))+
          geom_hline(yintercept = quantile(pull(Dataset, PlotAxisY), na.rm = TRUE, probs = 1 - PlotSecondaryLineQuantileProbs))+
          annotate(geom = "text",
                   x = 1,
                   y = 1.0001*quantile(pull(Dataset, PlotAxisY), na.rm = TRUE, probs = PlotSecondaryLineQuantileProbs),
                   angle = "90",
                   label = "Q right")+
          annotate(geom = "text",
                   x = 1,
                   y = 1.0001*quantile(pull(Dataset, PlotAxisY), na.rm = TRUE, probs = 1 - PlotSecondaryLineQuantileProbs),
                   angle = "90",
                   label = "Q left")
      } else {
        PlotBasicLabeled <- PlotBasicLabeled +
          geom_vline(xintercept = quantile(pull(Dataset, PlotAxisX), na.rm = TRUE, probs = PlotSecondaryLineQuantileProbs))+
          geom_vline(xintercept = quantile(pull(Dataset, PlotAxisX), na.rm = TRUE, probs = 1 - PlotSecondaryLineQuantileProbs))+
          annotate(geom = "text",
                   y = 0,
                   x = 1.0001*quantile(pull(Dataset, PlotAxisX), na.rm = TRUE, probs = PlotSecondaryLineQuantileProbs),
                   angle = "90",
                   label = "Q right")+
          annotate(geom = "text",
                   y = 0,
                   x = 1.0001*quantile(pull(Dataset, PlotAxisX), na.rm = TRUE, probs = 1 - PlotSecondaryLineQuantileProbs),
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
