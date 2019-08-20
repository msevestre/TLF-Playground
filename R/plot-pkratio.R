#' Create a PK-Ratio plot
#'
#' @param data TODO
#' @param metaData TODO
#' @param dataMapping TODO
#' @param plotConfiguration TODO
#'
#' @return a ggplot graphical object
#' @export
#'
#' @examples
plotPKRatio <- function(data, metaData, dataMapping = NULL, plotConfiguration = NULL) {
  # If no data mapping or plot configuration is input, use default
  configuration <- plotConfiguration %||% PKRatioPlotConfiguration$new()
  dataMapping <- dataMapping %||% PKRatioDataMapping$new()

  stopifnot("PKRatioDataMapping" %in% class(dataMapping))
  stopifnot("PKRatioPlotConfiguration" %in% class(configuration))

  x <- dataMapping$x
  y <- dataMapping$y
  
  colorGrouping <- getGrouping(data, dataMapping$colorGrouping)
  sizeGrouping <- getGrouping(data, dataMapping$sizeGrouping)
  shapeGrouping <- getGrouping(data, dataMapping$shapeGrouping)
  
  plotObject <- ggplot(data, aes(x = data[,x], y = data[,y], color = colorGrouping, size = sizeGrouping, shape = shapeGrouping))
  
  # Add Plot Configuration layers and PK Ratios
  plotObject <- plotConfiguration$defineWatermark(plotObject)
  plotObject <- plotConfiguration$defineLabels(plotObject, dataMapping)
  plotObject <- addRatioLines(plotObject, plotConfiguration)
  plotObject <- plotObject + geom_point()
  
  
  # Reposition Watermark (have to find a better way)
  # Layer is first one for Watermark, Data is Position of center of Watermark
  # To create a generic function to do it
  plotObject <- centerWatermark(plotObject, data[,dataMapping$x], data[,dataMapping$y])
  
  return(plotObject)

}

addRatioLines <- function(plot, plotConfiguration) {
  plot + geom_hline(yintercept = 1, linetype = "solid", color = "black", size = 2) +
    geom_hline(yintercept = 0.5, linetype = "dashed", color = "red", size = 1) +
    geom_hline(yintercept = 2, linetype = "dashed", color = "red", size = 1) +
    geom_hline(yintercept = 0.75, linetype = "dashed", color = "blue", size = 1) +
    geom_hline(yintercept = 1.55, linetype = "dashed", color = "blue", size = 1)
}

