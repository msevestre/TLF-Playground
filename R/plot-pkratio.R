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
  grouping <- dataMapping$grouping
  
  # data <- setGroupsFromMapping(data, grouping)
  
  plotObject <- ggplot(data, aes(x = data[,x], y = data[,y], color = data[,grouping]))
  
  # Add Plot Configuration layers and PK Ratios
  plotObject <- addPKRatioTheme(plotObject, plotConfiguration)
  plotObject <- addRatioLines(plotObject, plotConfiguration)
  plotObject <- plotObject + geom_point()
  
  
  # Reposition Watermark (have to find a better way)
  # Layer is first one for Watermark, Data is Position of center of Watermark
  # To create a generic function to do it
  plotObject$layers[[1]]$data$x <- mean(data[,dataMapping$x])
  plotObject$layers[[1]]$data$y <- mean(data[,dataMapping$y])
  
  return(plotObject)

}

addRatioLines <- function(plot, plotConfiguration) {
  plot + geom_hline(yintercept = 1, linetype = "solid", color = "black", size = 2) +
    geom_hline(yintercept = 0.5, linetype = "dashed", color = "red", size = 1) +
    geom_hline(yintercept = 2, linetype = "dashed", color = "red", size = 1) +
    geom_hline(yintercept = 0.75, linetype = "dashed", color = "blue", size = 1) +
    geom_hline(yintercept = 1.55, linetype = "dashed", color = "blue", size = 1)
}

addPKRatioTheme <- function(plot, plotConfiguration) {
  plot + labs(title=plotConfiguration$title, subtitle = plotConfiguration$subtitle, x=plotConfiguration$xlabel, y=plotConfiguration$ylabel) + 
    annotate(geom = "text",
             x = -Inf, # To find a way to center Watermark
             y = -Inf,
             label = plotConfiguration$watermark,
             color = "lightblue", fontface='bold', size=12, alpha=1,
             angle = 30)
  
}
