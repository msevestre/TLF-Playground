#' @title PlotConfiguration
#' @docType class
#' @description  Generic Plot Configuration
#' @export
PlotConfiguration <- R6::R6Class("PlotConfiguration",
## ----------------------------------
## List of plotConfiguration Variables
                                 public = list(
                                   title = "",
                                   subtitle = "",
                                   xlabel = "",
                                   ylabel = "",
                                   x.axis.unit = "",
                                   y.axis.unit = "",
                                   xlim = c(-Inf, Inf),
                                   ylim = c(-Inf, Inf),
                                   watermark = "TLF-Watermark",
                                   legend = list("Location" = "outside", "X.Location"="right", "Y.Location"="top"),
                                   title.font.size = 24,
                                   title.font.color = "blue",
                                   subtitle.font.size = 18,
                                   subtitle.font.color = "black",
                                   xlabel.font.size = 14,
                                   xlabel.font.color = "black",
                                   ylabel.font.size = 14,
                                   ylabel.font.color = "black",
                                   xaxis.font.size = 12,
                                   xaxis.font.color = "black",
                                   yaxis.font.size = 12,
                                   yaxis.font.color = "black",
                                   watermark.font.size = 16,
                                   watermark.font.color = "lightgreen",

## ----------------------------------------------
## Initializing function to be called with $new()
                                   initialize = function(themeName ="default",
                                                         title = "",
                                                         subtitle = paste("Date:", format(Sys.Date(), "%m-%d-%Y")),
                                                         xlabel = "",
                                                         ylabel = "",
                                                         x.axis.unit = "",
                                                         y.axis.unit = "",
                                                         watermark = "TLF-Watermark",
                                                         xlim = c(-Inf, Inf),
                                                         ylim = c(-Inf, Inf),
                                                         title.font.size = 24,
                                                         title.font.color = "blue",
                                                         subtitle.font.size = 18,
                                                         subtitle.font.color = "black",
                                                         xlabel.font.size = 14,
                                                         xlabel.font.color = "black",
                                                         ylabel.font.size = 14,
                                                         ylabel.font.color = "black",
                                                         xaxis.font.size = 12,
                                                         xaxis.font.color = "black",
                                                         yaxis.font.size = 12,
                                                         yaxis.font.color = "black",
                                                         watermark.font.size = 16,
                                                         watermark.font.color = "lightgreen",
                                                         legend = list("Location" = "outside", "X.Location"="right", "Y.Location"=NULL))
                                     {
                                                      
                                     self$title <- title
                                     self$subtitle <- subtitle
                                     self$xlabel <-
                                       getLabelWithUnit(xlabel, x.axis.unit)
                                     self$ylabel <-
                                       getLabelWithUnit(ylabel, y.axis.unit)
                                     self$watermark <- watermark
                                     
                                     self$xlim <- xlim
                                     self$ylim <- ylim
                                     
                                     self$title.font.size <- title.font.size
                                     self$title.font.color <- title.font.color
                                     self$subtitle.font.size <- subtitle.font.size
                                     self$subtitle.font.color <- subtitle.font.color
                                     self$xlabel.font.size <- xlabel.font.size
                                     self$xlabel.font.color <- xlabel.font.color
                                     self$ylabel.font.size <- ylabel.font.size
                                     self$ylabel.font.color <- ylabel.font.color
                                     self$xaxis.font.size <- xaxis.font.size
                                     self$xaxis.font.color <- xaxis.font.color
                                     self$yaxis.font.size <- yaxis.font.size
                                     self$yaxis.font.color <- yaxis.font.color
                                     self$watermark.font.size <- watermark.font.size
                                     self$watermark.font.color <- watermark.font.color
                                     
                                     self$legend <- legend
                                     },

## ---------------------------------------------------------------
## Printing function, only show main elements of plotConfiguration
                                   
                                   print = function() {
                                     cat("  Title: ", self$title, "\n", sep = "\t")
                                     cat("  Subtitle: ", self$subtitle, "\n", sep = "\t")
                                     cat("  Xlabel:  ", self$xlabel, "\n", sep = "\t")
                                     cat("  Ylabel:  ", self$ylabel, "\n", sep = "\t")
                                     cat("  Watermark:  ", self$watermark, "\n", sep = "\t")
                                     invisible(self)
                                   },
                                   
## ---------------------------------------------------------------
## Define Labels: plotConfiguration function that uses data mapping
## to set labels and legends
                                   defineLabels = function(plotHandle, dataMapping){
                                     
                                     # Titles and axes labels
                                     plotHandle <- plotHandle + labs(title=self$title, 
                                                                     subtitle = self$subtitle, 
                                                                     x=self$xlabel, 
                                                                     y=self$ylabel)
                                     
                                     # Set Fonts
                                     plotHandle <- plotHandle + theme(
                                       plot.title = element_text(color=self$title.font.color, size=self$title.font.size),
                                       axis.title.x = element_text(color=self$xlabel.font.color, size=self$xlabel.font.size),
                                       axis.title.y = element_text(color=self$ylabel.font.color, size=self$ylabel.font.size),
                                       axis.text.x = element_text(color=self$xaxis.font.color, size=self$xaxis.font.size),
                                       axis.text.y = element_text(color=self$yaxis.font.color, size=self$yaxis.font.size)
                                       )
                                     
                                     # Legend labels
                                     plotHandle <- setLegendPosition(plotHandle,
                                                                     Location = self$legend$Location,
                                                                     X.Location = self$legend$X.Location, 
                                                                     Y.Location = self$legend$Y.Location)
                                     
                                     # Redefine label of groups in legend
                                     if (is.null(dataMapping$colorGrouping)){
                                       plotHandle <- plotHandle + guides(color = "none")}else{
                                         plotHandle <- plotHandle + guides(color = guide_legend(dataMapping$colorGrouping))}
                                     if (is.null(dataMapping$sizeGrouping)){
                                       plotHandle <- plotHandle + guides(size = "none")}else{
                                         plotHandle <- plotHandle + guides(size = guide_legend(dataMapping$sizeGrouping))}
                                     if (is.null(dataMapping$shapeGrouping)){
                                       plotHandle <- plotHandle + guides(shape = "none")}else{
                                         plotHandle <- plotHandle + guides(shape = guide_legend(dataMapping$shapeGrouping))}
                                     
                                     return(plotHandle)
                                   },
## ----------------------------------------------------------
## Define Labels: plotConfiguration function to first define Watermark
                                   
                                   defineWatermark = function(plotHandle){
                                     # To center the Watermark, x and y currently needs to be redefine 
                                     # later in the actual plot, 
                                     # annotate might be dropped if a better solution is found
                                     plotHandle <- plotHandle + annotate(geom = "text",
                                                x = mean(self$xlim),
                                                y = mean(self$ylim),
                                                label = self$watermark,
                                                color = self$watermark.font.color, fontface='bold', size=self$watermark.font.size,
                                                angle = 30)
                                     
                                     return(plotHandle)
                                   },
## ----------------------------------------------------------
## Define Some Themes
  
  TLFtheme = function(){
    self$title.font.size <- 30
    self$title.font.color <- "firebrick4"
    self$subtitle.font.size <- 26
    self$subtitle.font.color <- "darkslateblue"
    self$xlabel.font.size <- 16
    self$xlabel.font.color <- "deepskyblue4"
    self$ylabel.font.size <- 16
    self$ylabel.font.color <- "deepskyblue4"
    self$xaxis.font.size <- 14
    self$xaxis.font.color <- "gray20"
    self$yaxis.font.size <- 14
    self$yaxis.font.color <- "gray20"
    self$watermark.font.size <- 16
    self$watermark.font.color <- "goldenrod3"
    
  },
  BWtheme = function(){
    self$title.font.color <- "gray10"
    self$subtitle.font.color <- "gray20"
    self$xlabel.font.color <- "gray30"
    self$ylabel.font.color <- "gray30"
    self$xaxis.font.color <- "gray40"
    self$yaxis.font.color <- "gray40"
    self$watermark.font.color <- "white"
  },
  Smalltheme = function(){
    self$title.font.size <- 14
    self$subtitle.font.size <- 12
    self$xlabel.font.size <- 10
    self$ylabel.font.size <- 10
    self$xaxis.font.size <- 8
    self$yaxis.font.size <- 8
    self$watermark.font.size <- 10
  }
  
)

)

# Function to center Watermark
centerWatermark <- function(plotObject, X, Y){
  plotObject$layers[[1]]$data$x <- mean(X)
  plotObject$layers[[1]]$data$y <- mean(Y)
  
  return(plotObject)
}