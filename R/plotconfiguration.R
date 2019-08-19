#' @title PlotConfiguration
#' @docType class
#' @description  Generic Plot Configuration
#' @export
PlotConfiguration <- R6::R6Class("PlotConfiguration",
                                 public = list(
                                   title = "",
                                   subtitle = "",
                                   xlabel = "",
                                   ylabel = "",
                                   x.axis.unit = "",
                                   y.axis.unit = "",
                                   watermark = "TLF-Watermark",
                                   
                                   initialize = function(title = "",
                                                         subtitle = paste("Date:", format(Sys.Date(), "%m-%d-%Y")),
                                                         xlabel = "",
                                                         ylabel = "",
                                                         x.axis.unit = "",
                                                         y.axis.unit = "",
                                                         watermark = "TLF-Watermark") {
                                     self$title <- title
                                     self$subtitle <- subtitle
                                     self$xlabel <-
                                       getLabelWithUnit(xlabel, x.axis.unit)
                                     self$ylabel <-
                                       getLabelWithUnit(ylabel, y.axis.unit)
                                     self$watermark <- watermark
                                   },
                                   
                                   print = function() {
                                     cat("  Title: ", self$title, "\n", sep = "\t")
                                     cat("  Subtitle: ", self$subtitle, "\n", sep = "\t")
                                     cat("  Xlabel:  ", self$xlabel, "\n", sep = "\t")
                                     cat("  Ylabel:  ", self$ylabel, "\n", sep = "\t")
                                     cat("  Watermark:  ", self$watermark, "\n", sep = "\t")
                                     invisible(self)
                                   }
                                 )
)

if (FALSE){
# Other options to be added later on
CmaxRatio.Configuration <- list(
  "Axes" = list(
    "XAxis" = list(
      "Scaling" = "Lin",
      "Caption" = paste0(CmaxRatio.DataMapping$Axes$X, " [", CmaxRatio.DataMapping$Unit$X, "]"),
      "Font" = 12,
      "Ticks" = NA,
      "RangeMode" = "Auto",
      "Range" = NA),
    "YAxis" = list(
      "Scaling" = "Lin",
      "Caption" = "Simulated Cmax / Observed Cmax",
      "Font" = 12,
      "Ticks" = NA,
      "RangeMode" = "Auto",
      "Range" = NA)),
  "Legend" = list(
    "Font" = 12,
    "Position" = "auto"),
  "Watermark" = list(
    "Text" = "WATERMARK !",
    "Font" = "30",
    "Position" = "Center")
)
}