#' @title PKRatioPlotConfiguration
#' @docType class
#' @description  Plot Configuration for PKRatio
#' @export
PKRatioPlotConfiguration <- R6::R6Class("PKRatioPlotConfiguration",
                                        inherit = PlotConfiguration,
                                        public = list(
                                          initialize = function(title = "PK Ratio Plot",
                                                                xlabel = "Age",
                                                                ylabel = "Simulated/Observed Ratio",
                                                                x.axis.unit = "yrs",
                                                                y.axis.unit = "") {
                                            super$initialize(
                                              title = title,
                                              xlabel = xlabel,
                                              ylabel = ylabel,
                                              x.axis.unit = x.axis.unit,
                                              y.axis.unit = y.axis.unit
                                            )
                                          }
                                        )
)
