PlotConfiguration <- R6::R6Class("PKRatioPlotConfiguration", 
  public = list(
    title = "", 
    xlabel = "",
    ylabel = "",
    watermark = "Watermark",
    theme = list(),
    
    initialize = function(title ="", xlabel ="", ylabel ="", watermark="Watermark") {
      self$title <- title
      self$xlabel <- xlabel
      self$ylabel <- ylabel
      self$watermark <- watermark
    },
  
  print = function() {
    cat("  Title: ", self$title, "\n", sep = "")
    cat("  Xlabel:  ", self$xlabel, "\n", sep = "")
    cat("  Ylabel:  ", self$ylabel, "\n", sep = "")
    cat("  Watermark:  ", self$watermark, "\n", sep = "")
    invisible(self)
  }
))

PKRatioPlotConfiguration <- R6::R6Class("PKRatioPlotConfiguration", 
  inherit = PlotConfiguration,
  public = list(
    x.axis.unit = "",
    y.axis.unit = "",
    
  initialize = function(title ="PK Ratio Plot", xlabel = "Age", ylabel ="Simulated/Observed Ratio", watermark ="Watermark",
                        x.axis.unit = "yrs", y.axis.unit = "") {
    
    addUnit2label <- function(label="", unit = "")
    {
      if (unit==""){label} else{paste0(label, " [", unit, "]")}
    }
    
    self$title <- title
    self$xlabel <- addUnit2label(xlabel, x.axis.unit)
    self$ylabel <- addUnit2label(ylabel, y.axis.unit)
    self$watermark <- watermark
  }
))
