#' @title PKRatioDataMapping
#' @docType class
#' @description  Data Mapping for PKRatio
#' @export
PKRatioDataMapping <- R6::R6Class("PKRatioDataMapping",
                                  inherit = XYDataMapping,
                                  public = list(
                                    
                                    colorGrouping = NULL,
                                    sizeGrouping = NULL,
                                    shapeGrouping = NULL,
                                    
                                    colorGroupingDataFrame = NULL,
                                    sizeGroupingDataFrame = NULL,
                                    shapeGroupingDataFrame = NULL,
                                    
                                    # Example of how to do some other stuff
                                    initialize = function(x = "Age", y="Ratio", colorGrouping=NULL, sizeGrouping = NULL, shapeGrouping = NULL, colorGroupingDataFrame = NULL, sizeGroupingDataFrame = NULL, shapeGroupingDataFrame = NULL) {
                                      
                                      colorGroupingList <- get_grouping_list(colorGrouping,colorGroupingDataFrame)
                                      sizeGroupingList  <- get_grouping_list(sizeGrouping,sizeGroupingDataFrame)
                                      shapeGroupingList <- get_grouping_list(shapeGrouping,shapeGroupingDataFrame)
                                      
                                      self$colorGrouping <- colorGroupingList$selfGrouping
                                      self$sizeGrouping  <- sizeGroupingList$selfGrouping
                                      self$shapeGrouping <- shapeGroupingList$selfGrouping
                                      
                                      self$colorGroupingDataFrame <- colorGroupingList$selfGroupingDataFrame
                                      self$sizeGroupingDataFrame  <- sizeGroupingList$selfGroupingDataFrame
                                      self$shapeGroupingDataFrame <- shapeGroupingList$selfGroupingDataFrame
                                      
                                      super$initialize(x, y)
                                    }
                                  )
)


get_grouping_list <- function(Grouping,GroupingDataFrame){
  if (!is.null(GroupingDataFrame)){
    selfGroupingDataFrame <- GroupingDataFrame
    if(is.null(Grouping)){
      selfGrouping<-tail(colnames(GroupingDataFrame),1)
    }
  }
  groupinglist <-list(selfGroupingDataFrame = selfGroupingDataFrame , selfGrouping = selfGrouping)
  return(groupinglist)
}


if (FALSE){
  # Example of definition of Data Mapping
  CmaxRatio.DataMapping <- list(
    "Axes" = list("X"="Age", "Y"=c("Ratio")),
    "Unit" = list("X"="yrs", "Y"=c(NULL)),
    "Grouping" = list("Color"= "Gender", "Symbol"=c("Compound", "Dose"))
  )
}