#' @title getLabelWithUnit
#' @param label Initial label without unit
#' @param unit Unit of label
#' @return Label [unit]
#' @description 
#' getLabelWithUnit write as a single character chain the association of label and unit
#' @export
#' @examples

getLabelWithUnit <- function(label="", unit = "")
{
  if (unit==""){label} else{paste0(label, " [", unit, "]")}
}