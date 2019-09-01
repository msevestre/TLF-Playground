#' @title addGuestLines
#' @param plotHandle ggplot object
#' @param minX min range of plot
#' @param minX min range of plot
#' @param delta parameter from Guest et al. article
#' @return plotHandle ggplot object
#' @description
#' plot limits from Guest et al. equation for DDI Ratio plot
#' @export
#' @examples
#' p <- ggplot()
#' addGuestLines(plotHandle=p)+scale_x_log10()+scale_y_log10()
addGuestLines <- function(plotHandle, minX=1e-2, maxX=1e2, delta=1) {
  # Guest et al boundaries for DDI Ratio Plots
  x <- seq(log10(minX), log10(maxX), 0.01)
  xGuest <- 10 ^ x

  symx <- xGuest
  symx[xGuest < 1] <- 1 / symx[symx < 1]

  GuestLimit <- (delta + 2 * (symx - 1)) / symx

  symUpperLimit <- symx * GuestLimit
  symLowerLimit <- symx / GuestLimit

  UpperLimit <- symUpperLimit
  LowerLimit <- symLowerLimit
  UpperLimit[xGuest < 1] <- 1 / symLowerLimit[xGuest < 1]
  LowerLimit[xGuest < 1] <- 1 / symUpperLimit[xGuest < 1]

  Guest.data.frame <- data.frame(x = xGuest, yup = UpperLimit, ylo = LowerLimit)

  # Plot
  plotHandle <- plotHandle +
    geom_line(data = data.frame(x = xGuest, y = UpperLimit), mapping = aes(x, y), linetype = "dashed", color = "black", size = 1) +
    geom_line(data = data.frame(x = xGuest, y = LowerLimit), mapping = aes(x, y), linetype = "dashed", color = "black", size = 1)

  return(plotHandle)
}