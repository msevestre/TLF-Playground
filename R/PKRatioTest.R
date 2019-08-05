# The next line simply remove the variables from the environment
# Similar to clear from Matlab
rm(list=ls())

# Load library ggplot2
library("ggplot2")

#-------------------------------------------------
# Example of definition of  Data for Cmax Ratio plot
# Hopefully, performed previoulsy by the OSP Suite

CmaxRatio.Data <- data.frame(
  "IndividualID" = c(1,2,3),
  "Population" = rep("Asian", 3),
  "Gender" = as.factor(c("M","F","F")),
  "Age" = c(1,2,3),
  "Compound" = rep("Aspirin", 3),
  "Dose" = c(2,3,4),
  "Organ" = rep("VenousBlood", 3),
  "Compartment" = rep("Plasma", 3),
  "Simulated" = c(12,9,5),
  "Observed" = c(10,8,5)
)
CmaxRatio.Data$Ratio <- CmaxRatio.Data$Simulated/CmaxRatio.Data$Observed

# Option to save/load Cmax Ratio table

#-------------------------------------------------
# Example of definition of MetaData
# Hopefully, performed previoulsy by the OSP Suite
CmaxRatio.MetaData <- list(
  "IndividualID" = list("Unit"=NA, "Dimension"=NA, "VariableType"="numeric"),
  "Population" = list("Unit"=NA, "Dimension"=NA, "VariableType"="character"),
  "Gender" = list("Unit"=NA, "Dimension"=NA, "VariableType"="logical"),
  "Age" = list("Unit"=NA, "Dimension"=NA, "VariableType"="numeric"),
  "Compound" = list("Unit"=NA, "Dimension"=NA, "VariableType"="character"),
  "Dose" = list("Unit"=NA, "Dimension"=NA, "VariableType"="numeric"),
  "Organ" = list("Unit"=NA, "Dimension"=NA, "VariableType"="character"),
  "Compartment" = list("Unit"=NA, "Dimension"=NA, "VariableType"="character"),
  "Simulated" = list("Unit"="g/mL", "Dimension"="Concentration", "VariableType"="numeric"),
  "Observed" = list("Unit"="g/mL", "Dimension"="Concentration", "LLOQ"=2.0, "VariableType"="numeric"),
  "Ratio" = list("Unit"=NULL, "Dimension"="Fraction", "VariableType"="numeric")
)
# Create GMFE as a MetaData for Ratio 
CmaxRatio.MetaData$Ratio$GMFE <- 10^(mean(abs(log10(CmaxRatio.Data$Ratio))))

#-------------------------------------------------
# Example of definition of Data Mapping
CmaxRatio.DataMapping <- list(
  "Axes" = list("X"="Age", "Y"=c("Ratio")),
  "Unit" = list("X"="yrs", "Y"=c(NULL)),
  "Grouping" = list("Color"= "Gender", "Symbol"=c("Compound", "Dose"))
)

#-------------------------------------------------
# Define Configuration
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

# The option theme from ggplot allow to update some features of the plot
themetest <- function()
{
  
  theme_classic() + theme(plot.background = element_rect(fill = "white"),
                          legend.justification = c("right", "center"),
                          axis.title = element_text(size = 16, face = "bold"),
                          axis.text = element_text(size = 10, face = "bold"),
                          aspect.ratio = 0.65) 
}

configtest <- function(figLayer, plotconfiguration)
{
  if (match(plotconfiguration$Axes$YAxis$Scaling, c("log", "Log", "LOG"), nomatch=FALSE))
  {figLayer <- figLayer + scale_y_log10()}
  if (match(plotconfiguration$Axes$XAxis$Scaling, c("log", "Log", "LOG"), nomatch=FALSE))
  {figLayer <- figLayer + scale_x_log10()}
  
  # Test of Watermark
  figLayer <- figLayer + annotate(geom = "text",
                                  x = -Inf, # To find a way to center Watermark
                                  y = -Inf,
                                  label = plotconfiguration$Watermark$Text,
                                  color = "grey", fontface='bold', size=15, alpha=0.5,
                                  angle = 45)
  
}



#-------------------------------------------------
# Example of definition of PK Ratio plot function
plotPKRatio <- function(data, metadata, datamapping, plotconfiguration)
{
  # Create ggplot object based on data and datamapping
  pkrp <- ggplot(data, aes(x=data[,datamapping$Axes$X], y=data[,datamapping$Axes$Y], color=data[,datamapping$Grouping$Color])) 
  pkrp <- pkrp + themetest()
  pkrp <- configtest(pkrp, plotconfiguration)
  
  # Add Plot Configuration layers
  # This might be done by calling a theme function later on
  pkrp <- pkrp + labs(title="PK Ratio Plot", subtitle = paste("Date:", format(Sys.Date(), "%m-%d-%Y")), x=plotconfiguration$Axes$XAxis$Caption, y=plotconfiguration$Axes$YAxis$Caption, color = CmaxRatio.DataMapping$Grouping$Color)
  
  # Plot specific ratio lines
  pkrp <- pkrp + geom_hline(yintercept=1, linetype="solid", color = "black", size = 1.5)
  pkrp <- pkrp + geom_hline(yintercept=1.5, linetype="dashed", color = "blue")
  pkrp <- pkrp + geom_hline(yintercept=1/1.5, linetype="dashed", color = "blue")
  pkrp <- pkrp + geom_hline(yintercept=2, linetype="dashed", color = "red")
  pkrp <- pkrp + geom_hline(yintercept=0.5, linetype="dashed", color = "red")
  
  # Plot specific ratio lines
  pkrp <- pkrp + geom_point()
  
  # Show plot
  pkrp
}

#-------------------------------------------------
# Test example of previous blocks
plotPKRatio(data = CmaxRatio.Data, metadata = CmaxRatio.MetaData, 
            datamapping = CmaxRatio.DataMapping, plotconfiguration = CmaxRatio.Configuration)