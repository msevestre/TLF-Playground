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
#' 
#' 
#' 



check_if_not_numeric <- function(vec,msg="Input must be numeric."){
  if (!is.numeric(vec)){
    stop(msg)
  }
} 


addCaptionsColumn <- function(df,dfinp,captionString = NULL){
  
  sel_df_cols <- df[colnames( dfinp[ seq ( 1,ncol(dfinp)-1 )] )] #get columns from df corresponding to column headings in dfinp (excluding the last column heading in dfinp)
  df_caption_factors <- rep(0,nrow(df)) #vector that is to be populated with factor levels that determine caption
  df_inp_levels <- seq(1,nrow(dfinp)) #factor levels associated with each caption
  for (k in df_inp_levels){ #for each caption 
    logic_matrix <- matrix(rep(FALSE,nrow(df)*ncol(sel_df_cols)),nrow(df))
    for (n in seq(1,ncol(sel_df_cols))){#for each column
      col_head <- colnames(sel_df_cols[n]) #get column header
      
      
      if ( is.list(dfinp[[col_head]]) ) { #case where a list is supplied for binning
        
        check_if_not_numeric(sel_df_cols[[col_head]] , msg = "Dataframe column entries to be binned must be numeric.") #check that data points to be binned are numeric
        
        sapply( dfinp[[col_head]], check_if_not_numeric , msg = "Bin limits must be numeric") #check that all bin limits are numeric
        
        if (!all(sapply(dfinp[[col_head]],function(x) return( length(x) == 2 )))) { #check that all bin limits are of length 2.
          stop("Each element of bin limits list must be a vector of length 2.")
        }
        
        if (!all(sapply(dfinp[[col_head]],function(x) return(x[2]>x[1])))) { #check that all bin limits are in increasing order
          stop("Bin limits must be increasing.")
        }
        
        logic_matrix[,n]<-sapply( sel_df_cols[[col_head]], function(x) return( (x >= dfinp[[col_head]][[k]][1]) & (x <= dfinp[[col_head]][[k]][2])   )  )   
        
      } else { #case where there is no binning, only matching between caption dataframe entries and data column entries
        
        
        
        logic_matrix[,n]<-sapply( sel_df_cols[[col_head]], function(x) return(   x == dfinp[[col_head]][k]  ) )   
        
        
        
      }
    }
    
    for (m in seq(1,nrow(df))){#for each row of data
      if (all(logic_matrix[m,])){ #if entire df row matches dfinp row
        df_caption_factors[m] <- df_inp_levels[k] #set factor level in df_caption_factors
      }
    }
    
    
  } 
  
  if (is.null(captionString))
  {
    captionString<-colnames(dfinp[ncol(dfinp)])
  }
  
  df[[captionString]]<- as.factor(rep(NA,nrow(df)))
  levels(df[[captionString]])<-dfinp[[captionString]]
  df[[captionString]][ df_caption_factors != 0 ] <- dfinp[[captionString]][  df_caption_factors[df_caption_factors!= 0 ]   ]
  
  return(df)
}

plotPKRatio <- function(data, metaData, dataMapping = NULL, plotConfiguration = NULL) {
  # If no data mapping or plot configuration is input, use default
  configuration <- plotConfiguration %||% PKRatioPlotConfiguration$new()
  dataMapping <- dataMapping %||% PKRatioDataMapping$new()
  
  stopifnot("PKRatioDataMapping" %in% class(dataMapping))
  stopifnot("PKRatioPlotConfiguration" %in% class(configuration))
  
  x <- dataMapping$x
  y <- dataMapping$y
  
  if (!is.null(dataMapping$colorGroupingDataFrame)){
    data<-addCaptionsColumn(data,dataMapping$colorGroupingDataFrame,dataMapping$colorGrouping)
  }
  
  if (!is.null(dataMapping$sizeGroupingDataFrame)){
    data<-addCaptionsColumn(data,dataMapping$sizeGroupingDataFrame,dataMapping$sizeGrouping)
  }
  
  if (!is.null(dataMapping$shapeGroupingDataFrame)){
    data<-addCaptionsColumn(data,dataMapping$shapeGroupingDataFrame,dataMapping$shapeGrouping)
  }
  
  
  
  colorGrouping <- getGrouping(data, dataMapping$colorGrouping)
  sizeGrouping  <- getGrouping(data, dataMapping$sizeGrouping)
  shapeGrouping <- getGrouping(data, dataMapping$shapeGrouping)
  
  plotObject <- ggplot()
  
  # Add Plot Configuration layers and PK Ratios
  plotObject <- plotConfiguration$setWatermark(plotObject)
  plotObject <- plotConfiguration$defineLabels(plotObject, dataMapping)
  plotObject <- plotConfiguration$addRatioLines(plotObject)
  plotObject <- plotObject + geom_point(data = data[, c(dataMapping$x, dataMapping$y, dataMapping$colorGrouping, dataMapping$sizeGrouping, dataMapping$shapeGrouping)], mapping = aes(x = data[, x], y = data[, y], color = colorGrouping, size = sizeGrouping, shape = shapeGrouping))
  
  return(plotObject)
}




