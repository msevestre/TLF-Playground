library(stringr)


get_binlabels_BinEdges <- function(x,binvec,varlabel){#function to create bin labels when BinEdges is specified
  
  if (x == 1){
    return(paste(varlabel,"<=",binvec[1]))
  }
  else if (x == length(binvec)+1) {
    return(paste(varlabel,">=", tail(binvec, n=1)  ) )
  }
  else {
    return( paste( binvec[x-1]  , "<=" , varlabel , "<" , binvec[x] ) )
  }
  
}


get_binlabels_NumBins <- function(x,binvec,varlabel){#function to create bin labels when NumBins is specified
  
  if (x == length(binvec)-1) {
    return(paste0( binvec[x] , "<=" ,   varlabel  , "<=" , binvec[x+1] ))
  }
  else {
    return(paste0( binvec[x] , "<=" ,   varlabel  , "<" , binvec[x+1] ))
  }
  
}

buildLabelCol <- function(n,groupingDF){ #function that pastes together the values of each column used for grouping in a dataframe row
  str = groupingDF[n,1]
  if (ncol(groupingDF)>1){
    for (i in seq(2,ncol(groupingDF))){
      str <- paste(str,groupingDF[n,i],sep = ", ")
    } 
  } 
  return(str)
}

changeCaption <-function(df,oldCaptionsLevels=NULL,newCaptionsVector=NULL){ #function to overide labels associated with specific factor levels in a factor dataframe column
  
  if (is.null(oldCaptionsLevels))
  {
    print("Select caption number to change.")
    
    for (n in seq(1,length(levels(df[,1])))){
      print(paste0(n,". ",levels(df[,1])[n]))
    }
    no_level_to_change <- as.numeric(readline(prompt="Enter number: "))
    new_caption <- readline(prompt="New caption: ")
    levels(df[,1])[no_level_to_change]<-new_caption
  } else if (is.null(newCaptionsVector)) {
    
    if (length() == 1)
    {
      print("Select caption number to change.")
      for (n in seq(1,length(levels(df[,1])))){
        print(paste0(n,". ",levels(df[,1])[n]))
      }
      no_level_to_change <- as.numeric(readline(prompt="Enter number: "))
      levels(df[,1])[no_level_to_change]<-newCaptionsVector
    } else{
      stop("A vector of old captions to be replaced is required.")
    }
  } else {
    for (n in seq(1,oldCaptionsLevels)){
      levels(df[,1])[n]<-newCaptionsVector[n]  
    }
  }
  return(df)   
}

getGrouping <- function(df, grouping, groupingName) { #function that takes a dataframe (df) a vector of dataframe column titles (grouping) and a name for the grouping (groupingName), returinging a dataframe column where each row summarizes the multiple groups that the correspinding df row falls into
  
  groupNames <-NULL
  for (n in seq(1,length(grouping))){
    groupNames<-c(groupNames,grouping[[n]]$name)
  }
  
  
  
  groupingDF <-df[,groupNames]
  
  
  for (n in seq(1,length(grouping))) {
    
    if ( !is.factor(groupingDF[[groupNames[n]]]) ){  #if column not a factor...
      
      if (is.numeric(groupingDF[[ groupNames[n] ]])){ #if the non-factor column is numeric, sort into bins
        
        if (!is.null(  grouping[[n]]$BinEdges )) { #if BinEdges are supplied, use those for binning
          bin_numbers_of_df_column_elements <- sapply( groupingDF[[ groupNames[n] ]] , findInterval , vec = c(-Inf,grouping[[n]]$BinEdges,Inf) , rightmost.closed = TRUE ) 
          labels_of_df_column_elements <- as.factor(sapply( bin_numbers_of_df_column_elements , get_binlabels_BinEdges , binvec = grouping[[n]]$BinEdges ,varlabel =  grouping[[n]]$name )) 
          groupingDF[[ groupNames[n] ]] <- labels_of_df_column_elements 
          print(groupingDF)
        }
        else if(!is.null(  grouping[[n]]$NumBins  )){ #if NumBins is supplied, divide range of column values into NumBins equally spaced bins and then bin the column entries into those bins
          x <- groupingDF[[ groupNames[n] ]]
          eq_bin_vec <- c(seq( min(x) , max(x) , length =  (grouping[[n]]$NumBins + 1) ) )
          print(eq_bin_vec)
          print(x)
          bin_numbers_of_df_column_elements <- sapply( x , findInterval , vec =   , eq_bin_vec     , rightmost.closed = TRUE   ) 
          print(bin_numbers_of_df_column_elements)
          labels_of_df_column_elements <- as.factor(sapply( bin_numbers_of_df_column_elements , get_binlabels_NumBins , binvec =   eq_bin_vec    , varlabel =  grouping[[n]]$name )) 
          groupingDF[[ groupNames[n] ]] <- labels_of_df_column_elements 
          print(groupingDF)
        }
        else{
          stop("Non-factor numeric columns to be used for grouping must have either a vector of bin edges (BinEdges) or a number of bins (NumBins) specified in the grouping list.")
        }
        
        
      }
      else {  #case where column is not numeric and is not a factor: make it into a factor
        groupingDF[[ groupNames[n] ]]  <- as.factor(groupingDF[[ groupNames[n] ]] )
      }
      
    }
    
  }
  
  #At this point, all columns of groupingDF should be factors.  Next, want to build a new dataframe column of labels of the grouping columns
  
  labels_column <- as.factor(sapply(seq(1,nrow(groupingDF)),buildLabelCol,groupingDF))
  labels_df = data.frame(labels_column)
  names(labels_df)<-groupingName
  
  return(labels_df) #this will return a single dataframe column that can be bound to the original dataframe and used for labeling data according to the groups specified in the input vector "grouping"
  
}


generate_df_column <-function(r,output0,output1) {
  
  if (r == 1)
  { 
    return(output0)
  }
  else
  { 
    return(output1)
  }
  
}


N <- 20 #number of individuals
#Create a dataframe for N individuals


df <- data.frame(
  "IndividualID" = seq(1,N),
  "Population" = sapply(round(runif(N,0,1)),generate_df_column,output0="Asian",output1="European"),
  "Gender" = sapply(round(runif(N,0,1)),generate_df_column,output0="M",output1="F"),
  "Age" = round(runif(N,20,40)),
  "Compound" = sapply(round(runif(N,0,1)),generate_df_column,output0="Aspirin",output1="Sugar"),
  "Dose" = sapply(round(runif(N,0,1)),generate_df_column,output0=3,output1=6),
  "Organ" = sapply(round(runif(N,0,1)),generate_df_column,output0="VenousBlood",output1="VenousBlood"),
  "Compartment" = sapply(round(runif(N,0,1)),generate_df_column,output0="Plasma",output1="Plasma"),
  "Simulated" = runif(N,1,20),
  "Observed" = runif(N,1,20)
)
df[["PKratio"]]<-(df$Simulated)/(df$Observed)

#grp_list = list( list(name = "Age" , BinEdges = c(22,25,30)  , NumBins = NULL , Caption = "New Caption" ) , list(name = "Population" , BinEdges = NULL, NumBins = NULL) )
grp_list = list( list(name = "Age" , BinEdges = NULL  , NumBins = 4 , Caption = "New Caption" ) , list(name = "Population" , BinEdges = NULL, NumBins = NULL) )


colorGroupingName <- c("NewColorGrouping")
df_grp<-getGrouping(df, grp_list,colorGroupingName)  
print(df_grp)


override_caption <- TRUE
if (override_caption) {
  #df_grp<-changeCaption(df_grp)
  #print(df_grp)
  df_grp<-changeCaption(df_grp,1,c("Old Asians"))
  print(df_grp)  
}



df<-cbind(df,df_grp)


plot_pkrk <-function(df,colorGrouping=NULL,shapeGrouping=NULL,sizeGrouping=NULL) {
  
  if (!is.null(colorGrouping)){
  color_inp <-df[[colorGrouping]]
  } else {
    color_inp<-NULL
  }
  
  if (!is.null(shapeGrouping)){
    shape_inp <-df[[shapeGrouping]]
  } else {
    shape_inp<-NULL
  }
  
  if (!is.null(sizeGrouping)){
    size_inp <-df[[sizeGrouping]]
  } else {
    size_inp<-NULL
  }
  
  pkrp <-ggplot(df, aes(x=df$Age, y=df$PKratio , color =  color_inp , shape = shape_inp, size = size_inp )) + geom_point()+ labs(title = "pkratios for each age", x="ages" , y = "pkratio" , color = colorGrouping , shape = shapeGrouping , size = sizeGrouping  )
  pkrp <-pkrp + geom_hline(yintercept=1, linetype="solid", color = "black", size = 2)
  pkrp <-pkrp + geom_hline(yintercept=0.5, linetype="dashed", color = "blue")
  pkrp <-pkrp + geom_hline(yintercept=2, linetype="dashed", color = "blue")
  pkrp <-pkrp + geom_hline(yintercept=0.25, linetype="dashed", color = "red")
  pkrp <-pkrp + geom_hline(yintercept=4, linetype="dashed", color = "red")
  
  return(pkrp)
}
pkrp <- plot_pkrk(df,colorGrouping=colorGroupingName)
show(pkrp)


