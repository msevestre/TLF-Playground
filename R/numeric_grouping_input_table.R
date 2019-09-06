library(stringr)
library(ggplot2)



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


plot_pkrp <-function(df,colorGrouping=NULL,shapeGrouping=NULL,sizeGrouping=NULL) {
  
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

N<-20;

df <- data.frame(
  "IndividualID" = seq(1,N),
  "Population" = sapply(round(runif(N,0,1)),generate_df_column,output0="Asian",output1="European"),
  "Gender" = sapply(round(runif(N,0,1)),generate_df_column,output0="M",output1="F"),
  "Age" = round(runif(N,4,8)),
  "Compound" = sapply(round(runif(N,0,1)),generate_df_column,output0="Aspirin",output1="Sugar"),
  "Dose" = sapply(round(runif(N,0,1)),generate_df_column,output0=3,output1=6),
  "Organ" = sapply(round(runif(N,0,1)),generate_df_column,output0="VenousBlood",output1="VenousBlood"),
  "Compartment" = sapply(round(runif(N,0,1)),generate_df_column,output0="Plasma",output1="Plasma"),
  "Simulated" = runif(N,1,20),
  "Observed" = runif(N,1,20)
)
df[["PKratio"]]<-(df$Simulated)/(df$Observed)



print(df)



dfinp<-data.frame(  "Gender"  = c("M","F") ,  "Age" = c(6,5) ,  "Caption" = c("Males Aged 6" , "Females Aged 5") )
myCaptionString <- "Special cases"
names(dfinp)[names(dfinp) == "Caption"]<- myCaptionString
df<-addCaptionsColumn(df,dfinp)
pkrp<-plot_pkrp( na.omit(df) , colorGrouping=myCaptionString , shapeGrouping=NULL , sizeGrouping=NULL )
print(df)
show(pkrp)

ageBounds<-list(c(3,6),c(7,8))
dfinp_bnds<-data.frame(   "Age" = I(  ageBounds ) ,  "Caption" = c("Patients aged 3-6" , "Patients aged 7-8") )
myCaptionString <- "Age bounds"
names(dfinp_bnds)[names(dfinp_bnds) == "Caption"]<- myCaptionString
df<-addCaptionsColumn(df,dfinp_bnds)
print(df)
pkrp<-plot_pkrp( df , colorGrouping=myCaptionString , shapeGrouping=NULL , sizeGrouping=NULL )
show(pkrp)


#graphics.off()
