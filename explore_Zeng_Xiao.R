library(plyr)
data(diamonds) #for testing purpose

explore <- function(dataframe, switch, cutoff_value, bin_value){
  explore_list <- list("explore")
  explore_list$frequency_table <- frequency_table(dataframe)
  explore_list$summary_column <- summary_table(dataframe)
  explore_list$r_squared <- rsquare(dataframe)
  
  
  if(missing(bin_value)){
    if (switch == "on") {
      explore_list$plots <- plots(dataframe, switch)
    }
    if (switch == "grid"){
      explore_list$plots <- plots(dataframe, switch)
    }
    
  }
  else{
    if (switch == "on") {
      explore_list$plots <- plots(dataframe, switch, bin_value)
    }
    if (switch == "grid"){
      explore_list$plots <- plots(dataframe, switch, bin_value)
    }
  }
  
  
  
  as.list(explore_list)
  
  return(explore_list)
}

frequency_table <- function(dataframe){
  column <-sapply(dataframe, is.factor)
  return(lapply(dataframe[,column],table))
}

summary_table <- function(dataframe){
  column <-sapply(dataframe, is.numeric)
  return(lapply(dataframe[,column],summary))
}


rsquare <- function(dataframe){
  #This function accept any dataframe as a parameter and returns a dataframe that contains each pair of column names in the first column in a single string separated by a -, 
  #and their corresponding Pearson correlation coefficient in the second column.
  #Parameters: A dataframe
  #Returns: A dataframe
  
  name_pairs <- cbind()  #Initiation
  rsquare <- cbind()
  
  for (i in (1:ncol(dataframe))){
    name1 = names(diamonds)[i]  #getting the first column name
    
    for (j in (i+1:ncol(dataframe))){  #only getting the columns after the first one
      if  (is.na(names(diamonds)[j]) == FALSE){
        if (names(diamonds)[j] != names(diamonds)[i]){
          name2 =names(diamonds)[j]}   #getting the second column name
        name_pairs <- rbind(name_pairs,paste(name1,name2,sep = "-", collapse = NULL))  #paste them in the "name1-name2" format
        
        if (is.numeric(dataframe[,name1]) == TRUE){  #only calculates the correlation for numeric columns
          if (is.numeric(dataframe[,name2]) == TRUE){
            lmodel<- lm(dataframe[,name1]~dataframe[,name2])
            rsquare <- rbind(rsquare,summary(lmodel)$r.squared)
            
            
            }#calculating the pearson correlations
          else{
            rsquare<- rbind(rsquare, NA)  #for non-numeric columns, the correletion is NA to avoid errors
          }}
        else{
          rsquare<- rbind(rsquare, NA)
        }
        
      }
    }
    
  }
  
  output = data.frame(name_pairs,rsquare) #combining two columns
  return(output)
}




correlation <- function(dataframe,cutoff_value){
  
    
    columns <- dataframe[sapply(dataframe, is.numeric)] 
    
    
    if(ncol(columns) >= 2) {
      b <- combn(colnames(columns), 2) #finds all combinations of the name pairs
      
      pairs <- paste(b[1,], b[2, ], sep = "-") 
      
      c <- cor(columns, method = "pearson")
      
      correlation <- c[which(lower.tri(c))] 
      
      result <- data.frame(pairs, correlation) #create a new data frame with our pairs 
      return(result)
      
    }
    else  #print this message if we can't find Pearson correlation
      print("Pearson Correlation cannot be computted because there are not enough numeric columns")
  }


plots <- function(dataframe, switch, bin_value){
  
  f_columns <- dataframe[sapply(dataframe, is.factor)]
  graph<-ggplot(dataframe,aes(f_columns), colour = "#808080") 
  
  graph1<-ggplot(dataframe,aes(price, colour = "#0000ff" )) +geom_histogram(binwidth = bin_value) 
  graph2<-ggplot(dataframe,aes(density(price), colour = "#0000ff")) +geom_histogram(binwidth = bin_value) 
  
  
  ##plot histogram and bar 
  a=colnames(dataframe2)
  b=colnames(dataframe1)
  if(switch[1]==checkswitch[[1]] || switch[1]==checkswitch[[3]]){
    par(mfrow=c(3,3))
    for(i in 1:length(a)){
      pdf(paste(colnames(dataframe2[,i]),"_Htg.pdf")) ## save as pdf file. 
      mk=mean(dataframe2[,i])
      mx=max(dataframe2[,i])
      mi=min(dataframe2[,i])
      h=hist(dataframe2[,i], xlab="Density", xlim=c(mi-0.5, mx+0.5), ylab="Count", col="lightblue", main=paste(colnames(dataframe2[i]),"Histogram"))
      abline(v=mk,col="red",lwd=2)
      dev.off()
    }
    par(mfrow=c(2,2))
    for(j in 1:length(b)){
      pdf(paste(colnames(dataframe1[,j]),"_Bar.pdf"))
      bar=barplot(table(dataframe1[,j]), ylab="Counts", col="gray", main=paste(colnames(dataframe1[j]),"Gray Bar Plot"))
      dev.off()
    }
  }
  else if(switch[1]==checkswitch[[3]]){
    par(mfrow=c(3,3))
    for(i in 1:length(a)){
      pdf(paste(colnames(dataframe2[,i]),"_Htg.pdf"))
      mk=mean(dataframe2[,i])
      mx=max(dataframe2[,i])
      mi=min(dataframe2[,i])
      h=hist(dataframe2[,i], xlab="Density", xlim=c(mi-0.5, mx+0.5), ylab="Count", col="lightblue", main=paste(colnames(dataframe2[i]),"Histogram"))
      abline(v=mk,col="red",lwd=2)
      grid(NULL, NULL, lwd = 2, lty="dotted", col="red")
      dev.off()
    }
  }
  else{
    h=c()
    bar=c()
  }
  rhis=list(h)
  rbar=list(bar)
  ##If the plot switch parameter is "on" or "grid" then plot a pair of 
  ##blue histograms with a vertical red line at the mean for every numerical
  ##variable at each number of bins integer specified in the bin vector parameter.
  
  
  result<-list(result1,result2,result3,result4,rhis,rbar)
  return(result)
}





