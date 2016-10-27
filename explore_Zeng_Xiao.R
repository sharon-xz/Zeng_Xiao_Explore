library(plyr)
data(diamonds)

explore <- function(dataframe, switch, cutoff_value, bin_value){
  explore_list <- list("explore")
  explore_list$frequency_table <- frequency_table(dataframe)
  explore_list$summary_column <- summary_table(dataframe)
  explore_list$r_squared <- rsquare(dataframe)
  if(missing(bin_value)){}
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
  name_pairs <- cbind()  #Initiation
  rsquare <- cbind()
  i <- 0
  columns <- dataframe[sapply(dataframe, is.numeric)]
  
  if(ncol(columns) >= 2) {
    pairs <- combn(colnames(columns), 2) 
    
    name_pairs <- paste(pairs[1,], pairs[2, ], sep = "-") 
    for (i in (1:ncol(pairs))){
      x <- pairs[1,i]
      y <- pairs[2,i]
      lmodel <- lm(dataframe[x]~ dataframe[y])
      rsquare <- rbind(rsquare,summary(lmodel)$r.squared)
    }
    
    #rsquared <- paste(summary(lm(dataframe[pairs[1,]]~ dataframe$pairs[[2,]]))$r.squared)
    
    
    #rsquared <- lapply(lm(pairs[1,]~ pairs[2,]), summary$r.squared)
    
    result <- data.frame(pairs, rsquared) 
    return(result)
    
  }
  else{
    print("R-squared cannot be computted because there are not enough numeric columns")}
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
  graph<-ggplot(dataframe,aes(f_columns, colour = "#808080")) 
  
  graph1<-ggplot(dataframe,aes(price, colour = "#0000ff" )) +geom_histogram(binwidth = bin_value) 
  graph2<-ggplot(dataframe,aes(density(price), colour = "#0000ff")) +geom_histogram(binwidth = bin_value) 
}


