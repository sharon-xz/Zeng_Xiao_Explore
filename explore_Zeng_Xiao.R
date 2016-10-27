library(plyr)

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
  rsquared <- cbind()
  
  for (i in (1:ncol(dataframe))){
  name1 = names(diamonds)[i]  #getting the first column name
  
  for (j in (i+1:ncol(dataframe))){  #only getting the columns after the first one
    if  (is.na(names(diamonds)[j]) == FALSE){
      if (names(diamonds)[j] != names(diamonds)[i]){
        name2 =names(diamonds)[j]}   #getting the second column name
      name_pairs <- rbind(name_pairs,paste(name1,name2,sep = "-", collapse = NULL))  #paste them in the "name1-name2" format
      
      if (is.numeric(dataframe[,name1]) == TRUE){  #only calculates the correlation for numeric columns
        if (is.numeric(dataframe[,name2]) == TRUE){
          linear_model<- lm(dataframe[,name1] ~ dataframe[,name2]) 
          rsquared<- rbind(rsquared, summary(linear_model)$r.squared)}#calculating the pearson correlations
        else{
          rsquared<- rbind(rsquared, NA)  #for non-numeric columns, the correletion is NA to avoid errors
        }
      }
      else{
        rsquared<- rbind(rsquared, NA)
      }
      
    }
  }

}

output = data.frame(name_pairs,rsquared) #combining two columns
return(output)
}

correlation <- function(dataframe,cutoff_value){
  #This function accept any dataframe as a parameter and returns a dataframe that contains each pair of column names in the first column in a single string separated by a -, 
  #and their corresponding Pearson correlation coefficient in the second column.
  #Parameters: A dataframe
  #Returns: A dataframe
  
  name_pairs <- cbind()  #Initiation
  correlations <- cbind()
  
  for (i in (1:ncol(dataframe))){
    name1 = names(diamonds)[i]  #getting the first column name
    
    for (j in (i+1:ncol(dataframe))){  #only getting the columns after the first one
      if  (is.na(names(diamonds)[j]) == FALSE){
        if (names(diamonds)[j] != names(diamonds)[i]){
          name2 =names(diamonds)[j]}   #getting the second column name
          name_pairs <- rbind(name_pairs,paste(name1,name2,sep = "-", collapse = NULL))  #paste them in the "name1-name2" format
        
        if (is.numeric(dataframe[,name1]) == TRUE){  #only calculates the correlation for numeric columns
          if (is.numeric(dataframe[,name2]) == TRUE){
            correlations<- rbind(correlations, cor(dataframe[,name1],dataframe[,name2], method="pearson"))}#calculating the pearson correlations
          else{
            correlations<- rbind(correlations, NA)  #for non-numeric columns, the correletion is NA to avoid errors
          }}
        else{
          correlations<- rbind(correlations, NA)
        }
        
      }
    }
    
  }
  
  output = data.frame(name_pairs,correlations) #combining two columns
  return(output)
}



plots <- function(dataframe, switch){}


