#HW 7&8 Resubmission
#Please see HW8 at the branch "hw8"
#Xiao Zeng

library(plyr)
library(ggplot2)
library(grid)
library(gridExtra)

explore <- function(dataframe, switch, cutoff_value, bin_value){
  #This function accepts any dataframe as a parameter and returns a list of related statistical information about this dataframe. 
  
  #Parameters: A dataframe, a switch value(on, off, grid), a cutoff_value for correlations, a list of bin_values
  
  #Returns: A list of statistics and plots
  
  
  if (is.data.frame(dataframe) == TRUE){ #Checking if the input is a dataframe
  
    explore_list <- list("explore") #Initiation of a list
    explore_list$frequency_table <- frequency_table(dataframe) 
  
    explore_list$summary_column <- summary_table(dataframe)
  
    explore_list$r_squared <- rsquare(dataframe)
  
    explore_list$correlation <- correlation(dataframe ,cutoff_value)
  
  
    if(missing(bin_value)){ #if the user does not provide the list of bin values
      if (switch == "on" || switch == "grid"){ #if the swicth is "on" or "grid", then we call the plots function
        explore_list$plots <- plots(dataframe, switch)}
    }
      
  
    
    else{ #if the user has provided a list of bin values
      if (is.list(bin_value)){ #Checking if the bin value is a valid list
        if (switch == "on" || switch == "grid"){
          explore_list$plots <- plots(dataframe, switch, bin_value)
        }
      }
  
     else{ #if the bin value is not a valid list
         return("Please input a valid bin value as a list of values")}
    }
      
   
   as.list(explore_list) #Making a list
  
   return(explore_list)
  }
  if (is.data.frame(dataframe) == FALSE){ #If the dataframe is not valid, asking the user to input a valid dataframe
    return("Please input a valid dataframe")}
  
}



frequency_table <- function(dataframe){
  #This function accept any dataframe as a parameter and returns a frequency table for every categorical and logical variable
  #Parameters: A dataframe
  #Returns: A frequency table
  column <-sapply(dataframe, is.factor)
  return(lapply(dataframe[,column],table))
}

summary_table <- function(dataframe){
  #This function accept any dataframe as a parameter and returns a summary statistics table for each numerical variable
  #Parameters: A dataframe
  #Returns: A summary table
  column <-sapply(dataframe, is.numeric)
  return(lapply(dataframe[,column],summary))
}


rsquare <- function(dataframe){
  #This function accept any dataframe as a parameter and returns a dataframe that contains each pair of column names in the first column in a single string separated by a -, 
  #and their corresponding r-squared values in the second column.
  #Parameters: A dataframe
  #Returns: A dataframe
  
  name_pairs <- cbind()  #Initiation
  rsquare <- cbind()
  ##Prof G - Calling out diamonds specifically here
  ##Prof G - makes the code non-generalized and
  ##Prof G - specific only to diamonds
  for (i in (1:ncol(dataframe))){
    name1 = names(diamonds)[i]  #getting the first column name
    
    for (j in (i+1:ncol(dataframe))){  #only getting the columns after the first one
      if  (is.na(names(diamonds)[j]) == FALSE){
        if (names(diamonds)[j] != names(diamonds)[i]){
          name2 =names(diamonds)[j]}   #getting the second column name
        name_pairs <- rbind(name_pairs,paste(name1,name2,sep = "-", collapse = NULL))  #paste them in the "name1-name2" format
        
        if (is.numeric(dataframe[,name1]) == TRUE){  #only calculates the r-squared value for numeric columns
          if (is.numeric(dataframe[,name2]) == TRUE){
            lmodel<- lm(dataframe[,name1]~dataframe[,name2])
            rsquare <- rbind(rsquare,summary(lmodel)$r.squared)
            #Creating a linear model and get the r squared value
            
            }
          else{
             rsquare<- rbind(rsquare, NA)  #for non-numeric columns, the r-squared is NA to avoid errors
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
    #This function accept any dataframe as a parameter and returns a dataframe that contains each pair of column names in the first column in a single string separated by a -, 
    #and their corresponding Pearson correlation coefficient in the second column.
    #Parameters: A dataframe and a cutoff value
    #Returns: A dataframe
  
  
    
    columns <- dataframe[sapply(dataframe, is.numeric)]  #getting numeric columns
    
    
    if(ncol(columns) >= 2) {
      b <- combn(colnames(columns), 2) #finds all combinations of the name pairs
      
      pairs <- paste(b[1,], b[2, ], sep = "-") 
      
      c <- cor(columns, method = "pearson")
      
      correlation <- c[which(lower.tri(c))] 
      correlation <- sapply(correlation, function(x) ifelse(x > cutoff_value, x, 'Less than Threshold')) #Replacing values that are less than the threshold with error message
      result <- data.frame(pairs, correlation) #Combining both columns
      names(result)<-c("Variable Pairs", "Pearson Exceeds Threshold")
      return(result)
      #r[which(abs(r) > t)]
    }
    else  #if we can't find Pearson correlation
      print("Pearson Correlation cannot be computted because there are not enough numeric columns")
  }




plots <- function(dataframe, switch, bin_value){

  
  #This function accept any dataframe as a parameter and returns a pair of blue histograms with a vertical red line at the mean for numerical coloums
  #For non-numeric columns, a gray bar graph would be printed
  
  #Parameters: A dataframe, a switch value(on, off, grid), a list of bin_values
  
  #Returns: None, but the graphs are printed on screen. 
  
  
  
  for (i in (1:ncol(diamonds))){
    j <- diamonds[,i]  #every column
    if (is.numeric(j)==TRUE){ #For numeric columns
      
      if(missing(bin_value)){ #For missing values

        print(ggplot(dataframe,aes(j)) +geom_density(fill = 'blue') + abline(v=mean(j),col = "red")) #Density graph without binwidth value
        print(ggplot(dataframe,aes(j)) +geom_histogram(fill = 'blue') +abline(v=mean(j),col = "red") )#Count graph without binwidth value
        
      }
      else{
        
        for (k in bin_value){
          print(ggplot(dataframe,aes(j)) +stat_density(bw =k, fill = 'blue')+abline(v=mean(j),col = "red")) #Density graph with binwidth value
          print(ggplot(dataframe,aes(j)) +geom_histogram(binwidth = k,fill = 'blue')+abline(v=mean(j),col = "red")) #Count graph with binwidth value
          
        }
      }
      
    }
    else{#For non numeric columns,plot a gray bar graph
     
      
      print(ggplot(dataframe,aes(j)) +stat_count())
      
      
    }
  }
  if(missing(bin_value)){}
  else{  #For the griding part
    if (switch=='grid'){
      plot.new()
      
      l <- length(bin_value)
      
      for (k in bin_value){#Creating grid for density histogram
        
        figure1<-ggplot(dataframe,aes(j)) +stat_density(bw =k, fill = 'blue')+abline(v=mean(j),col = "red")}
      
      
      
      
      l <- length(bin_value)
      
      for (k in bin_value){#Creating grid for count histogram
        figure2<-ggplot(dataframe,aes(j)) +geom_histogram(binwidth = k,fill = 'blue')+abline(v=mean(j),col = "red")
        
        
      }
      
      print(grid.arrange(figure1,figure2)) #Putting the graphs in one graph
    }
  }
  return("All the graphs are printed on screen")
}


bv = list(10,20,30)
explore(diamonds,'on',0.5, bv)

