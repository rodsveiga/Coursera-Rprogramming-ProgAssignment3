## 3 - RANKING HOSPITALS BY OUTCOME IN A STATE

rankhospital <- function(given_state, outcome, num = "best"){
  
  ## Reading the "Outcome of Care Measures" data
  care_measures <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check if the state is valid
  states <- unique(care_measures[,7]) ## Vectors of all the states in the data frame
  if(given_state %in% states == FALSE){ 
    stop("Invalid state.")
  }
  
  ## Check if the outcome is valid
  outcome_testvec <- c("heart attack", "heart failure", "pneumonia")
  if(outcome %in% outcome_testvec == FALSE){ 
    stop("Invalid outcome.")
  } else{
    ## As the names and the original data frame are too long we prefer to work with "coln"
    ## As we read the data as "character", we coerce to numeric the columns we want
    if(outcome == "heart attack"){
      coln <- 11
      care_measures[,11] <- as.numeric(care_measures[,11])
    }
    if(outcome == "heart failure"){
      coln <- 17
      care_measures[,17] <- as.numeric(care_measures[,17])
    }
    if(outcome == "pneumonia"){
      coln <- 23
      care_measures[,23] <- as.numeric(care_measures[,23])
    }
  }
  
  
  ## Return hospital name in that state with a given ranking 
  
  ## Here we split the data in a list by states, so we can take a look in 
  ## the given state element
  state_data <- split(care_measures, care_measures$State)
  
  ## Splited_data: given_state element of the state_data list.
  splited_data <- state_data[[given_state]]
  ## Note that splited_data is a data.frame, while state_data is a list
  
  ## Now that we heve splited the date in the state we want, for simplicity, 
  ## let's store only the two column we want.
  ## Column 1: Hospital Name
  ## Column 2: The outcome data
  my_data <- data.frame( c(splited_data[ ,2]), c(splited_data[,coln]) )
  
  ## To get the ranking we have to order the column2. We use the order() function
  my_data <- my_data[order(my_data[,2]),]
  
  ##### After the order(), after you do everything, now you remove the NA's
  mydata_good <- complete.cases(my_data)
  my_data <- my_data[mydata_good,]
  
  ############
  
  ## Printing the output:
  
  ## If num == "best" we want my_data[1,1]
  ## If num == "worst" we want my_data[nrow(my_data),1]
  ## if num == integer we want my_data[integer,1]
  if( num == "best"){
    aux <- 1
  } else{ 
    if (num == "worst"){
      aux <- nrow(my_data)
    } else {
      aux <- num 
    }
  }

  if( aux > nrow(my_data)){ 
    NA }
    else{
  ##################################################
  
  ties1 <- duplicated(my_data[,2],fromLast = FALSE)
  ties2 <- duplicated(my_data[,2],fromLast = TRUE)
  ties <- ties1 | ties2
  
  if(ties[aux] == FALSE){
    print(as.character(my_data[aux,1]))
  } else{
    
    # Here we have all the tied data in a dataframe called output
    output <- my_data[ which(my_data[,2] == my_data[aux,2] ), ]
    
    # We store the positon of the object we want in the output dataframe
    vec <- which(output[,1] == my_data[aux,1])
    
    # Orderin the output dataframe
    output_alphab <- sort(output[,1])
    
    as.character(output_alphab[vec])
    
    if(num == "worst"){
      print(as.character(output_alphab[nrow(output)]) )
    } else{
      print(as.character(output_alphab[vec]))
    }
  }
  
}
  
}