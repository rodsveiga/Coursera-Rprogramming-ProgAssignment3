## 2 - FINDING THE BEST HOSPITAL IN A STATE

## The first argument is the 2-character abreviated name of a state [,7].

## The outcomes can be one of "heart attack" [,11], "heart failure" [,17], or "pneumonia" [,23] 

best <- function(given_state, outcome){
  
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
      coln = 11
      care_measures[,11] <- as.numeric(care_measures[,11])
    }
    if(outcome == "heart failure"){
      coln = 17
      care_measures[,17] <- as.numeric(care_measures[,17])
    }
    if(outcome == "pneumonia"){
      coln = 23
      care_measures[,23] <- as.numeric(care_measures[,23])
    }

  } 
  
  ## Return hospital name in that state with lowest 30-day death 
  
  ## Here we split the data in a list by states, so we can take a look in the given state element
  state_data <- split(care_measures, care_measures$State)
  
  ## Splited_data: given_state element of the state_data list.
  splited_data <- state_data[[given_state]]
  ## Note that splited_data is a data.frame, while state_data is a list

  ## Now we create a new data.frame, which contains only the lines which refer to the minimum
  ## we want. Note the introduction of the which() function.
  output <- splited_data[which(splited_data[,coln]== min(splited_data[,coln], na.rm = TRUE)), ]

  ## Handling ties: Alphabetical order: note the introduction of the sort() function
  output_alphab <- sort(output[,2])
  output_alphab[1]

}