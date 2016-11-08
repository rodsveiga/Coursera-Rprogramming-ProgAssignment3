## 3 - RANKING HOSPITALS IN ALL STATES - v4

rankall <- function(outcome, num = "best"){
  
  care_measures <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
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
  
  my_data <- data.frame(c(care_measures$State),c(care_measures$Hospital.Name),c(care_measures[,coln]),stringsAsFactors=FALSE )
  
  state_data <- split(my_data, my_data[,1])
  
  num_states <- length(state_data)
  
  if( num == "best"){ num_int <- 1} 
    else{ 
        if (num == "worst"){ num_int <- -1 }
         else {
           num_int <- num 
              }
         }
  
  final <- data.frame("hospital" = character(num_states),
                      "state" = character(num_states),
                      stringsAsFactors=FALSE)

  
  for(i in 1:num_states){
    
    s_data <- state_data[[i]]

    
    df <- data.frame( s_data[ ,2], s_data[,3], stringsAsFactors=FALSE )
    
    df <- df[order(df[,2]), ] ## order(..., na.last = TRUE, decreasing = FALSE)
    
    ##### After the order(), after you do everything, now you remove the NA's
    df_good <- complete.cases(df)
    df <- df[df_good,]
    #####
    
    max_rank <- nrow(df)
    
    if( num == "worst"){ num_int <- max_rank }
    
    if( num_int > max_rank ){ 
      stocks <- c(NA, s_data[1,1])}
    else{
 
      ties1 <- duplicated(df[,2],fromLast = FALSE)
      ties2 <- duplicated(df[,2],fromLast = TRUE)
      ties <- ties1 | ties2

      if( ties[num_int] == FALSE ){
        stocks <- c( df[num_int,1], s_data[1,1] )

      } else{

        repeated <- df[ which(df[,2] == df[num_int,2] ), ]
        
        ####################
        # We store the positon of the object we want in the output dataframe
        vec <- which(repeated[,1] == df[num_int,1])
        
        
        ########################
        
        alphabetic <- sort(repeated[,1])

        
        if( num == "worst"){ 
          stocks <- c( alphabetic[max_rank], s_data[1,1] )
        } else{
          stocks <- c( alphabetic[vec], s_data[1,1])
          }
    
         }
    
    }
    
    final[i,] <- stocks

  }
  
 ## print("Output. Head:")
##  print(head(final,10))
  
##  print("Output. Tail:")
##  tail(final,10)
  
  final
  
}