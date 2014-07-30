rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Coerce the death rate columns to be numeric. 
  ## Heart.Attack is col 11.
  data[,11] <- as.numeric(data[,11])
  
  ## Heart.Failure is col 17.
  data[,17] <- as.numeric(data[,17])
  
  ## Pneumonia is col 23.
  data[,23] <- as.numeric(data[,23])
  
  ## Check that state and outcome are valid
  if (outcome != "heart attack" && outcome != "heart failure" && outcome != "pneumonia"){
    stop("invalid outcome")
  }
  
  if (state %in% data[,7] == FALSE) {
    stop("invalid state")
  }
  
  ## Return hospital in that state with the given rank
  ## in 30-day death rate
  
  
  ## For outcome == "heart attack"
  
  ## Sort the data for state by col 11.
  attach(dfs)
  sd <- dfs[order(dfs[,11]),]
  sd[num,2]
  
  
  
  
  
  
  
  
}