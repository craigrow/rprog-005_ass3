best <- function(state, outcome) {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Coerce the death rate columns to be numeric. 
  ## Heart.Attack is col 11.
  data[,11] <- as.numeric(data[,11])
  
  ## Heart.Failure is col 17.
  data[,17] <- as.numeric(data[,17])
  
  ## Pneumonia is col 23.
  data[,23] <- as.numeric(data[,23])
  
  ## Check that the state and outcome are valid
  if (outcome != "heart attack" && outcome != "heart failure" && outcome != "pneumonia"){
    stop("invalid outcome")
  }
  
  if (state %in% data[,7] == FALSE) {
    stop("invalid state")
  }
  
  ## Return the hospital in the state with the lowest 30 death rate.
  oss <- subset(data, State == state)
  if (outcome == "heart attack"){
    bad <- is.na(oss[,11])
    minob <- min(oss[,11][!bad])
    besth <- subset(oss, oss[,11] == minob)
  }
  
  if (outcome == "heart failure"){
    bad <- is.na(oss[,17])
    min(oss[,17][!bad])
    minob <- min(oss[,17][!bad])
    besth <- subset(oss, oss[,17] == minob)
  }
  
  if (outcome == "pneumonia"){
    bad <- is.na(oss[,23])
    min(oss[,23][!bad])
    minob <- min(oss[,23][!bad])
    besth <- subset(oss, oss[,23] == minob)
  }
  
  besth[,2]
  
}