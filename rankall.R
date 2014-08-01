rankall <- function(outcome, num = "best") {
  ## Load the data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check for input errors
  ## TODO
  
  ## Create vars to hold state and hospital names.
  states <- vector()
  hosp <- vector()
  
  ## Set the outcome column
  if (outcome == "heart attack"){
    outcome <- 11
  }
  
  if (outcome == "heart failure") {
    outcome <- 17
  }
  
  if (outcome == "pneumonia") {
    outcome <- 23
  }
  
  ## For each state...
  ## Find the first state.
  if (nrow(data) != 0) {
    state <- as.vector(data[1,7])
    states <- c(states, state)
  
    ## MOVE all hospitals for that state to a new data frame with the necessary columns.
    state_df <- subset(data, State == state)
    ## TODO, now delete them from the source data frame
    data <- subset(data, State != state)
  
    ## Sort the new state-specific data frame.
    state_df <- state_df[with(state_df, order(state_df[,outcome], state_df[,2])),]
  
    ## Pull the desired hospital and add it to a result data frame.
    hosp <- c(hosp, state_df[num,2])
  }
  
  ## Create a new DF and populate it
  answer <- data.frame(states, hosp)
  answer
  
  
}