rankall <- function(outcome, num = "best") {
  ## Load the data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  if (outcome != "heart attack" && outcome != "heart failure" && outcome != "pneumonia"){
    stop("invalid outcome")
  }
  
  ## Create some vars to use later
  wstates <- vector()
  whospitals <- vector()
  if (num == "best" | num == "worst") {
    rank <- 1
  }
  if (num != "best" && num !="worst") {
    rank <- num
  }
  worst <- FALSE
  if (num == "worst") {
    worst <- TRUE
  }
  
  ## Create the working dataframe.
  hospitals <- data[,2]
  states <- data[,7]
  if (outcome == "heart attack") {
    outcomes <- as.numeric(data[,11])
  }
  if (outcome == 'heart failure') {
    outcomes <- as.numeric(data[,17])
  }
  if (outcome == "pneumonia") {
    outcomes <- as.numeric(data[,23])
  }
  wdata <- data.frame(hospitals, states, outcomes)
  wdata <- wdata[complete.cases(wdata),]
  
  ## Loop throught the data for each state.
  while (nrow(wdata) != 0) {
    wstate <- as.vector(wdata[1,2])
    wstates <- c(wstates, wstate)
    
    ## MOVE all hospitals for that state to a new data frame with the necessary columns.
    state_df <- subset(wdata, states == wstate)
    ## TODO, now delete them from the source data frame
    wdata <- subset(wdata, states != wstate)
    
    ## Sort the new state-specific data frame.
    state_df <- state_df[with(state_df, order(outcomes, hospitals)),]
    if (worst == TRUE) {
      state_df <- state_df[with(state_df, order(-outcomes, hospitals)),]
    }
    ## Pull the desired hospital and add it to a result data frame.
    whospital <- as.vector(state_df[rank, 1])
    whospitals <- as.vector(c(whospitals, whospital))
  }
  
  answer <- data.frame(whospitals, wstates)
  answer <- answer[with(answer, order(wstates)),]
  names(answer)[names(answer) == "whospitals"] <- "hospital"
  names(answer)[names(answer) == "wstates"] <- "state"
  row.names(answer) <- wstates
  answer
  
}