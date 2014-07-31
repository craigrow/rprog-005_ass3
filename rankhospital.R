rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  col <- 0
  worst <- FALSE
  if (num == "worst") {
    worst <- TRUE
  }
  
  ## Check that state and outcome are valid
  if (outcome != "heart attack" && outcome != "heart failure" && outcome != "pneumonia"){
    stop("invalid outcome")
  }
  
  if (state %in% data[,7] == FALSE) {
    stop("invalid state")
  }
    
  ## Subset data 
  data <- subset(data, State == state)
  
  if (num == "best" | num == "worst") {
    num <- 1
  }
  
  if (outcome == "heart attack") {
    col <- 11
  }
  
  if (outcome == "heart failure") {
    col <- 17
  }
  
  if (outcome == "pneumonia") {
    col <- 23
  }
  
  names <- data[,2]
  
  outcomes <- as.numeric(data[,col])
  wdata <- data.frame(names, outcomes)
  wdata <- wdata[complete.cases(wdata),]
  
  if (worst == FALSE) {
    wdata <- wdata[with(wdata, order(outcomes, names)),]
  }
  
  if (worst == TRUE) {
    wdata <- wdata[with(wdata, order(-outcomes, names)),]
  }
  answer <- as.vector(wdata[num,1])
  answer
}