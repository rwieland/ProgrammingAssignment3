rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  ds <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ds <- ds[,c(2, 7, 11, 17, 23)]
  names(ds) = c('hospital', 'state', 'heart attack', 'heart failure', 'pneumonia')
  
  ## Check that state and outcome are valid
  if (!(state %in% ds[,'state']))  {
    stop("invalid state")
  } 
  if (!(outcome %in% names(ds))) {
    stop("invalid outcome")
  }
  
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  ds <- ds[ds$state == state, c('hospital', outcome)]
  ds[,outcome] <- as.numeric(ds[,outcome])
  ds <- ds[complete.cases(ds),]
  ds <- ds[order(ds[,outcome], ds$hospital),]
  if (num == "best") {
    ds[1, 'hospital']
  } else if (num == "worst") {
    ds[nrow(ds), 'hospital']
  } else {
    ds[num,'hospital']
  }
}