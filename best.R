best <- function(state, outcome) {
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
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  ds <- ds[ds$state == state, c('hospital', outcome)]
  ds[,outcome] <- as.numeric(ds[,outcome])
  ds <- ds[complete.cases(ds),]
  mn <- min(ds[,outcome])
  ds <- ds[ds[outcome] == mn, 'hospital']
  ds <- sort(ds)
  ds[1]
}