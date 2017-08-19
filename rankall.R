rankall <- function(outcome, num = "best") {
  ## Read outcome data
  ds <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ds <- ds[,c(2, 7, 11, 17, 23)]
  names(ds) = c('hospital', 'state', 'heart attack', 'heart failure', 'pneumonia')
  
  ## Check that outcome is valid
  if (!(outcome %in% names(ds))) {
    stop("invalid outcome")
  }
  
  ## For each state, find the hospital of the given rank
  rs <- data.frame('hospital' = character(),
                   'state' = character())
  states <- unique(ds[,'state'])
  states <- sort(states)
  for (s in states) {
    x <- ds
    x <- x[x$state == s, c('hospital', outcome)]
    x[,outcome] <- as.numeric(x[,outcome])
    x <- x[complete.cases(x),]
    x <- x[order(x[,outcome], x$hospital),]
    if (num == "best") {
      y <- x[1, 'hospital']
    } else if (num == "worst") {
      y <- x[nrow(x), 'hospital']
    } else {
      y <- x[num,'hospital']
    }
    rs <- rbind(rs, data.frame('hospital' = y, 'state' = s, row.names = s))
  }
  
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  rs
}