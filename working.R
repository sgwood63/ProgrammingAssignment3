
  ## Read outcome data
  outcomeSet <- read.csv('outcome-of-care-measures.csv')
  state <- 'NY'
  outcome <- 'pneumonia'
  
  #outcome <- "heart attack"
  #outcome <- "pneumonia"
  
  if (outcome == "heart attack") {
    outcomeFocus <-'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack'
  } else if (outcome == "heart failure") {
    outcomeFocus <-'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure'
  } else if (outcome == "pneumonia") {
    outcomeFocus <-'Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia'
  } else {
    stop("invalid outcome")
  }
  
  #stateData <- outcomeSet[outcomeSet[,'State'] == state,]
  stateData <- subset(outcomeSet, State == state)
  
  
  if (nrow(stateData) == 0) {
    stop('invalid state')
  }
  print(paste("# Hospitals in ", state, ":", nrow(stateData)))
  
  levelsAsNumeric <- as.numeric(levels(stateData[[outcomeFocus]]))
  
  minForOutcome <- min(stateData[[outcomeFocus]], na.rm = TRUE)
  
  # minForOutcome is numeric
  
  print(paste("min for outcome", outcomeFocus, ":", minForOutcome))
  
  #need to compare the numeric with the factor levels
  #hospitalsWithMin <- stateData[as.numeric(as.character(stateData[[outcomeFocus]])) == minForOutcome,]
  
  hospitalsWithMin <- subset(stateData, as.numeric(as.character(stateData[[outcomeFocus]])) == minForOutcome)
  hospitalsWithMin <- subset(stateData, stateData[[outcomeFocus]] == minForOutcome)
  print(paste("# Hospitals with outcome minimum: ", nrow(hospitalsWithMin)))
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  #sort(hospitalsWithMin['Hospital.Name'])
  bestHospital <- sort(hospitalsWithMin['Hospital.Name'])[1]
  bestHospital[1,'Hospital.Name']
  
  
  # source("best.R")
   best("TX", "heart attack")
  [1] "CYPRESS FAIRBANKS MEDICAL CENTER"
  
   best("TX", "heart failure")
  [1] "FORT DUNCAN MEDICAL CENTER"
  
   best("MD", "heart attack")
  [1] "JOHNS HOPKINS HOSPITAL, THE"
  
   best("MD", "pneumonia")
  [1] "GREATER BALTIMORE MEDICAL CENTER"
  
   > best("BB", "heart attack")
  
   Error in best("BB", "heart attack") : invalid state
  
   > best("NY", "hert attack")
  Error in best("NY", "hert attack") : invalid outcome
  >
