## find the best hospital (name) for a given US state and outcome type

best <- function(state, outcome) {
  ## Arguments:
  ##  state: 2 letter US state code or territory, including PR (Puerto Rico), GU (Guam)

  ## outcome is one of "heart attack", "heart failure", or "pneumonia"
  ##    this is mapped to a particualr column in the data set
  
  if (outcome == "heart attack") {
    outcomeFocus <-'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack'
  } else if (outcome == "heart failure") {
    outcomeFocus <-'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure'
  } else if (outcome == "pneumonia") {
    outcomeFocus <-'Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia'
  } else {
    stop("invalid outcome")
  }
  
  ## Read outcome data
  outcomeSet <- read.csv('outcome-of-care-measures.csv', 
                         na.strings = c("Not Available"), stringsAsFactors = FALSE)
  
  ## Narrow to data for the state
  stateData <- subset(outcomeSet, State == state)
  
  ## if the stateDate is empty, we were given a bad state
  if (nrow(stateData) == 0) {
    stop('invalid state')
  }
  
  #print(paste("# Hospitals in ", state, ":", nrow(stateData)))
  
  ## find the minimum value for the outcome across hospitals in the state
  ## there may be some NAs: ignore them
  minForOutcome <- min(stateData[[outcomeFocus]], na.rm = TRUE)

  #print(paste("min for outcome", outcomeFocus, ":", minForOutcome))
  
  ## find the hospitals which have the minimum for the particular outcome

  hospitalsWithMin <- subset(stateData, stateData[[outcomeFocus]] == minForOutcome)
  #print(paste("# Hospitals with outcome minimum: ", nrow(hospitalsWithMin)))
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  
  ## there may be multiple hospitals with the same lowest value.
  ## return the first alphabetically, as per the assignment instructions
  bestHospital <- sort(hospitalsWithMin['Hospital.Name'])[1]
  bestHospital[1,'Hospital.Name']
}