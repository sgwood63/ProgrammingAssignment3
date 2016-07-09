nthRankedHospital <- function(stateData, outcomeFocus, rank = 1) {
  ## assumed to be called from rankhospital, so parameters are valid
  
  ## if passed rank is beyond the number of hospitals in the state,
  ##    return NA
  if (rank > nrow(stateData)) {
    return(NA)
  }
  
  ## sort the stateData by outcome and then hospital name
  ## and return the hospital name
  stateData[order(stateData[,outcomeFocus],stateData[,'Hospital.Name']), ][rank,'Hospital.Name']
}

rankhospital <- function(stateData, outcomeFocus, num = "best") {
  ## Arguments:
  ##  stateData: 2 letter US state code or territory, including PR (Puerto Rico), GU (Guam)
  
  ## outcomeFocus is a column name in the data set

  ## if the stateDate is empty, we were given a bad state
  if (nrow(stateData) == 0) {
    return(NA)
  }
  
  #print(paste("# Hospitals in ", state, ":", nrow(stateData)))
  
  if (is.numeric(num) && num > 0) {
    # get the nuth ranked hospital for the metric in the state
    return(nthRankedHospital(stateData, outcomeFocus, num))
    
  } else if (num =='best') {
    ## find the minimum value for the outcome across hospitals in the state
    ## there may be some NAs: ignore them
    valueForOutcome <- min(stateData[[outcomeFocus]], na.rm = TRUE)
    #print(paste("MIN for outcome", outcomeFocus, ":", valueForOutcome))
    
  } else if (num =='worst') {
    ## find the maxmum value for the outcome across hospitals in the state
    ## there may be some NAs: ignore them
    valueForOutcome <- max(stateData[[outcomeFocus]], na.rm = TRUE)
    #print(paste("MAX for outcome", outcomeFocus, ":", valueForOutcome))
    
  } else {
    stop('invalid num')
  }
  
  if (is.na(valueForOutcome)) {
    return(NA)
  }
  
  ## find the hospitals which have the min/max required value for the particular outcome
  
  hospitalsWithOutcome <- subset(stateData, stateData[[outcomeFocus]] == valueForOutcome)
  #print(paste("# Hospitals with outcome: ", nrow(hospitalsWithOutcome)))
  
  if (is.na(hospitalsWithOutcome) || nrow(hospitalsWithOutcome) == 0) {
    return(NA)
  }
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  
  ## there may be multiple hospitals with the same outcome value.
  ## return the first alphabetically, as per the assignment instructions
  bestHospitals <- sort(hospitalsWithOutcome[['Hospital.Name']])
  bestHospitals[1]
}


rankall <- function(outcome, num = "best") {
  ## Arguments:
  ##
  ## outcome is one of "heart attack", "heart failure", or "pneumonia"
  ##    this is mapped to a particular column in the data set
  ##
  ##  num: 'best', 'worst' or an integer that selects the Xth best hospital for the outcome
  
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

  ## initialize result data frame
  rankedHospitalsPerState <- NULL
  
  ## For each state, find the hospital of the given rank
  
  for (state in sort(unique(outcomeSet$State))) {
    ## Narrow to data for the state that is valid for the outcome
    
    ##print(paste("Processing state: ", state))
    
    stateData <- subset(outcomeSet, State == state)
    hospital <- rankhospital(stateData, outcomeFocus, num)
    rankedHospitalsPerState <- rbind(rankedHospitalsPerState, c(hospital, state)) 
  }

  if (!is.null(rankedHospitalsPerState)) {
    colnames(rankedHospitalsPerState) <- c('hospital', 'state')
    rownames(rankedHospitalsPerState) <- rankedHospitalsPerState[,'state']
    #transform(rankedHospitalsPerState, hospital = as.factor(hospital,
    #                                    state = as.factor(state)))
  }
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  rankedHospitalsPerState
}