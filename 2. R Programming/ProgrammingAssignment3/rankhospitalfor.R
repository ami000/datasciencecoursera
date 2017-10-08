rankhospital <- function(state, outcome, num = "best") {
       
       ## Read outcome data
       
       data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
       
       ## Check that state and outcome are valid
       
       states <- unique(data[, 7])
       outcomes <- c("heart attack", "heart failure", "pneumonia") 
       
       if (!state %in% states) {
              stop("invalid state")
       }
       
       if (!outcome %in% outcomes) {
              stop("invalid outcome")
       }
       
       if (outcome == "heart attack") {
              outcome <- 11
       }
       
       if (outcome == "heart failure") {
              outcome <- 17
       }
       
       if (outcome == "pneumonia") {
              outcome <- 23
       }
       
       data[, 11] <- suppressWarnings(as.numeric(data[, 11])) # heart attack
       data[, 17] <- suppressWarnings(as.numeric(data[, 17])) # heart failure
       data[, 23] <- suppressWarnings(as.numeric(data[, 23])) # pneumonia
       
       stateframe <- data[data[, 7] == state, ]
       
       
       statesort <- stateframe[order(stateframe[outcome], stateframe[2]),]
       
       if (num == "best") {num <- 1}
       if (num == "worst") {
              num <- nrow(statesort)
              ret <- c(statesort[num, 2], state)
              return(ret)
              num = "worst"}
       else{
       ret <- c(statesort[num, 2], state)
       return(ret)
       }
       ## Return hospital name in that state with the given rank
       ## 30-day death rate
       
}


rankall <- function(outcome, num = "best") {
       
       data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
       
       ## Check that state and outcome are valid
       
       states <- sort(unique(data[, 7]))

       mat <- matrix(, nrow = 0, ncol = 2)

       hospital <- rep("", length(states))
       
       for (i in (1:length(states))) {
              out <- matrix(rankhospital(states[i], outcome, num), ncol = 2)
              mat <- rbind(mat, out)
       }
       
       mat

       }
