outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
head(outcome)
outcome[, 11] <- as.numeric(outcome[, 11])
outcome[, 17] <- as.numeric(outcome[, 17])
outcome[, 23] <- as.numeric(outcome[, 23])
hist(outcome[, 11])
#testfunction best()
#best("TX","heart attack")

best <- function(state, least) {
    ## Read outcome data #att #fai #pne
    outcome2=outcome[outcome$State==state,]
    k=c("heart attack","heart failure","pneumonia")
    ## Check that state and outcome are valid
    if (sum(state %in% outcome$State)==0) {
        stop("invalid state", call.=FALSE)
    }
    
    
    #if (sum(least %in% k)==0) {
    #    stop('invalid outcome',call.=FALSE)
    #}
    
    #sort3=with(s,s[order(Hospital.Name,Rate),2])[1]
    
    ## Return hospital name in that state with lowest 30-day death rate
    if (least==k[1]) {
        #n1=order(outcome2[,11],decreasing=FALSE)[1]
        #att=outcome2[n1,2]
        #att
        att=with(outcome2,outcome2[order(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,Hospital.Name,na.last = TRUE),2])[1]
        att
    }
    else {
        if (least ==k[2]) {
            fai=with(outcome2,outcome2[order(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,Hospital.Name,na.last = TRUE),2])[1]
            fai
        }
        else {
            if (least==k[3]) {
                pne=with(outcome2,outcome2[order(Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia,Hospital.Name,na.last = TRUE),2])[1]
                pne
            }
            else {stop('invalid outcome',call.=FALSE)}
        }
    }
}

#use with()
#s=read.csv('sample.csv',header=TRUE)
#with(s,s[order(Rate,Hospital.Name)[2],order(Rate,Hospital.Name)])