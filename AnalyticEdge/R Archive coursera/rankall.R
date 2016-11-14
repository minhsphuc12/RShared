outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
outcome[, 11] <- as.numeric(outcome[, 11])
outcome[, 17] <- as.numeric(outcome[, 17])
outcome[, 23] <- as.numeric(outcome[, 23])
outcome[, 7] = as.character(outcome[,7])
#testfunction rankall()
#rankall('heart attack',1)



rankall = function(least, num = "best") {
    ## Read outcome data
    u=unique(outcome$State)
    k=c("heart attack","heart failure","pneumonia")
    emptydf=outcome[0,]
    if (num =='best') {
        num = 1
    }
    
    if (num =='worst') {
        if (least==k[1]) {
            for ( state in u) {
                outcome2=outcome[outcome$State==state,]
                att=with(outcome2,outcome2[order(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,Hospital.Name,decreasing=TRUE)[1],])
                emptydf[nrow(emptydf)+1,]=att
                
            }
            finalresult=data.frame(hospital=emptydf$Hospital.Name,state=emptydf$State)
            
        }
        else {
            if (least ==k[2]) {
                for (state in u) {
                    outcome2=outcome[outcome$State==state,]
                    att=with(outcome2,outcome2[order(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,Hospital.Name,decreasing=TRUE)[1],])
                    emptydf[nrow(emptydf)+1,]=att
                }
                finalresult=data.frame(hospital=emptydf$Hospital.Name,state=emptydf$State)
            }
            else {
                if (least==k[3]) {
                    for (state in u) {
                        outcome2=outcome[outcome$State==state,]
                        att=with(outcome2,outcome2[order(Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia,Hospital.Name,decreasing=TRUE)[1],])
                        emptydf[nrow(emptydf)+1,]=att
                    }
                    finalresult=data.frame(hospital=emptydf$Hospital.Name,state=emptydf$State)
                }
                else {stop('invalid outcome',call.=FALSE)}
            }
        }   
    }
    #if (num =='worst') {
    #    num=length
    #
    
    ## Check that state and outcome are valid
    
    
    ## For each state, find the hospital of the given rank
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    if (least==k[1]) {
        #with(outcome,outcome[order(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,Hospital.Name,na.last=TRUE),])
        #l=c(1:length(outcome[,2]))
        #att
        for ( state in u) {
            outcome2=outcome[outcome$State==state,]
            att=with(outcome2,outcome2[order(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,Hospital.Name)[num],],na.rm=TRUE)
            emptydf[nrow(emptydf)+1,]=att
            
        }
        finalresult=data.frame(hospital=emptydf$Hospital.Name,state=emptydf$State)
        
    }
    else {
        if (least ==k[2]) {
            for (state in u) {
                outcome2=outcome[outcome$State==state,]
                att=with(outcome2,outcome2[order(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,Hospital.Name)[num],])
                emptydf[nrow(emptydf)+1,]=att
            }
            finalresult=data.frame(hospital=emptydf$Hospital.Name,state=emptydf$State)
        }
        else {
            if (least==k[3]) {
                for (state in u) {
                    outcome2=outcome[outcome$State==state,]
                    att=with(outcome2,outcome2[order(Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia,Hospital.Name)[num],])
                    emptydf[nrow(emptydf)+1,]=att
                }
                finalresult=data.frame(hospital=emptydf$Hospital.Name,state=emptydf$State)
            }
            else {stop('invalid outcome',call.=FALSE)}
        }
    }   
}    
    
    