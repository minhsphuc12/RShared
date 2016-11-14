outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
head(outcome)
outcome[, 11] <- as.numeric(outcome[, 11])
outcome[, 17] <- as.numeric(outcome[, 17])
outcome[, 23] <- as.numeric(outcome[, 23])

#testfunction rankhostpital()
#rankhospital('TX','heart attack',3)

rankhospital <- function(state, least, num = "best") {
    ## Read outcome data
    outcome2=outcome[outcome$State==state,]
    k=c("heart attack","heart failure","pneumonia")
    ## Check that state and outcome are valid
    if (sum(state %in% outcome$State)==0) {
        stop("invalid state", call.=FALSE)
    }
    if (num =='best') {
        num=1
    }
    #worst part
    if (num =='worst') {
        if (least==k[1]) {
            #att=with(outcome2,outcome2[order(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,Hospital.Name,na.last = TRUE,decreasing=TRUE),2])[num]
            tu=with(outcome2,outcome2[order(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,Hospital.Name,na.last = TRUE,decreasing=TRUE),])
            #att=tu[na.omit(tu$Hospital.Name),]
            att=tu[1,2]
            att
        }
        else {
            if (least ==k[2]) {
                tu=with(outcome2,outcome2[order(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,Hospital.Name,na.last = TRUE,decreasing=TRUE),])
                fai=tu[1,2]
                fai
            }
            else {
                if (least==k[3]) {
                    tu=with(outcome2,outcome2[order(Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia,Hospital.Name,decreasing=TRUE),])
                    pne=tu[1,2]
                    pne
                }
                else {stop('invalid outcome',call.=FALSE)}
            }
        }  
    }    
    else {
        ## Return hospital name in that state with the given rank 
        ##30-day death rate
        if (least==k[1]) {
            att=with(outcome2,outcome2[order(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,Hospital.Name,na.last = TRUE),2])[num]
            att
            
        }
        else {
            if (least ==k[2]) {
                fai=with(outcome2,outcome2[order(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,Hospital.Name,na.last = TRUE),2])[num]
                fai
            }
            else {
                if (least==k[3]) {
                    pne=with(outcome2,outcome2[order(Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia,Hospital.Name,na.last = TRUE),2])[num]
                    pne
                }
                else {stop('invalid outcome',call.=FALSE)}
            }
        }
    } 
}
