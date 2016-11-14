pollutantmean <- function(directory, pollutant, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    if (directory !="specdata") {
        stop ("invalid directory", call.=FALSE)
    }
    
    ## 'pollutant' is a character vector of length 1 indicating
    ## the name of the pollutant for which we will calculate the
    ## mean; either "sulfate" or "nitrate".
    if (is.element(pollutant,c('sulfate','nitrate'))==FALSE) {
        stop ("invalid pollutant", call.=FALSE)
    }
    #same with '''pollutant %in% c('sulfate','nitrate') = TRUE'''
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    if (is.numeric(id)==FALSE) {
        stop ("invalid id", call.=FALSE)
    }
    
    ## Return the mean of the pollutant across all monitors list
    ## in the 'id' vector (ignoring NA values)
    df = data.frame(matrix(vector(), 0, 4))
    
    if (pollutant=='sulfate') 
        for (i in id) {
            if (i <10 ) {
                filepath=paste("C:/Users/Nas/Documents/R/specdata/00",i,'.csv',sep="")
                a=read.csv(filepath,header=TRUE)
                b=subset(a,!is.na(a$sulfate))
                df=rbind(df,b)
            }
            if (i>=10 & i<100 ){
                filepath=paste("C:/Users/Nas/Documents/R/specdata/0",i,'.csv',sep="")
                a=read.csv(filepath,header=TRUE)
                b=subset(a,!is.na(a$sulfate))
                df=rbind(df,b)
            }
            if (i>100){
                filepath=paste("C:/Users/Nas/Documents/R/specdata/",i,'.csv',sep="")
                a=read.csv(filepath,header=TRUE)
                b=subset(a,!is.na(a$sulfate))
                df=rbind(df,b)
            }
        }
        mean(df[,2])
    if (pollutant=='nitrate') 
        for (i in id) {
            if (i <10 ) {
                filepath=paste("C:/Users/Nas/Documents/R/specdata/00",i,'.csv',sep="")
                a=read.csv(filepath,header=TRUE)
                b=subset(a,!is.na(a$nitrate))
                df=rbind(df,b)
            }
            if (i>=10 & i<100 ){
                filepath=paste("C:/Users/Nas/Documents/R/specdata/0",i,'.csv',sep="")
                a=read.csv(filepath,header=TRUE)
                b=subset(a,!is.na(a$nitrate))
                df=rbind(df,b)
            }
            if (i>100){
                filepath=paste("C:/Users/Nas/Documents/R/specdata/",i,'.csv',sep="")
                a=read.csv(filepath,header=TRUE)
                b=subset(a,!is.na(a$nitrate))
                df=rbind(df,b)
            }
        }
        mean(df[,3])
}