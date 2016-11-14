complete <- function(directory, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    if (directory !="specdata") {
        stop ("invalid directory", call.=FALSE)
    }
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    if (is.numeric(id)==FALSE) {
        stop ("invalid id", call.=FALSE)
    }
    ## Return a data frame of the form:
    ## id nobs
    ## 1  117
    ## 2  1041
    ## ...
    ## where 'id' is the monitor ID number and 'nobs' is the
    ## number of complete cases
    df = data.frame(matrix(vector(), 0, 2))
    
    for (i in id) {
        if (i <10 ) {
            filepath=paste("C:/Users/Nas/Documents/R/specdata/00",i,'.csv',sep="")
            a=read.csv(filepath,header=TRUE)
            b=sum(complete.cases(a)==TRUE)
            s=c(i,b)
            df=rbind(df,s)
        }
        if (i>=10 & i<100 ){
            filepath=paste("C:/Users/Nas/Documents/R/specdata/0",i,'.csv',sep="")
            a=read.csv(filepath,header=TRUE)
            b=sum(complete.cases(a)==TRUE)
            s=c(i,b)
            df=rbind(df,s)
        }
        if (i>100){
            filepath=paste("C:/Users/Nas/Documents/R/specdata/",i,'.csv',sep="")
            a=read.csv(filepath,header=TRUE)
            b=sum(complete.cases(a)==TRUE)
            s=c(i,b)
            df=rbind(df,s)
        }
    }
    colnames(df)=c('id','nobs')
    df
}
