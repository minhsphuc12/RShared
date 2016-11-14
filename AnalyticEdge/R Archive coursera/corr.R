corr <- function(directory, threshold = 0) {
    
    correlations = c()
    
    for (afile in list.files(directory, full.names=TRUE)) {
        dataset <- read.csv(afile,TRUE)
        if (nrow(dataset[complete.cases(dataset),]) > threshold) {
            obs <- dataset[complete.cases(dataset),]
            correlations <- append(correlations, cor(obs$sulfate, obs$nitrate))
        }
    }
    
    correlations
}