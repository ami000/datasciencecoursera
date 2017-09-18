complete <- function(directory, id = 1:332) {
    
    
    files <- list.files(directory, full.names = TRUE)
    ## Store full path names to a  files (character vector) to read the files directly
    monitor_data <- lapply(files[id], function(x) read.csv(x, header = TRUE))
       nobs <- sapply(monitor_data, function(x) sum(complete.cases(x)))
    ## Count the number of rows which contain all 'TRUE' values.
    data.frame('id' = id, 'nobs' = nobs)
    ## Combine the two vectors into a data frame
    
    
}