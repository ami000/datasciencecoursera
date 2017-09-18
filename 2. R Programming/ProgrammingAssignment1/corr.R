corr <- function(directory, threshold = 0) {
   
    
    files <- list.files(directory, full.names = TRUE)
    data <- lapply(files, read.csv, header = TRUE)
    
    valid_data <- lapply(data, function(x) x[complete.cases(x),])
#     Store all complete cases in valid_data
    if (is.null(valid_data) == TRUE){
        numeric(0)
    }
    else {
        correlations <- numeric(0)
        # Initialize the data set that pass the threshold requirement
        for (i in seq_along(valid_data)){
            if (nrow(valid_data[[i]]) >= threshold){
                correlations <- c(correlations, cor(valid_data[[i]]$sulfate, valid_data[[i]]$nitrate))
            }
            
        }
       correlations
    }

}
 