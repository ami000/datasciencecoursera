pollutantmean <- function(directory, pollutant, id = 1:332) {
	    
    files <- list.files(directory, full.names = TRUE)
    ## Store the full paths of all the files containing pollutant data
    target_monitor <- lapply(files[id], function(x) read.csv(x, header = TRUE))
    ## Store data of selected monitors into target_monitor
    result_data <- lapply(target_monitor, function(x) x[, pollutant])
    ## Store the pollutant values as a list in result_data
    pollutant_all <- unlist(result_data)

    mean(pollutant_all, na.rm = TRUE)
}
    
    