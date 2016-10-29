pollutantmean <- function(directory, pollutant, id=0) {

                files_full <- list.files(directory, full.names=TRUE)            #creates a list of files
                tmp <- vector(mode = "list", length = length(files_full))       #create an empty list that's the length of our expected output

                for (i in seq_along(files_full)) {                              #loops through the files  
                        tmp[[i]] <- read.csv(files_full[[i]])                   #read in each of the csv files and place them inside of our list.
                }
                output <- do.call(rbind, tmp)                                   #combine tmp into a single data frame
                output_subset <- output[output$ID %in% id, pollutant]  #subsets the rows that match the 'pollutant' argument

                     mean(output_subset, na.rm=TRUE)      #identifies the means 
                                                               #while stripping out the NAs
                
}
