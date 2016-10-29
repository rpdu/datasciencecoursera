complete <- function(directory, no) {
        
        files_full <- list.files(directory, full.names=TRUE)            #creates a list of files
        tmp <- vector(mode = "list", length = length(files_full))       #create an empty list that's the length of our expected output
                mno<-length(no)
                num <- numeric(mno)
                ID <- numeric(mno)
                nobs <- numeric(mno)

        for (i in no) { 
                #loops through the files related to id value
                  if (i %in% no) {  
   
                     tmp[[i]] <- read.csv(files_full[[i]])   #read in each csv files 2 place them inside list
                     good <- complete.cases(tmp[[i]])
                     num[i] <- nrow(tmp[[i]][good,])
                     ID[i]<-i
                     nobs[i]<-num[i]
                                } 
                }   

                df<-na.omit(data.frame(ID, nobs))
                df_output <<- subset(df, df$ID>0 & df$nobs>0)
                rownames(df_output) <- seq(length=nrow(df_output))
                print(df_output)
                

}     
