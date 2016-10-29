corr <- function(directory, threashold = 0) {
        thr <- threashold
        files_full <- list.files(directory, full.names=TRUE)            #creates a list of files
        tmp <- vector(mode = "list", length = length(files_full))       #create an empty list that's the length of our expected output
        num  <- numeric()
        ID   <- numeric()
        nobs <- numeric()
        vcor <- numeric()
        
        for (i in seq_along(files_full)) #loops through the files  
                {
                tmp[[i]] <- read.csv(files_full[[i]])   #read in each csv files 2 place them inside list
                good <- complete.cases(tmp[[i]])
                num[i] <- nrow(tmp[[i]][good,])
                ID[i]<-i
                nobs[i]<-num[i]
                if (num[i]>threashold) {
                      
                        v3nit<-(tmp[[i]]$nitrate)
                        v3sulf<-(tmp[[i]]$sulfate)
                        great <- complete.cases(v3nit,v3sulf)
                        
                        df3<<-data.frame(nitrate=v3nit[great], sulfate=v3sulf[great])
                       # str(df3)
                        vcor[i]<-cor(df3$nitrate,df3$sulfate)                                }
                }
        #df3_output <<- subset(df3)
        best <- complete.cases(vcor)
        output <- vcor[best] 
}