setwd("//SP3AKER11-PC/Users/Public/Statistics public/Coursera/R Programming/Programming Assignments/1. Air Pollution/")


pollutantmean <- function(directory, pollutant, id = 1:332) {
  
  #the path in the working directory to the list of specdata files
  #filepath[3] corresponds to "./specdata/003.csv
  #The files are sorted in alphabetical order, on the full path if full.names = TRUE
  filepath <- dir(directory, full.names=TRUE)           
  
  dat <- data.frame()                                     
  for (i in id) {                                         
    dat <- rbind(dat, read.csv(filepath[i]))
  }
  mean(dat[, pollutant], na.rm=T)                         
}

## confirm:
pollutantmean("specdata", "sulfate", 1:10)
pollutantmean("specdata", "nitrate", 70:72)
pollutantmean("specdata", "nitrate", 23)


complete <- function(directory, id = 1:332) {
  filepath <- dir(directory, full.names=TRUE)
  complete.data <- data.frame()    
  
  for(i in id){
    data <- read.csv(filepath[i])
    complete.data <- rbind(complete.data, c(i, sum(complete.cases(data))))
  } 
  colnames(complete.data) <- c("id", "nobs")
  return(complete.data)
}

#confirm
complete("specdata", 1)
complete("specdata", c(2, 4, 8, 10, 12))
complete("specdata", 30:25)


corr <- function(directory, threshold = 0) {
  filepath <- dir(directory, full.names=TRUE)
  corrr <- numeric()
  
  for(i in 1:332) {
    data <- read.csv(filepath[i])
    completely.observed <- sum(complete.cases(data))
    data.clean <- na.omit(data)
    if (completely.observed >= threshold) {
      corrr[i] <- cor(data.clean$sulfate, data.clean$nitrate)
    }
    else {
      corrr[i] <- NA   # optional, R would have added NA in index [i] anyway
    }
  }
  clean.corrr <- na.omit(corrr)
  return(clean.corrr)
}

#confirm

cr <- corr("specdata", 150)
head(cr)
summary(cr)
cr <- corr("specdata", 400)
head(cr)
summary(cr)
cr <- corr("specdata", 400)
head(cr)
summary(cr)
cr <- corr("specdata", 5000)
summary(cr)