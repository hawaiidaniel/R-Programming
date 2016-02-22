pollutantmean <- function(directory, pollutant, id = 1:332) {
  
  setwd("C:/Users/wli/Documents/")
  all_files=list.files(directory,full.names = T)
  dat=data.frame()
  for (i in id) {
    dat=rbind(dat,read.csv(all_files[i]))
  }
  
  mean(dat[ ,pollutant],na.rm = T)
  
}

pollutantmean("specdata","sulfate",1:10)






pollutantmean <- function(directory, pollutant, id = 1:332) {
  directory=substitute(directory)
  directory=as.character(directory)
  a=sprintf("C:/Users/wli/Documents/%s",directory)
  all_files=list.files(a,full.names = T)
  dat=data.frame()
  for (i in id) {
    dat=rbind(dat,read.csv(all_files[i]))
  }
  
  mean(dat[ ,pollutant],na.rm = T)
  
  
  
}

pollutantmean(specdata,"sulfate",1:10)
