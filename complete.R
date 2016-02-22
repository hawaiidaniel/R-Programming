complete=function(directory,id=1:332) {
  
  setwd("C:/Users/wli/Documents/")
  all_files=list.files(directory,full.names = T)
  dat=data.frame()
  for (i in id) {
    
    num=sum(complete.cases(read.csv(all_files[i])))
    dat=rbind(dat,data.frame(i,num))
    
  }
  colnames(dat)=c("id","nobs")
  return(dat)
  
}


complete("specdata", c(2, 4, 8, 10, 12))
complete("specdata", 30:25)
complete("specdata", 3)
