pollutantmean=function(directory,pollutant,id=1:2){


lc=setwd("C:/Users/daniel/Downloads/quiz1_data/")
all_files=list.files(lc,pattern="*.csv",full.names=F)

dat=data.frame()
for(i in seq_along(all_files)) {
  dat=rbind(dat,read.csv(all_files[i]))
}

sulfate=dat$sulfate
nitrate=dat$nitrate
mean1=mean(sulfate,na.rm=T)
mean2=mean(nitrate,na.rm=T)

}