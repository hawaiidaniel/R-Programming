wd=setwd(gsub("\\\\","/",readClipboard()))
wd
list.files(wd)
outcome <- read.csv("outcome-of-care-measures.csv")
outcome[,11]=as.numeric(outcome[,11])
hist(outcome[,11])

best=function(state,outcome){
  dat=read.csv("outcome-of-care-measures.csv",colClasses = "character")
  dat=dat[,c(2,7,11,17,23)]
  for (j in 3:5) {
    for (i in 1:nrow(dat)) {
      if(dat[i,j]=="Not Available"){
        dat[i,j]=NA
      }
    }
  }
  dat=dat[complete.cases(dat),]
  colnames(dat)=c("name","states","heart attack","heart failure","pneumonia")
  if(!state %in% unique(dat[,2])){stop("invalid state")}
  if(!outcome %in% names(dat[3:5,])){stop("invalid outcome")}
  #dat[,1:2]=sapply(dat[,1:2],as.character)  
  ##The following step is very important when converting factor to numeric
  ##If you do as.numeric to a factor it will convert the levels to numeric not the actual values
  #dat[,3:5]=sapply(sapply(dat[,3:5],as.character),as.numeric)
  dat[,3:5]=sapply(dat[,3:5],as.numeric)
  dat=subset(dat,states==state,select = c("name", outcome))
  dat=dat[order(dat["name"]),]
  dat[which.min(dat[,outcome]),1]
}

best("TX","heart attack")
best("TX", "heart failure")
best("MD", "heart attack")
best("MD", "pneumonia")
best("BB", "heart attack")
best("NY", "hert attack")



rankhospital=function(state,outcome,num="best"){
  dat=read.csv("outcome-of-care-measures.csv",colClasses = "character")
  dat=dat[,c(2,7,11,17,23)]
  for (j in 3:5) {
    for (i in 1:nrow(dat)) {
      if(dat[i,j]=="Not Available"){
        dat[i,j]=NA
      }
    }
  }
  dat=dat[complete.cases(dat),]
  colnames(dat)=c("name","states","heart attack","heart failure","pneumonia")
  if(!state %in% unique(dat[,2])){stop("invalid state")}
  if(!outcome %in% names(dat[3:5,])){stop("invalid outcome")}
  if((is.numeric(num)) & num>nrow(dat)){return(NA)}
  dat[,3:5]=sapply(dat[,3:5],as.numeric)
  dat=subset(dat,states==state,select = c("name", outcome))
  dat=dat[order(dat[outcome],dat["name"]),]
  switch (num,
          "best" = {num=1},
          "worst" = {num=nrow(dat)})
  dat[num,1]
}


rankhospital("TX", "heart failure", 4)
rankhospital("MD", "heart attack", "worst")
rankhospital("MN", "heart attack", 5000)




rankall=function(outcome,num="best"){
  dat=read.csv("outcome-of-care-measures.csv",colClasses = "character")
  dat=dat[,c(2,7,11,17,23)]
  for (j in 3:5) {
    for (i in 1:nrow(dat)) {
      if(dat[i,j]=="Not Available"){
        dat[i,j]=NA
      }
    }
  }
  dat=dat[complete.cases(dat),]
  colnames(dat)=c("name","states","heart attack","heart failure","pneumonia")
  # if(!state %in% unique(dat[,2])){stop("invalid state")}
  if(!outcome %in% names(dat[3:5,])){stop("invalid outcome")}
  if((is.numeric(num)) & num>nrow(dat)){return(NA)}
  dat[,3:5]=sapply(dat[,3:5],as.numeric)
  dat=subset(dat,select = c("name", "states","heart attack"))
  dat=dat[order(dat["states"],dat[outcome],dat["name"]),]
  result=data.frame()
  
  for (i in 1:length(unique(dat$states))) {
    
    newdat=subset(dat,states==unique(dat$states)[i],select = c("name","states"))
    switch (num,
          "best" = {num=1},
          "worst" = {num=nrow(newdat)})
    if(num>nrow(newdat)){
      temp=data.frame(NA,newdat[,i])
      result=rbind(result,temp)}
    else
    {result=rbind(result,newdat[num,])}
    return(result)
  }
  
}
rankall("heart attack", 10)











dat=read.csv("outcome-of-care-measures.csv",colClasses = "character")
dat=dat[,c(2,7,11,17,23)]
for (j in 3:5) {
  for (i in 1:nrow(dat)) {
    if(dat[i,j]=="Not Available"){
      dat[i,j]=NA
    }
  }
}
dat=dat[complete.cases(dat),]
colnames(dat)=c("name","states","heart attack","heart failure","pneumonia")
# if(!state %in% unique(dat[,2])){stop("invalid state")}
# if(!outcome %in% names(dat[3:5,])){stop("invalid outcome")}
#if(is.numeric(num) & num>nrow(dat)){return(NA)}


dat[,3:5]=sapply(dat[,3:5],as.numeric)
dat=subset(dat,select = c("name", "states","heart attack"))
dat=dat[order(dat["states"],dat["heart attack"],dat["name"]),]
result=data.frame()
# newdat=subset(dat,states==unique(dat$states)[1],select = c("name","states",
#                                                            "heart attack"))
num=10



for (i in 1:length(unique(dat$states))) {
  newdat=subset(dat,states==unique(dat$states)[i],select = c("name","states"))
  # switch (num,
  #       "best" = {num=1},
  #       "worst" = {num=nrow(dat)})
  if(num>nrow(newdat)){
    temp=data.frame(NA,newdat[,i])
    result=rbind(result,temp)}
  else
  {result=rbind(result,newdat[num,])}
}











