wd=setwd(gsub("\\\\","/",readClipboard()))
wd
list.files(wd)
outcome <- read.csv("outcome-of-care-measures.csv")
outcome[,11]=as.numeric(outcome[,11])
hist(outcome[,11])

best=function(state,outcome){
  dat=read.csv("outcome-of-care-measures.csv")
  dat=dat[,c(2,7,11,17,23)]
  colnames(dat)=c("name","states","heart attack","heart failure","pneumonia")
  dat=subset(dat,states==state,select = c("name", outcome))
  dat=dat[order(dat[,"name"]),]
  dat=dat[which.min(dat[,outcome]),]
  result=as.character(dat[1,1])
  result
  
}

best("TX","heart attack")
best("TX", "heart failure")
best("MD", "heart attack")
best("MD", "pneumonia")





outcome=read.csv("outcome-of-care-measures.csv")
outcome=outcome[,c(2,7,11,17,23)]
for(i in 3:5){
  outcome[,i]=as.numeric(outcome[,i])
}
colnames(outcome)=c("name","state","heart attack","heart failure","pneumonia")
b=subset(outcome,state=="MD",select = c("name","pneumonia"))
c=b[order(b[,"name"]),]
d=c[which.min(c[,"pneumonia"]),]
d[1,1]
a=c("x1","x2","x3")
b=c(7:9)
c=data.frame(a,b)
c[max(c$b),]




dat=read.csv("outcome-of-care-measures.csv")
dat=dat[,c(2,7,11,17,23)]
# for(i in 3:5){
#   dat[,i]=as.numeric(dat[,i])
# }
colnames(dat)=c("name","state","heart_attack","heart_failure","pneumonia")
dat=subset(dat,state=="MD",select = c("name", "pneumonia"))
dat=dat[order(dat[,"name"]),]
dat2=dat[,"pneumonia"]
which.min(unlist(dat2))

dat=dat[which.min(dat[,"pneumonia"]),]
result=dat[1,1]
result



x <- c(1:4, 0:5, 11)
which.min(x)
which.max(x)



