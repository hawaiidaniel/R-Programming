#Q1
library(datasets)
data(iris)
x=iris[,1]
s=split(x,iris$Species)$virginica
round(mean(s))

#Q2
library(datasets)
data(iris)
apply(iris[,1:4],2,mean)

#Q3
library(datasets)
data(mtcars)
tapply(mtcars$mpg,mtcars$cyl,mean)
sapply(split(mtcars$mpg, mtcars$cyl), mean)
with(mtcars, tapply(mpg, cyl, mean))

#Q4
library(datasets)
data(mtcars)
avg=tapply(mtcars$hp,mtcars$cyl,mean)
avg=avg[1]-avg[3]
round(abs(avg))
