timeForGrowth<-function(size,rate,salary) {
  log(salary/(rate*size))/(log(1+rate))
}