fct_timeForGrowth<-function(size,rate,salary,dayWeek='d',friday=TRUE) {
  if (grepl("d",tolower(dayWeek))) {
    log((salary/rate)/size)/(log(1+rate/(4+friday)))
  } else {
    log((salary/rate)/size)/(log(1+rate))
  }
}



# 
# Wt = W0*(1+R)^t
# log(W1/W0)/log(1+R) = t
# 
# Dt = D0*(1+R/d)^t
# log(Dt/D0)/log(1+R/d) = t