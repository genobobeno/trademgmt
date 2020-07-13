fct_expGrowth<-function(size,rate,periods,dayWeek='d',friday=TRUE) {
  if (dayWeek=="d") {
    size*(1+rate/(4+friday))^(periods)
  } else {
    size*(1+rate)^(periods)
  }
}