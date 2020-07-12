fct_expGrowth<-function(size,rate,weeks,dayWeek='d',friday=TRUE) {
  if (dayWeek=="d") {
    size*(1+rate/(4+friday))^((4+friday)*weeks)
  } else {
    size*(1+rate)^(weeks)
  }
}