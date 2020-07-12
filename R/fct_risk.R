fct_risk<-function(size,percent,edgeWinRate,trades) {
  size*percent*trades*(1-edgeWinRate)
}