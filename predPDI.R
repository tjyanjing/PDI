# calcualte theta [v/v] from the water potneial in [bar]
# need to adjust the input accordingly to [cm]

pred.wc <- function(my.data){
  param <- fit.pdi(my.data)  
  # calcualte and replot the modeled results
  my.data$pred.wc <- pdi(theta.s = param$theta.s,   theta.r = param$theta.r, h=1e3*my.data$wp, ha =   param$ha, h0 = param$h0, alpha = param$alpha, n   = param$n )
  my.data <- my.data[order(my.data$pred.wc),]
  return(my.data)
}

