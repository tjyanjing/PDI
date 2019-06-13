# PDI model
pdi <- function(theta.s, theta.r, h, ha, h0, alpha, n ){
  #capillary realtive saturation 
  gamma <- function(alpha, h, n){
    m = 1-1/n
    gamma = (1/(1+(alpha*h)^n))^m
    return(gamma)
  }
   
  gamma.h = gamma(alpha, h, n)
  gamma.h0 = gamma(alpha,h0,n)
  
  Scap = (gamma.h - gamma.h0)/(1-gamma.h0)
  
  # adsorption relative saturation
  x = log(h)
  xa = log(ha)
  x0 = log(h0)
  b = 0.1 + (0.2/n^2)*(1-exp(-(theta.r/(theta.s-theta.r))^2))
  
  Sad = 1+ (1/(xa-x0))*(x-xa+b*log(1+exp((xa-x)/b)))
  
  # key equation
  theta = (theta.s-theta.r)*Scap+theta.r*Sad
  return(theta)

}

# test
pdi(0.5,0.1,seq(1,1000,10),1000,100000,0.3,2)

plot(seq(1,1000,10),pdi(0.5,0.1,seq(1,1000,10),1000,100000,0.3,2))
