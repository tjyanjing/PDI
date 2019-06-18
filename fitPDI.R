# fit the PID model to water retention charateristics, return the parameters

# list of parameters:

# theta.s:  satuarated water content [v/v]
# theta.r:  residual water content [v/v]; the                maximum water content of film flow
# h0:       water potential at truly zero water              content [cm]
# ha:       water potential at residual water                content [cm]
# alpha:    fitting parameter [-]
# n:        fitting parameter [-]

# input and output:

# h:        water potenital [cm]
# theta:    water content [v/v]


fit.pdi <- function(my.data = my.data){
  # inital gueses, lower and upper limit of         parameters
  s.par = list(theta.s=0.5,theta.r=0.01,alpha=0.01, ha = 1e3, h0 = 1e7, n=5)
  l.par = c(theta.s=0.1,theta.r=0.001,alpha=0.000001,ha = 1, h0 = 1e4, n=1.05)
  u.par = c(theta.s=0.9,theta.r=0.3,alpha=100, ha = 1e6, h0 = 1e10, n=10)

  # fit the PDI model
  # require the PDI model
  library(minpack.lm)
  fit.wc <- nlsLM(wc~pdi(theta.s, theta.r, h =1000*wp, ha, h0, alpha, n), data = my.data, start = s.par, lower = l.par, upper = u.par)
param <- coef(fit.wc)
param <- data.frame(t(param))

return(param)
}




