# next step is to separate them into three componensts
# fitting; re-caluclation; visulaization
# need to double check the initial setting and saturation settings

# water retention curve fittings for PDI model
# write the PDI function
rm(list = ls(all=TRUE))
source("C:/Users/yj200/Dropbox/Research - Rhizosphere hydrology/Documents/Manuscript/PDI model/PDI/PDI_wrc.R")
my.data<- read.csv("C:/Users/yj200/Dropbox/Research - Rhizosphere hydrology/WP4C/100218/all/data.trans_all_02.csv")
#at saturation the water potential is set at 0.2 m or 0.02 bar
my.data <- rbind(my.data, c(0.48,0.02,0))
# wp(MPa)= -0.0005*conc.(mgN/L) - 0.0069
# wp(90 mgN/L) = -0.0519 MPa; WP4C: -2/-1.6 bar
# wp(360 mgN/L) = -0.1869 MPa; WP4c:-0.4/-0.9 bar
my.data <- rbind(my.data, c(0.48,0.2,90))
my.data <- rbind(my.data, c(0.48,0.7,360))
names(my.data) <- c("wc","wp","conc")

# guesses theta in v/v, h in cm
# theta.s, theta.r, h, ha, h0, alpha, n
fit.pdi <- function(my.data = my.data){
  s.par = list(theta.s=0.5,theta.r=0.01,alpha=0.01, ha = 1e3, h0 = 1e7, n=5)
l.par = c(theta.s=0.1,theta.r=0.001,alpha=0.000001,ha = 1, h0 = 1e4, n=1.05)
u.par = c(theta.s=0.9,theta.r=0.3,alpha=100, ha = 1e6, h0 = 1e10, n=10)

# fit the PDI model
library(minpack.lm)

fit.wc <- nlsLM(wc~pdi(theta.s, theta.r, h =1000*wp, ha, h0, alpha, n), data = my.data, start = s.par, lower = l.par, upper = u.par)
param <- coef(fit.wc)
param <- data.frame(t(param))

return(param)
}
fit.pdi(my.data[my.data$conc == 0,])



fit.wc <- function(my.data){
  param <- fit.pdi(my.data)  
  # calcualte and replot the modeled results
  my.data$fit.wc <- pdi(theta.s = param$theta.s,   theta.r = param$theta.r, h=1e3*my.data$wp, ha =   param$ha, h0 = param$h0, alpha = param$alpha, n   = param$n )
  my.data <- my.data[order(my.data$fit.wc),]
  return(my.data)
}

# split my.data
my.data.0 <- fit.wc(my.data[my.data$conc == 0,])
my.data.90 <- fit.wc(my.data[my.data$conc == 90,])
my.data.360 <- fit.wc(my.data[my.data$conc == 360,])
my.data <- rbind(my.data.0, my.data.90, my.data.360)


# plot(log10(1e3*my.data$bar), my.data$vwc, xlim = range(log10(1e3*my.data$bar)), ylim = c(0,1), ylab = "vwc (v/v)", xlab = "wp (cm)")
# par(new=T)
# plot(log10(1e3*my.data$bar), my.data$fit, type = "l", col = "red", xlim = range(log10(1e3*my.data$bar)), ylim = c(0,1), ylab = "vwc (v/v)", xlab = "wp (cm)")



# write this plot into fucntion as virsualizatio n and then loop the data for more plots

plot(my.data$fit.wc, 101.97*my.data$wp, log = "y", type ="n", xlab ="Water content (w/w)", ylab = "Water potneial (m)", xlim = range(my.data$wc), ylim = range(101.97*my.data$wp), col = "blue")
lines(my.data[order(my.data$wp),]$fit.wc, 101.97*my.data[order(my.data$wp),]$wp, log = "y")
par(new = TRUE)
plot(my.data$wc, 101.97*my.data$wp, log = "y", xlab ="", ylab = "", xlim = range(my.data$wc), ylim = range(101.97*my.data$wp), col = "red")
legend("topright",c("Measurement","PDI Model"),col=c("red","black"),pch=c(1, NA), lty=c(NA,1), bty="n")


p<-
  ggplot(my.data)+
geom_point(alpha = 0.5, aes(x = wc, y = wp, colour = factor(conc)))+
geom_line(aes(x = fit.wc, y= wp, colour = factor(conc)))+
  scale_y_log10()+
  scale_x_log10()+
  xlab("Water content (v/v)")+
  ylab("Water potential (m)")+
  scale_colour_discrete(name="Nutrient conc.\n    (mgN/L)")+
  # theme(legend.title="Nutrient conc. (mgN/l)")+
  theme_bw()

ggsave("C:/Users/yj200/Dropbox/Research - Rhizosphere hydrology/Documents/Manuscript/figs/pdi_fit_02.svg", p)
