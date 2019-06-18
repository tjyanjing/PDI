# plot the water rention curves with data and fitted PDI curves
# water content [v/v]
# water potential [m]

plot_wrc <- function(my.data){
  library(ggplot2)
  p<-ggplot(my.data)+
geom_point(alpha = 0.5, aes(x = wc, y = wp, colour = factor(conc)))+
geom_line(aes(x = pred.wc, y= wp, colour = factor(conc)))+
  scale_y_log10()+
  # scale_x_log10()+
  xlab("Water content (v/v)")+
  ylab("Water potential (m)")+
  scale_colour_discrete(name="Nutrient conc.\n    (mgN/L)")+
  # theme(legend.title="Nutrient conc. (mgN/l)")+
  theme_bw()
  return(p)
}

