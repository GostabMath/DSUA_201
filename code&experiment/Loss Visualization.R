

library(ggplot2)
library(reshape2)
library(data.table)

jobFolder = getwd()

xmin=-4
xmax=4
d = data.table(margin = seq(xmin,xmax,length.out=3000))
d[, Zero_One := as.numeric(margin<0)]
d[, Hinge := pmax(1-margin, 0)]
d[, Perceptron := pmax(-margin, 0)]
d[, Logistic := log(1+ exp(-margin))]
d[, Exponential := exp(-margin)]
d[, Square := (1-margin)^2]

dd = as.data.table(melt(d, id.vars="margin"))#, variable.name="Loss")
setnames(dd, "variable", "Loss")


plotOrder = c("Zero_One", "Perceptron", "Hinge", "Logistic")
plotOrder = c("Zero_One", "Hinge", "Logistic", "Square")


p = ggplot(dd[Loss %in% plotOrder] , aes(x=margin, y=value, color=Loss))
p = p + geom_line(size=1.6)+ xlab("Margin m=yf(x)") + ylab("Loss(m)")+xlim(-3,3)+ylim(0,4)
p = p + theme(legend.justification=c(0,0), legend.position=c(0.6,.6))
#fname = paste0(jobFolder,"/loss.",paste(plotOrder[1:i],collapse="."),".png")
#print(fname)
#ggsave(filename=fname,plot=p, units="in", width=6, height = 4)
p