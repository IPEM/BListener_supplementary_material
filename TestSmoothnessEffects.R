# 3.1 Smoothness effects----
# Figure 2
library(reshape2); library(tseries)
library(ggplot2); library(gridExtra); library(grid); library(GGally)
### Prepare functions of BListener package an other
library(BListener) 
source("./BLplot.R")
# create a sinusoidal signal
times = seq(from=1,to=75)/75  # length.out=lenn) # we create a time sequence
cy <- 8 + .5*sin(2*pi*times)  # 2^8 milliseconds, half octave up and octave half down in log2dur
# add .15 sd noise
set.seed(134)
oy = rnorm(75,0,.15)  # sd of .15
oy[1] = 0  # to have a controlled beginning
r = oy + cy
cr = cumsum(r)
plot(r~cr,xlab="object",ylab="IOI (log2dur)")
lines(cy ~ cr)
# translate to milliseconds and generate u
rr <- as.numeric(t(2^r))
u <- 2^r
R1 <- BLmain(u,meter=1,V =.0000001,W=0.01)
Post1 <- BLpost(R1)
pl1 <- BLplot(R1,0)
R2 <- BLmain(u,meter=1,V=.00001,W=0.001)
Post2 <- BLpost(R2)
pl2 <- BLplot(R2,0)
R3 <- BLmain(u,meter=1,V=.001,W=0.00001)
Post3 <- BLpost(R3)
pl3 <- BLplot(R3,0)
#### Diagnostic for after having ran the algorithm ----
pll <- list()
for (ii in 1:3) {
  if(ii == 1) R <- R1
  if(ii == 2) R <- R2
  if(ii == 3) R <- R3
I <- as.data.frame(cbind(R1$Timeline,R$Innovation))#,Name))
colnames(I) <- c("Timeline","Innovation")
Imelted <- melt(I,id=c("Timeline"))#,"Name"))
diagplot1 <- ggplot() + #ggtitle("Couple 16 Trial 5") +
  #IOI-observed
  geom_point(data=Imelted, aes(x=Timeline,y = value, shape = factor(variable)), size = 1.5) +
  theme_bw() +
  theme(legend.position="none")+
  labs(y = "IOI (log2dur)" , x="time (sa)")+
  geom_line(data=Imelted, aes(x=Timeline,y=Timeline*0),size=.7)
#https://stackoverflow.com/questions/4357031/qqnorm-and-qqline-in-ggplot2
qqplot.data <- function (vec) # argument: vector of numbers
{
  # following four lines from base R's qqline()
  y <- quantile(vec[!is.na(vec)], c(0.25, 0.75))
  x <- qnorm(c(0.25, 0.75))
  slope <- diff(y)/diff(x)
  int <- y[1L] - slope * x[1L]
  d <- data.frame(resids = vec)
  ggplot(d, aes(sample = resids)) + stat_qq() + geom_abline(slope = slope, intercept = int)
}
diagplot2 <- qqplot.data(Imelted$value) + theme_bw()
pll[[ii]] = list(diagplot1,diagplot2)
}
# grid.arrange(
#   pl1,pll[[1]][[1]],pll[[1]][[2]],
#   pl2,pll[[2]][[1]],pll[[2]][[2]],
#   pl3,pll[[3]][[1]],pll[[3]][[2]],
#   ncol=3)
F1 <- rbind(
  Post1$Fluctuation1,
  Post2$Fluctuation1,
  Post3$Fluctuation1)
colnames(F1) <- "Fluctuation1"
row1 = print(paste('r =',Post1$parameters$V/Post1$parameters$W))
row2 = print(paste('r =',Post2$parameters$V/Post2$parameters$W))
row3 = print(paste('r =',Post3$parameters$V/Post3$parameters$W))
rownames(F1) <- c(row1,row2,row3)
S <- rbind(
  Post1$Stability,
  Post2$Stability,
  Post3$Stability)
colnames(S) <- "Stability"
F2 <- rbind(
  Post1$Fluctuation2,
  Post2$Fluctuation2,
  Post3$Fluctuation2)
colnames(F2) <- "Fluctuation2"
g1  <- tableGrob(round(F1,digits=2))#, cols=c(""))
g2  <- tableGrob(round(F2,digits=2))#, cols=c("IOI1"))
g3  <- tableGrob(round(S,digits=2))#, cols=c("IOI1"))
grid.newpage()
grid.arrange(arrangeGrob(
  pl1,pll[[1]][[1]],pll[[1]][[2]],
  pl2,pll[[2]][[1]],pll[[2]][[2]],
  pl3,pll[[3]][[1]],pll[[3]][[2]],
g1,g2,g3, ncol=3) )



