# 3.2. Regularization effects----
# Figure 3
library(reshape2); library(tseries)
library(ggplot2); library(gridExtra); library(grid); library(GGally)
### Prepare functions of BListener package an other
library(BListener) 
source("./BLplot.R")
lenn = 100
r1 <- seq(from=400,to=500,length.out=lenn)
r2 <- seq(from=300,to=200,length.out=lenn)
r3 <- seq(from=100,to=100,length.out=lenn)
r4 <- seq(from=100,to=100,length.out=lenn)
lr1 <- log2(r1)
lr2 <- log2(r2)
lr3 <- log2(r3)
lr4 <- log2(r4)
set.seed(234)
rr1 = r1*0; count=1; for(v in r1){ print(v);rr1[count] <- rnorm(1,0, .1*v); count <- count+1}
rr2 = r2*0; count=1; for(v in r2){ rr2[count] <- rnorm(1,0, .1*v); count <- count+1}
rr3 = r3*0; count=1; for(v in r3){ rr3[count] <- rnorm(1,0, .1*v); count <- count+1}
rr4 = r4*0; count=1; for(v in r4){ rr4[count] <- rnorm(1,0, .1*v); count <- count+1}
lrr1 = lr1*0; count=1; for(v in lr1){ lrr1[count] <- rnorm(1,0, .15); count <- count+1}
lrr2 = lr2*0; count=1; for(v in lr2){ lrr2[count] <- rnorm(1,0, .15); count <- count+1}
lrr3 = lr3*0; count=1; for(v in lr3){ lrr3[count] <- rnorm(1,0, .15); count <- count+1}
lrr4 = lr4*0; count=1; for(v in lr4){ lrr4[count] <- rnorm(1,0, .15); count <- count+1}
par(mfrow=c(1,1))
ts.plot(cbind(r1+rr1,r2+rr2,r3+rr3,r4+rr4), type="p", ylab="duration (milliseconds)",xlab="number of objects")
lines(r1)
lines(r2)
lines(r3)
ts.plot(cbind(lr1+lrr1,lr2+lrr2,lr3+lrr3),
        type="p", xlab="object",
        ylab="IOI (log2dur)", main="dummy"
)
lines(lr1)
lines(lr2)
lines(lr3)
#r <- cbind(r1+rr1,r2+rr2,r3+rr3,r4+rr4)
r <- cbind(lr1+lrr1,lr2+lrr2,lr3+lrr3)
r <- as.numeric(t(2^r))
u <- cumsum(r)
u <- diff(c(0,u))
mtr = c(4,3,1)
R <- BLmain(u,mtr,tg=100,V=.000001,W=.001)
Post1 <- BLpost(R)
pl1 <- BLplot(R,1)
R <- BLmain(u,mtr,tg=100,g=.1,V=.000001,W=.001)
Post2 <- BLpost(R)
pl2 <- BLplot(R,1)
#grid.arrange( pl1, pl2,ncol=2)
# Preparing and plotting the Table
F1 <- cbind(
  Post1$Fluctuation1,
  Post2$Fluctuation1)
rownames(F1) <- c("IOI1","IOI2","IOI3")
S <- cbind(
  Post1$Stability,
  Post2$Stability)
F2 <- cbind(
  Post1$Fluctuation2,
  Post2$Fluctuation2)
Mod1 <- cbind(F1[,1],F2[,1],S[,1])
colnames(Mod1) <- c("F","F2","S")
Mod2 <- cbind(F1[,2],F2[,2],S[,2])
colnames(Mod2) <- c("F","F2","S")
g1  <- tableGrob(round(Mod1,digits=2))#, cols=c("IOI1","IOI2","IOI3"))
g2  <- tableGrob(round(Mod2,digits=2))#, cols=c("IOI1","IOI2","IOI3"))
grid.newpage()
h <- grobHeight(g1)
w <- grobWidth(g1)
title1 <- textGrob("g=0", y=unit(0.5,"npc") + h, vjust=0)#, gp=gpar(fontsize=15))
title2 <- textGrob("g=0.1", y=unit(0.5,"npc") + h, vjust=0)#, gp=gpar(fontsize=15))
gt1 <- gTree(children=gList(g1, title1))
gt2 <- gTree(children=gList(g2, title2))
# FIGURE
grid.arrange(arrangeGrob(
  pl1,
  pl2,
  gt1,gt2, ncol=2) )


