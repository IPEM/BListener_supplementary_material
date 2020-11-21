# MIT2019Choir
# Figure 4

#rm(list = ls())
setwd('/Volumes/ASIL_Data/Marc/BListener_package')
library(crqa); library(reshape2);
library(ggplot2); library(gridExtra); library(grid); library(GGally)

#### Prepare the data
load("/Volumes/ASIL_Data/Marc/BListener_supplementary_material/BLdata/MIT2019Choir4MicOnsets.RData")
load("/Volumes/ASIL_Data/Marc/BListener_supplementary_material/BLdata/MIT2019ChoirOmniMicOnsets.RData")

### Prepare functions of BListener package an other
source("/Volumes/ASIL_Data/Marc/BListener_package/Blistener/R/BLpost.R")
source("/Volumes/ASIL_Data/Marc/BListener_package/Blistener/R/BLmain.R")
source("/Volumes/ASIL_Data/Marc/BListener_supplementary_material/BLplot.R")

#### Run the analysis
u <- MIT2019Choir4MicOnsets
u=u[u>10]
R <- BLmain(u,outt=1.5,tg=360,meter = c(8,4,2,1), V= .00001, W = .001)
Post1 <- BLpost(R)
pl1 <- BLplot(R,0)
R <- BLmain(u,g=.1,outt=1.5,tg=360,meter = c(8,4,2,1), V= .00001, W = .001)
Post2 <- BLpost(R)
pl2 <- BLplot(R,0)

uu <- MIT2019ChoirOmniMicOnsets
R <- BLmain(uu,g=0,outt=1.5,tg=360,meter = c(8,4,2,1), V= .00001, W = .001)
Post3 <- BLpost(R)
pl3 <- BLplot(R,0)
R <- BLmain(uu,g=0.1, outt=1.5,tg=360,meter = c(8,4,2,1), V= .00001, W = .001)
Post4 <- BLpost(R)
pl4 <- BLplot(R,0)

#### Create Figure 4

  T1 <- cbind(
    F1 = Post1$Fluctuation1,
    F2 = Post1$Fluctuation2,
    S = Post1$Stability
    )
  rownames(T1) <- c("IOI1","IOI2","IOI3","IOI4")
  T2 <- cbind(
    F1 = Post2$Fluctuation1,
    F2 = Post2$Fluctuation2,
    S = Post2$Stability
  )
  rownames(T2) <- c("IOI1","IOI2","IOI3","IOI4")
  T3 <- cbind(
    F1 = Post3$Fluctuation1,
    F2 = Post3$Fluctuation2,
    S = Post3$Stability
  )
  rownames(T3) <- c("IOI1","IOI2","IOI3","IOI4")
  T4 <- cbind(
    F1 = Post4$Fluctuation1,
    F2 = Post4$Fluctuation2,
    S = Post4$Stability
  )
  rownames(T4) <- c("IOI1","IOI2","IOI3","IOI4")


  g1  <- tableGrob(round(T1,digits=2))#
  g2  <- tableGrob(round(T2,digits=2))#
  g3  <- tableGrob(round(T3,digits=2))#
  g4  <- tableGrob(round(T4,digits=2))#,


  grid.newpage()
  h <- grobHeight(g1)
  w <- grobWidth(g1)
  title1 <- textGrob("1", y=unit(0.5,"npc") + h, vjust=0)#, gp=gpar(fontsize=15))
  title2 <- textGrob("2", y=unit(0.5,"npc") + h, vjust=0)#, gp=gpar(fontsize=15))
  title3 <- textGrob("3", y=unit(0.5,"npc") + h, vjust=0)#, gp=gpar(fontsize=15))
  title4 <- textGrob("4", y=unit(0.5,"npc") + h, vjust=0)#, gp=gpar(fontsize=15))

  gt1 <- gTree(children=gList(g1, title1))
  gt2 <- gTree(children=gList(g2, title2))
  gt3 <- gTree(children=gList(g3, title3))
  gt4 <- gTree(children=gList(g4, title4))

  #tiff("test.tiff", units="in", width=5, height=5, res=300)

  # FIGURE

  grid.arrange(arrangeGrob(
    pl1, pl2,
    pl3, pl4, ncol=2))

  # TABLE
  grid.arrange(arrangeGrob(
    gt1, gt2, gt3,gt4, ncol=2))


 gg1 <- arrangeGrob(
   pl1, pl2,
   pl3, pl4, ncol=2)

 # TABLE
 gg2 <- arrangeGrob(
   gt1, gt2, gt3,gt4, ncol=2)
 grid.arrange(gg1,gg2, ncol=2)


  # insert ggplot code
 # dev.off()
