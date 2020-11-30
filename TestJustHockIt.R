# Figure 5-8

library(crqa); library(reshape2);
library(ggplot2); library(gridExtra); library(grid); library(GGally)

### Prepare functions of BListener package an other
library(BListener) 
source("./BLplot.R")

#### Prepare the data
data("JustHockIt")
IOIdataset <- JustHockIt$IOIdataset
Condition <- JustHockIt$Condition
AllAnnotations <- JustHockIt$AllAnnotations
AllAgency <- JustHockIt$AllAgency

plo <- list()
Couple <- list(1, 4, 5, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21)
Trial <- as.list(seq(1,8,1))

for (ii in c(1:2)) {
  if (ii == 1) g <- 0 else g <- 0.1
  OutcomeTrial <- list()
  OutcomeTrialCouple <- list()
  for(couple in Couple){
    for(trial in Trial){
      R <- BLmain(IOIdataset[[couple]][[trial]],
                  meter = c(3,2,1),
                  outt = 1.5, g = g)
      pl <- BLplot(R, 1)
      Post <- BLpost(R)
      OutcomeTrial[[trial]] <- list(R = R, pl = pl, Post = Post)
    }
    OutcomeTrialCouple[[couple]] =  OutcomeTrial
  }

  #### Fluctuation analysis and plotting overall comparison
  c <- length(Couple)
  t <- length(Trial)
  FluctuationMatrix <- matrix(nrow = c*t, ncol = 6)
  i <- 0
  for (couple in Couple) {
    for (trial in Trial) {
      i <- i + 1
      fluctuation <- OutcomeTrialCouple[[couple]][[trial]]$Post$Fluctuation1[2]
      outlier <- OutcomeTrialCouple[[couple]][[trial]]$Post$Outlier
      stability <- OutcomeTrialCouple[[couple]][[trial]]$Post$Stability[2]
      lengte <- tail(OutcomeTrialCouple[[couple]][[trial]]$R$Timeline,n=1)
      condition <- Condition[couple,trial+1]
      FluctuationMatrix[i,] <- cbind(couple,trial,condition,
                                     fluctuation,stability,outlier/lengte)
    }
  }
  FluctuationDataFrame <- as.data.frame(FluctuationMatrix)
  colnames(FluctuationDataFrame) <- c("couple","trial","condition","Fluctuation1","Stability","Outlier")

  FluctuationDataFrame$couple <- with(FluctuationDataFrame, reorder(couple, Fluctuation1, mean))
  plotfluctuation <- ggplot(FluctuationDataFrame, aes(x = couple, y = Fluctuation1 )) +
    geom_boxplot(position=position_dodge(1)) +
    theme(legend.position="none") + theme_bw() +
    labs(y=expression(atop("Fluctuation1", paste("(log2dur)"))), x = "duets")+
    scale_fill_grey(start = 0.8, end = 0.2)
  plotstability <- ggplot(FluctuationDataFrame, aes(x = couple, y = Stability )) +
    geom_boxplot(position=position_dodge(1)) +
    theme(legend.position="none") + theme_bw() +
    labs(y=expression(atop("Stability", paste("(log2dur)"))), x = "duets")+
    scale_fill_grey(start = 0.8, end = 0.2)
  plotoutlier <- ggplot(FluctuationDataFrame, aes(x = couple, y = Outlier )) +
    geom_boxplot(position = position_dodge(1)) +
    theme(legend.position = "none") + theme_bw() +
    labs(y=expression(atop("Outlier", paste("(%)"))), x = "duets")+
    scale_fill_grey(start = 0.8, end = 0.2)

  #### Narration analysis
  c <- length(Couple)
  t <- length(Trial)
  NarrationMatrix <- matrix(nrow = c*t, ncol = 5)
  i <- 0
  for (couple in Couple) {
    for (trial in Trial) {
      i <- i + 1
      Narr <- OutcomeTrialCouple[[couple]][[trial]]$R$Narrative
      Narr <- as.vector(na.omit(Narr))
      res <- crqa(ts1 = Narr, ts2 = Narr, delay = 1, embed = 4,
                  rescale = 0, radius = 1, normalize = 2, mindiagline = 2,
                  minvertline = 2, tw = 0, whiteline = FALSE)
      condi <-Condition[couple, trial+1]
      RR <- res$RR
      DET <- res$DET
      NarrationMatrix[i,] <- c(couple, trial, condi, RR, DET)
    }
  }
  NarrationDataFrame <- as.data.frame(NarrationMatrix)
  colnames(NarrationDataFrame) <- c("couple","trial","condi","RR","DET")
  NarrationDataFrame$couple <- FluctuationDataFrame$couple
  plotnarration <- ggplot(NarrationDataFrame, aes(x=couple, y=RR )) +
    geom_boxplot(position = position_dodge(1)) +
    theme(legend.position = "none") + theme_bw() +
    labs(y=expression(atop("Narration", paste("(RR)"))), x="duets") +
    scale_fill_grey(start = 0.8, end = 0.2)

  plo[[ii]] <- list(plotfluctuation, plotstability, plotoutlier,plotnarration)
  if (ii == 1){
    OutcomeTrialCoupleA <- OutcomeTrialCouple
    AnalysisDataFrameA <- as.data.frame(cbind(FluctuationDataFrame[,1:6],
                                            NarrationDataFrame[,4:5]))
  } else {
    OutcomeTrialCoupleB <- OutcomeTrialCouple
    AnalysisDataFrameB <- as.data.frame(cbind(FluctuationDataFrame[,1:6],
                                            NarrationDataFrame[,4:5]))
  }
}

## Plotting of Figures 5 6 --
# FIGURE 5 ----
Fig5 <- arrangeGrob(
  OutcomeTrialCoupleA[[16]][[5]]$pl,
OutcomeTrialCoupleA[[4]][[5]]$pl,
OutcomeTrialCoupleB[[16]][[5]]$pl,
OutcomeTrialCoupleB[[4]][[5]]$pl,
ncol=2, nrow=2)


#Tables
T1 <- cbind(
  F = OutcomeTrialCoupleA[[16]][[5]]$Post$Fluctuation1,
  F2 = OutcomeTrialCoupleA[[16]][[5]]$Post$Fluctuation2,
  S = OutcomeTrialCoupleA[[16]][[5]]$Post$Stability
)
rownames(T1) <- c("IOI1","IOI2","IOI3")

T2 <- cbind(
  F1 = OutcomeTrialCoupleA[[4]][[5]]$Post$Fluctuation1,
  F2 = OutcomeTrialCoupleA[[4]][[5]]$Post$Fluctuation2,
  S = OutcomeTrialCoupleA[[4]][[5]]$Post$Stability
)
rownames(T2) <- c("IOI1","IOI2","IOI3")

T3 <- cbind(
  F1 = OutcomeTrialCoupleB[[16]][[5]]$Post$Fluctuation1,
  F2 = OutcomeTrialCoupleB[[16]][[5]]$Post$Fluctuation2,
  S = OutcomeTrialCoupleB[[16]][[5]]$Post$Stability
)
rownames(T3) <- c("IOI1","IOI2","IOI3")

T4 <- cbind(
  F1 = OutcomeTrialCoupleB[[4]][[5]]$Post$Fluctuation1,
  F2 = OutcomeTrialCoupleB[[4]][[5]]$Post$Fluctuation2,
  S = OutcomeTrialCoupleB[[4]][[5]]$Post$Stability
)
rownames(T4) <- c("IOI1","IOI2","IOI3")
g1 <- tableGrob(round(T1,digits=2))#
g2 <- tableGrob(round(T2,digits=2))#
g3 <- tableGrob(round(T3,digits=2))#
g4 <- tableGrob(round(T4,digits=2))#,

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

Tab5 <- arrangeGrob(gt1, gt2, gt3,gt4, ncol=2)
grid.arrange(Fig5,Tab5,ncol=2)


# FIGURE 6 ----
Couples <- list(4,16)
trial <- 5
#OutcomeTrialCoupleA
for (couple in Couples) {
  delay = 1; embed = 4; rescale =  0; radius = 1; normalize = 2; minvertline = 2;
  mindiagline = 2; tw = 0; whiteline = FALSE
  Narr <- OutcomeTrialCoupleA[[couple]][[trial]]$R$Narrative
  Narr <- as.vector(na.omit(Narr))
  res <- crqa(Narr,Narr,delay,embed,rescale,radius,normalize,minvertline,mindiagline)
  mRP <- melt(as.matrix(res$RP), varnames = c("TimeV1", "TimeV2"), value.name = "Recurrence")

  pl <-
    ggplot(mRP, aes(x = TimeV1, y = TimeV2, fill = Recurrence)) +
    coord_fixed()+
    geom_raster() +
    theme(
      axis.line = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      axis.text  = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank()
    ) +
    # ggtitle("Binary Recurrence Plot") +
    scale_fill_manual(values = c("black","white"),
                      breaks = c(TRUE, FALSE)) +
    theme_bw() +
    labs(y="time", x="time")+
    theme(legend.position = "none")

  if(couple == Couples[1]) p1 <- pl
  if(couple == Couples[2]) p2 <- pl
}
#OutcomeTrialCoupleB
for (couple in Couples) {
  delay = 1; embed = 4; rescale =  0; radius = 1; normalize = 2; minvertline = 2;
  mindiagline = 2; tw = 0; whiteline = FALSE
  Narr <- OutcomeTrialCoupleB[[couple]][[trial]]$R$Narrative
  Narr <- as.vector(na.omit(Narr))
  res <- crqa(Narr,Narr,delay,embed,rescale,radius,normalize,minvertline,mindiagline)
  mRP <- melt(as.matrix(res$RP), varnames = c("TimeV1", "TimeV2"), value.name = "Recurrence")

  pl <-
    ggplot(mRP, aes(x = TimeV1, y = TimeV2, fill = Recurrence)) +
    coord_fixed()+
    geom_raster() +
    theme(
      axis.line = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      axis.text  = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank()
    ) +
    # ggtitle("Binary Recurrence Plot") +
    scale_fill_manual(values = c("black","white"),
                      breaks = c(TRUE, FALSE)) +
    theme_bw() +
    labs(y="time", x="time")+
    theme(legend.position = "none")

  if(couple == Couples[1]) p3 <- pl
  if(couple == Couples[2]) p4 <- pl
}

grid.arrange(p2,p1,p4,p3, ncol=2)

# FIGURE 7 ----
grid.arrange(plo[[1]][[1]], plo[[2]][[1]],
             plo[[1]][[2]], plo[[2]][[2]],
             plo[[1]][[3]],  plo[[2]][[3]],
             plo[[1]][[4]],  plo[[2]][[4]],
             ncol=2, nrow=4)

# FIGURE 8 ----
for (ii in c(1,2)){
  if (ii==1) AnalysisDataFrame <- AnalysisDataFrameA # No regularization
  else AnalysisDataFrame <- AnalysisDataFrameB # Regularization

# Make subject dataframe with Agency and Annotations
### Put All Agency in matrix ---
Agency <- as.matrix(unlist(AllAgency[["agencyperduo"]]))
# reform the matrix
AgencyMatrix <- matrix(nrow=21, ncol=9)
index <- 0
for (i in Agency[,1]) {
  index<-index+1
  AgencyMatrix[i,] <- Agency[index,]
}
AgencyMatrix[is.nan(AgencyMatrix)] <-NA
### OK now ready to be used
### Construct the SubjectiveDataFrame ---
c <- as.numeric(length(Couple))
t <- as.numeric(length(Trial))
SubjectiveMatrix <- matrix(nrow=c*t, ncol=5)
i <- 0
for (couple in Couple) {
  for (trial in Trial) {
    i <- i+1
    condi <-Condition[couple,trial+1]
    annotation <- as.numeric(unlist(AllAnnotations$ANNDATA[[couple + (trial - 1) * 21]]))
    agency <- as.numeric(AgencyMatrix[couple,trial+1])
    SubjectiveMatrix[i,]<-c(couple,trial,condi,annotation,agency)
  }
}
SubjectiveDataFrame <- as.data.frame(SubjectiveMatrix)
colnames(SubjectiveDataFrame) <- c("couple","trial","condi","Quality","Agency")
# ATTENTION couple 1 is REFERENCE and should not be taken into account for correlation analysis !!!
# Construct the AnalysisDataFrame containing all data, using AnalysisDataFrame from previous
# section
AnalysisDataFrame <- as.data.frame(cbind(AnalysisDataFrame,
                                         SubjectiveDataFrame[,4:5]))
AnalysisDataFrameNoRef <- AnalysisDataFrame[-(1:8),] # Final matrix

### Correlations ---
#write.csv(AnalysisDataFrameNoRef, "AnalysisDataFrameNoRef.csv", row.names=FALSE)

#c<- cor.test(AnalysisDataFrame$annotation,AnalysisDataFrame$ferror, use="pairwise.complete.obs")
#cor.test(AnalysisDataFrame$annotation,AnalysisDataFrame$agency, use="pairwise.complete.obs")


# https://briatte.github.io/ggcorr/
ggcorr(AnalysisDataFrameNoRef[,c(4:10)], #method = c("complete", "pearson"),
       geom = "blank", label = TRUE, label_size = 4, label_round = 2, hjust = 0.75,layout.exp = 1)+
  geom_point(size = 15, aes(color = coefficient > 0, alpha = abs(coefficient) > 0.5)) +
  scale_alpha_manual(values = c("TRUE" = 0.25, "FALSE" = 0)) +
  guides(color = FALSE, alpha = FALSE)

ggcorr(AnalysisDataFrameNoRef[,c(4:10)], #method = c("complete", "pearson"),
       label = TRUE, label_size = 4, label_round = 2, hjust = 0.75,layout.exp = 1)+
  # geom_point(size = 15, aes(color = coefficient > 0, alpha = abs(coefficient) > 0.5)) +
  #  scale_alpha_manual(values = c("TRUE" = 0.25, "FALSE" = 0)) +
  guides(color = FALSE, alpha = FALSE)

cp <- ggcorr(AnalysisDataFrameNoRef[,c(4,5,6,7,9,10)], method = c("complete", "kendall"),
             label = TRUE, label_size = 4, label_round = 2, hjust = 0.75,layout.exp = 1)+
  # geom_point(size = 15, aes(color = coefficient > 0, alpha = abs(coefficient) > 0.5)) +
  #  scale_alpha_manual(values = c("TRUE" = 0.25, "FALSE" = 0)) +
  guides(color = FALSE, alpha = FALSE) #+
# labs(title="Correlations (Kendall)")
if (ii == 1) cp1 <- cp # No regularization
else cp2 <- cp # Regularization
}

grid.arrange(cp1,cp2, ncol = 2)


