BLplot <- function(R,tempo){
print(paste('plot tempo =',tempo))

O = as.data.frame(cbind(R$Timeline,R$Observed))#
names(O)[1] <-"Timeline"
Omelt <- melt(O,id=c("Timeline"))#,"Name"))

P <- as.data.frame(cbind(R$Timeline,R$Prediction))#
names(P)[1] <-"Timeline"
Pmelt <- melt(P,id=c("Timeline"))#,"Name"))

Ot       <- as.data.frame(cbind(R$Timeline,log2(R$Outliers)))#,Name))
names(Ot)[1] <-"Timeline"
Otmelt <- melt(Ot,id=c("Timeline"))#,"Name"))

Vplus       <- as.data.frame(cbind(R$Timeline,R$Prediction+ 2*sqrt(R$Variance)))#,Name))
names(Vplus)[1] <-"Timeline"
Vplusmelt <- melt(Vplus,id=c("Timeline"))#,"Name"))

Vmin        <- as.data.frame(cbind(R$Timeline,R$Prediction- 2*sqrt(R$Variance)))#,Name))
names(Vmin)[1] <-"Timeline"
Vminmelt  <- melt(Vmin,id=c("Timeline"))#,"Name"))

Tempo              <- as.data.frame(cbind(R$Timeline,R$Tempo))
names(Tempo)[1] <-"Timeline"
Tempomelt  <- melt(Tempo,id=c("Timeline"))#,"Name"))

pl <- ggplot() +
  geom_point(data=Omelt, aes(x=Timeline,y = value, shape = factor(variable)),  size = 2) +
  geom_line(data=Pmelt, aes(x=Timeline,y = value, color = factor(variable)), size = 1)+
  geom_point(data=Otmelt, aes(x=Timeline, y = value, color = "gray"),size = 2) +
  theme_bw() +
  theme(legend.position="none")+
  labs(y = "IOI (log2dur)" , x="time (sa)") 

if(tempo==1) {
  pl <- pl + geom_line(data = Tempomelt, aes(x=Timeline,y = value), size = 1.2,colour="black",linetype="dotted")
}
#pl +   
#  geom_line(data=Vplusmelt, aes(x=Timeline,y = value, color = factor(variable)), size = .8,linetype="dotted")  +
#  geom_line(data=Vminmelt, aes(x=Timeline,y = value, color = factor(variable)), size = .8,linetype="dotted")

return(pl)
}