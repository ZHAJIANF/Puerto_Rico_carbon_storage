rm(list=ls())

setwd("/users/zhajianf/desktop/July 2021 Desktop/Oct 30 desktop/Research 2019")


c.dat<- read.csv("2022_09_13_PR_Carbon_Data_for_anaysis_updated.csv") ## this is the updated df with correct AGCB 

###########################
#						  #
##      Figure 4         ##
#						  #
###########################
par(oma=c(2,2,2,0),mai=c(1,0.6,1,0.5))
colnames(c.dat)
plot(c.dat$SOC,c.dat$tot_farm_C_updated,col="red",pch=19, xlab=NA, ylab=NA,yaxt="n",xaxt="n")
soc.model <- lm(c.dat$tot_farm_C_updated ~ c.dat$SOC)
abline(soc.model,col="red")
summary(soc.model)
axis(3)
mtext("Soil Organic Carbon",side=3,line=2.2)

par(new=T)
plot(c.dat$tot_shade_tree_C_updated,c.dat$tot_farm_C_updated,ylab="",xlab="",yaxt="n",pch=19)
axis(2,las=2)
mtext(expression(paste("Farm C Stock (Mg ", ha^-1,")")),side=2,line=2.5)
mtext(expression(paste("Tree C Stock (Mg ", ha^-1,")")),side=1,line=2.5)
tree.model <- lm(c.dat$tot_farm_C_updated ~ c.dat$tot_shade_tree_C_updated)
abline(tree.model)
summary(tree.model)

legend(0,310,col=c("black","red"),legend=c("SOC","Trees"),pch=19,cex=0.8)


###########################
#						  #
##      Figure 5         ##
#						  #
###########################

shade.dat <- c.dat[c.dat$shade == "Shaded",]
sun.dat <- c.dat[c.dat$shade == "No Trees",]


colnames(c.dat)
par(mfrow=c(1,1),mai=c(0.8,0.8,0.1,0.1),oma=c(1,1,1,1))

shade.x <- jitter(rep(1,length(shade.dat$tot_farm_C_updated)),amount=0.1)
sun.x <- jitter(rep(2,length(sun.dat $tot_farm_C_updated)),amount=0.1)

shade.y <- shade.dat$tot_farm_C_updated
sun.y <- sun.dat$tot_farm_C_updated

plot(NA,xlab="",ylab="",yaxt="n",ylim=c(10,210),xlim=c(0.5,2.5),xaxt="n")
axis(2,las=2)
axis(1,labels=c("Shade","No Shade"),at=c(1,2),las=1)
mtext("Farm management",side=1,line=2.5)
mtext(expression(paste("Farm C Stock (Mg ", ha^-1,")")),side=2,line=2.5)
points(shade.x, shade.y,col="grey")
points(sun.x, sun.y,col="grey")
means <- c(mean(shade.y),mean(sun.y))
errors <- c(sd(shade.y)/sqrt(length(shade.y)),sd(sun.y)/sqrt(length(sun.y)))
points(c(1,2),means,pch=19)
arrows(c(1,2),means + errors,c(1,2),means - errors,angle=90,code=3,length=0.05)

###########################
#						  #
##      Figure 7         ##
#						  #
###########################

