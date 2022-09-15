rm(list=ls())


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

legend(0,310,col=c("red","black"),legend=c("SOC","Trees"),pch=19,cex=0.8)


###############################################################
# 		     	Statistical tests for regressions             #
###############################################################

summary(soc.model)
summary(tree.model)



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



###############################################################
# Statistical test for difference bewteen shade and no shade  #
###############################################################
wilcox.test(shade.y,sun.y)	
			
#data:  shade.y and sun.y
#W = 1081, p-value = 1.708e-07
#alternative hypothesis: true location shift is not equal to 0




###########################
#						  #
##      Figure 7         ##
#						  #
###########################

colnames(c.dat)


## these are all of the explanatory variables we are looking at 
c.dat$CC_avg

c.dat$measurement_height_2019
## need to convert coffe height to meters
c.dat$coffe_height_m <- c.dat$measurement_height_2019 * 0.3048

c.dat$avg_canopy_ht

c.dat$canopy_cover

c.dat$ground_cover_10cm2

c.dat$num_crops


## these are our response variables 
c.dat$tot_AGCB_updated
c.dat$tot_farm_C_updated
c.dat$SOC


plot(c.dat$canopy_cover ,c.dat$tot_AGCB_updated)
plot(c.dat$CC_avg ,c.dat$tot_AGCB_updated)

fig.dat <- c.dat[,c("tot_farm_C_updated","tot_AGCB_updated","SOC","CC_avg", "avg_canopy_ht", "coffe_height_m","ground_cover_10cm2", "num_crops")]
head(fig.dat)



stat.output <- data.frame(matrix(ncol=4,nrow=1))
colnames(stat.output) <- c("x.var","y.var","cor","p.val")

names <- c("Farm C","Farm AGCB","SOC","% canopy cover","canopy height (m)","coffee height","% ground cover","# of crops")
par(mfrow=c(5,3),mai=c(0.5,0.5,0.1,0.1))
for(x in 4:dim(fig.dat)[2] ){
	for(y in 1:3 ){
		#y <- 1 ## goes 1-3
		#x <- 5 ## starts on 4
		x.dat <- fig.dat[,x]
		y.dat <- fig.dat[,y]
		plot(x.dat,y.dat,xlab="",ylab="",yaxt="n",cex=0.5)
		axis(2,las=2)
		mtext(names[x],side=1,line=2.5,cex=0.7)
		mtext(names[y],side=2,line=2.8,cex=0.7)
		
		if(cor.test(x.dat, y.dat,method="spearman")$p.value <= 0.05){
			points(x.dat,y.dat,cex=1,pch=19,col="goldenrod")
		}else{
			points(x.dat,y.dat,cex=1,pch=19,col="black")
		}
		
		coeff <- round(cor.test(x.dat, y.dat,method="spearman")$estimate,digits=4)
		cor.p <- round(cor.test(x.dat, y.dat,method="spearman")$p.value,digits=4)
		temp.df <- data.frame(names[x],names[y], coeff, cor.p)
		colnames(temp.df) <- c("x.var","y.var","cor","p.val")
		stat.output <- rbind(stat.output,temp.df)
		
	}
}

stat.output.df <- stat.output[2:dim(stat.output)[1],]
rownames(stat.output.df) <- NULL

#write.csv(stat.output.df,"2022_09_13_PR_Carbon_Fig_7_spearman_correlation_stats.csv")




