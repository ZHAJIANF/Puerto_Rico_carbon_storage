rm(list=ls())

setwd("/users/zhajianf/desktop/July 2021 Desktop/Oct 30 desktop/Research 2019")

#c.dat <- read.csv("2022_04_19_PR_Carbon_Data_for_anaysis.csv")

c.dat<- read.csv("2022_09_13_PR_Carbon_Data_for_anaysis_updated.csv") ## this is the updated df with correct AGCB 


######################################## composition on farms
#### for each of the farms, look at the proportion that each catagory adds to the overall carbon on the farm
perc.citrus <-numeric(71)
perc.coffee<-numeric(71)
perc.trees<-numeric(71)
perc.soc<-numeric(71)
perc.banana<-numeric(71)
tot.c<-numeric(71)
tot.c.from.dat<-numeric(71)
col.vec <-numeric(71)
tree.col.vec <-numeric(71)

for(x in 1:71){ ## therea re 71 fincas 
	#x <- 1
	a.farm.dat <- c.dat[x,]
	head(c.dat)
	
	c.dat$grupo_cultivo
	#Co=Coffee; Co/Ci=Coffee-Citrus; Co/M=Coffee-Musa; Co/Ci/M=Coffee-Citrus-Musa
	
	if(a.farm.dat$grupo_cultivo == "Co"){
		col.vec[x] <- "red"
	}else if(a.farm.dat$grupo_cultivo == "CoT"){
		col.vec[x] <- "dark red"
	}else if(a.farm.dat$grupo_cultivo == "CoCi"){
		col.vec[x] <- "green"
	}else if(a.farm.dat$grupo_cultivo == "CoCiT"){
		col.vec[x] <- "dark green"
	}else if(a.farm.dat$grupo_cultivo == "CoM"){
		col.vec[x] <- "yellow"
	}else if(a.farm.dat$grupo_cultivo == "CoMT"){
		col.vec[x] <- "goldenrod"
	}else if(a.farm.dat$grupo_cultivo == "CoCiM"){
		col.vec[x] <- "blue"
	}else if(a.farm.dat$grupo_cultivo == "CoCiMT"){
		col.vec[x] <- "dark blue"
	}
	
		if(a.farm.dat$grupo_cultivo == "CoCiMT" | a.farm.dat$grupo_cultivo == "CoMT" |a.farm.dat$grupo_cultivo == "CoCiT" |a.farm.dat$grupo_cultivo == "CoT" ){
		tree.col.vec[x] <- "dark green"
	}else{
		tree.col.vec[x] <- "dark red"
	}
	
	## clean the values becuase NAs 
	citrus.val <- a.farm.dat$tot_citrus_C
	coffee.val <- a.farm.dat$tot_coffee_C
	trees.val <- a.farm.dat$tot_shade_tree_C_updated
	soc.val <- a.farm.dat$SOC
	banana.val <- a.farm.dat$AGCB_banana
	
	## if they are NAs then that means there wasnt anything there so the biomass with be zero 
	if(is.na(citrus.val)){
		citrus.val <- 0 
	}
	if(is.na(coffee.val)){
		coffee.val <- 0 
	}
	if(is.na(trees.val)){
		trees.val <- 0 
	}
	if(is.na(soc.val)){
		soc.val <- 0 
	}
	if(is.na(banana.val)){
		banana.val <- 0 
	}
	
	####### 
	############ Check with Javiar 
	## tot.farm carbon and the sum of these dont match. Need to figure out what is going on here... 
	#a.farm.dat$tot_farm_C[x]
	sum.total <- sum(citrus.val,coffee.val,trees.val,soc.val,banana.val)
	tot.c[x] <- sum.total
	tot.c.from.dat[x] <- a.farm.dat$tot_farm_C_updated
	perc.citrus[x] <- citrus.val/sum.total
	perc.coffee[x] <- coffee.val/sum.total
	perc.trees[x] <- trees.val/sum.total
	perc.soc[x] <- soc.val/sum.total
	perc.banana[x] <- banana.val/sum.total
	
	
	### now using the denominator from the data total which includes the root biomasses. 
	sum.total <- sum(citrus.val,coffee.val,trees.val,soc.val,banana.val)
	tot.c[x] <- sum.total
	tot.c.from.dat[x] <- a.farm.dat$tot_farm_C_updated
	perc.citrus[x] <- citrus.val/a.farm.dat$tot_farm_C_updated
	perc.coffee[x] <- coffee.val/a.farm.dat$tot_farm_C_updated
	perc.trees[x] <- trees.val/a.farm.dat$tot_farm_C_updated
	perc.soc[x] <- soc.val/a.farm.dat$tot_farm_C_updated
	perc.banana[x] <- banana.val/a.farm.dat$tot_farm_C_updated
	
}
## make them percentages 
perc.citrus <- 100* perc.citrus
perc.coffee<- 100* perc.coffee
perc.trees<- 100* perc.trees
perc.soc<- 100* perc.soc
perc.banana<- 100* perc.banana

######## 2022 04 11 new analysis ######

c.dat$perc.citrus <- perc.citrus
c.dat$perc.coffee <- perc.coffee
c.dat$perc.trees <- perc.trees
c.dat$perc.soc <- perc.soc
c.dat$perc.banana <- perc.banana



###### colored points in the background
par(mfrow=c(1,1),mai=c(0.8,0.8,0.1,0.1))
jit.val <- 0.2
lwd.val <- 1.5
pch.val <- 1
cex.val <- 1
cex.val.mean <- 1
plot(NA,ylim=c(-0.05,100),xlim=c(0.5,5.5),yaxt="n",xaxt="n",ylab="",xlab="")
axis(2,las=2)
axis(1,labels=c("Musa","Coffee","Citrus","Shade","SOC"),at=seq(1,5,1),las=2)
x.val <- jitter(rep(1,(71)),amount= jit.val)
points(x.val, perc.banana,col= col.vec,lwd=lwd.val,pch=pch.val,cex=cex.val)
x.val <- jitter(rep(2,(71)),amount= jit.val)
points(x.val, perc.coffee,col= col.vec,lwd=lwd.val,pch=pch.val,cex=cex.val)
x.val <- jitter(rep(3,(71)),amount= jit.val)
points(x.val, perc.citrus,col= col.vec,lwd=lwd.val,pch=pch.val,cex=cex.val)
x.val <- jitter(rep(4,(71)),amount= jit.val)
points(x.val, perc.trees,col= col.vec,lwd=lwd.val,pch=pch.val,cex=cex.val)
x.val <- jitter(rep(5,(71)),amount= jit.val)
points(x.val, perc.soc,col= col.vec,lwd=lwd.val,pch=pch.val,cex=cex.val)
mtext("% of farm C",side=2,line=2.5)

index <- seq(1,5,1)
mean <- c(mean(perc.banana),mean(perc.coffee),mean(perc.citrus),mean(perc.trees),mean(perc.soc))
## standard deviation
error <- c(sd(perc.banana),sd(perc.coffee),sd(perc.citrus),sd(perc.trees),sd(perc.soc))

##standard error
error <- c(sd(perc.banana)/sqrt(length(perc.banana)),sd(perc.coffee)/sqrt(length(perc.coffee)),sd(perc.citrus)/sqrt(length(perc.citrus)),sd(perc.trees)/sqrt(length(perc.trees)),sd(perc.soc)/sqrt(length(perc.soc)))
## 1.96 sigma is the 95% CI
error <- error*1.96

points(index,mean,col="black",pch=17,cex= cex.val.mean)
arrows(index,mean+error,index,mean-error,code=3,length=0.03,angle=90,col="black",lwd=2)
legend(0.5,100,legend=c("Co","CoT","CoCi","CoCiT","CoM","CoMT", "CoCiM","CoCiMT"),col=c("red","dark red","green","dark green","yellow","goldenrod","blue","dark blue"),pch=1,cex=0.8)




## Tree coors
jit.val <- 0.2
lwd.val <- 1.5
pch.val <- 1
cex.val <- 1
cex.val.mean <- 1
plot(NA,ylim=c(-0.05,100),xlim=c(0.5,5.5),yaxt="n",xaxt="n",ylab="",xlab="")
axis(2,las=2)
axis(1,labels=c("Musa","Coffee","Citrus","Shade","SOC"),at=seq(1,5,1),las=2)
x.val <- jitter(rep(1,(71)),amount= jit.val)
points(x.val, perc.banana,col= tree.col.vec,lwd=lwd.val,pch=pch.val,cex=cex.val)
x.val <- jitter(rep(2,(71)),amount= jit.val)
points(x.val, perc.coffee,col= tree.col.vec,lwd=lwd.val,pch=pch.val,cex=cex.val)
x.val <- jitter(rep(3,(71)),amount= jit.val)
points(x.val, perc.citrus,col= tree.col.vec,lwd=lwd.val,pch=pch.val,cex=cex.val)
x.val <- jitter(rep(4,(71)),amount= jit.val)
points(x.val, perc.trees,col= tree.col.vec,lwd=lwd.val,pch=pch.val,cex=cex.val)
x.val <- jitter(rep(5,(71)),amount= jit.val)
points(x.val, perc.soc,col= tree.col.vec,lwd=lwd.val,pch=pch.val,cex=cex.val)
mtext("% of farm C",side=2,line=2.5)

index <- seq(1,5,1)
mean <- c(mean(perc.banana),mean(perc.coffee),mean(perc.citrus),mean(perc.trees),mean(perc.soc))
## standard deviation
error <- c(sd(perc.banana),sd(perc.coffee),sd(perc.citrus),sd(perc.trees),sd(perc.soc))

##standard error
error <- c(sd(perc.banana)/sqrt(length(perc.banana)),sd(perc.coffee)/sqrt(length(perc.coffee)),sd(perc.citrus)/sqrt(length(perc.citrus)),sd(perc.trees)/sqrt(length(perc.trees)),sd(perc.soc)/sqrt(length(perc.soc)))
## 1.96 sigma is the 95% CI
error <- error*1.96


points(index,mean,col="goldenrod",pch=17,cex= cex.val.mean)
arrows(index,mean+error,index,mean-error,code=3,length=0.03,angle=90,col="goldenrod",lwd=2)
legend(0.5,100,legend=c("Shade", "No Shade"),col=c("dark green","dark red"),pch=19,cex=0.8)



## grey background points 
#par(mfrow=c(1,1),mai=c(0.8,0.8,0.1,0.1))
jit.val <- 0.15
plot(NA,ylim=c(-0.05,100),xlim=c(0.5,5.5),yaxt="n",xaxt="n",ylab="",xlab="")
axis(2,las=2)
axis(1,labels=c("Musa","Coffee","Citrus","Shade","SOC"),at=seq(1,5,1),las=2)
x.val <- jitter(rep(1,(71)),amount= jit.val)
points(x.val, perc.banana,col="grey",cex=cex.val)
x.val <- jitter(rep(2,(71)),amount= jit.val)
points(x.val, perc.coffee,col="grey",cex=cex.val)
x.val <- jitter(rep(3,(71)),amount= jit.val)
points(x.val, perc.citrus,col="grey",cex=cex.val)
x.val <- jitter(rep(4,(71)),amount= jit.val)
points(x.val, perc.trees,col="grey",cex=cex.val)
x.val <- jitter(rep(5,(71)),amount= jit.val)
points(x.val, perc.soc,col="grey",cex=cex.val)
mtext("% of farm C",side=2,line=2.5)

index <- seq(1,5,1)
mean <- c(mean(perc.banana),mean(perc.coffee),mean(perc.citrus),mean(perc.trees),mean(perc.soc))
## standard deviation
error <- c(sd(perc.banana),sd(perc.coffee),sd(perc.citrus),sd(perc.trees),sd(perc.soc))

##standard error
error <- c(sd(perc.banana)/sqrt(length(perc.banana)),sd(perc.coffee)/sqrt(length(perc.coffee)),sd(perc.citrus)/sqrt(length(perc.citrus)),sd(perc.trees)/sqrt(length(perc.trees)),sd(perc.soc)/sqrt(length(perc.soc)))
## 1.96 sigma is the 95% CI
error <- error*1.96

points(index,mean,col="black",pch=17,cex= cex.val.mean)
arrows(index,mean+error,index,mean-error,code=3,length=0.03,angle=90,col="black",lwd=2)









