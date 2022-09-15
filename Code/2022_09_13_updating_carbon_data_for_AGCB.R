#2022_09_13_updating_carbon_data_for_AGCB

setwd("/users/zhajianf/desktop/July 2021 Desktop/Oct 30 desktop/Research 2019")
c.dat <- read.csv("2022_04_19_PR_Carbon_Data_for_anaysis.csv")
update.dat <- read.csv("2022_09_13_Javier_updated_AGCB.csv")

head(update.dat)
colnames(c.dat)
c.dat$Site
update.dat$Site

updated.agcb <- numeric(length(c.dat$Site))

for(x in 1:length(c.dat$Site)){
	
	#x <- 1
	if(sum(update.dat$Site == c.dat$Site[x]) == 0){
		## i doubled checked the c.dat and it looks like if it is missing from the updated.at then it is equal to zero. So just skipping these and having the zero is correct
		#updated.agcb[x] <- "missing"

	}else{
	new.AGCB <- update.dat[update.dat$Site == c.dat$Site[x],]$revised_AGCB_trees
	updated.agcb[x] <- new.AGCB
	
	}
	
}

c.dat$tot_AGCB_trees_updated <- updated.agcb

data.frame(c.dat$tot_AGCB_trees_updated,c.dat$tot_AGCB_trees)
max(c.dat$tot_AGCB_trees)

sort(update.dat$revised_AGCB_trees)

head(c.dat)
colnames(c.dat)

### all of these calculations will need to be updated due to the error in the origional data
### lets update them in order to keep track of dependant calculations 
 
#1.) tot_shade_tree_C <- tot_AGCB_trees + tot_coarse_root_biomass_trees

#2.) tot_AGCB <-  AGCB_coffee + AGCB_banana + AGCB_citrus + tot_AGCB_trees

#3.) tot_farm_C <- tot_AGCB + coarse_root_biomass_coffee + coarse_root_biomass_citrus + tot_coarse_root_biomass_trees + SOC


tot_shade_tree_C_updated <- c.dat$tot_AGCB_trees_updated + c.dat$tot_coarse_root_biomass_trees

tot_AGCB_updated <- c.dat$AGCB_coffee + c.dat$AGCB_banana + c.dat$AGCB_citrus + tot_shade_tree_C_updated


tot_farm_C_updated <- tot_AGCB_updated + c.dat$coarse_root_biomass_coffee + c.dat$coarse_root_biomass_citrus + c.dat$tot_coarse_root_biomass_trees + c.dat$SOC

c.dat$tot_shade_tree_C_updated <- tot_shade_tree_C_updated
c.dat$tot_AGCB_updated <- tot_AGCB_updated
c.dat$tot_farm_C_updated <- tot_farm_C_updated

head(c.dat)


#write.csv(c.dat,"2022_09_13_PR_Carbon_Data_for_anaysis_updated.csv")






