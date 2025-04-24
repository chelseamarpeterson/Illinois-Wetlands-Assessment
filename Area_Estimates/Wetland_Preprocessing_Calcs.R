path_to_nwi_data = "C:/Users/Chels/OneDrive - University of Illinois - Urbana/Illinois Wetlands Risk Assessment/Results/NWI_Data"

library(ggplot2)
library(dplyr)
library(patchwork)
library(tidyr)

# conversions
AcPerHa = 2.47105

################################################################################
# Estimate area of excavated/unexcavated ponds
setwd(path_to_nwi_data)
nondeep.df = read.csv("IL_WS_Step4_NonDeep_ExportTable.csv")

pond.area = sum(nondeep.df[which(nondeep.df$WETLAND_TYPE == "Freshwater Pond"),"Area_Acres"])
x.pond.area = sum(nondeep.df[which(nondeep.df$WETLAND_TYPE == "Freshwater Pond" & nondeep.df$FIRST_MODI == "Excavated"),"Area_Acres"])
ux.pond.area = sum(nondeep.df[which(nondeep.df$WETLAND_TYPE == "Freshwater Pond" & nondeep.df$FIRST_MODI != "Excavated"),"Area_Acres"])

ux.pond.area.sum = nondeep.df[which(nondeep.df$WETLAND_TYPE == "Freshwater Pond"),] %>% 
                   group_by(WATER_REGI) %>%
                   summarize(area.acres.total = sum(Area_Acres))

################################################################################
# Estimate area effect of each exclusion

# read in nondeep file
nondeep.df = read.csv("IL_WS_Step5_NonDeep_JoinGroupMerge_Table.csv")
sum(nondeep.df$ImpAre[-which(is.na(nondeep.df$ImpAre))])
areas = nondeep.df[-which(is.na(nondeep.df$ImpAre)),c("ACRES","ImpAre","ImpPer")]

# read in NLCD x NWI intersection file
intersect.df = read.csv("IL_WS_Step6_NonDeep_NLCD_Intersect_Table.csv")

# estimate total leveed area with grid codes 82, 23, and 24
df.23 = intersect.df[which(intersect.df$gridcode == 23),]
df.24 = intersect.df[which(intersect.df$gridcode == 24),]
df.82 = intersect.df[which(intersect.df$gridcode == 82),]
leveed23 = sum(df.23[which(df.23$Within_Levee == 1),"Area_Ha_NLCD"])
leveed24 = sum(df.24[which(df.24$Within_Levee == 1),"Area_Ha_NLCD"])
leveed82 = sum(df.82[which(df.82$Within_Levee == 1),"Area_Ha_NLCD"])
total23 = sum(df.23[,"Area_Ha_NLCD"])
total24 = sum(df.24[,"Area_Ha_NLCD"])
total82 = sum(df.82[,"Area_Ha_NLCD"])

# total area and count
area.total = sum(intersect.df$Area_Ha_NLCD)
count.total = length(unique(intersect.df$NWI_ID))
round(area.total)
round(area.total*AcPerHa)
count.total

# remove cultivated crops
intersect.df.no82 = intersect.df[-which(intersect.df$gridcode == 82),]
area.no82 = sum(intersect.df.no82$Area_Ha_NLCD)
count.no82 = length(unique(intersect.df.no82$NWI_ID))
round(area.no82)
round(area.total-area.no82)
count.no82
count.total-count.no82

# remove developed medium intensity
intersect.df.no23 = intersect.df.no82[-which(intersect.df.no82$gridcode == 23),]
area.no23 = sum(intersect.df.no23$Area_Ha_NLCD)
count.no23 = length(unique(intersect.df.no23$NWI_ID))
round(area.no23)
round(area.no82-area.no23)
count.no23
count.no82-count.no23

# remove developed high intensity
intersect.df.no24 = intersect.df.no23[-which(intersect.df.no23$gridcode == 24),]
area.no24 = sum(intersect.df.no24$Area_Ha_NLCD)
count.no24 = length(unique(intersect.df.no24$NWI_ID))
count.no24
count.no23-count.no24
round(area.no24)
round(area.no23-area.no24)

# remove farmed
intersect.df.nofarmed = intersect.df.no24[-which(intersect.df.no24$FIRST_MODI == "Farmed"),]
area.nofarmed = sum(intersect.df.nofarmed$Area_Ha_NLCD)
count.nofarmed = length(unique(intersect.df.nofarmed$NWI_ID))
count.nofarmed
count.no24-count.nofarmed
round(area.nofarmed)
round(area.no24-area.nofarmed)

# remove excavated
intersect.df.excavated = intersect.df.nofarmed[-which(intersect.df.nofarmed$FIRST_MODI == "Excavated"),]
area.excavated = sum(intersect.df.excavated$Area_Ha_NLCD)
count.excavated = length(unique(intersect.df.excavated$NWI_ID))
count.excavated
count.nofarmed-count.excavated
round(area.excavated)
round(area.nofarmed-area.excavated)

# remove artificially flooded
intersect.df.noartflood = intersect.df.excavated[-which(intersect.df.excavated$WATER_REGI == "Artificially Flooded"),]
area.noartflood = sum(intersect.df.noartflood$Area_Ha_NLCD)
count.noartflood = length(unique(intersect.df.noartflood$NWI_ID))
count.noartflood
count.excavated-count.noartflood
round(area.noartflood)
round(area.excavated-area.noartflood)

# remove area >= 0.10 ac
areathres.df = read.csv("IL_WS_Step11_AreaThreshold_Table.csv")
area.areathres = sum(areathres.df$Area_Ha)
count.areathres = length(unique(areathres.df$NWI_ID))
count.areathres
count.noartflood-count.areathres
round(area.areathres)
round(area.noartflood-area.areathres)

################################################################################
# plot farmed and developed areas for each wetland flood frequency threshold

# remove excavated, farmed, art flooded, < 0.10 ac
art.fld.tf = (intersect.df$WATER_REGI == "Artificially Flooded")
f.and.ex.tf = (intersect.df$FIRST_MODI %in% c("Excavated","Farmed"))
df1 = intersect.df[-which(art.fld.tf & f.and.ex.tf),]
df2 = df1[which(df1$NWI_ID %in% unique(areathres.df$NWI_ID)),]
landcov.sum = df2[which(df2$gridcode %in% c(23,24,82)),c("WATER_REGI","gridcode","Area_Acres")] %>%
              group_by(WATER_REGI, gridcode) %>%
              summarize(area = sum(Area_Acres))
ggplot(landcov.sum, 
       aes(x=area, 
           y=factor(WATER_REGI, levels=water.reg.order), 
           fill=factor(gridcode))) + 
       geom_bar(stat="identity")

# estimate cumulative land cover for each water regi
cum.df = data.frame(matrix(nrow=8, ncol=4))
colnames(cum.df) = c("water_cutoff","23","24","82")
for (i in 1:n.w) {
  cum.df[i,"water_cutoff"] = water.reg.order[i]
  wrs = water.reg.order[1:i]
  cum.df[i,"23"] = sum(landcov.sum[(landcov.sum$WATER_REGI %in% wrs & landcov.sum$gridcode == "23"),"area"])
  cum.df[i,"24"] = sum(landcov.sum[(landcov.sum$WATER_REGI %in% wrs & landcov.sum$gridcode == "24"),"area"])
  cum.df[i,"82"] = sum(landcov.sum[(landcov.sum$WATER_REGI %in% wrs & landcov.sum$gridcode == "82"),"area"])
}
cum.melt = melt(cum.df)
ggplot(cum.melt, 
       aes(x=value, 
           y=factor(water_cutoff, levels=water.reg.order), 
           fill=factor(variable))) + 
  geom_bar(stat="identity")
