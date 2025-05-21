path_to_nwi_data = "C:/Users/Chels/OneDrive - University of Illinois - Urbana/Illinois Wetlands Risk Assessment/Results/NWI_Data"
path_to_gitrepo = "C:/Users/Chels/OneDrive - University of Illinois - Urbana/Illinois Wetlands Risk Assessment/Public-Repo"

setwd(path_to_nwi_data)

library(dplyr)
library(ggplot2)
library(reshape2)
library(ggpattern)

################################################################################
# step 1: comparing effects of exclusions between original NWI and DU

## read in DU x NLC intersect table
du.nlcd.df = read.csv("Step5_DU_IL_NonDeep_NLCD_Intersect.csv")

# total area calculation
sum(du.nlcd.df$Area_Ha_NLCD)
length(unique(du.nlcd.df$FID_Step4_DU_IL_NonDeep))

# remove cultivated crops
du.no82.df = du.nlcd.df[-which(du.nlcd.df$gridcode == 82),]
round(sum(du.no82.df$Area_Ha_NLCD))
length(unique(du.no82.df$FID_Step4_DU_IL_NonDeep))

# remove developed medium intensity
du.no23.df = du.no82.df[-which(du.no82.df$gridcode == 23),]
round(sum(du.no23.df$Area_Ha_NLCD))
length(unique(du.no23.df$FID_Step4_DU_IL_NonDeep))

# remove developed high intensity
du.no24.df = du.no23.df[-which(du.no23.df$gridcode == 24),]
round(sum(du.no24.df$Area_Ha_NLCD))
length(unique(du.no24.df$FID_Step4_DU_IL_NonDeep))

## read in DU summary table
nwi.du.areas = read.csv("NWI_DU_Exclusions_Summary.csv")
colnames(nwi.du.areas) = c("dataset","exclusion","area","count")
nwi.du.areas$area = as.numeric(nwi.du.areas$area)
ex.order = unique(nwi.du.areas$exclusion)
datasets = c("Original NWI 1980-1987","Ducks Unlimited 2005")
p1 = ggplot(nwi.du.areas, aes(y=factor(exclusion, levels=rev(ex.order)), 
                              x=area, 
                              group=factor(dataset, levels=datasets),
                              color=factor(dataset, levels=datasets),
                              linetype=factor(dataset, levels=datasets))) + 
            geom_line(linewidth=1) + 
            labs(y="Exclusion",x="Area (ha)",
                 group="Dataset",color="Dataset",
                 linetype="Dataset") + 
            scale_color_manual(values=c("forestgreen","steelblue4")) +
            scale_x_continuous(labels=scales::comma,limits=c(375000,830000)) +
            theme(text=element_text(size=12), legend.position="none")
p2 = ggplot(nwi.du.areas, aes(y=factor(exclusion, levels=rev(ex.order)), 
                              x=count, 
                              group=factor(dataset, levels=datasets),
                              color=factor(dataset, levels=datasets),
                              linetype=factor(dataset, levels=datasets))) + 
            geom_line(linewidth=1) + 
            labs(y="",x="Polygon count",
                 group="Dataset",color="Dataset",
                 linetype="Dataset") + 
            scale_color_manual(values=c("forestgreen","steelblue4")) +
            scale_x_continuous(labels=scales::comma,limits=c(195000,500000)) +
            theme(text=element_text(size=12),
                  axis.text.y=element_blank())
p1 + p2

################################################################################
# step 2: compare jurisdictional status between original NWI and DU

## read in original NWI and DU table for wetlands above 0.10 ac (396,572 ha)
nwi.df = read.csv("IL_WS_Step11_AreaThreshold.csv")
du.df = read.csv("Step10_DU_IL_AreaThreshold.csv")

# update du column name
colnames(du.df)[which(colnames(du.df) == "WETLAND_TY")] = "WETLAND_TYPE"

# compare total areas 
sum(nwi.df$Area_Ha)
sum(du.df$Area_Ha)

# water regimes / wetland flood frequency cutoffs
water.regimes = c("Permanently Flooded","Intermittently Exposed",
                  "Semipermanently Flooded","Seasonally Flooded/Saturated",
                  "Seasonally Flooded","Seasonally Saturated",
                  "Temporary Flooded","Intermittently Flooded")
water.reg.labels = c("Permanently Flooded","Intermittently Exposed",
                     "Semipermanently Flooded","Seasonally Flooded/Saturated",
                     "Seasonally Flooded","Seasonally Saturated",
                     "Temporarily Flooded","Intermittently Flooded")
n.w = length(water.regimes)

# stream flow permanence criteria
perm.levels = c("Perennial","Intermittent","Ephemeral")
perm.abrvs = c("1","2","3")
n.p = length(perm.abrvs)

# make vectors for buffer scenarios
buf.dists = c("1","10","20","100")
buf.cols = paste("buf", buf.dists, sep="")
n.b = length(buf.dists)

# calculate non-WOTUS area
area.df = data.frame(matrix(nrow=2*n.w*n.p*n.b, ncol=16))
colnames(area.df) = c("dataset","water_cutoff","perm_level","buf_dist","area","delta_area",
                      "leveed_only","non_intersect_only","below_flood_only",
                      "leveed_and_non_intersect","leveed_and_below_flood",
                      "non_intersect_and_below_flood","leveed_non_intersect_below_flood",
                      "pond","emergent","forest")
n = 1
for (d in 1:2) {
  if (d == 1) {
    ws.df = nwi.df
  } else {
    ws.df = du.df
  }
  for (i in 1:n.w) {
    for (j in 1:n.p) {
      for (k in 1:n.b) {
        # dataset
        area.df[n,"dataset"] = datasets[d]
        
        # assign policy scenario labels
        area.df[n,"water_cutoff"] = water.regimes[i]
        area.df[n,"perm_level"] = perm.levels[j]
        area.df[n,"buf_dist"] = buf.dists[k]
        
        # create column for flow permanence and buffer distance combination
        buf.ws.col = paste("Waters_Intersect", perm.abrvs[j], buf.dists[k], sep="_")
        
        # get all water regimes up to the ith and then identify rows with insufficient flooding
        wrs1 = water.regimes[1:i]
        wrs.inds1 = !(ws.df$WATER_REGI %in% wrs1)
        
        # identify all wetland polygons that (1) don't meet the flood frequency cutoff,
        # (2) occur within a leveed area, or (3) don't intersect the WOTUS buffer
        area1 = sum(ws.df[which(wrs.inds1 | (ws.df$Within_Levee == 1 | ws.df[,buf.ws.col] == 0)),"Area_Ha"])
        area.df[n,"area"] = area1
        
        # calculate the incremental increase in non-WOTUS area relative to less strict 
        # flood frequency cutoff
        if (i < n.w) {
          wrs2 = water.regimes[1:(i+1)]
          wrs.inds2 = !(ws.df$WATER_REGI %in% wrs2)
          area2 = sum(ws.df[which(wrs.inds2 | (ws.df$Within_Levee == 1 | ws.df[,buf.ws.col] == 0)),"Area_Ha"])
          area.df[n,"delta_area"] = area1 - area2 
        } else {
          area.df[n,"delta_area"] = 0
        }
        
        # calculate cumulative non-WOTUS area by reason for loss of jurisdiction
        area.df[n,"leveed_only"] = sum(ws.df[which(!wrs.inds1 & (ws.df$Within_Levee == 1 & ws.df[,buf.ws.col] == 1)),"Area_Ha"])
        area.df[n,"non_intersect_only"] = sum(ws.df[which(!wrs.inds1 & (ws.df$Within_Levee == 0 & ws.df[,buf.ws.col] == 0)),"Area_Ha"])
        area.df[n,"leveed_and_non_intersect"] = sum(ws.df[which(!wrs.inds1 & (ws.df$Within_Levee == 1 & ws.df[,buf.ws.col] == 0)),"Area_Ha"])
        area.df[n,"below_flood_only"] = sum(ws.df[which(wrs.inds1 & (ws.df$Within_Levee == 0 & ws.df[,buf.ws.col] == 1)),"Area_Ha"])
        area.df[n,"leveed_and_below_flood"] = sum(ws.df[which(wrs.inds1 & (ws.df$Within_Levee == 1 & ws.df[,buf.ws.col] == 1)),"Area_Ha"])
        area.df[n,"non_intersect_and_below_flood"] = sum(ws.df[which(wrs.inds1 & (ws.df$Within_Levee == 0 & ws.df[,buf.ws.col] == 0)),"Area_Ha"])
        area.df[n,"leveed_non_intersect_below_flood"] = sum(ws.df[which(wrs.inds1 & (ws.df$Within_Levee == 1 & ws.df[,buf.ws.col] == 0)),"Area_Ha"])
        
        # calculate cumulative non-WOTUS wetland area for each wetland type
        pond.df = ws.df[which(ws.df$WETLAND_TYPE == "Freshwater Pond"),]
        emerg.df = ws.df[which(ws.df$WETLAND_TYPE == "Freshwater Emergent Wetland"),]
        forest.df = ws.df[which(ws.df$WETLAND_TYPE == "Freshwater Forested/Shrub Wetland"),]
        wrs.inds.p = !(pond.df$WATER_REGI %in% wrs1)
        wrs.inds.e = !(emerg.df$WATER_REGI %in% wrs1)
        wrs.inds.f = !(forest.df$WATER_REGI %in% wrs1)
        area.df[n,"pond"] = sum(pond.df[which(wrs.inds.p | (pond.df$Within_Levee == 1 | pond.df[,buf.ws.col] == 0)),"Area_Ha"])
        area.df[n,"emergent"] = sum(emerg.df[which(wrs.inds.e | (emerg.df$Within_Levee == 1 | emerg.df[,buf.ws.col] == 0)),"Area_Ha"])
        area.df[n,"forest"] = sum(forest.df[which(wrs.inds.f | (forest.df$Within_Levee == 1 | forest.df[,buf.ws.col] == 0)),"Area_Ha"])
        
        # increment counter
        n = n + 1
      }
    }
  }
}

# calculate statistics by dataset
area.stats.df = area.df %>%
                group_by(dataset, water_cutoff) %>% 
                summarize(mean = mean(area),
                          min = min(area),
                          max = max(area))

# update water label
area.stats.df$water_label = rep(0, nrow(area.stats.df))
for (i in 1:n.w) { area.stats.df$water_label[which(area.stats.df$water_cutoff == water.regimes[i])] = water.reg.labels[i] }

# plot area by dataset and water cutoff
ggplot(area.stats.df, aes(x=mean, 
                          y=factor(water_label, levels=water.reg.labels), 
                          group=factor(dataset, levels=datasets), 
                          color=factor(dataset, levels=datasets),
                          linetype=factor(dataset, levels=datasets))) +
        geom_line(linewidth=0.8) +
        geom_ribbon(data = area.stats.df,
                    aes(xmin=min, xmax=max, 
                        group=factor(dataset, levels=datasets), 
                        color=factor(dataset, levels=datasets),
                        linetype=factor(dataset, levels=datasets)), 
                    alpha=0.1, linewidth=0.9) +
        labs(y="Wetland Flood Frequency Cutoff",
             x="Non-WOTUS Wetland Area (ha)",
             color="Dataset",
             linetype="Dataset",
             group="Dataset") +
        scale_x_continuous(limits=c(0,450000), labels=scales::comma) +
        scale_color_manual(values=c("Original NWI 1980-1987"="black",
                                    "Ducks Unlimited 2005"="green4")) +
        scale_linetype_manual(values = c("Original NWI 1980-1987"="solid",
                                         "Ducks Unlimited 2005"="dashed")) +
        theme(text = element_text(size=15),
              legend.key.size = unit(0.8,'cm'))

# make dataframes for mean, min, and max areas for each group of reaons for loss of jurisdiction
area.mean.simple = area.df %>%
                   group_by(dataset, water_cutoff) %>% 
                   summarize(`Behind levee` = mean(leveed_only + leveed_and_below_flood),
                  `Non-intersecting` = mean(non_intersect_only + non_intersect_and_below_flood),
                  `Behind levee & non-intersecting` = mean(leveed_and_non_intersect + leveed_non_intersect_below_flood),
                  `Below flood-frequency cutoff` = mean(below_flood_only))
area.min.simple = area.df %>%
                  group_by(dataset, water_cutoff) %>% 
                  summarize(`Behind levee` = min(leveed_only + leveed_and_below_flood),
                            `Non-intersecting` = min(non_intersect_only + non_intersect_and_below_flood),
                            `Behind levee & non-intersecting` = min(leveed_and_non_intersect + leveed_non_intersect_below_flood),
                            `Below flood-frequency cutoff` = min(below_flood_only))
area.max.simple = area.df %>%
                  group_by(dataset, water_cutoff) %>% 
                  summarize(`Behind levee` = max(leveed_only + leveed_and_below_flood),
                            `Non-intersecting` = max(non_intersect_only + non_intersect_and_below_flood),
                            `Behind levee & non-intersecting` = max(leveed_and_non_intersect + leveed_non_intersect_below_flood),
                            `Below flood-frequency cutoff` = max(below_flood_only))
area.mean.simple$stat = "Mean"
area.min.simple$stat = "Minimum"
area.max.simple$stat = "Maximum"

# melt mean, min, and max dataframes
area.mean.melt = melt(area.mean.simple, 
                      id.variables=c("dataset","water_cutoff"), 
                      value.name=c("Mean"),
                      variable.name=c("Reason for Lack\nof Federal Jurisdiction"))
area.min.melt = melt(area.min.simple, 
                     id.variables=c("dataset","water_cutoff"), 
                     value.name=c("Minimum"),
                     variable.name=c("Reason for Lack\nof Federal Jurisdiction"))
area.max.melt = melt(area.max.simple, 
                     id.variables=c("dataset","water_cutoff"), 
                     value.name=c("Maximum"),
                     variable.name=c("Reason for Lack\nof Federal Jurisdiction"))

# join mean, min, and max dataframes
area.comb.melt = right_join(area.mean.melt, area.min.melt, by=c("dataset","water_cutoff","Reason for Lack\nof Federal Jurisdiction"))
area.comb.melt = right_join(area.comb.melt, area.max.melt, by=c("dataset","water_cutoff","Reason for Lack\nof Federal Jurisdiction"))
area.melt = melt(area.comb.melt[,c("dataset","water_cutoff","Reason for Lack\nof Federal Jurisdiction","Mean","Minimum","Maximum")],
                 variable.name="stat",value.name="area")

# plot area by dataset, reason, and water cutoff
area.comb.melt$water_label = rep(0, nrow(area.comb.melt))
for (i in 1:n.w) { area.comb.melt$water_label[which(area.comb.melt$water_cutoff == water.regimes[i])] = water.reg.labels[i] }
reason.order = c("Below flood-frequency cutoff","Non-intersecting","Behind levee","Behind levee & non-intersecting")
ggplot(area.comb.melt, aes(x = Mean, 
                           y = factor(water_label, levels=water.reg.labels),
                           group=factor(dataset, levels=datasets), 
                           color=factor(dataset, levels=datasets),
                           linetype=factor(dataset, levels=datasets))) + 
       geom_line(linewidth=0.8) +
       geom_ribbon(data = area.comb.melt,
                   aes(xmin=Minimum, xmax=Maximum, 
                       group=factor(dataset, levels=datasets), 
                       color=factor(dataset, levels=datasets),
                       linetype=factor(dataset, levels=datasets)), 
                   alpha=0.1, linewidth=0.9) + 
       labs(y="Wetland Flood Frequency Cutoff",
            x="Mean Non-WOTUS Wetland Area (ha)",
            fill="Reason for Lack\nof Federal Jurisdiction") +
       scale_x_continuous(labels=scales::comma) +
       theme(text = element_text(size=15),
             legend.key.size = unit(0.8,'cm'),
             axis.text.y=element_blank()) +
       facet_wrap(.~`Reason for Lack\nof Federal Jurisdiction`,
                  scales = "free")

# calculate statistics by type
type.melt = melt(area.df[,c("dataset","water_cutoff","perm_level","buf_dist","pond","emergent","forest")],
                 id.variables=c("dataset","water_cutoff","perm_level","buf_dist"),
                 value.name="area",
                 variable.name="type")
type.stats.df = type.melt %>%
                group_by(dataset, water_cutoff, type) %>% 
                summarize(mean = mean(area),
                          min = min(area),
                          max = max(area))
type.stats.df$water_label = rep(0, nrow(type.stats.df))
for (i in 1:n.w) { type.stats.df$water_label[which(type.stats.df$water_cutoff == water.regimes[i])] = water.reg.labels[i] }
ggplot(type.stats.df, aes(x = mean, 
                           y = factor(water_label, levels=water.reg.labels),
                           group=factor(dataset, levels=datasets), 
                           color=factor(dataset, levels=datasets),
                           linetype=factor(dataset, levels=datasets))) + 
       geom_line(linewidth=0.8) +
       geom_ribbon(data = type.stats.df,
                   aes(xmin=min, xmax=max, 
                       group=factor(dataset, levels=datasets), 
                       color=factor(dataset, levels=datasets),
                       linetype=factor(dataset, levels=datasets)), 
                   alpha=0.1, linewidth=0.9) + 
       labs(y="Wetland Flood Frequency Cutoff",
            x="Mean Non-WOTUS Wetland Area (ha)",
            fill="Reason for Lack\nof Federal Jurisdiction") +
       scale_x_continuous(labels=scales::comma) +
       theme(text = element_text(size=15),
             legend.key.size = unit(0.8,'cm'),
             axis.text.y=element_blank()) +
       facet_wrap(.~type,scales = "free")

################################################################################
# step 2: compare protection levels between original NWI and DU

## read in original NWI and DU table for wetlands above 0.10 ac (396,572 ha)
nwi.df2 = read.csv("IL_WS_Step12_GAP_Union_CntyIntersect.csv")
du.df2 = read.csv("Step12_DU_IL_GAP_Union_CntyIntersect.csv")

# update du column name
colnames(du.df2)[which(colnames(du.df2) == "WETLAND_TY")] = "WETLAND_TYPE"

# compare total areas 
sum(nwi.df$Area_Ha); sum(nwi.df2$Area_Ha)
sum(du.df$Area_Ha); sum(du.df2$Area_Ha)

## specify counties with stormwater ordinances that protect wetlands
pro.cnties = c("Cook","DeKalb","DuPage","Grundy","Kane","McHenry","Lake","Will")
n.cp = length(pro.cnties)

## create column that combines GAP and county information
nwi.df2$Protected_Status = rep("Unprotected", nrow(nwi.df2))
du.df2$Protected_Status = rep("Unprotected", nrow(du.df2))
for (i in seq(1,2)) {nwi.df2$Protected_Status[which(nwi.df2$GAP_Sts == i)] = "Managed for biodiversity"}
for (i in seq(1,2)) {du.df2$Protected_Status[which(du.df2$GAP_Sts == i)] = "Managed for biodiversity"}
nwi.df2$Protected_Status[which((nwi.df2$NAME %in% pro.cnties) & !(nwi.df2$GAP_Sts %in% c(1,2)))] = "County stormwater ordinance"
du.df2$Protected_Status[which((du.df2$NAME %in% pro.cnties) & !(du.df2$GAP_Sts %in% c(1,2)))] = "County stormwater ordinance"
nwi.df2$Protected_Status[which(!(nwi.df2$NAME %in% pro.cnties) & (nwi.df2$GAP_Sts == 3))] = "Managed for multiple uses"
du.df2$Protected_Status[which(!(du.df2$NAME %in% pro.cnties) & (du.df2$GAP_Sts == 3))] = "Managed for multiple uses"
nwi.df2$Protected_Status[which(!(nwi.df2$NAME %in% pro.cnties) & (nwi.df2$GAP_Sts == 4))] = "Unprotected"
du.df2$Protected_Status[which(!(du.df2$NAME %in% pro.cnties) & (du.df2$GAP_Sts == 4))] = "Unprotected"

# create vectors for protection level categories
pro.cats = sort(unique(nwi.df2$Protected_Status))
n.c = length(pro.cats)
pro.order = c("Unprotected","Managed for multiple uses","County stormwater ordinance","Managed for biodiversity")

## sum non-WOTUS area in each gap category
gap.area.df = data.frame(matrix(nrow=2*n.c*n.w*n.p*n.b, ncol=6))
colnames(gap.area.df) = c("dataset","gap","water_cutoff","perm_level","buf_dist","area")
n = 1
for (d in 1:2) {
  if (d == 1) {
    gap.df = nwi.df2
  } else {
    gap.df = du.df2
  }
  for (g in 1:n.c) {
    gap.df.sub = gap.df[which(gap.df$Protected_Status == pro.cats[g]),]
    for (j in 1:n.w) {
      for (k in 1:n.p) {
        for (b in 1:n.b) {
          gap.area.df[n,"dataset"] = datasets[d]
          gap.area.df[n,"gap"] = pro.cats[g]
          gap.area.df[n,"water_cutoff"] = water.regimes[j]
          gap.area.df[n,"perm_level"] = perm.levels[k]
          gap.area.df[n,"buf_dist"] = buf.dists[b]
          buf.ws.col = paste("Waters_Intersect", perm.abrvs[k], buf.dists[b], sep="_")
          wrs = water.regimes[1:j]
          wrs.inds = !(gap.df.sub$WATER_REGI %in% wrs)
          gap.area.df[n,"area"] = sum(gap.df.sub[which(wrs.inds | (gap.df.sub$Within_Levee == 1 | gap.df.sub[,buf.ws.col] == 0)),"Area_Ha"])
          n = n + 1
        }
      }
    }
  }
}

# calculate area statistics for each dataset, gap category, and water cutoff
gap.stats.df = gap.area.df %>%
               group_by(dataset, gap, water_cutoff) %>% 
               summarize(mean = mean(area),
                         min = min(area),
                        max = max(area))

# compare area statistics between datasets
gap.stats.df$water_label = rep(0, nrow(gap.stats.df))
for (i in 1:n.w) { gap.stats.df$water_label[which(gap.stats.df$water_cutoff == water.regimes[i])] = water.reg.labels[i] }
ggplot(gap.stats.df, aes(x=mean, 
                         y = factor(water_label, levels=water.reg.labels),
                         group=factor(dataset, levels=datasets), 
                         color=factor(dataset, levels=datasets),
                         linetype=factor(dataset, levels=datasets))) + 
       geom_line(linewidth=0.8) +
       facet_wrap(.~gap, scales="free_x") +
       labs(x="Area (ha)",y="",color="Dataset",
            group="Dataset",linetype="Dataset")

## estimate unprotected non-WOTUS area in each type by water regime

# remove singular empty row
nwi.df2 = nwi.df2[-which(nwi.df2$WETLAND_TYPE == ""),]

# character vector for types
wetland.types = sort(unique(nwi.df2$WETLAND_TYPE))
n.t = length(wetland.types)

# add unprotected column
nwi.df2$Not_Protected = 1*(nwi.df2$Protected_Status == "Unprotected")
du.df2$Not_Protected = 1*(du.df2$Protected_Status == "Unprotected")

# sum areas
type.area.df = data.frame(matrix(nrow=2*n.t*n.w*n.p*n.b, ncol=6))
colnames(type.area.df) = c("dataset","type","water_cutoff","perm_level","buf_dist","area")
n = 1
for (d in 1:2) {
  if (d == 1) {
    gap.df = nwi.df2
  } else {
    gap.df = du.df2
  }
  for (i in 1:n.t) {
    df.sub = gap.df[which(gap.df$WETLAND_TYPE == wetland.types[i]),]
    for (j in 1:n.w) {
      for (k in 1:n.p) {
        for (b in 1:n.b) {
          type.area.df[n,"dataset"] = datasets[d]
          type.area.df[n,"type"] = wetland.types[i]
          type.area.df[n,"water_cutoff"] = water.regimes[j]
          type.area.df[n,"perm_level"] = perm.levels[k]
          type.area.df[n,"buf_dist"] = buf.dists[b]
          buf.ws.col = paste("Waters_Intersect", perm.abrvs[k], buf.dists[b], sep="_")
          wrs = water.regimes[1:j]
          wrs.inds = !(df.sub$WATER_REGI %in% wrs)
          non.jurisdictional.inds = 1*(wrs.inds | (df.sub$Within_Levee == 1 | df.sub[,buf.ws.col] == 0))
          non.protected.inds = df.sub$Not_Protected
          type.area.df[n,"area"] = sum(df.sub[which(non.jurisdictional.inds == 1 & non.protected.inds == 1),"Area_Ha"])
          n = n + 1
        }
      }
    }
  }
}

# summary statistics by wetland type
type.area.sum = type.area.df %>%
                group_by(dataset, type, water_cutoff) %>% 
                summarize(mean = mean(area),
                          min = min(area),
                          max = max(area))

# plot area by type
type.area.sum$water_label = rep(0, nrow(type.area.sum))
for (i in 1:n.w) { type.area.sum$water_label[which(type.area.sum$water_cutoff == water.regimes[i])] = water.reg.labels[i] }
ggplot(type.area.sum, aes(x=mean, 
                         y = factor(water_label, levels=water.reg.labels),
                         group=factor(dataset, levels=datasets), 
                         color=factor(dataset, levels=datasets),
                         linetype=factor(dataset, levels=datasets))) + 
      geom_line(linewidth=0.8) +
      facet_wrap(.~type, scales="free_x") +
      labs(x="Area (ha)",y="",color="Dataset",
           group="Dataset",linetype="Dataset")
