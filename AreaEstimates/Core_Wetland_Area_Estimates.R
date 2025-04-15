path_to_datafiles = "C:/Users/Chels/OneDrive - University of Illinois - Urbana/Illinois Wetlands Risk Assessment/Results"
path_to_gitrepo = "C:/Users/Chels/OneDrive - University of Illinois - Urbana/Illinois Wetlands Risk Assessment/Public-Repo"

library(ggplot2)
library(reshape2)
library(patchwork)
library(dplyr)
library(remotes)
library(ggpattern)
library(gridExtra)

# conversions
AcPerHa = 2.47105

################################################################################
# Figure 1 & Table 1: calculate and plot area of wetlands that could be 
# non-jurisdictional based on levee, wetland flood frequency, and flow permanence 
# criteria

setwd(path_to_datafiles)

## step 1: estimate total area of wetlands in the state (397,186 ha)
base.df = read.csv("IL_WS_Step10_WaterRegime_Table.csv")
total.state.area.ha = sum(base.df$Area_Ha)

# check for area agreement
total.state.area.ha
sum(base.df$Area_Acres)/AcPerHa

## step 2: read in table for wetlands above 0.10 ac (396,572 ha)
ws.df = read.csv("IL_WS_Step11_AreaThreshold_Table.csv")

# check for area agreement
sum(ws.df$Area_Ha)
sum(ws.df$Area_Acres/AcPerHa)

## step 3: make character vectors for policy scenarios 

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

# step 4: create data frame with to estimate non-WOTUS areas in various categories, 
# e.g. total, by reason for loss of jurisdiction, and by wetland type
area.df = data.frame(matrix(nrow=n.w*n.p*n.b, ncol=15))
colnames(area.df) = c("water_cutoff","perm_level","buf_dist","area","delta_area",
                      "leveed_only","non_intersect_only","below_flood_only",
                      "leveed_and_non_intersect","leveed_and_below_flood",
                      "non_intersect_and_below_flood","leveed_non_intersect_below_flood",
                      "pond","emergent","forest")
n = 1
for (i in 1:n.w) {
  for (j in 1:n.p) {
    for (k in 1:n.b) {
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

# check area equality across non-WOTUS criteria
round(area.df$area - area.df$leveed_only - area.df$non_intersect_only - area.df$below_flood_only - area.df$leveed_and_non_intersect - area.df$leveed_and_below_flood - area.df$non_intersect_and_below_flood - area.df$leveed_non_intersect_below_flood,1)

## step 5: estimate max contribution of stream permanence and buffer distances to area uncertainty

# buffer uncertainty across flow permanence levels
area.buf.stats.df = area.df %>%
                    group_by(water_cutoff, perm_level) %>% 
                    summarize(mean = mean(area),
                              min = min(area),
                              max = max(area))
area.buf.stats.df$range = area.buf.stats.df$max - area.buf.stats.df$min
round(max(area.buf.stats.df$range))

# flow uncertainty across buffer distance levels
area.perm.stats.df = area.df %>%
                     group_by(water_cutoff, buf_dist) %>% 
                     summarize(mean = mean(area),
                               min = min(area),
                               max = max(area))
area.perm.stats.df$range = area.perm.stats.df$max - area.perm.stats.df$min
round(max(area.perm.stats.df$range))

## step 6: calculate statistics for each wetland type
area.type.df = area.df[,c("water_cutoff","buf_dist","perm_level","pond","emergent","forest")]
area.type.melt = melt(area.type.df, 
                      id.vars=c("water_cutoff","buf_dist","perm_level"),
                      variable.name="type",value.name="area")
type.stats.df = area.type.melt %>%
                group_by(water_cutoff, type) %>% 
                summarize(mean = mean(area),
                          min = min(area),
                          max = max(area))

## step 7: calculate statistics for each water regime (Table 1)

# estimate cumulative area at each wetland flood-frequency cutoff
area.stats.df = area.df %>%
                group_by(water_cutoff) %>% 
                summarize(mean = mean(area),
                          min = min(area),
                          max = max(area))
area.stats.df$study = "This study"

# estimate cumulative area as percentage of total state area 
percent.stats.df = area.stats.df
percent.stats.df[,c("min","mean","max")] = area.stats.df[,c("min","mean","max")]/total.state.area.ha*100

# estimate incremental area at each wetland flood-frequency cutoff
area.del.stats.df = area.df %>%
                    group_by(water_cutoff) %>% 
                    summarize(mean = mean(delta_area),
                              min = min(delta_area),
                              max = max(delta_area))

# estimate incremental area as percentage of total state area
perc.del.stats.df = area.del.stats.df
perc.del.stats.df[,c("min","mean","max")] = area.del.stats.df[,c("min","mean","max")]/total.state.area.ha*100

# print neatly for tables in order of increasing wetland flood frequency
area.mean.sort = sort(area.stats.df$mean, index.return=T)
area.stats.df[area.mean.sort$ix,]
percent.stats.df[area.mean.sort$ix,]

area.del.stats.df[area.mean.sort$ix,]
round(data.frame(percent.stats.df[area.mean.sort$ix,c("mean","min","max")]),2)
round(data.frame(perc.del.stats.df[area.mean.sort$ix,c("mean","min","max")]),8)

## step 8: compare results to previous studies 

setwd(path_to_gitrepo)

# read in levin (2002) and Simmmons et al. (2024) estimates
ls.df = read.csv("AreaEstimates/PreviousStudies/Levins_Simmons_Lane_Estimates_Acres.csv", sep=",")
ls.df.area = ls.df[which(ls.df$type == "area"),-which(colnames(ls.df) == "type")]

# Levin (2002) estimate
levin.est.df = ls.df.area[which(ls.df.area$study == "Levin (2002)"),]
levin.est.df[,c("mean","min","max")] = levin.est.df[,c("mean","min","max")]/AcPerHa
for (i in 1:n.w) {
  levin.est.df$water_cutoff = rev(water.regimes)[i]
  area.stats.df = rbind(area.stats.df, levin.est.df[,colnames(area.stats.df)])
}

# Simmons et al. (2024) dataframes
sim.est.df = ls.df.area[which(ls.df.area$study == "Simmons et al. (2024)"),]
sim.est.df[,c("mean","min","max")] = sim.est.df[,c("mean","min","max")]/AcPerHa
for (i in 1:n.w) {
  sim.est.df$water_cutoff = rev(water.regimes)[i]
  area.stats.df = rbind(area.stats.df, sim.est.df[,colnames(area.stats.df)])
}

# Gold (2024) estimates
gold.est.df = read.csv("AreaEstimates/PreviousStudies/Gold_Estimates_Acres.csv", sep=",")
gold.est.df = gold.est.df[which(gold.est.df$type == "area"),-which(colnames(gold.est.df) == "type")]
gold.est.df[,c("mean","min","max")] = gold.est.df[,c("mean","min","max")]/AcPerHa
gold.est.df$study = "Gold (2024)"
area.stats.df = rbind(area.stats.df, gold.est.df[,colnames(area.stats.df)])

# step 9: calculate uncertainty ranges
area.stats.df$range = area.stats.df$max - area.stats.df$min
percent.stats.df$range = percent.stats.df$max - percent.stats.df$min

## step 10: plot non-WOTUS area and reason for lack of jurisdiction side by side (Figure 1)

# make dataframes for mean, min, and max areas for each group of reaons for loss of jurisdiction
area.mean.simple = area.df %>%
                    group_by(water_cutoff) %>% 
                    summarize(`Behind levee` = mean(leveed_only + leveed_and_below_flood),
                              `Non-intersecting` = mean(non_intersect_only + non_intersect_and_below_flood),
                              `Behind levee & non-intersecting` = mean(leveed_and_non_intersect + leveed_non_intersect_below_flood),
                              `Below flood-frequency cutoff` = mean(below_flood_only))
area.min.simple = area.df %>%
                   group_by(water_cutoff) %>% 
                   summarize(`Behind levee` = min(leveed_only + leveed_and_below_flood),
                             `Non-intersecting` = min(non_intersect_only + non_intersect_and_below_flood),
                             `Behind levee & non-intersecting` = min(leveed_and_non_intersect + leveed_non_intersect_below_flood),
                             `Below flood-frequency cutoff` = min(below_flood_only))
area.max.simple = area.df %>%
                  group_by(water_cutoff) %>% 
                  summarize(`Behind levee` = max(leveed_only + leveed_and_below_flood),
                            `Non-intersecting` = max(non_intersect_only + non_intersect_and_below_flood),
                            `Behind levee & non-intersecting` = max(leveed_and_non_intersect + leveed_non_intersect_below_flood),
                            `Below flood-frequency cutoff` = max(below_flood_only))
area.mean.simple$stat = "Mean"
area.min.simple$stat = "Minimum"
area.max.simple$stat = "Maximum"

# melt mean, min, and max dataframes
area.mean.melt = melt(area.mean.simple, 
                      id.variables=c("water_cutoff"), 
                      value.name=c("Mean"),
                      variable.name=c("Reason for Lack\nof Federal Jurisdiction"))
area.min.melt = melt(area.min.simple, 
                     id.variables=c("water_cutoff"), 
                     value.name=c("Minimum"),
                     variable.name=c("Reason for Lack\nof Federal Jurisdiction"))
area.max.melt = melt(area.max.simple, 
                     id.variables=c("water_cutoff"), 
                     value.name=c("Maximum"),
                     variable.name=c("Reason for Lack\nof Federal Jurisdiction"))

# join mean, min, and max dataframes
area.comb.melt = right_join(area.mean.melt, area.min.melt, by=c("water_cutoff","Reason for Lack\nof Federal Jurisdiction"))
area.comb.melt = right_join(area.comb.melt, area.max.melt, by=c("water_cutoff","Reason for Lack\nof Federal Jurisdiction"))
area.melt = melt(area.comb.melt[,c("water_cutoff","Reason for Lack\nof Federal Jurisdiction","Mean","Minimum","Maximum")],
                 variable.name="stat",value.name="area")

# print areas by reason
for (i in 1:4) {
  print(area.comb.melt[which(area.comb.melt$Reason == reason.order[i]),c("Mean","Minimum","Maximum")][area.mean.sort$ix,])
}

# dataframe for state total (did not use)
state.df = data.frame("Total State Area" = total.state.area.ha)
colnames(state.df) = "Total State Area"
area.stats.df$water_label = rep(0, nrow(area.stats.df))
area.comb.melt$water_label = rep(0, nrow(area.comb.melt))
for (i in 1:n.w) { area.stats.df$water_label[which(area.stats.df$water_cutoff == water.regimes[i])] = water.reg.labels[i] }
for (i in 1:n.w) { area.comb.melt$water_label[which(area.comb.melt$water_cutoff == water.regimes[i])] = water.reg.labels[i] }

# Figure 1
studies = sort(unique(area.stats.df$study))
reason.order = c("Below flood-frequency cutoff","Non-intersecting","Behind levee","Behind levee & non-intersecting")
p1 = ggplot(area.stats.df, aes(x=mean, 
                               y=factor(water_label, levels=water.reg.labels), 
                               group=factor(study, levels=studies), 
                               color=factor(study, levels=studies),
                               linetype=factor(study, levels=studies))) +
            geom_line(linewidth=0.8) +
            geom_ribbon(data = area.stats.df,
                        aes(xmin=min, xmax=max, 
                            group=factor(study, levels=studies), 
                            color=factor(study, levels=studies),
                            linetype=factor(study, levels=studies)), 
                        alpha=0.1, linewidth=0.9) +
            labs(y="Wetland Flood Frequency Cutoff",
                 x="Non-WOTUS Wetland Area (ha)",
                 color="Source of Range Estimate",
                 linetype="Source of Range Estimate",
                 group="Source of Range Estimate") +
            scale_x_continuous(limits=c(0,400000), labels=scales::comma) +
            scale_color_manual(values=c("goldenrod1","green4","darkorange2","black")) +
            scale_linetype_manual(values = c("dashed","dotted","longdash","solid",
                                             rep("dashed",2),rep("dotted",2),
                                             rep("longdash",2),rep("solid",2)))+
            theme(text = element_text(size=12),
                  legend.key.size = unit(0.7,'cm'))
p2 = ggplot(area.comb.melt) +
            geom_col_pattern(aes(x=Mean, 
                  y = factor(water_label, levels=water.reg.labels),
                  fill = factor(`Reason for Lack\nof Federal Jurisdiction`, levels=reason.order),
                  pattern = factor(`Reason for Lack\nof Federal Jurisdiction`, levels=reason.order)),
                  pattern_fill="black",
                  pattern_density=0.1,
                  pattern_spacing=0.025) +
            scale_pattern_manual(values=c("stripe","circle","crosshatch","none")) +
            labs(y="",
                 x="Mean Non-WOTUS Wetland Area (ha)",
                 fill="Reason for Lack\nof Federal Jurisdiction",
                 pattern="Reason for Lack\nof Federal Jurisdiction") +
            scale_x_continuous(limits=c(0,400000), labels=scales::comma) +
            theme(text = element_text(size=12),
                  legend.key.size = unit(0.7,'cm'),
                  axis.text.y=element_blank())
p3 = p1 + p2
p3
setwd(path_to_gitrepo)
ggsave("AreaEstimates/Figure1_Jurisdictional_Plot.png", 
        plot = p3, width = 36, height = 12, units="cm")

################################################################################
# Figure 2 & Table 2: calculate and plot area of non-jurisdictional wetland in 
# each GAP category

setwd(path_to_datafiles)

## step 1: read in gap intersect table
gap.df = read.csv("IL_WS_Step12_GAP_Union_CntyIntersect_Table.csv")

# check area totals
sum(gap.df$Shape_Area/10^4); sum(ws.df$Shape_Area/10^4)
sum(gap.df$Area_Ha); sum(ws.df$Area_Ha)
sum(gap.df$Area_Acres/AcPerHa); sum(ws.df$Area_Acres/AcPerHa)

## step 2: specify counties with stormwater ordinances that protect wetlands
pro.cnties = c("Cook","DeKalb","DuPage","Grundy","Kane","McHenry","Lake","Will")
n.cp = length(pro.cnties)

## step 3: create column that combines GAP and county information
gap.df$Protected_Status = rep("No protection", nrow(gap.df))
for (i in seq(1,2)) {gap.df$Protected_Status[which(gap.df$GAP_Sts == i)] = "Managed for biodiversity"}
gap.df$Protected_Status[which((gap.df$NAME %in% pro.cnties) & !(gap.df$GAP_Sts %in% c(1,2)))] = "County stormwater ordinance"
gap.df$Protected_Status[which(!(gap.df$NAME %in% pro.cnties) & (gap.df$GAP_Sts == 3))] = "Managed for multiple uses"
gap.df$Protected_Status[which(!(gap.df$NAME %in% pro.cnties) & (gap.df$GAP_Sts == 4))] = "No protection"

# create vectors for protection level categories
pro.cats = sort(unique(gap.df$Protected_Status))
n.cats = length(pro.cats)
pro.order = c("No protection","Managed for multiple uses","County stormwater ordinance","Managed for biodiversity")

## step 4: sum non-WOTUS area in each gap category
gap.area.df = data.frame(matrix(nrow=n.cats*n.w*n.p*n.b, ncol=5))
colnames(gap.area.df) = c("gap","water_cutoff","perm_level","buf_dist","area")
n = 1
for (g in 1:n.cats) {
  gap.df.sub = gap.df[which(gap.df$Protected_Status == pro.cats[g]),]
  for (j in 1:n.w) {
    for (k in 1:n.p) {
      for (b in 1:n.b) {
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

## step 5: calculate minimum, mean, and maximum for each water regime and gap cateogry
gap.stats.df = gap.area.df %>%
               group_by(gap, water_cutoff) %>% 
               summarize(mean = mean(area),
                         min = min(area),
                         max = max(area))

## step 6: estimate area with no protection (Table 2, columns 2-4)

# absolute area
np.stats.df = gap.stats.df[which(gap.stats.df$gap == "No protection"),]
np.area.sort = sort(np.stats.df$mean, index.return=TRUE)
np.stats.df$range = np.stats.df$max - np.stats.df$min
np.stats.df[np.area.sort$ix,]

# area as percentage of state area
np.percent.df = np.stats.df
np.percent.df[,c("mean","min","max")] = np.stats.df[,c("mean","min","max")]/total.state.area.ha*100
np.percent.df$range = np.percent.df$max - np.percent.df$min
np.percent.sort = sort(np.percent.df$mean, index.return=TRUE)
round(data.frame(np.percent.df[np.percent.sort$ix,c("mean","min","max","range")]),2)

# area as percentage of Non-WOTUS area
np.percent.nonCWA = np.stats.df[np.area.sort$ix,c("mean","min","max")]/area.stats.df[np.area.sort$ix,c("mean","min","max")]*100
round(np.percent.nonCWA, 2)

# step 7: estimate differences between vulnerable and protected wetlands (Table 2, columns 5-7)
area.diff.df = area.stats.df[area.mean.sort$ix,c("mean","min","max")] - np.stats.df[np.area.sort$ix,c("mean","min","max")]
perc.diff.df = percent.stats.df[area.mean.sort$ix,c("mean","min","max")] - np.percent.df[np.area.sort$ix,c("mean","min","max")]
perc.noCWA.diff.df = area.diff.df/area.stats.df[area.mean.sort$ix,c("mean","min","max")]*100

round(area.diff.df)
round(perc.diff.df, 2)
round(perc.noCWA.diff.df, 2)

## step 8: estimate area in each protection category (Table A7)

# managed for biodiversity
bio.df = gap.stats.df[which(gap.stats.df$gap == "Managed for biodiversity"),]
bio.sort = sort(bio.df$mean, index.return=T)
bio.df[bio.sort$ix,]
round(bio.df[bio.sort$ix,c("mean","min","max")]/total.state.area.ha*100,2)

# county stormwater ordinance
cnty.df = gap.stats.df[which(gap.stats.df$gap == "County stormwater ordinance"),]
cnty.sort = sort(cnty.df$mean, index.return=T)
cnty.df[cnty.sort$ix,]
round(cnty.df[cnty.sort$ix,c("mean","min","max")]/total.state.area.ha*100,2)

# managed for multiple uses
mult.df = gap.stats.df[which(gap.stats.df$gap == "Managed for multiple uses"),]
mult.sort = sort(mult.df$mean, index.return=T)
mult.df[mult.sort$ix,]
round(mult.df[mult.sort$ix,c("mean","min","max")]/total.state.area.ha*100,2)
bio.df[bio.sort$ix,c("mean","min","max")] + cnty.df[cnty.sort$ix,c("mean","min","max")] + mult.df[mult.sort$ix,c("mean","min","max")]

## step 9: estimate area without protection based on wetland type

# remove singular empty row
gap.df = gap.df[-which(gap.df$WETLAND_TYPE == ""),]

# character vector for types
wetland.types = sort(unique(gap.df$WETLAND_TYPE))
n.t = length(wetland.types)

# add unprotected column
gap.df$Not_Protected = 1*(gap.df$Protected_Status == "No protection")

# estimate unprotected non-WOTUS area in each type by water regime
type.area.df = data.frame(matrix(nrow=n.t*n.w*n.p*n.b, ncol=5))
colnames(type.area.df) = c("type","water_cutoff","perm_level","buf_dist","area")
n = 1
for (i in 1:n.t) {
  df.sub = gap.df[which(gap.df$WETLAND_TYPE == wetland.types[i]),]
  for (j in 1:n.w) {
    for (k in 1:n.p) {
      for (b in 1:n.b) {
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

# summary statistics by wetland type
type.area.sum = type.area.df %>%
                group_by(type, water_cutoff) %>% 
                summarize(mean = mean(area),
                          min = min(area),
                          max = max(area))

## step 10: summarize area estimates for Table A8

# forested
forest.df = type.area.sum[which(type.area.sum$type == "Freshwater Forested/Shrub Wetland"),]
forest.sort = sort(forest.df$mean, index.return=T)
forest.df[forest.sort$ix,]
round(forest.df[forest.sort$ix,c("mean","min","max")]/total.state.area.ha*100,2)

# emergent
emerg.df = type.area.sum[which(type.area.sum$type == "Freshwater Emergent Wetland"),]
emerg.sort = sort(emerg.df$mean, index.return=T)
emerg.df[emerg.sort$ix,]
round(emerg.df[emerg.sort$ix,c("mean","min","max")]/total.state.area.ha*100,2)

# ponts
pond.df = type.area.sum[which(type.area.sum$type == "Freshwater Pond"),]
pond.sort = sort(pond.df$mean, index.return=T)
pond.df[pond.sort$ix,]
round(pond.df[pond.sort$ix,c("mean","min","max")]/total.state.area.ha*100,2)

## step 11: gold and simmons unprotected area estimates
setwd(path_to_gitrepo)
gold.gap.df = read.csv("AreaEstimates/PreviousStudies/Gold_IL_Gap12_Area_Percent.csv")
gold.area.df = gold.gap.df[,c("wr","mean.area","min.area","max.area")]
gold.perc.df = gold.gap.df[,c("wr","mean.percent","min.percent","max.percent")]
study.area.df = data.frame(matrix(nrow=2*8, ncol=5))
colnames(study.area.df) = c("water_cutoff","mean","min","max","study")
study.area.df[,"water_cutoff"] = rep(rev(water.regimes), 2)
study.area.df[1:8,"study"] = "Gold (2024)"
study.area.df[9:16,"study"] = "Simmons et al. (2024)"
study.area.df[1,c("mean","min","max")] = gold.area.df[1,c("mean.area","min.area","max.area")]
study.area.df[2:8,c("mean","min","max")] = gold.area.df[,c("mean.area","min.area","max.area")]
study.area.df[9:16,"mean"] = 13791.56
study.area.df[9:16,"min"] = 9393.814
study.area.df[9:16,"max"] = 32530
study.area.df$gap = "Managed for biodiversity"
study.area.df[,c("mean","min","max")] = study.area.df[,c("mean","min","max")]/AcPerHa
study.melt = melt(study.area.df)
study.minmax = study.melt[-which(study.melt$variable == "mean"),]

# step 12: plot area in each gap category and vegetation type
type.lowercase = c("Freshwater emergent wetland","Freshwater forested/shrub wetland","Freshwater pond")
type.order = c("Freshwater forested/shrub wetland","Freshwater emergent wetland","Freshwater pond")
for (i in 1:3) { type.area.sum$type[which(type.area.sum$type == wetland.types[i])] = type.lowercase[i] }
gap.stats.df$water_label = rep(0, nrow(gap.stats.df))
type.area.sum$water_label = rep(0, nrow(type.area.sum))
study.area.df$water_label = rep(0, nrow(study.area.df))
for (i in 1:n.w) { gap.stats.df$water_label[which(gap.stats.df$water_cutoff == water.regimes[i])] = water.reg.labels[i] }
for (i in 1:n.w) { type.area.sum$water_label[which(type.area.sum$water_cutoff == water.regimes[i])] = water.reg.labels[i] }
for (i in 1:n.w) { study.area.df$water_label[which(study.area.df$water_cutoff == water.regimes[i])] = water.reg.labels[i] }
p1 = ggplot(gap.stats.df) + 
            geom_col_pattern(aes(x=mean, 
                                 y=factor(water_label, levels=water.reg.labels),
                                 fill=factor(gap, levels=pro.order),
                                 pattern=factor(gap, levels=pro.order)),
                             pattern_fill="black",
                             pattern_density=0.1,
                             pattern_spacing=0.025) +
            labs(y="Wetland Flood Frequency Cutoff",
                 x="Mean Non-WOTUS Wetland Area (ha)",
                 fill="gap",pattern="gap") +
            scale_x_continuous(limits=c(0,400000), labels=scales::comma) +
            guides(shape=guide_legend(title="Previous Estimate of Area \nManaged for Biodiversity"),
                   fill=guide_legend(title="Protection Level"),
                   pattern=guide_legend(title="Protection Level")) +
            geom_line(data=study.area.df, aes(x=mean,
                                              y=factor(water_label, levels=water.reg.labels),
                                              color=study, 
                                              group=study), size=0.5) +
            geom_point(data=study.area.df, aes(x=mean,
                                               y=factor(water_label, levels=water.reg.labels),
                                               color=study, 
                                               group=study,
                                               shape=study), size=3) +
            scale_color_manual(values=c("goldenrod1","darkorange2"),
                               name="Previous Estimate of Area \nManaged for Biodiversity",
                               labels=c("Gold (2024)","Simmons et al. (2024)")) +
            scale_shape_manual(values=c(16,17),
                               name="Previous Estimate of Area \nManaged for Biodiversity",
                               labels=c("Gold (2024)","Simmons et al. (2024)")) +
            scale_fill_manual(values=c("darkgray","coral1","darkcyan","darkorchid")) +
            scale_pattern_manual(values=c("stripe","none","crosshatch","circle")) +
            theme(text = element_text(size=12),
                  legend.key.size = unit(0.7,'cm'))
p2 = ggplot(type.area.sum) + 
            geom_col_pattern(aes(x=mean, 
                                 y=factor(water_label, levels=water.reg.labels),
                                 fill=factor(type, levels=rev(type.order)),
                                 pattern=factor(type, levels=rev(type.order))),
                             pattern_fill="black",
                             pattern_density=0.05,
                             pattern_spacing=0.025,
                             pattern_angle=0) + 
            labs(y="",
                 x="Mean Unprotected\nNon-WOTUS Wetland Area (ha)") + 
            scale_x_continuous(limits=c(0,400000), labels=scales::comma) +
            theme(axis.text.y=element_blank()) +
            guides(fill=guide_legend(title="Wetland Type"),
                   pattern=guide_legend(title="Wetland Type")) +
            scale_fill_manual(values=c("dodgerblue3","darkgreen","burlywood4")) +
            scale_pattern_manual(values=c("stripe","none","crosshatch")) +
            theme(text = element_text(size=12),
                  legend.key.size = unit(0.7,'cm'))
p3 = p1 + p2
p3
ggsave("AreaEstimates/Figure2_Unprotected_Plot_Raw.png", 
       plot = p3, width = 36, height = 12, units="cm")

