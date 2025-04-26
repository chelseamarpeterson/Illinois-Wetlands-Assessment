path_to_nwi_data = "C:/Users/Chels/OneDrive - University of Illinois - Urbana/Illinois Wetlands Risk Assessment/Results/NWI_Data"
path_to_gitrepo = "C:/Users/Chels/OneDrive - University of Illinois - Urbana/Illinois Wetlands Risk Assessment/Public-Repo"

library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)

# conversion
AcPerHa = 2.47105

# read in wetland table
setwd(path_to_nwi_data)
ws.df = read.csv("IL_WS_Step12_GAP_Union_CntyIntersect_Table.csv")

# check area sums
sum(ws.df$Shape_Area / 10^4)
sum(ws.df$Area_Ha)
sum(ws.df$Area_Acres / AcPerHa)

# water regimes
water.regimes = c("Permanently Flooded","Intermittently Exposed","Semipermanently Flooded",
                    "Seasonally Flooded/Saturated","Seasonally Flooded","Seasonally Saturated",
                    "Temporary Flooded","Intermittently Flooded")
water.reg.labels = c("Permanently Flooded","Intermittently Exposed","Semipermanently Flooded",
                     "Seasonally Flooded/Saturated","Seasonally Flooded","Seasonally Saturated",
                     "Temporarily Flooded","Intermittently Flooded")
n.w = length(water.regimes)

# flow permanence and buffer distance scenarios
buf.dists = c(1,10,20,100)
n.b = length(buf.dists)
perm.levels = seq(1,3)
n.p = length(perm.levels)

# all counties
all.counties = sort(unique(ws.df$NAME))
n.c = length(all.counties)

# counties with protection
pro.cnties = c("Cook","DeKalb","DuPage","Grundy","Kane","McHenry","Lake","Will")
n.cp = length(pro.cnties)

# created new protected status column
ws.df$Protected_Status = rep("No protection", nrow(ws.df))
for (i in seq(1,2)) {ws.df$Protected_Status[which(ws.df$GAP_Sts == i)] = "Managed for biodiversity"}
ws.df$Protected_Status[which((ws.df$NAME %in% pro.cnties) & !(ws.df$GAP_Sts %in% c(1,2)))] = "County stormwater ordinance"
ws.df$Protected_Status[which(!(ws.df$NAME %in% pro.cnties) & (ws.df$GAP_Sts == 3))] = "Managed for multiple uses"
ws.df$Protected_Status[which(!(ws.df$NAME %in% pro.cnties) & (ws.df$GAP_Sts == 4))] = "No protection"

# protected status categories
pro.cats = sort(unique(ws.df$Protected_Status))
n.cats = length(pro.cats)

# create column based on protection status
ws.df$Not_Protected = 1*(ws.df$Protected_Status == "No protection")

# sum unprotected nonjurisdictional area by county and water cutoff
stats.df.order = data.frame(matrix(nrow=n.c*n.w*n.p*n.b, ncol=5))
colnames(stats.df.order) = c("county","water_cutoff","stream_perm","buf_dist","area")
n = 1
for (i in 1:n.c) {
  cnty.df.sub = ws.df[which(ws.df$NAME == all.counties[i]),]
  for (j in 1:n.w) {
    for (k in 1:n.p) {
      for (b in 1:n.b) {
        stats.df.order[n,"county"] = all.counties[i]
        stats.df.order[n,"water_cutoff"] = water.regimes[j]
        stats.df.order[n,"stream_perm"] = perm.levels[k]
        stats.df.order[n,"buf_dist"] = buf.dists[b]
        buf.ws.col = paste("Waters_Intersect", perm.levels[k], buf.dists[b], sep="_")
        wrs = water.regimes[1:j]
        wrs.inds = !(cnty.df.sub$WATER_REGI %in% wrs)
        non.jurisdictional.inds = (wrs.inds | (cnty.df.sub$Within_Levee == 1 | cnty.df.sub[,buf.ws.col] == 0))
        non.protected.inds = cnty.df.sub$Not_Protected
        stats.df.order[n,"area"] = sum(cnty.df.sub[which(non.jurisdictional.inds == 1 & non.protected.inds == 1),"Area_Ha"])
        n = n + 1
      }
    }
  }
}

# calculate mean, min, and max areas across scenarios for each county and water regime
area.stats.sum = stats.df.order %>% 
                 group_by(county, water_cutoff) %>%
                 summarize(mean = mean(area),
                           min = min(area),
                           max = max(area))

# reshape to get areas for each county in the columns
wr.abrevs = c("PF","IE","SPF","SFS","SF","SS","TF","IF")
for (i in 1:n.w) { area.stats.sum$water_cutoff[area.stats.sum$water_cutoff == water.regimes[i]] = wr.abrevs[i] }
wide.df = data.frame(matrix(nrow=n.c, ncol=0))
wide.df$county = all.counties
for (i in 1:n.w) {
  wr.rows = area.stats.sum[area.stats.sum$water_cutoff == wr.abrevs[i],]
  colnames(wr.rows)[3:5] = paste(colnames(wr.rows)[3:5],wr.abrevs[i],sep="_")
  wide.df = cbind(wide.df, wr.rows[,colnames(wr.rows)[3:5]])
}

# add column for sensitivity
wide.df$area_uncert = (wide.df$mean_PF - wide.df$mean_IF)/(wide.df$mean_SS)
wide.df$area_uncert[is.na(wide.df$area_uncert)] = 0

# write file
setwd(path_to_gitrepo)
write.csv(wide.df, "County_Statistics/IL_WS_Step13_County_Stats.csv", row.names=F)

################################################################################
# estimate without protection based on wetland type

# remove singular row without wetland type
ws.df = ws.df[-which(ws.df$WETLAND_TYPE == ""),]
wetland.types = sort(unique(ws.df$WETLAND_TYPE))
n.t = length(wetland.types)

# sum unprotected nonjurisdictional area by county and water cutoff
cnty.type.df = data.frame(matrix(nrow=n.c*n.t*n.w*n.p*n.b, ncol=6))
colnames(cnty.type.df) = c("county","wetland_type","water_cutoff","stream_perm","buf_dist","area")
n = 1
for (i in 1:n.c) {
  for (t in 1:n.t) {
    df.sub = ws.df[which(ws.df$NAME == all.counties[i] & ws.df$WETLAND_TYPE == wetland.types[t]),]
    for (j in 1:n.w) {
      for (k in 1:n.p) {
        for (b in 1:n.b) {
          cnty.type.df[n,"county"] = all.counties[i]
          cnty.type.df[n,"wetland_type"] = wetland.types[t]
          cnty.type.df[n,"water_cutoff"] = water.regimes[j]
          cnty.type.df[n,"stream_perm"] = perm.levels[k]
          cnty.type.df[n,"buf_dist"] = buf.dists[b]
          buf.ws.col = paste("Waters_Intersect", perm.levels[k], buf.dists[b], sep="_")
          wrs = water.regimes[1:j]
          wrs.inds = !(df.sub$WATER_REGI %in% wrs)
          non.jurisdictional.inds = (wrs.inds | (df.sub$Within_Levee == 1 | df.sub[,buf.ws.col] == 0))
          non.protected.inds = df.sub$Not_Protected
          cnty.type.df[n,"area"] = sum(df.sub[which(non.jurisdictional.inds == 1 & non.protected.inds == 1),"Area_Ha"])
          n = n + 1
        }
      }
    }
  }
}

# calculate mean, min, and max areas across scenarios for each county and water regime
cnty.type.sum = cnty.type.df %>% 
                group_by(county, wetland_type, water_cutoff) %>%
                summarize(mean = mean(area),
                          min = min(area),
                          max = max(area))

# calculate uncertainty and plot by wetland type
cnty.type.sf = cnty.type.sum[which(cnty.type.sum$water_cutoff == "Seasonally Flooded"),]
cnty.type.pf = cnty.type.sum[which(cnty.type.sum$water_cutoff == "Permanently Flooded"),]
cnty.type.if = cnty.type.sum[which(cnty.type.sum$water_cutoff == "Intermittently Flooded"),]
cnty.type.sf$uncertainty = (cnty.type.pf$mean - cnty.type.if$mean)/cnty.type.sf$mean
cnty.tot.type.join = right_join(cnty.type.sf, wide.df, by="county")
type.labs = c("Freshwater emergent wetland",
              "Freshwater forested/shrub wetland",
              "Freshwater pond")
cnty.type.sf$type_lab = rep(0, nrow(cnty.type.sf))
for (i in 1:3) { cnty.type.sf$type_lab[which(cnty.type.sf$wetland_type == wetland.types[i])] = type.labs[i] }
p1 = ggplot(wide.df, 
            aes(x=log(mean_SF), 
                y=log(area_uncert))) +
            geom_point(size=2) + #geom_smooth(method='lm')+
            labs(x="Log(Total UP-NW Area [ha])", 
                 y="Log(Total Area Uncertainty)") + 
            xlim(5,10) +
            theme(text = element_text(size=12))
p2 = ggplot(cnty.tot.type.join, 
            aes(x=log(mean), 
                y=log(area_uncert), 
                color=type_lab,
                shape=type_lab)) + 
            geom_point(size=2) + #geom_smooth(method='lm')+
            labs(x="", 
                 y="",
                 color="Wetland Type",shape="Wetland Type") +
            theme(text = element_text(size=12))
p3 = ggplot(cnty.tot.type.join, 
            aes(x=log(mean), 
                y=log(uncertainty), 
                color=type_lab,
                shape=type_lab)) + 
            geom_point(size=2) + #geom_smooth(method='lm')+
            labs(x="Log(Type UP-NW Area [ha])", 
                 y="Log(Type Area Uncertainty)",
                 color="Wetland Type",shape="Wetland Type") +
            theme(text = element_text(size=12)) +
            guides(color="none", shape="none")
p4 = (p1 + p2)/(plot_spacer() + p3)
p4
ggsave("County_Statistics/FigureA2_WetlandArea_Uncertainty.png", 
       plot = p3, width = 16, height = 16, units="cm")

# write file
write.csv(cnty.type.sum[,c("county","wetland_type","water_cutoff","mean","min","max")], 
          "County_Statistics/IL_WS_Step13_County_Type_Stats.csv",row.names = F)

# write file for each wetland type
type.abs = c("Emergent","Forested","Pond")
for (i in 1:n.t) {
  type.df = cnty.type.sum[cnty.type.sum$wetland_type == wetland.types[i],]
  cut.df = type.df[type.df$water_cutoff == "Seasonally Flooded",]
  colnames(cut.df)[4:6] = paste(c("mean","min","max"),type.abs[i],sep="_")
  write.csv(cut.df[,c("county",paste(c("mean","min","max"),type.abs[i],sep="_"))], 
            paste("County_Statistics/IL_WS_Step13_County_",type.abs[i],".csv",sep=""),
            row.names = F)
}

################################################################################
## plot county-level results

# histogram of mean areas across counties
ggplot(cnty.type.sum, aes(x=Mean_Area, 
                          y=factor(water_cutoff, levels=water.regimes))) + 
       geom_boxplot() +
       labs(x="Mean Wetland Area Without Protection (ac)", 
            y="Wetland Flood Frequency Threshold") +
       guides(fill=guide_legend(title="Wetland Flood Frequency Cutoff")) #+ 
       #scale_x_continuous(limits=c(0,31000), labels=scales::comma)

# line plot of mean areas by water_regime
area.stats.order = data.frame(matrix(nrow=0, ncol=5))
for (i in 1:n.w) {
  wr.stats.sum = area.stats.sum[which(area.stats.sum$water_cutoff == water.regimes[i]),]
  wr.county.sort = sort(wr.stats.sum$Mean_Area, index.return=T)
  wr.stats.order = wr.stats.sum[wr.county.sort$ix,c("county","water_cutoff","Mean_Area","Min_Area","Max_Area")]
  area.stats.order = rbind(area.stats.order, wr.stats.order)
}

area.stats.order = area.stats.order %>% separate(county, c("cnty", "x"), sep = " County")
area.stats.order$cnty = trimws(area.stats.order$cnty)
pf.stats.sum = area.stats.order[which(area.stats.order$water_cutoff == water.regimes[1]),]
pf.mean.sort = sort(pf.stats.sum$Mean_Area, index.return=T)
pf.cnty.order = pf.stats.sum$cnty[pf.mean.sort$ix]

omit.cnty = c("Lake","DuPage","McHenry","Cook","Will","Grundy","DeKalb","Kane")
ggplot(area.stats.order[-which(area.stats.order$cnty %in% omit.cnty),], 
       aes(x=Mean_Area, y = factor(cnty, levels=pf.cnty.order[8:n.c]),
                           color=factor(water_cutoff, levels=water.regimes),
                           group=factor(water_cutoff, levels=water.regimes))) + 
       geom_point() + 
       labs(x="Mean Wetland Area Without Protection (ac)", 
            y="County Name") +
       theme(axis.text.y = element_text(size = 6)) + 
       guides(fill=guide_legend(title="Wetland Flood Frequency Threshold")) + 
       scale_x_continuous(limits=c(0,31000), labels=scales::comma)


# plot histogram of difference between permanently flooded and seasonally saturated by county
cnty.diff.df = data.frame(matrix(nrow=n.c, ncol=4))
colnames(cnty.diff.df) = c("county_short","county","mean_area","area_diff")
cnty.short = sort(unique(area.stats.order$cnty))
cnty.long = sort(unique(area.stats.sum$county))
for (i in 1:n.c) {
  cnty.diff.df[i,"county_short"] = cnty.short[i]
  cnty.diff.df[i,"county"] = cnty.long[i]
  cnty.i.df = area.stats.order[which(area.stats.order$cnty == cnty.short[i]),]
  cnty.max = cnty.i.df[which(cnty.i.df$water_cutoff == "Permanently Flooded"),"Max_Area"]
  cnty.min = cnty.i.df[which(cnty.i.df$water_cutoff == "Seasonally Saturated"),"Min_Area"]
  cnty.diff.df[i,"area_diff"] = cnty.max - cnty.min
  cnty.diff.df[i,"mean_area"] = cnty.i.df[which(cnty.i.df$water_cutoff == "Semipermanently Flooded"),"Mean_Area"]
}

ggplot(cnty.diff.df, aes(y=area_diff, x=mean_area)) + geom_point()

