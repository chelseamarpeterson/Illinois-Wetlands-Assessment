path_to_datafiles = "C:/Users/Chels/OneDrive - University of Illinois - Urbana/Illinois Wetlands Risk Assessment/Results/NWI_Data"

library(dplyr)

# conversions
AcPerHa = 2.47105

################################################################################

# read in CJEST csv
setwd(path_to_datafiles)
ws.df = read.csv("IL_WS_Step15_CJEST_Intersect.csv")

# check area sums
sum(ws.df$Wetland_Area_Ha)
sum(ws.df$Wetland_Area_Acres/AcPerHa)

# counties with protection
pro.cnties = paste(c("Cook","DeKalb","DuPage","Grundy","Kane","McHenry","Lake","Will"), "County", sep=" ")
ws.df = ws.df[-which(ws.df$CF %in% pro.cnties),]

# water regimes
water.regimes = c("Permanently Flooded","Intermittently Exposed","Semipermanently Flooded",
                    "Seasonally Flooded/Saturated","Seasonally Flooded","Seasonally Saturated",
                    "Temporary Flooded","Intermittently Flooded")
n.w = length(water.regimes)

# buffer distance and flow permanence scenarios
buf.dists = c(1,10,20,100)
n.b = length(buf.dists)
perm.levels = seq(1,3)
n.p = length(perm.levels)

# all unique cjest polygons
cjest.polys = unique(ws.df$GEOID10)
cjest.ids = unique(ws.df$CJEST_ID)
n.cj = length(cjest.polys)

# check count agreement
k = 0
for (i in 1:n.cj) { k = k + 1*(ws.df$CJEST_ID[which(ws.df$GEOID10 == cjest.polys[i])[1]] == cjest.ids[i]) }
k == n.cj

# sum unprotected non-jurisdictional area by cjest polygon
area.df = data.frame(matrix(nrow=n.cj*n.w*n.p*n.b, ncol=6))
colnames(area.df) = c("GEOID10","CJEST_ID","water_cutoff","perm_level","buf_dist","area")
n = 1
for (i in 1:n.cj) {
  df.sub = ws.df[which(ws.df$CJEST_ID == cjest.ids[i]),]
  for (j in 1:n.w) {
    for (k in 1:n.p) {
      for (b in 1:n.b) {
        area.df[n,"GEOID10"] = cjest.polys[i]
        area.df[n,"CJEST_ID"] = cjest.ids[i]
        area.df[n,"water_cutoff"] = water.regimes[j]
        area.df[n,"perm_level"] = perm.levels[k]
        area.df[n,"buf_dist"] = buf.dists[b]
        buf.ws.col = paste("Waters_Intersect", perm.levels[k], buf.dists[b], sep="_")
        wrs = water.regimes[1:j]
        wrs.inds = !(df.sub$WATER_REGI %in% wrs)
        non.jurisdictional.inds = (wrs.inds | (df.sub$Within_Levee == 1 | df.sub[,buf.ws.col] == 0))
        area.df[n,"area"] = sum(df.sub[which(non.jurisdictional.inds == 1),"Wetland_Area_Ha"])
        n = n + 1
      }
    }
  }
}

# calculate mean, min, and max areas across scenarios for each county and water regime
area.stats.sum = area.df %>% 
                 group_by(GEOID10, CJEST_ID, water_cutoff) %>%
                 summarize(mean_area = mean(area),
                           min_area = min(area),
                           max_area = max(area))

# write summary file for all wetland flood-frequency cutoffs
apply(area.stats.sum, 2, function(x) sum(is.na(x)))
area.stats.sum$CJEST_ID = as.integer(area.stats.sum$CJEST_ID)
write.csv(area.stats.sum,"IL_WS_Step16_CJEST_Unprotected_Area.csv", row.names=F)

## write summary file for seasonally flooded cutoff only
sf.stats.sum = area.stats.sum[which(area.stats.sum$water_cutoff == "Seasonally Flooded"),]
write.csv(sf.stats.sum,"IL_WS_Step16_CJEST_UnproArea_SF.csv", row.names=F)

## compute 90th quantile for Figure 5
q.df = read.csv("IL_WS_Step16_CJEST_UnproJoin_SF_NormArea.csv", header=T)
quantile(q.df$norm_area, prob=c(0.9))
