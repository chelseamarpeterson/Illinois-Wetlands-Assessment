path_to_nwi_data = "C:/Users/Chels/OneDrive - University of Illinois - Urbana/Illinois Wetlands Risk Assessment/Results/NWI_Data"
path_to_cejst = "C:/Users/Chels/OneDrive - University of Illinois - Urbana/Illinois Wetlands Risk Assessment/Databases/CEJST"
path_to_gitrepo = "C:/Users/Chels/OneDrive - University of Illinois - Urbana/Illinois Wetlands Risk Assessment/Public-Repo"

library(ggplot2)
library(reshape2)
library(dplyr)
library(rethinking)

################################################################################
# combine cejst data and unprotected wetland data for illinois

# read in CEJST data
setwd(path_to_cejst)
cj.df = read.csv("IL_CJEST_Data.csv")
  
# read in wetland area data 
setwd(path_to_nwi_data)
area.df = read.csv("IL_WS_Step16_CJEST_Unprotected_Area.csv")

# water regimes
water.regimes = c("Permanently Flooded","Intermittently Exposed","Semipermanently Flooded",
                    "Seasonally Flooded/Saturated","Seasonally Flooded","Seasonally Saturated",
                    "Temporary Flooded","Intermittently Flooded")
water.reg.labels = c("Permanently Flooded","Intermittently Exposed","Semipermanently Flooded",
                    "Seasonally Flooded/Saturated","Seasonally Flooded","Seasonally Saturated",
                    "Temporarily Flooded","Intermittently Flooded")
n.w = length(water.regimes)

# protected counties
pro.cnties = paste(c("Cook","DeKalb","DuPage","Grundy","Kane","McHenry","Lake","Will"),"County",sep=" ")
sum(cj.df$CF %in% pro.cnties)

# add zeros to cejst polygons without area data
cjest.ids = sort(unique(cj.df$CJEST_ID))
n.cj = length(cjest.ids)
for (i in 1:n.cj) {
  cj.id = cjest.ids[i]
  geo.id = cj.df$GEOID10[which(cj.df$CJEST_ID == cj.id)]
  if (!(cj.id %in% area.df$CJEST_ID)) {
    new.df = data.frame(matrix(nrow=n.w, ncol=6))
    colnames(new.df) = colnames(area.df)
    new.df$GEOID10 = geo.id
    new.df$CJEST_ID = cj.id
    new.df$water_cutoff = water.regimes
    new.df$mean_area = rep(0,n.w)
    new.df$min_area = rep(0,n.w)
    new.df$max_area = rep(0,n.w)
    area.df = rbind(area.df, new.df)
  }
}
length(unique(area.df$CJEST_ID))*8

# join area and cj df
colnames(cj.df)[which(colnames(cj.df) == "Area_Ha")] = "Tract_Ha"
area.cj.df = left_join(cj.df, area.df, by=c("CJEST_ID","GEOID10"))

# normalize wetland areas by census tract areas
area.cj.df$Tract_Normalized_Wetland_Area = area.cj.df$mean_area/area.cj.df$Tract_Ha

# add indicator for occurrence in a protected county
area.cj.df$cntyid = (area.cj.df$CF %in% pro.cnties)
sum(area.cj.df$cntyid)

# create new data frame for tracts outside counties with protections
area.cj.df.unprotected = area.cj.df[which(area.cj.df$cntyid==0),]

# names for groups of census tracts 
groups = c("all","unprotected")
group.labels = c("All", "Unprotected")
n.g = length(groups)

# put dataframes into lists
area.dfs = list()
area.dfs[["all"]] = area.cj.df
area.dfs[["unprotected"]] = area.cj.df.unprotected
  
# calculate global mean for each set of tracts and normalize relative areas by global mean
global.means = list()
for (i in 1:n.g) { 
  grp = groups[i]
  global.means[[grp]] = mean(area.dfs[[grp]]$Tract_Normalized_Wetland_Area)
  area.dfs[[grp]]$Mean_Normalized_Wetland_Area = area.dfs[[grp]]$Tract_Normalized_Wetland_Area/global.means[[grp]]
}

################################################################################
# CEJST Analysis 1: fit Bayesian distributions to each indicator group

# indicator names
indicators = c("f","c","a","b","p","w")
ind.abrvs = c("Flood","Climate","Ag","Building","Population","Wildfire")
n.v = length(indicators)
ind.cols = c("FLD_ET","N_CLT_EOMI","EAL_ET","EBL_ET","EPL_ET","WFR_ET")

# put normalized areas and indicators into list
area.lists = list()
for (i in 1:n.v) {
  ind = indicators[i]
  area.lists[[ind]] = list()
  for (j in 1:n.g) {
    grp = groups[j]
    df = area.dfs[[grp]]
    area.lists[[ind]][[grp]] = list(area_norm = as.vector(df$Mean_Normalized_Wetland_Area),
                                    cutoff_id = as.integer(factor(df$water_cutoff)),
                                    risk_id = as.integer(factor(df[,ind.cols[i]])))
  }
}

## fit models for normalized area
set.seed(314)
m.list = list()
for (i in 1:n.v) {
  ind = indicators[i]
  m.list[[ind]] = list() 
  for (j in 1:n.g) {
    grp = groups[j]
    area.list.ij = area.lists[[ind]][[grp]]
    m.ij = ulam(alist(area_norm ~ normal(mu, sigma),
                   log(mu) <- a[cutoff_id,risk_id],
                   matrix[cutoff_id,risk_id]: a ~ dnorm(0,1),
                   sigma ~ dexp(1)),
             data=area.list.ij, chains=5, log_lik=T)
    m.list[[ind]][[grp]] = m.ij
    
    # re-scale posterior estimates
    exp.a.ij = exp(extract.samples(m.ij)$a)*global.means[[grp]]
    
    # calculate posterior difference and melt dataframe
    diff.ij = exp.a.ij[,,2] - exp.a.ij[,,1]
    colnames(diff.ij) = levels(factor(area.dfs[[grp]]$water_cutoff))
    
    # write to file
    write.csv(diff.ij, 
              paste("CEJST_Analysis/Posteriors/",paste(ind.abrvs[i],group.labels[j],"Indicator_PostDiff.csv",sep="_"),sep=""), 
              row.names=F)
  }
}

################################################################################
# CEJST Analysis 2: evaluate if mean wetland area has relationship with FLD_PFS, 
# or share of properties at risk of flood in 30 years (percentile)

# make lists with FLD_PFS
fld.lists = list()
for (i in 1:n.g) {
  grp = groups[i]
  df = area.dfs[[grp]]
  fld.lists[[grp]] = list(cutoff_id = as.integer(factor(df$water_cutoff)),
                          area_norm = as.vector(df$Mean_Normalized_Wetland_Area), 
                          fld_pfs = as.vector(df$FLD_PFS))
}

# fit models
set.seed(271)
linear.models = list()
for (i in 1:n.g) {
  grp = groups[i]
  ml <- ulam(alist(fld_pfs ~ normal(mu, sigma),
                         log(mu) <- a[wid] + b[wid]*area_norm,
                         c(a, b)[wid] ~ multi_normal( c(a_fld,b_fld) , Rho , sigma_fld ),
                         a_fld ~ normal(0,1),
                         b_fld ~ normal(0,1),
                         sigma_fld ~ exponential(1),
                         sigma ~ exponential(1),
                         Rho ~ lkj_corr(2)), 
                   data=fld.list.all, chains=5, log_lik=TRUE)
  linear.models[[grp]]
}


ml.f.unprotected <- ulam(alist(fld_pfs ~ normal(mu, sigma),
                               log(mu) <- a[wid] + b[wid]*area_norm,
                               c(a, b)[wid] ~ multi_normal( c(a_fld,b_fld) , Rho , sigma_fld ),
                               a_fld ~ normal(0,1),
                               b_fld ~ normal(0,1),
                               sigma_fld ~ exponential(1),
                               sigma ~ exponential(1),
                               Rho ~ lkj_corr(2)) , 
             data=fld.list.unprotected, chains=5, log_lik=TRUE)

#summary(lm(log(FLD_PFS)~Mean_Normalized_Wetland_Area, 
#             data=area.cj.df.unprotected[which(area.cj.df.unprotected$wid==8),]))  


# traceplots
#traceplot(ml.f)
#traceplot(ml.fc)

# extract posterior samples and calculate 95% HPDIs
post.df = data.frame(matrix(nrow=n.w, ncol=5))
colnames(post.df) = c("group","water_cutoff","mean","2.5","97.5")
k = 1
for (i in 1:2) {
  if (i == 1) {
    m = ml.f.all
    df = area.cj.df
    group_name = "All"
  } else {
    m = ml.f.unprotected
    df = area.cj.df.unprotected
    group_name = "Unprotected counties"
  }
  samples = extract.samples(m)
  post.a = samples$a
  post.b = samples$b
  for (j in 1:n.w) {
    post.df[k,"group"] = group_name
    post.df[k,"water_cutoff"] = levels(factor(area.cj.df$water_cutoff))[j]
    post.df[k,"mean"] = mean(post.b[,j])
    post.df[k,"2.5"] = HPDI(post.b[,j], prob=0.95)[1]
    post.df[k,"97.5"] = HPDI(post.b[,j], prob=0.95)[2]
    k = k + 1
  }
}

# save posterior dataframe
setwd(path_to_gitrepo)
write.csv(post.df, "CEJST_Analysis/Posteriors/Linear_Model_EffectSize_HPDIs.csv", row.names=F)

# plot lines at Seasonally Flooded cutoff
n = 2000
samples = extract.samples(ml.f.unprotected)
post.a = samples$a
post.b = samples$b
post.line.df = data.frame(matrix(nrow=0, ncol=5))
colnames(post.line.df) = c("water_cutoff","area","mean","2.5","97.5")
for (i in 1:n.w) {
  wr = levels(factor(area.cj.df.unprotected$water_cutoff))[i]
  wr.ind = which(area.cj.df.unprotected$water_cutoff == wr)
  df.wr = data.frame(matrix(nrow=n, ncol=5))
  colnames(df.wr) = c("water_cutoff","area","mean","2.5","97.5")
  wr.area.max = max(area.cj.df.unprotected$Mean_Normalized_Wetland_Area[wr.ind])
  area.range = seq(0, wr.area.max, by=wr.area.max/(n-1))
  df.wr[1:n,"water_cutoff"] = wr
  df.wr[1:n,"area"] = area.range*Mean_Tract_Normalized_Wetland_Area*1000
  for (j in 1:n) {
    pred.ij = exp(post.a[,i] + post.b[,i]*area.range[j])
    hpdi.ij = HPDI(pred.ij, prob=0.95)
    df.wr[j,"mean"] = mean(pred.ij)
    df.wr[j,"2.5"] = hpdi.ij[1]
    df.wr[j,"97.5"] = hpdi.ij[2]
  }
  post.line.df = rbind(post.line.df, df.wr)
}
max(post.line.df[which(post.line.df$water_cutoff == "Seasonally Flooded"),"area"])

# write dataframe for posterior linear prediction
write.csv(post.line.df, "CEJST_Analysis/Posteriors/Linear_Model_Prediction.csv", row.names=F)

# write area dataframe without protected counties for plotting
write.csv(area.cj.df.unprotected, "CEJST_Analysis/Posteriors/CEJST_WetlandArea_Dataframe_NoUnproCnties.csv", row.names=F)

################################################################################
# side analysis: logistic regression

# modify data list
area.df.simple$Mean_CenterScale = (area.df.simple$Mean_DivArea - mean(area.df.simple$Mean_DivArea))/sd(area.df.simple$Mean_DivArea)
cf.list = list(area = as.vector(area.df.simple$Mean_CenterScale),
               wid = area.df.simple$wid,
               ft = area.df.simple$FLD_ET,
               ct = area.df.simple$N_CLT_EOMI)

# fit model for flood indicator
m.fp <- ulam(alist(ft ~ dbinom(1, p),
                   logit(p) <- a + b[wid]*area,
                   a ~ dnorm(0, 1),
                   b[wid] ~ dnorm(0, 1)),
             data=cf.list, chains=5, log_lik=TRUE)
m.cp <- ulam(alist(ct ~ dbinom(1, p),
                   logit(p) <- a + b[wid]*area,
                   a ~ dnorm(0, 1),
                   b[wid] ~ dnorm(0, 1)),
             data=cf.list, chains=5, log_lik=TRUE)
mf.sum = precis(m.fp, depth=2)
mc.sum = precis(m.cp, depth=2)
model.sums = list()
model.sums[["f"]] = mf.sum
model.sums[["c"]] = mc.sum

fsum.df = data.frame(b_mean = mf.sum$mean[2:9],
                     b_5 = mf.sum$`5.5%`[2:9],
                     b_95 = mf.sum$`94.5%`[2:9],
                     wr = levels(factor(area.df.simple$water_cutoff)),
                     m = model.labels[1])
csum.df = data.frame(b_mean = mc.sum$mean[2:9],
                     b_5 = mc.sum$`5.5%`[2:9],
                     b_95 = mc.sum$`94.5%`[2:9],
                     wr = levels(factor(area.df.simple$water_cutoff)),
                     m = model.labels[2])
sum.df = rbind(fsum.df, csum.df)
ggplot(sum.df, aes(x=b_mean,
                   y=factor(wr, level=water.regimes),
                   color=m)) +
       geom_point() + 
       geom_errorbarh(aes(xmin=b_5, xmax=b_95, 
                          y=factor(wr, level=water.regimes)), 
                      height=0.2) + 
       labs(x="Effect size for probability versus wetland area",
            y="Wetland Flood Frequency Cutoff") + 
       guides(color=guide_legend(title="CEJST Indicator")) +
       scale_color_manual(values = c("blue","red"))

# visualize distributions
w.cols = levels(factor(area.df.simple$water_cutoff))
n.c = nrow(area.df.simple[which(area.df.simple$water_cutoff == "Permanently Flooded"),])
dist.df = data.frame(matrix(nrow=16,ncol=7))
colnames(dist.df) = c("m","water_cutoff","mean","5","95","25","75")
k = 1
for (j in 1:2) {
  m.sum = model.sums[[models[j]]]
  for (i in 1:8) {
    dist.df[k,"m"] = model.labels[j]
    dist.df[k,"water_cutoff"] = w.cols[i]
    dist.df[k,"mean"] = mean(inv_logit(m.sum$mean[1] + m.sum$mean[i+1]*cf.list$area[which(cf.list$wid == i)]))
    dist.df[k,"5"] = HPDI(inv_logit(m.sum$mean[1] + m.sum$mean[i+1]*cf.list$area[which(cf.list$wid == i)]), prob=0.90)[1]
    dist.df[k,"95"] = HPDI(inv_logit(m.sum$mean[1] + m.sum$mean[i+1]*cf.list$area[which(cf.list$wid == i)]), prob=0.90)[2]
    dist.df[k,"25"] = HPDI(inv_logit(m.sum$mean[1] + m.sum$mean[i+1]*cf.list$area[which(cf.list$wid == i)]), prob=0.50)[1]
    dist.df[k,"75"] = HPDI(inv_logit(m.sum$mean[1] + m.sum$mean[i+1]*cf.list$area[which(cf.list$wid == i)]), prob=0.50)[2]
    k = k + 1
  }
}

ggplot(dist.df, aes(x=mean, color=m,
                    y=factor(water_cutoff, levels=water.regimes))) + 
  geom_point() + 
  geom_errorbarh(aes(xmin=`5`,xmax=`95`,color=m,
                     y=factor(water_cutoff, levels=water.regimes)), height=0.2) +
  labs(x="Probability of indicator being true",
       y="Wetland Flood Frequency Cutoff") + 
  guides(color=guide_legend(title="CEJST Indicator")) +
  scale_color_manual(values = c("blue","red"))


