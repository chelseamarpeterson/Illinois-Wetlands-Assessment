path_to_datafiles = "C:/Users/Chels/OneDrive - University of Illinois - Urbana/Illinois Wetlands Risk Assessment/Results/NWI_Data"
path_to_cejst = "C:/Users/Chels/OneDrive - University of Illinois - Urbana/Illinois Wetlands Risk Assessment/Databases/CEJST"
path_to_gitrepo = "C:/Users/Chels/OneDrive - University of Illinois - Urbana/Illinois Wetlands Risk Assessment/Public-Repo"

library(ggplot2)
library(reshape2)
library(dplyr)
library(rethinking)

################################################################################
# combine cejst data and unprotected wetland data for illinois

# read in dataframes
setwd(path_to_cejst)
cj.df = read.csv("IL_CJEST_Data.csv")

setwd(path_to_datafiles)
area.df = read.csv("IL_WS_Step16_CJEST_Unprotected_Area.csv")

# water regimes
water.regimes = c("Permanently Flooded","Intermittently Exposed","Semipermanently Flooded",
                    "Seasonally Flooded/Saturated","Seasonally Flooded","Seasonally Saturated",
                    "Temporary Flooded","Intermittently Flooded")
water.reg.labels = c("Permanently Flooded","Intermittently Exposed","Semipermanently Flooded",
                    "Seasonally Flooded/Saturated","Seasonally Flooded","Seasonally Saturated",
                    "Temporarily Flooded","Intermittently Flooded")
n.w = length(water.regimes)

# add zeros to cjest polygons without area data
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
length(unique(area.df$CJEST_ID))

# combine dataframes
area.cj.df = right_join(cj.df, area.df, by = c("CJEST_ID"))

# count number of census tracts in each category
sum(cj.df$FLD_ET == 1)
sum(cj.df$N_CLT_EOMI == 1)

# make index for water regime, climate indicator, and flood indicator
area.cj.df$wid = as.integer(factor(area.cj.df$water_cutoff))
area.cj.df$fid = as.integer(factor(area.cj.df$FLD_ET))
area.cj.df$cid = as.integer(factor(area.cj.df$N_CLT_EOMI))

# additional indices for explaining climat indicator results
area.cj.df$pid = as.integer(factor(area.cj.df$EPL_ET)) # expected population loss
area.cj.df$aid = as.integer(factor(area.cj.df$EAL_ET)) # expected agricultural loss
area.cj.df$bid = as.integer(factor(area.cj.df$EBL_ET)) # expected building loss
area.cj.df$wfid = as.integer(factor(area.cj.df$WFR_ET)) # wildfire risk

# normalize wetland areas by census tract areas
area.cj.df$Mean_DivArea = area.cj.df$mean_area/area.cj.df$Area_Ha

# normalize relative areas by global mean
area.cj.df$Mean_Norm = area.cj.df$Mean_DivArea/mean(area.cj.df$Mean_DivArea)

################################################################################
# CEJST Analysis 1: fit Bayesian distributions to each indicator group

# put normalized areas and indicators into list
area.list = list(area_norm = as.vector(area.cj.df$Mean_Norm),
                 wid = area.cj.df$wid,
                 fid = area.cj.df$fid,
                 cid = area.cj.df$cid,
                 pid = area.cj.df$pid,
                 aid = area.cj.df$aid,
                 bid = area.cj.df$bid,
                 wfid = area.cj.df$wfid)

## fit models for normalized area
set.seed(314)

# flooding and climate (main analyses)
m.f = ulam(alist(area_norm ~ normal(mu, sigma),
                 log(mu) <- a[wid,fid],
                 matrix[wid,fid]: a ~ dnorm(0,1),
                 sigma ~ dexp(1)),
           data=area.list, chains=5, log_lik=T)
m.c = ulam(alist(area_norm ~ normal(mu, sigma),
                 log(mu) <- a[wid,cid],
                 matrix[wid,cid]: a ~ dnorm(0,1),
                 sigma ~ dexp(1)),
           data=area.list, chains=5, log_lik=T)

# populatin, agriculture, and building loss plus wildfire risk (secondary analyses)
m.p = ulam(alist(area_norm ~ normal(mu, sigma),
                 log(mu) <- a[wid,pid],
                 matrix[wid,pid]: a ~ dnorm(0,1),
                 sigma ~ dexp(1)),
           data=area.list, chains=1, log_lik=T)
m.a = ulam(alist(area_norm ~ normal(mu, sigma),
                 log(mu) <- a[wid,aid],
                 matrix[wid,aid]: a ~ dnorm(0,1),
                 sigma ~ dexp(1)),
           data=area.list, chains=1, log_lik=T)
m.b = ulam(alist(area_norm ~ normal(mu, sigma),
                 log(mu) <- a[wid,bid],
                 matrix[wid,bid]: a ~ dnorm(0,1),
                 sigma ~ dexp(1)),
           data=area.list, chains=1, log_lik=T)
m.w = ulam(alist(area_norm ~ normal(mu, sigma),
                 log(mu) <- a[wid,wfid],
                 matrix[wid,wfid]: a ~ dnorm(0,1),
                 sigma ~ dexp(1)),
           data=area.list, chains=1, log_lik=T)

# put models into list
models = c("f","c","a","b","p","w")
n.m = length(models)
model.labels = c("\u2265 90th percentile\nfor 30-year flood risk",
                 "At least one climate\nthreshold exceeded",
                 "\u2265 90th percentile\nfor agricultural loss",
                 "\u2265 90th percentile\nfor building loss",
                 "\u2265 90th percentile\nfor population loss",
                 "\u2265 90th percentile\nfor wildfire risk")
model.abrvs = c("Flood","Climate","Ag","Building","Population","Wildfire")
m.list = list()
m.list[["f"]] = m.f
m.list[["c"]] = m.c   
m.list[["a"]] = m.a
m.list[["b"]] = m.b  
m.list[["p"]] = m.p
m.list[["w"]] = m.w

# trace & trank plots
#traceplot(m.f)
#traceplot(m.c)

# sample posterior distributions
setwd(path_to_gitrepo)
#for (i in 1:n.m) {
for (i in 1:2) {
  # get info for ith model
  m.i = models[i]
  model.i = m.list[[m.i]]
  
  # re-scale posterior estimates
  exp.a.i = exp(extract.samples(model.i)$a)*mean(area.cj.df$Mean_DivArea)

  # calculate posterior difference and melt dataframe
  diff.i = exp.a.i[,,2] - exp.a.i[,,1]
  colnames(diff.i) = levels(factor(area.cj.df$water_cutoff))
 
  # write to file
  write.csv(diff.i, 
            paste("CEJST_Analysis/Posteriors/",paste(model.abrvs[i],"Indicator_PostDiff.csv",sep="_"),sep=""), 
            row.names=F)
}

################################################################################
# CEJST Analysis 2: evaluate if mean wetland area has relationship with FLD_PFS, 
# or share of properties at risk of flood in 30 years (percentile)

# make dataframe that only has tracts with unprotected wetland area greater than zero
pro.cnties = paste(c("Cook","DeKalb","DuPage","Grundy","Kane","McHenry","Lake","Will"), "County", sep=" ")
area.cj.df.nc = area.cj.df[-which(area.cj.df$CF %in% pro.cnties),]

# normalize areas by global county mean
area.cj.df.nc$Mean_DivArea = area.cj.df.nc$mean_area/area.cj.df.nc$Area_Ha
area.cj.df.nc$Mean_Norm = area.cj.df.nc$Mean_DivArea/mean(area.cj.df.nc$Mean_DivArea)

# make lists with FLD_PFS
fld.list = list(wid = area.cj.df$wid,
                area_norm = as.vector(area.cj.df$Mean_Norm), 
                fld_pfs = as.vector(area.cj.df$FLD_PFS))
fld.list.nc = list(wid = area.cj.df.nc$wid,
                area_norm = as.vector(area.cj.df.nc$Mean_Norm), 
                fld_pfs = as.vector(area.cj.df.nc$FLD_PFS))

# fit models
set.seed(271)
ml.f <- ulam(alist(fld_pfs ~ normal(mu, sigma),
                    log(mu) <- a[wid] + b[wid]*area_norm,
                    c(a, b)[wid] ~ multi_normal( c(a_fld,b_fld) , Rho , sigma_fld ),
                    a_fld ~ normal(0,1),
                    b_fld ~ normal(0,1),
                    sigma_fld ~ exponential(1),
                    sigma ~ exponential(1),
                    Rho ~ lkj_corr(2)), 
              data=fld.list, chains=1, log_lik=TRUE)
ml.fc <- ulam(alist(fld_pfs ~ normal(mu, sigma),
                   log(mu) <- a[wid] + b[wid]*area_norm,
                   c(a, b)[wid] ~ multi_normal( c(a_fld,b_fld) , Rho , sigma_fld ),
                   a_fld ~ normal(0,1),
                   b_fld ~ normal(0,1),
                   sigma_fld ~ exponential(1),
                   sigma ~ exponential(1),
                   Rho ~ lkj_corr(2)) , 
             data=fld.list.nc, chains=5, log_lik=TRUE)

# compare models
#precis(ml.f, depth=3)
precis(ml.fc, depth=3)[c(4,12),"mean"]

# traceplots
#traceplot(ml.f)
#traceplot(ml.fc)

# extract posterior samples and calculate 95% HPDIs
post.df = data.frame(matrix(nrow=n.w, ncol=5))
colnames(post.df) = c("model","water_cutoff","mean","2.5","97.5")
k = 1
for (i in c(2)) {
  if (i == 1) {
    m = ml.f
    df = area.cj.df
    model_name = "All tracts"
  } else {
    m = ml.fc
    df = area.cj.df.nc
    model_name = "Only unprotected tracts"
  }
  samples = extract.samples(m)
  post.a = samples$a
  post.b = samples$b
  for (j in 1:n.w) {
    post.df[k,"model"] = model_name
    post.df[k,"water_cutoff"] = levels(factor(df$water_cutoff))[j]
    post.df[k,"mean"] = mean(post.b[,j])
    post.df[k,"2.5"] = HPDI(post.b[,j], prob=0.95)[1]
    post.df[k,"97.5"] = HPDI(post.b[,j], prob=0.95)[2]
    k = k + 1
  }
}
  
# print effect sizes in order of decreasing wetland flood frequency
water.reg.ind = rep(0, n.w)
for (i in 1:n.w) { water.reg.ind[i] = which(post.df$water_cutoff == water.regimes[i]) }
round(post.df[water.reg.ind,c("mean","2.5","97.5")],3)

# save posterior dataframe
write.csv(post.df, "CEJST_Analysis/Posteriors/Linear_Model_EffectSize_HPDIs.csv", row.names=F)

# plot lines at Seasonally Flooded cutoff
n = 500
sf.ind = which(area.cj.df.nc$water_cutoff == "Seasonally Flooded")
samples = extract.samples(ml.fc)
post.a = samples$a
post.b = samples$b
post.line.df = data.frame(matrix(nrow=0, ncol=5))
colnames(post.line.df) = c("water_cutoff","area","mean","2.5","97.5")
for (i in c(5)) {
  df.wr = data.frame(matrix(nrow=n, ncol=5))
  colnames(df.wr) = c("water_cutoff","area","mean","2.5","97.5")
  area.range = seq(min(area.cj.df.nc$Mean_Norm[sf.ind]),
                   max(area.cj.df.nc$Mean_Norm[sf.ind]), 
                   by=(max(area.cj.df.nc$Mean_Norm[sf.ind])-min(area.cj.df.nc$Mean_Norm[sf.ind]))/n)
  df.wr[1:n,"water_cutoff"] = levels(factor(area.cj.df.nc$water_cutoff))[i]
  for (j in 1:n) {
    pred.ij = exp(post.a[,i] + post.b[,i]*area.range[j])
    hpdi.ij = HPDI(pred.ij, prob=0.95)
    df.wr[j,"area"] = area.range[j]*mean(area.cj.df.nc$Mean_DivArea)*1000
    df.wr[j,"mean"] = mean(pred.ij)
    df.wr[j,"2.5"] = hpdi.ij[1]
    df.wr[j,"97.5"] = hpdi.ij[2]
  }
  post.line.df = rbind(post.line.df, df.wr)
}

# write dataframe for posterior linear prediction
write.csv(post.line.df, "CEJST_Analysis/Posteriors/Linear_Model_Prediction.csv", row.names=F)

# write area dataframe without protected counties for plotting
write.csv(area.cj.df.nc, "CEJST_Analysis/Posteriors/CEJST_WetlandArea_Dataframe_NoUnproCnties.csv", row.names=F)

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


