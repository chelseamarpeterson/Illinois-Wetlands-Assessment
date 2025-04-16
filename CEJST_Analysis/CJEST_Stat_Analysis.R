setwd('C:/Users/Chels/OneDrive - University of Illinois - Urbana/Illinois Wetlands Risk Assessment/Databases/CJEST Data/usa')

library(ggplot2)
library(reshape2)
library(patchwork)
library(dplyr)
library(rethinking)

# read in dataframes
cj.df = read.csv("IL_CJEST_Data.csv")
area.df = read.csv("IL_WS_Step16_CJEST_Unprotected_Area.csv")

# water regimes
water.reg.order = c("Permanently Flooded","Intermittently Exposed","Semipermanently Flooded",
                    "Seasonally Flooded/Saturated","Seasonally Flooded","Seasonally Saturated",
                    "Temporary Flooded","Intermittently Flooded")
water.reg.labels = c("Permanently Flooded","Intermittently Exposed","Semipermanently Flooded",
                    "Seasonally Flooded/Saturated","Seasonally Flooded","Seasonally Saturated",
                    "Temporarily Flooded","Intermittently Flooded")
n.w = length(water.reg.order)

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
    new.df$water_cutoff = water.reg.order
    new.df$mean_area = rep(0,n.w)
    new.df$min_area = rep(0,n.w)
    new.df$max_area = rep(0,n.w)
    area.df = rbind(area.df, new.df)
  }
}
length(unique(area.df$CJEST_ID))

# combine dataframes
area.cj.df = right_join(cj.df, area.df, by = c("CJEST_ID"))

# sum number of census tracts in each category
sum(cj.df$FLD_ET == 1)
sum(cj.df$N_CLT_EOMI == 1)

# make index for water regime, climate indicator, and flood indicator
area.cj.df$wid = as.integer(factor(area.cj.df$water_cutoff))
area.cj.df$fid = as.integer(factor(area.cj.df$FLD_ET))
area.cj.df$cid = as.integer(factor(area.cj.df$N_CLT_EOMI))
area.cj.df$pid = as.integer(factor(area.cj.df$EPL_ET))
area.cj.df$aid = as.integer(factor(area.cj.df$EAL_ET))
area.cj.df$bid = as.integer(factor(area.cj.df$EBL_ET))
area.cj.df$wfid = as.integer(factor(area.cj.df$WFR_ET))

################################################################################
# fit bayesian distributions to each group

## comparisons of mean area between groups

# normalize areas by global county mean
area.cj.df$Mean_DivArea = area.cj.df$mean_area/area.cj.df$Area_Ha
area.cj.df$Mean_Norm = area.cj.df$Mean_DivArea/mean(area.cj.df$Mean_DivArea)

# plots
hist(log(area.cj.df$Mean_DivArea))
hist(log(area.cj.df$Mean_Norm))

# put data into list
area.list = list(area_norm = as.vector(area.cj.df$Mean_Norm),
                 wid = area.cj.df$wid,
                 fid = area.cj.df$fid,
                 cid = area.cj.df$cid,
                 pid = area.cj.df$pid,
                 aid = area.cj.df$aid,
                 bid = area.cj.df$bid,
                 wfid = area.cj.df$wfid)

# fit models for normalized area
set.seed(314)
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
#n.m = length(models)
n.m = 2
model.labels = c("\u2265 90th percentile\nfor 30-year flood risk",
                 "At least one climate\nthreshold exceeded",
                 "\u2265 90th percentile\nfor agricultural loss",
                 "\u2265 90th percentile\nfor building loss",
                 "\u2265 90th percentile\nfor population loss",
                 "\u2265 90th percentile\nfor wildfire risk")
m.list = list()
m.list[["f"]] = m.f
m.list[["c"]] = m.c   
#m.list[["a"]] = m.a
#m.list[["b"]] = m.b  
#m.list[["p"]] = m.p
#m.list[["w"]] = m.w

# look at posterior estimates
#precis(m.f, depth=3)
#precis(m.c, depth=3)

# trace & trank plots
#traceplot(m.f)
#traceplot(m.c)

# sample posterior distributions
a.list = list()
diff.list = list()
melt.list = list()
for (i in 1:n.m) {
  m.i = models[i]
  model.i = m.list[[m.i]]
  exp.a.i = exp(extract.samples(model.i)$a)*mean(area.cj.df$Mean_DivArea)

  # calculate difference and melt dataframe
  diff.i = exp.a.i[,,2] - exp.a.i[,,1]
  colnames(diff.i) = levels(factor(area.cj.df$water_cutoff))
  melt.i = melt(diff.i)
  melt.i$model = model.labels[i]
  
  # save to lists
  a.list[[m.i]] = exp.a.i
  diff.list[[m.i]] = diff.i
  melt.list[[m.i]] = melt.i
  
  # write to file
  if (i == 1) {
    write.csv(diff.i, paste("FloodRisk_Difference.csv",sep=""), row.names=F)
  } else {
    write.csv(diff.i, paste("ClimateRisk_Difference.csv",sep=""), row.names=F)
  }
}

# read in difference distributions
#diff.list = list()
#melt.list = list()
#for (i in 1:n.m) {
#  if (i == 1) {
#    diff.i = read.csv("FloodRisk_Difference.csv", sep=",", header=T, check.names=F)
#  } else {
#    diff.i = read.csv("ClimateRisk_Difference.csv", sep=",", header=T, check.names=F)
#  }
#  diff.list[[models[i]]] = diff.i
#  melt.i = melt(diff.i)
#  melt.i$model = model.labels[i]
#  melt.list[[models[i]]] = melt.i
#}

# plot entire posterior distribution
#ggplot(melt.list$f, 
#       aes(x=1000*value, fill=model)) + 
#       geom_density() + 
#       facet_wrap(.~factor(Var2, levels=water.reg.order), ncol=1) + 
#       labs(x="Posterior Mean Area Difference [Wetland ha/1000 Census Tract ha]\n(Indicator True - False)",
#            y="Density") + 
#       scale_fill_manual(values=c("blue")) + 
#       guides(fill=guide_legend(title="CEJST Indicator"))

# calculate HPDIs for each model
m.stats = data.frame(matrix(nrow=n.m*n.w, ncol=5))
colnames(m.stats) = c("model","water_cutoff","mean","2.5","97.5")
k = 1
for (i in 1:n.m) {
  m.i = models[i]
  means.i = apply(diff.list[[m.i]], 2, mean)
  hpdi.95.i = apply(diff.list[[m.i]], 2, HPDI, prob=0.95)
  for (j in 1:8) {
    m.stats[k,"model"] = model.labels[i]
    m.stats[k,"water_cutoff"] = names(means.i)[j]
    m.stats[k,"mean"] = means.i[j]
    m.stats[k,"2.5"] = hpdi.95.i[1,j]
    m.stats[k,"97.5"] = hpdi.95.i[2,j]
    k = k + 1
  }
}

# plot hpdis by model and watercutoff
m.stats$water_label = rep(0, nrow(m.stats))
for (i in 1:n.w) { m.stats$water_label[which(m.stats$water_cutoff == water.reg.order[i])] = water.reg.labels[i] }
p1 = ggplot(m.stats[which(m.stats$model == model.labels[1]),], 
                     aes(x=1000*mean, 
                     y=factor(water_label, levels=water.reg.labels),
                     color=factor(model, levels=model.labels[1]))) + 
        geom_point(position=position_dodge(0.5)) +
        geom_vline(xintercept=0) +
        geom_errorbarh(aes(xmin=1000*`2.5`,xmax=1000*`97.5`, 
                           y=factor(water_label, levels=water.reg.labels), 
                           color=factor(model, levels=model.labels[1])),
                       position=position_dodge(0.5), 
                       height=0.2) +
        labs(x="Posterior Mean Area Difference (Indicator True - False)\n[Unprotected Wetland ha/1000 Census Tract ha]",
             y="Wetland Flood Frequency Cutoff") +
        guides(color=guide_legend(title="CEJST Indicator")) +
        scale_color_manual(values=c("blue")) + 
        scale_x_continuous(breaks=seq(-8,2,2),
                           labels=seq(-8,2,2),
                           limits=c(-7,2)) +
        theme(text = element_text(size=12))
p2 = ggplot(m.stats[which(m.stats$model == model.labels[2]),], 
                     aes(x=1000*mean, 
                     y=factor(water_label, levels=water.reg.labels), 
                     color=factor(model, levels=model.labels[2]))) + 
        geom_point(position=position_dodge(0.5)) +
        geom_vline(xintercept=0) +
        geom_errorbarh(aes(xmin=1000*`2.5`,xmax=1000*`97.5`, 
                           y=factor(water_label, levels=water.reg.labels), 
                           color=factor(model, levels=model.labels[2])),
                       position=position_dodge(0.5), 
                       height=0.2) +
        labs(x="Posterior Mean Area Difference (Indicator True - False)\n[Unprotected Wetland ha/1000 Census Tract ha]",
             y="Wetland Flood Frequency Cutoff") +
        guides(color=guide_legend(title="CEJST Indicator")) +
        scale_color_manual(values=c("red")) + 
        scale_x_continuous(breaks=seq(-2,8,2),
                           labels=seq(-2,8,2),
                           limits=c(-2,7)) +
        theme(text = element_text(size=12))
p3 = p1 + p2
p3
setwd("C:/Users/Chels/OneDrive - University of Illinois - Urbana/Illinois Wetlands Risk Assessment/Documents/Figures")
ggsave("Figure6_Posterior_ClimateFlood_Differences.png", 
       plot = p3, width = 36, height = 12, units="cm")

ggplot(m.stats, 
       aes(x=1000*mean, 
           y=factor(water_label, levels=water.reg.labels),
           color=factor(model, levels=model.labels))) + 
       geom_point(position=position_dodge(0.5)) +
       geom_vline(xintercept=0) +
       geom_errorbarh(aes(xmin=1000*`2.5`,xmax=1000*`97.5`, 
                          y=factor(water_label, levels=water.reg.labels), 
                          color=factor(model, levels=model.labels)),
                      position=position_dodge(0.5), 
                      height=0.2) +
       labs(x="Posterior Mean Area Difference (Indicator True - False)\n[Unprotected Wetland ha/1000 Census Tract ha]",
            y="Wetland Flood Frequency Cutoff") +
       guides(color=guide_legend(title="CEJST Indicator")) +
       scale_x_continuous(breaks=seq(-6,0,2),
                          labels=seq(-6,0,2)) +
       theme(text = element_text(size=12))


# print results for paper
m.stats$mean100 = m.stats$mean*1000
m.stats$`2.5_100` = m.stats$`2.5`*1000
m.stats$`97.5_100` = m.stats$`97.5`*1000

wr.id1 = rep(0, n.w)
wr.id2 = rep(0, n.w)
for (i in 1:n.w) { wr.id1[i] = which(m.stats$water_cutoff == water.reg.order[i])[1] }
for (i in 1:n.w) { wr.id2[i] = which(m.stats$water_cutoff == water.reg.order[i])[2] }

round(m.stats[wr.id1,c("mean100","2.5_100","97.5_100")],2)
round(m.stats[wr.id2,c("mean100","2.5_100","97.5_100")],2)


####################################################################################
### evaluate if CJEST flood/climate metrics have relationship with mean wetland area
### FLD_PFS: share of properties at risk of flood in 30 years (percentile)

# make dataframe that only has tracts with unprotected wetland area greater than zero
pro.cnties = paste(c("Cook","DeKalb","DuPage","Grundy","Kane","McHenry","Lake","Will"), "County", sep=" ")
area.cj.df.nz = area.cj.df[-which(area.cj.df$mean_area == 0),]
area.cj.df.nc = area.cj.df[-which(area.cj.df$CF %in% pro.cnties),]

# normalize areas by global county mean
area.cj.df.nz$Mean_DivArea = area.cj.df.nz$mean_area/area.cj.df.nz$Area_Ha
area.cj.df.nc$Mean_DivArea = area.cj.df.nc$mean_area/area.cj.df.nc$Area_Ha
area.cj.df.nz$Mean_Norm = area.cj.df.nz$Mean_DivArea/mean(area.cj.df.nz$Mean_DivArea)
area.cj.df.nc$Mean_Norm = area.cj.df.nc$Mean_DivArea/mean(area.cj.df.nc$Mean_DivArea)

# plots
hist(area.cj.df.nz$mean_area)
mean(area.cj.df.nz$mean_area)
hist(area.cj.df.nz$Mean_DivArea)
mean(area.cj.df.nz$Mean_DivArea)*1000
ggplot(area.cj.df.nz, 
       aes(x=Mean_Norm, y=FLD_PFS)) + 
       geom_point() +
       labs(x="Mean Wetland Area without Protection (ac)",
            y="Share of properties at risk of flood in 30 years (percentile)") +
       facet_wrap(.~factor(water_cutoff, levels=water.reg.order), ncol=4,
                   scales = "free_x")

summary(lm(FLD_PFS~log(Mean_Norm), data=area.cj.df.nz[which(area.cj.df.nz$water_cutoff == "Seasonally Flooded"),]))
summary(lm(log(FLD_PFS)~Mean_Norm, data=area.cj.df.nz[which(area.cj.df.nz$water_cutoff == "Seasonally Flooded"),]))
summary(lm(FLD_PFS~Mean_DivArea, data=area.cj.df.nc[which(area.cj.df.nc$water_cutoff == "Seasonally Flooded"),]))
summary(lm(log(FLD_PFS)~Mean_Norm, data=area.cj.df[which(area.cj.df$water_cutoff == "Seasonally Flooded"),]))

# make list with FLD_PFS
fld.list = list(wid = area.cj.df$wid,
                   area_norm = as.vector(area.cj.df$Mean_Norm), 
                   fld_pfs = as.vector(area.cj.df$FLD_PFS),
                   bl_pfs = as.vector(area.cj.df$EBLR_PFS),
                   pl_pfs = as.vector(area.cj.df$EPLR_PFS))
fld.list.nz = list(wid = area.cj.df.nz$wid,
                   area_norm = as.vector(area.cj.df.nz$Mean_Norm), 
                   fld_pfs = as.vector(area.cj.df.nz$FLD_PFS),
                   bl_pfs = as.vector(area.cj.df.nz$EBLR_PFS),
                   pl_pfs = as.vector(area.cj.df.nz$EPLR_PFS))
fld.list.nc = list(wid = area.cj.df.nc$wid,
                area_norm = as.vector(area.cj.df.nc$Mean_Norm), 
                fld_pfs = as.vector(area.cj.df.nc$FLD_PFS),
                bl_pfs = as.vector(area.cj.df.nc$EBLR_PFS),
                pl_pfs = as.vector(area.cj.df.nc$EPLR_PFS))

# fit models
set.seed(271)
ml.f <- ulam(alist(fld_pfs ~ normal(mu, sigma),
                    log(mu) <- a[wid] + b[wid]*area_norm,
                    c(a, b)[wid] ~ multi_normal( c(a_fld,b_fld) , Rho , sigma_fld ),
                    a_fld ~ normal(0,1),
                    b_fld ~ normal(0,1),
                    sigma_fld ~ exponential(1),
                    sigma ~ exponential(1),
                    Rho ~ lkj_corr(2)) , 
              data=fld.list , chains=2 , log_lik=TRUE)
ml.fc <- ulam(alist(fld_pfs ~ normal(mu, sigma),
                   log(mu) <- a[wid] + b[wid]*area_norm,
                   c(a, b)[wid] ~ multi_normal( c(a_fld,b_fld) , Rho , sigma_fld ),
                   a_fld ~ normal(0,1),
                   b_fld ~ normal(0,1),
                   sigma_fld ~ exponential(1),
                   sigma ~ exponential(1),
                   Rho ~ lkj_corr(2)) , 
             data=fld.list.nc , chains=5 , log_lik=TRUE)
ml.fz <- ulam(alist(fld_pfs ~ normal(mu, sigma),
                    log(mu) <- a[wid] + b[wid]*area_norm,
                    c(a, b)[wid] ~ multi_normal( c(a_fld,b_fld) , Rho , sigma_fld ),
                    a_fld ~ normal(0,1),
                    b_fld ~ normal(0,1),
                    sigma_fld ~ exponential(1),
                    sigma ~ exponential(1),
                    Rho ~ lkj_corr(2)) , 
              data=fld.list.nz , chains=1 , log_lik=TRUE)

# compare models
precis(ml.f, depth=3)
precis(ml.fc, depth=3)
precis(ml.f, depth=2)[,"mean"]*100

# traceplots
#traceplot(ml.f)

# extract posterior samples
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
  
water.reg.ind = rep(0, n.w)
for (i in 1:n.w) { water.reg.ind[i] = which(post.df$water_cutoff == water.reg.order[i]) }
round(post.df[water.reg.ind,c("mean","2.5","97.5")],3)

# plot effect sizes for results normalized by census tract area
post.df$water_label = rep(0, nrow(post.df))
for (i in 1:n.w) { post.df$water_label[which(post.df$water_cutoff == water.reg.order[i])] = water.reg.labels[i] }
p1 = ggplot(post.df, 
           aes(x=mean, 
               y=factor(water_label, levels=water.reg.labels))) + 
           geom_vline(xintercept=0) + 
           geom_point(position=position_dodge(0.5)) +
           geom_errorbarh(aes(xmin=`2.5`, 
                              xmax=`97.5`, 
                              y=factor(water_label, levels=water.reg.labels)), 
                          height=0.2, position=position_dodge(0.5)) +
           labs(y="Wetland Flood Frequency Cutoff",
                x="Posterior Effect Size for Log(Share of Properties with Flood Risk [%])\nv. Unprotected Non-WOTUS Wetland Area [ha/1000 tract ha]") +
           theme(text = element_text(size=12))

#for Share of Properties at Risk of Flooding in 30 Years 
#v.\n Log(Unprotected Wetland Area [ha/1000 tract ha] + 1)

#summary(lm(Mean_Norm~FLD_PFS, data=area.cj.df.nz[which(area.cj.df.nz$water_cutoff == "Seasonally Flooded"),]))
# plot lines
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

# plot posterior line
p2 = ggplot() + geom_point(data=area.cj.df.nc[sf.ind,],
                           aes(x=Mean_DivArea*1000, 
                               y=100*FLD_PFS)) + 
                          geom_line(data=post.line.df,
                                    aes(x=area, 
                                        y=100*mean), 
                                    color="blue") + 
                          geom_ribbon(data=post.line.df,
                                      aes(x=area, 
                                          ymin=100*`2.5`, 
                                          ymax=100*`97.5`),
                                      alpha=0.2, color="blue") +
                          labs(y="Share of Properties at Risk of Flooding in 30 Years [%]",
                               x="Unprotected Non-WOTUS Wetland Area [ha/1000 tract ha]") +
                          theme(text = element_text(size=12))
p3 = p1 + p2
p3
setwd("C:/Users/Chels/OneDrive - University of Illinois - Urbana/Illinois Wetlands Risk Assessment/Documents/Figures")
ggsave("Figure7_Posterior_FloodLinearRegression.png", 
       plot = p3, width = 32, height = 14, units="cm")

################################################################################
## logistic regression

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
                   y=factor(wr, level=water.reg.order),
                   color=m)) +
  geom_point() + 
  geom_errorbarh(aes(xmin=b_5, xmax=b_95, 
                     y=factor(wr, level=water.reg.order)), 
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
                    y=factor(water_cutoff, levels=water.reg.order))) + 
  geom_point() + 
  geom_errorbarh(aes(xmin=`5`,xmax=`95`,color=m,
                     y=factor(water_cutoff, levels=water.reg.order)), height=0.2) +
  labs(x="Probability of indicator being true",
       y="Wetland Flood Frequency Cutoff") + 
  guides(color=guide_legend(title="CEJST Indicator")) +
  scale_color_manual(values = c("blue","red"))

################################################################################

# wastewater discharge percentile
ggplot(area.cj.df, aes(x=mean_area/Area_Acres, y=WF_PFS)) + geom_point()
ggplot(area.cj.df, aes(x=log(mean_area/Area_Acres), y=WF_PFS)) + geom_point()

# leaky storage tanks percentile
ggplot(area.cj.df, aes(x=mean_area, y=UST_PFS)) + geom_point()
ggplot(area.cj.df, aes(x=log(mean_area), y=UST_PFS)) + geom_point()

# impervious surface or cropland percentile
ggplot(area.cj.df, aes(x=mean_area, y=IS_PFS)) + geom_point()
ggplot(area.cj.df, aes(x=log(mean_area), y=IS_PFS)) + geom_point()

# fire risk percentile
ggplot(area.cj.df, aes(x=mean_area, y=WFR_PFS)) + geom_point()
ggplot(area.cj.df, aes(x=log(mean_area), y=WFR_PFS)) + geom_point()

# building, population, ag loss
ggplot(area.cj.df, aes(x=log(mean_area), y=EBLR_PFS)) + geom_point()
ggplot(area.cj.df, aes(x=log(mean_area), y=EPLR_PFS)) + geom_point()
ggplot(area.cj.df, aes(x=log(mean_area), y=EALR_PFS)) + geom_point()

# 
x = seq(0, 210, by=10)/mean(area.cj.df.nz$Mean_DivArea)/1000
n = length(x)
delta = c(0)
all_f = c(exp(-0.77+0.049*x[1])*100)
for (i in 2:n) {
  f = exp(-0.77+0.049*x[i])*100
  delta = c(delta, f-all_f[i-1])
  all_f = c(all_f, f)
}
plot(x[2:n]*mean(area.cj.df.nz$Mean_DivArea)*1000, delta[2:n], ylim=c(1.2,2.4))
min(delta[2:n])
max(delta[2:n])
