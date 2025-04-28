path_to_gitrepo = "C:/Users/Chels/OneDrive - University of Illinois - Urbana/Illinois Wetlands Risk Assessment/Public-Repo"

setwd(path_to_gitrepo)

library(ggplot2)
library(patchwork)

# water regimes
water.regimes = c("Permanently Flooded","Intermittently Exposed","Semipermanently Flooded",
                  "Seasonally Flooded/Saturated","Seasonally Flooded","Seasonally Saturated",
                  "Temporary Flooded","Intermittently Flooded")
water.reg.labels = c("Permanently Flooded","Intermittently Exposed","Semipermanently Flooded",
                     "Seasonally Flooded/Saturated","Seasonally Flooded","Seasonally Saturated",
                     "Temporarily Flooded","Intermittently Flooded")
n.w = length(water.regimes)

################################################################################
## plot results for CEJST analysis 1

# model names
models = c("f","c","a","b","p","w")
n.m = length(models)
model.labels = c("\u2265 90th percentile\nfor 30-year flood risk",
                 "At least one climate\nthreshold exceeded",
                 "\u2265 90th percentile\nfor agricultural loss",
                 "\u2265 90th percentile\nfor building loss",
                 "\u2265 90th percentile\nfor population loss",
                 "\u2265 90th percentile\nfor wildfire risk")
model.abrvs = c("Flood","Climate","Ag","Building","Population","Wildfire")

# read in difference distributions
diff.list = list()
melt.list = list()
for (i in 1:n.m) {
  diff.i = read.csv(paste("CEJST_Analysis/Posteriors/",paste(model.abrvs[i],"Indicator_PostDiff.csv",sep="_"),sep=""), 
                    sep=",", header=T, check.names=F)
  diff.list[[models[i]]] = diff.i
  melt.i = melt(diff.i)
  melt.i$model = model.labels[i]
  melt.list[[models[i]]] = melt.i
}

# plot entire posterior distribution
ggplot(melt.list$f, 
       aes(x=1000*value, fill=model)) + 
       geom_density() + 
       facet_wrap(.~factor(variable, levels=water.regimes), ncol=1) + 
       labs(x="Posterior Mean Area Difference [Wetland ha/1000 Census Tract ha]\n(Indicator True - False)",
            y="Density") + 
       scale_fill_manual(values=c("blue")) + 
       guides(fill=guide_legend(title="CEJST Indicator"))

# calculate 95% highest posterior density interval (HPDI) for each model
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

# plot HPDIs by model and watercutoff
m.stats$water_label = rep(0, nrow(m.stats))
for (i in 1:n.w) { m.stats$water_label[which(m.stats$water_cutoff == water.regimes[i])] = water.reg.labels[i] }
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
            labs(x="Posterior Mean Area Difference (Indicator True - False)\n[Unprotected non-WOTUS wetland ha/1000 tract ha]",
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
            labs(x="Posterior Mean Area Difference (Indicator True - False)\n[unprotected non-WOTUS wetland ha/1000 tract ha]",
                 y="Wetland Flood Frequency Cutoff") +
            guides(color=guide_legend(title="CEJST Indicator")) +
            scale_color_manual(values=c("red")) + 
            scale_x_continuous(breaks=seq(-2,8,2),
                               labels=seq(-2,8,2),
                               limits=c(-2,7)) +
            theme(text = element_text(size=12))
p3 = p1 + p2
p3
ggsave("CEJST_Analysis/Figures/Figure6_Posterior_ClimateFlood_Differences.png", 
        plot = p3, width = 36, height = 12, units="cm")

# plot results for all models to explain climate results
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
        labs(x="Posterior Mean Area Difference (Indicator True - False)\n[unprotected Wetland ha/1000 Census Tract ha]",
             y="Wetland Flood Frequency Cutoff") +
        guides(color=guide_legend(title="CEJST Indicator")) +
        scale_x_continuous(breaks=seq(-6,0,2),
                           labels=seq(-6,0,2)) +
        theme(text = element_text(size=12))


## print results for paper

# multiply by 1,000 to get wetland area per 1,000 census tract ha
# as opposed to wetland area per 1 census tract ha
m.stats$mean100 = m.stats$mean*1000
m.stats$`2.5_100` = m.stats$`2.5`*1000
m.stats$`97.5_100` = m.stats$`97.5`*1000

# indices for water regimes in proper roder
wr.id1 = rep(0, n.w)
wr.id2 = rep(0, n.w)
for (i in 1:n.w) { wr.id1[i] = which(m.stats$water_cutoff == water.regimes[i])[1] }
for (i in 1:n.w) { wr.id2[i] = which(m.stats$water_cutoff == water.regimes[i])[2] }

# print
round(m.stats[wr.id1,c("mean100","2.5_100","97.5_100")],2)
round(m.stats[wr.id2,c("mean100","2.5_100","97.5_100")],2)

################################################################################
## plot results for CEJST analysis 2

# read in dataframes
post.df = read.csv("CEJST_Analysis/Posteriors/Linear_Model_EffectSize_HPDIs.csv")
post.line.df = read.csv("CEJST_Analysis/Posteriors/Linear_Model_Prediction.csv")
area.cj.df.nc = read.csv("CEJST_Analysis/Posteriors/CEJST_WetlandArea_Dataframe_NoUnproCnties.csv")
sf.ind = which(area.cj.df.nc$water_cutoff == "Seasonally Flooded")

# plot effect sizes for results normalized by census tract area
post.df$water_label = rep(0, nrow(post.df))
for (i in 1:n.w) { post.df$water_label[which(post.df$water_cutoff == water.regimes[i])] = water.reg.labels[i] }
p1 = ggplot(post.df, 
            aes(x=mean, 
                y=factor(water_label, levels=water.reg.labels))) + 
            geom_vline(xintercept=0) + 
            geom_point(position=position_dodge(0.5)) +
            geom_errorbarh(aes(xmin=`X2.5`, 
                               xmax=`X97.5`, 
                               y=factor(water_label, levels=water.reg.labels)), 
                           height=0.2, position=position_dodge(0.5)) +
            labs(y="Wetland Flood Frequency Cutoff",
                 x="Posterior Effect Size for Log(Share of Properties with Flood Risk [%])\nv. Unprotected Non-WOTUS Wetland Area [ha/1000 tract ha]") +
            theme(text = element_text(size=12))
p1

# plot posterior line
p2 = ggplot() + 
     geom_point(data=area.cj.df.nc[sf.ind,],
                           aes(x=Mean_DivArea*1000, 
                               y=100*FLD_PFS)) + 
     geom_line(data=post.line.df,
               aes(x=area, 
                   y=100*mean), 
               color="blue") + 
     geom_ribbon(data=post.line.df,
                 aes(x=area, 
                     ymin=100*`X2.5`, 
                     ymax=100*`X97.5`),
                 alpha=0.2, color="blue") +
     labs(y="Share of Properties at Risk of Flooding in 30 Years [%]",
          x="Unprotected Non-WOTUS Wetland Area [ha/1000 tract ha]") +
     theme(text = element_text(size=12))
p2

# save combined figure
p3 = p1 + p2
p3
ggsave("CEJST_Analysis/Figures/Figure7_Posterior_FloodLinearRegression.png", 
       plot = p3, width = 32, height = 14, units="cm")

# plot incremental increase in slope of nonlinear model
area_norm = seq(0, 210, by=10)/mean(area.cj.df.nc$Mean_DivArea)/1000
n = length(area_norm)
delta = c()
all_f = c(exp(-0.77156311+0.04860032*area_norm[1])*100)
for (i in 2:n) {
  f = exp(-0.77156311+0.04860032*area_norm[i])*100
  delta = c(delta, f-all_f[i-1])
  all_f = c(all_f, f)
}
plot(area_norm[2:n]*mean(area.cj.df.nc$Mean_DivArea)*1000, delta, ylim=c(1.2,3))
min(delta)
max(delta)
min(area.cj.df.nc[sf.ind,]$Mean_DivArea*1000)
max(area.cj.df.nc[sf.ind,]$Mean_DivArea*1000)
min(post.line.df$mean*100)
max(post.line.df$mean*100)
