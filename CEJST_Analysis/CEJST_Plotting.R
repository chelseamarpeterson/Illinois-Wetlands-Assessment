path_to_gitrepo = "C:/Users/Chels/OneDrive - University of Illinois - Urbana/Illinois Wetlands Risk Assessment/Public-Repo"
setwd(path_to_gitrepo)

library(ggplot2)
library(patchwork)
library(reshape2)
library(rethinking)

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
indicators = c("c","f","a","b","p","w")
groups = c("all","unprotected")
n.i = length(indicators)
n.g = length(groups)
ind.labels = c("Climate Risk","Flood Risk","Agricultural Loss Risk",
              "Building Loss Risk","Population Loss Risk","Wildfire Risk")
#ind.labels = c("At least one climate\nthreshold exceeded",
#               "\u2265 90th percentile\nfor 30-year flood risk",
#               "\u2265 90th percentile\nfor agricultural loss",
#               "\u2265 90th percentile\nfor building loss",
#               "\u2265 90th percentile\nfor population loss",
#               "\u2265 90th percentile\nfor wildfire risk")
group.labels = c("All","Unprotected counties")
ind.abrvs = c("Climate","Flood","Ag","Building","Population","Wildfire")
grp.abrvs = c("All","Unprotected")

# read in difference distributions
diff.list = list()
melt.list = list()
for (i in 1:n.i) {
  ind.i = indicators[i]
  diff.list[[ind.i]] = list()
  melt.list[[ind.i]] = list()
  for (j in 1:n.g) {
    grp.j = groups[j]
    if (ind.abrvs[i] == "Wildfire" & grp.abrvs[j] == "Unprotected") {
      next
    } else {
      diff.ij = read.csv(paste("CEJST_Analysis/Posteriors/",
                               paste(ind.abrvs[i],grp.abrvs[j],
                                     "Indicator_PostDiff.csv",sep="_"),sep=""), 
                         sep=",", header=T, check.names=F)
      diff.list[[ind.i]][[grp.j]] = diff.ij
      melt.ij = melt(diff.ij)
      melt.ij$model = paste(ind.labels[i],group.labels[j],sep=":")
      melt.list[[ind.i]][[grp.j]] = melt.ij
    }
  }
}

# plot entire posterior distribution
ggplot(melt.list$c$all, 
       aes(x=1000*value, fill=model)) + 
       geom_density() + 
       facet_wrap(.~factor(variable, levels=water.regimes), ncol=1) + 
       labs(x="Posterior mean Area Difference [wetland ha/1,000 tract ha]\n(Indicator True - False)",
            y="Density") + 
       scale_fill_manual(values=c("blue")) + 
       guides(fill=guide_legend(title="CEJST Indicator"))
ggplot(melt.list$c$unprotected, 
       aes(x=1000*value, fill=model)) + 
       geom_density() + 
       facet_wrap(.~factor(variable, levels=water.regimes), ncol=1) + 
       labs(x="Posterior Mean Area Difference [wetland ha/1,000 tract ha]\n(Indicator True - False)",
            y="Density") + 
       scale_fill_manual(values=c("blue")) + 
       guides(fill=guide_legend(title="CEJST Indicator"))

# calculate 95% highest posterior density interval (HPDI) for each model
m.stats = data.frame(matrix(nrow=(n.i*n.g*n.w-n.w), ncol=6))
colnames(m.stats) = c("indicator","group","water_cutoff","mean","2.5","97.5")
n = 1
for (i in 1:n.i) {
  ind.i = indicators[i]
  for (j in 1:n.g) {
    grp.j = groups[j]
    if (ind.abrvs[i] == "Wildfire" & grp.abrvs[j] == "Unprotected") {
      next
    } else {
      means.ij = apply(diff.list[[ind.i]][[grp.j]], 2, mean)
      hpdi.95.ij = apply(diff.list[[ind.i]][[grp.j]], 2, HPDI, prob=0.95)
      for (k in 1:8) {
        m.stats[n,"indicator"] = ind.labels[i]
        m.stats[n,"group"] = group.labels[j]
        m.stats[n,"water_cutoff"] = names(means.ij)[k]
        m.stats[n,"mean"] = means.ij[k]
        m.stats[n,"2.5"] = hpdi.95.ij[1,k]
        m.stats[n,"97.5"] = hpdi.95.ij[2,k]
        n = n + 1
      }
    }
  }
}

# HPDIs by indicator and group: climate and flooding
m.stats$water_label = rep(0, nrow(m.stats))
for (i in 1:n.w) { m.stats$water_label[which(m.stats$water_cutoff == water.regimes[i])] = water.reg.labels[i] }
p1 = ggplot(m.stats[which(m.stats$indicator %in% c("Climate Risk","Flood Risk")),]) + 
       geom_point(aes(x=1000*mean, 
                      y=factor(water_label, levels=water.reg.labels),
                      color=factor(indicator,levels=ind.labels[1:2]),
                      group=factor(group, levels=group.labels)),
                  position=position_dodge(0.5)) +
       geom_vline(xintercept=0) +
       geom_errorbarh(aes(xmin=1000*`2.5`,xmax=1000*`97.5`, 
                          y=factor(water_label, levels=water.reg.labels), 
                          color=factor(indicator,levels=ind.labels[1:2]),
                          linetype=factor(group, levels=group.labels)),
                      position=position_dodge(0.5), 
                      height=0.2) +
       labs(x="Posterior Mean Area Difference (Indicator True - False)\n[Unprotected non-WOTUS wetland ha/1,000 tract ha]",
            y="Wetland Flood-Frequency Cutoff") +
       scale_color_manual(values=c("Climate Risk"="red",
                                   "Flood Risk"="blue")) +
       scale_linetype_manual(values=c("All"="solid",
                                      "Unprotected counties"="longdash")) +
       scale_x_continuous(limits=c(-10,30)) +
       guides(color="none",linetype=guide_legend(title="Census tract group")) +
       theme(text = element_text(size=14)) + 
       facet_wrap(.~factor(indicator,levels=ind.labels[1:2]))
p1
ggsave("CEJST_Analysis/Figures/Figure6_Posterior_ClimateFlood_Differences.png", 
       plot = p1, width = 32, height = 12, units="cm", dpi = 600)


## plot all indicators
p2 = ggplot(m.stats) + 
        geom_point(aes(x=1000*mean, 
                       y=factor(water_label, levels=water.reg.labels),
                       color=factor(indicator,levels=ind.labels[1:2]),
                       group=factor(group, levels=group.labels)),
                   position=position_dodge(0.5)) +
        geom_vline(xintercept=0) +
        geom_errorbarh(aes(xmin=1000*`2.5`,xmax=1000*`97.5`, 
                           y=factor(water_label, levels=water.reg.labels), 
                           color=factor(indicator,levels=ind.labels[1:2]),
                           linetype=factor(group, levels=group.labels)),
                       position=position_dodge(0.5), 
                       height=0.2) +
        labs(x="Posterior Mean Area Difference (Indicator True - False)\n[Unprotected non-WOTUS wetland ha/1,000 tract ha]",
             y="Wetland Flood-Frequency Cutoff") +
        guides(color="none",linetype=guide_legend(title="Census tract group")) +
        theme(text = element_text(size=14)) + 
        facet_wrap(.~factor(indicator,levels=ind.labels))
p2
#ggsave("CEJST_Analysis/Figures/Figure6_Posterior_ClimateFlood_Differences.png", 
#       plot = p1, width = 32, height = 12, units="cm", dpi = 600)

## print results for paper

# multiply by 1,000 to get wetland area per 1,000 census tract ha
# as opposed to wetland area per 1 census tract ha
m.stats$mean1000 = m.stats$mean*1000
m.stats$`2.5_1000` = m.stats$`2.5`*1000
m.stats$`97.5_1000` = m.stats$`97.5`*1000

# climate stats
clim.all.stats = m.stats[which(m.stats$indicator == "Climate Risk" & m.stats$group == "All"),]
wr.order = c()
for (i in 1:n.w) { wr.order = c(wr.order, which(clim.all.stats$water_cutoff == water.regimes[i])) }
round(clim.all.stats[wr.order,"mean1000"],2)
round(clim.all.stats[wr.order,"2.5_1000"],2)
round(clim.all.stats[wr.order,"97.5_1000"],2)

clim.unprotected.stats = m.stats[which(m.stats$indicator == "Climate Risk" & m.stats$group == "Unprotected counties"),]
round(clim.unprotected.stats[wr.order,"mean1000"],2)
round(clim.unprotected.stats[wr.order,"2.5_1000"],2)
round(clim.unprotected.stats[wr.order,"97.5_1000"],2)

# flood stats
flood.all.stats = m.stats[which(m.stats$indicator == "Flood Risk" & m.stats$group == "All"),]
round(flood.all.stats[wr.order,"mean1000"],2)
round(flood.all.stats[wr.order,"2.5_1000"],2)
round(flood.all.stats[wr.order,"97.5_1000"],2)

flood.unprotected.stats = m.stats[which(m.stats$indicator == "Flood Risk" & m.stats$group == "Unprotected counties"),]
round(flood.unprotected.stats[wr.order,"mean1000"],2)
round(flood.unprotected.stats[wr.order,"2.5_1000"],2)
round(flood.unprotected.stats[wr.order,"97.5_1000"],2)

################################################################################
## plot results for CEJST analysis 2

# plot effect sizes for results normalized by census tract area
post.df = read.csv("CEJST_Analysis/Posteriors/FLD_PFS_And_Area_Normalized_Linear_Model_EffectSize_HPDIs.csv")
post.df$water_label = rep(0, nrow(post.df))
for (i in 1:n.w) { post.df$water_label[which(post.df$water_cutoff == water.regimes[i])] = water.reg.labels[i] }
post.df.b.up = post.df[which(post.df$group == "Unprotected" & post.df$parameter=="b"),]
p1 = ggplot(post.df.b.up, 
            aes(x=mean, 
                y=factor(water_label, levels=water.reg.labels))) + 
            geom_vline(xintercept=0) + 
            geom_point(position=position_dodge(0.5)) +
            geom_errorbarh(aes(xmin=`X2.5`, 
                               xmax=`X97.5`, 
                               y=factor(water_label, levels=water.reg.labels)), 
                           height=0.2, position=position_dodge(0.5)) +
            labs(y="Wetland Flood-Frequency Cutoff",
                 x="Posterior Effect Size for Log(Share of Properties with Flood Risk [%])\nv. Unprotected Non-WOTUS Wetland Area [ha/1,000 tract ha]") +
            theme(text = element_text(size=15))
p1

# plot posterior line
post.line.df = read.csv("CEJST_Analysis/Posteriors/FLD_PFS_And_Area_Normalized_Linear_Model_Prediction.csv")
area.cj.df.unprotected = read.csv("CEJST_Analysis/Posteriors/CEJST_WetlandArea_Dataframe_NoUnproCnties.csv")
wr = "Seasonally Flooded" #wr = "Permanently Flooded" #wr = "Intermittently Flooded"
fld.pfs.mean = mean(area.cj.df.unprotected$FLD_PFS)
area.df.wr = area.cj.df.unprotected[which(area.cj.df.unprotected$water_cutoff == wr),]
line.df.wr = post.line.df[which(post.line.df$water_cutoff == wr),]
p2 = ggplot() + 
     geom_point(data=area.df.wr,
                aes(x=Tract_Normalized_Wetland_Area*1000, 
                    y=FLD_PFS*100)) + 
     geom_line(data=line.df.wr,
               aes(x=area, 
                   y=mean), 
               color="blue",linewidth=0.8) + 
     geom_ribbon(data=line.df.wr,
                 aes(x=area, 
                     ymin=`X2.5`, 
                     ymax=`X97.5`),
                 alpha=0.2, color=NA) +
     labs(y="Share of Properties at Risk of Flooding in 30 Years [%]",
          x="Unprotected Non-WOTUS Wetland Area [ha/1,000 tract ha]") +
     scale_y_continuous(limits=c(0,106),breaks=seq(0,100,25)) +
     theme(text = element_text(size=15))
p2

# save combined figure
p3 = p1 + theme(plot.margin = unit(c(1,20,1,1), "pt")) + p2
p3
ggsave("CEJST_Analysis/Figures/Figure7_Posterior_FloodLinearRegression.png", 
       plot = p3, width = 38, height = 16, units="cm", dpi=600)

## print effect sizes for table

# unprotected tracts
post.df.up.a = post.df[which(post.df$group == "Unprotected" & post.df$parameter=="a"),]
round(post.df.up.a[wr.order,c("mean","X2.5","X97.5")],3)

post.df.up.b = post.df[which(post.df$group == "Unprotected" & post.df$parameter=="b"),]
round(post.df.up.b[wr.order,c("mean","X2.5","X97.5")],3)

# all tracts
post.df.all.a = post.df[which(post.df$group == "All" & post.df$parameter=="a"),]
round(post.df.all.a[wr.order,c("mean","X2.5","X97.5")],2)

post.df.all.b = post.df[which(post.df$group == "All" & post.df$parameter=="b"),]
round(post.df.all.b[wr.order,c("mean","X2.5","X97.5")],2)

# approximate slope of line
max.fld.pls = max(line.df.wr$mean)
min.fld.pls = min(line.df.wr$mean)
(max.fld.pls-min.fld.pls)/(max(line.df.wr$area))*10

# plot incremental increase in slope of nonlinear model
area_norm = seq(0, 207.5, by=1)/mean(area.cj.df.unprotected$Tract_Normalized_Wetland_Area)/1000
wr.a = post.df.up.a[which(post.df.up.a$water_cutoff == "Seasonally Flooded"),"mean"]
wr.b = post.df.up.b[which(post.df.up.b$water_cutoff == "Seasonally Flooded"),"mean"]
n = length(area_norm)
delta = c()
all_f = c(exp(wr.a+wr.b*area_norm[1])*mean(area.cj.df.unprotected$FLD_PFS)*100)
for (i in 2:n) {
  f = exp(wr.a+wr.b*area_norm[i])*mean(area.cj.df.unprotected$FLD_PFS)*100
  delta = c(delta, f-all_f[i-1])
  all_f = c(all_f, f)
}
plot(area_norm[2:n]*mean(area.cj.df.unprotected$Tract_Normalized_Wetland_Area)*1000, delta)
min(delta)
max(delta)



