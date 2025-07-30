path_to_gitrepo = "C:/Users/Chels/OneDrive - University of Illinois - Urbana/Illinois Wetlands Risk Assessment/Public-Repo"
path_to_main = "C:/Users/Chels/OneDrive - University of Illinois - Urbana/Illinois Wetlands Risk Assessment"

library(ggplot2)
library(dplyr)
library(patchwork)
library(tidyr)

################################################################################
# plot length of flowlines by type to decide length cutoffs for unknown streams/rivers

# read in NHD data clipped to flowline extent
setwd(path_to_main)
flowlines.df = read.csv("Results/NHD_Data/NHDFlowline_IL_Step1_Clip.csv")

# make histograms of upstream drainage area by fcode
fcodes = c(46006,46003,46007)
pie.df = flowlines.df[which(flowlines.df$fcode %in% fcodes),]
labels = c("Perennial","Intermittent","Ephemeral")
pie.df$label = rep(0, nrow(pie.df))
for (i in 1:3) { pie.df$label[which(pie.df$fcode == fcodes[i])] = labels[i] }
lines = data.frame(levels=c(0.25, 2.5, 25))
pie.sum = pie.df %>%
  group_by(fcode) %>% 
  summarize(median = median(totdasqkm),
            q25 = quantile(totdasqkm, prob=0.25),
            q75 = quantile(totdasqkm, prob=0.75))
lines1 = data.frame(levels=as.vector((pie.sum$q75[c(3,1)] + pie.sum$q25[1:2])/2))
lines2 = data.frame(levels=c(0.3,3.5))
ggplot(pie.df, 
       aes(x=log(totdasqkm), 
           fill=factor(label, levels=labels))) + 
       geom_density() + 
       geom_vline(data=lines2, 
                  aes(xintercept=log(levels)))

################################################################################
# estimate length of flowlines by type

# read in table with all classifications
fl.df1 = read.csv("Results/NHD_Data/NHDFlowline_IL_Step4_HydroClass.csv")
fl.df2 = read.csv("Results/NHD_Data/NHDFlowline_IL_Step5_HydroClass.csv")
fl.df.join = left_join(fl.df1, fl.df2, by = c("permanent_identifier"))

# estimate total county and length of known perennial, intermittent, and ephemeral
sum(fl.df.join$fcode == 46006)
sum(fl.df.join$fcode == 46003)
sum(fl.df.join$fcode == 46007)
fl.df.join[which(fl.df.join$fcode %in% c(46006,46003,46007)),c("fcode","Length_km")] %>%
           group_by(fcode) %>%
           summarize(total = sum(Length_km))

# estimate total length of canals with non-null GNIS name
sum(fl.df.join$ftype == 336 & fl.df.join$gnis_name != "")
fl.df.join[(fl.df.join$ftype == 336 & fl.df.join$gnis_name != ""),c("ftype","Length_km")] %>%
           group_by(ftype) %>%
           summarize(total = sum(Length_km))

## estimate total length of unknown streams in each category 

# perennial
sum(fl.df.join$fcode == 46000 & fl.df.join$totdasqkm > 16.98)
fl.df.join[(fl.df.join$fcode == 46000 & fl.df.join$totdasqkm > 16.98),c("fcode","Length_km")] %>%
           group_by(fcode) %>%
           summarize(total = sum(Length_km))

# intermittent
sum(fl.df.join$fcode == 46000 & (fl.df.join$totdasqkm > 0.60 & fl.df.join$totdasqkm <= 16.98))
fl.df.join[(fl.df.join$fcode == 46000 & (fl.df.join$totdasqkm > 0.60 & fl.df.join$totdasqkm <= 16.98)),c("fcode","Length_km")] %>%
          group_by(fcode) %>%
          summarize(total = sum(Length_km))

# ephemeral
sum(fl.df.join$fcode == 46000 & fl.df.join$totdasqkm <= 0.60)
fl.df.join[(fl.df.join$fcode == 46000 & fl.df.join$totdasqkm <= 0.60),c("fcode","Length_km")] %>%
           group_by(fcode) %>%
           summarize(total = sum(Length_km))

# estimate total length of perennial, intermittent, ephemeral, and NA
fl.df.join[,c("Final_Hydro_Class","Length_km")] %>%
            group_by(Final_Hydro_Class) %>%
            summarize(total = sum(Length_km))

sum(fl.df.join$Final_Hydro_Class[-which(is.na(fl.df.join$Final_Hydro_Class))] == 1)
sum(fl.df.join$Final_Hydro_Class[-which(is.na(fl.df.join$Final_Hydro_Class))] == 2)
sum(fl.df.join$Final_Hydro_Class[-which(is.na(fl.df.join$Final_Hydro_Class))] == 3)

# estimate length of streams classified via connectivity roles
fl.df.nona2 = fl.df.join[-which(is.na(fl.df.join$Final_Hydro_Class)),]
fl.df.nona1 = fl.df.join[-which(is.na(fl.df.join$Hydro_Class.y)),]
p2 = sum(fl.df.nona2$Final_Hydro_Class == 1)
p1 = sum(fl.df.nona1$Hydro_Class.y == 1)
p2 - p1

p2.l = sum(fl.df.nona2[fl.df.nona2$Final_Hydro_Class == 1,"Length_km"])
p1.l = sum(fl.df.nona1[fl.df.nona1$Hydro_Class.y == 1,"Length_km"])
p2.l - p1.l

i2 = sum(fl.df.nona2$Final_Hydro_Class == 2)
i1 = sum(fl.df.nona1$Hydro_Class.y == 2)
i2 - i1

i2.l = sum(fl.df.nona2[fl.df.nona2$Final_Hydro_Class == 2,"Length_km"])
i1.l = sum(fl.df.nona1[fl.df.nona1$Hydro_Class.y == 2,"Length_km"])
i2.l - i1.l

e2 = sum(fl.df.nona2$Final_Hydro_Class == 3)
e1 = sum(fl.df.nona1$Hydro_Class.y == 3)
e2 - e1

e2.l = sum(fl.df.nona2[fl.df.nona2$Final_Hydro_Class == 3,"Length_km"])
e1.l = sum(fl.df.nona1[fl.df.nona1$Hydro_Class.y == 3,"Length_km"])
e2.l - e1.l

## estimate length and number of streams classified as intermittent due to intersects with intermittent waterbody

i1.c = sum(fl.df.join$fcode == 46003)
i1.l = sum(fl.df.join[fl.df.join$fcode == 46003,"Length_km"])

i2.c = sum(fl.df.join$fcode == 46000 & (fl.df.join$totdasqkm > 0.60 & fl.df.join$totdasqkm <= 16.98))
i2.l = sum(fl.df.join[fl.df.join$fcode == 46000 & (fl.df.join$totdasqkm > 0.60 & fl.df.join$totdasqkm <= 16.98),"Length_km"])

i3.c = sum(fl.df.nona2$Final_Hydro_Class == 2) - sum(fl.df.nona1$Hydro_Class.y == 2)
i3.l = sum(fl.df.nona2[fl.df.nona2$Final_Hydro_Class == 2,"Length_km"]) - sum(fl.df.nona1[fl.df.nona1$Hydro_Class.y == 2,"Length_km"])

it.c = sum(fl.df.nona2$Final_Hydro_Class == 2)
it.l = sum(fl.df.nona2[fl.df.nona2$Final_Hydro_Class == 2,"Length_km"])

it.l-i3.l-i2.l-i1.l
it.c-i3.c-i2.c-i1.c

## estimate count and length of flowlines before any were removed due to isolation
fl.df0 = read.csv("Results/NHD_Data/NHDFlowline_IL_Step1_Clip.csv")
fl.df1 = read.csv("Results/NHD_Data/NHDFlowline_IL_Step4_HydroClass.csv")

# total length and count
nrow(fl.df0)
sum(fl.df0$Length_km)

# total non-isolated due to lack of upstream or downstream hydrosequence
nrow(fl.df0) - nrow(fl.df1)
sum(fl.df0$Length_km) - sum(fl.df1$Length_km)

# estimate number and length removed due to NA
sum(is.na(fl.df.join$Final_Hydro_Class))
sum(fl.df.join[is.na(fl.df.join$Final_Hydro_Class),c("Length_km")])

# estimate number and length of non-isolated flowlnnes
nrow(fl.df.nona2)
sum(fl.df.join[-which(is.na(fl.df.join$Final_Hydro_Class)),c("Length_km")])

## estimate count and length of flowlines classified as perennial due to intersection with NHD Area
fl.step1.df = read.csv("Results/NHD_Data/NHDFlowline_IL_Step2_NonIsolated.csv")
area.int.df = read.csv("Results/NHD_Data/NHDFlowline_IL_Step3_StreamClassification1.csv")

sum(area.int.df$Hydro_Class == 1) - sum(fl.step1.df$Hydro_Class == 1)
sum(area.int.df[area.int.df$Hydro_Class == 1,"Length_Km"]) - sum(fl.step1.df[fl.step1.df$Hydro_Class == 1,"Length_Km"])

## estimate count and length of flowlines classified as perennial due to intersection with NHD waterbody
# 103630-412-21654-1158-5-48141 = 32260
# 62248 - 282 - 12484 - 1511 - 3 - 41816 = 6152
water.int.df = read.csv("Results/NHD_Data/NHDFlowline_IL_Step3_StreamClassification2.csv")
sum(water.int.df[water.int.df$Hydro_Class == 1,"Length_Km"]) - sum(area.int.df[area.int.df$Hydro_Class == 1,"Length_Km"])
sum(water.int.df$Hydro_Class == 1) - sum(area.int.df$Hydro_Class == 1)

### final numerical check
## total - perennial - intermittent - ephemeral
# length: 191189 - 62248 - 126675 - 2266 = 0
# count: 273241 - 103630 - 165159 - 4452 = 0

## total - total isolated  - total non-isolated
# length: 192680 - 52 - 1439 - 191189 = 0
# count: 274857 - 89 - 1527 - 273241 = 0

## total perennial
# length: 62248 - 282 - 6152 - 12484 - 1511 - 3 - 41816 = 0
# count: 103630 - 412 - 32260 - 21654 - 1158 - 5 - 48141 = 0

## total intermittent
# length: 126675 - 1933 - 229 - 25 - 124488 = 0
# count: 165159 - 2859 - 1471 - 75 - 160754 = 0

## total ephemeral
# length: 2266 - 1725 - 80 - 461 = 0
# count: 4452 - 2484 - 439 - 1529 = 0

################################################################################
# Figure A4: Plot length of flowlines by stream order

setwd(path_to_main)

# read in merged file with isolated flowlines
fl.df = read.csv("Results/NHD_Data/NHDFlowline_IL_Step8_MergeIsolatedFlowlines.csv")
fl.df.br = read.csv("Results/NHD_Data/NHDFlowline_IL_Step8_MergeIsolatedFlowlines_Brinkerhoff.csv")

# check length columns
sum(fl.df$lengthkm)
sum(fl.df.br$lengthkm)

# sum length by stream order
fl.ord.sum = fl.df %>%
             group_by(streamorde) %>%
             summarize(total.length = sum(lengthkm))

fl.ord.sum.br = fl.df.br %>%
            group_by(streamorde) %>%
            summarize(total.length = sum(lengthkm))

# cumulative sum
fl.ord.sum$cumulative.length = cumsum(fl.ord.sum$total.length)
fl.ord.sum.br$cumulative.length = cumsum(fl.ord.sum.br$total.length)

# sum by hydrologic class 
fl.class.sum = fl.df %>%
               group_by(Final_Hydro_Class) %>%
               summarize(total.length = sum(lengthkm))

fl.class.sum.br = fl.df.br %>%
                  group_by(Brinkerhoff_Hydro_Class) %>%
                  summarize(total.length = sum(lengthkm))
fl.class.sum.br$Brinkerhoff_Hydro_Class[which(is.na(fl.class.sum.br$Brinkerhoff_Hydro_Class))]=4

# add columns for dataset
fl.class.sum$dataset = "Original NHDPlus"
fl.class.sum.br$dataset = "Brinkerhoff et al. (2024)"
colnames(fl.class.sum.br)[1] = "Final_Hydro_Class"
fl.class.sum2 = rbind(fl.class.sum, fl.class.sum.br)

# add column for types
detail.labs = c("Isolated","Non-isolated ephemeral","Non-isolated intermittent",
                "Non-isolated perennial")
fl.class.sum2$type = c(detail.labs, detail.labs)

# make plot
ggplot(fl.class.sum2, aes(y=type, 
                          x=total.length,
                          fill=dataset)) + 
       geom_bar(stat="identity",position="dodge") +
       scale_x_continuous(labels=scales::comma) + 
       labs(y="",x="Total Length (km)",fill="Dataset")

# totals for isolated, eph, int, per
types = c("Isolated","Ephemeral","Intermittent","Perennial")
lengths.org = c(1491,2266,126675,62248)
cum.lengths = cumsum(lengths.org)
hydro.df = data.frame(type = types,
                      length = lengths.org,
                      cum.length = cum.lengths)
hydro.df$group = "Flow permanence &\nconnectivity class"
hydro.df$type.detail = detail.labs

# repeat hydro df for each stream order
hydro.df.rep = data.frame(matrix(nrow=0,ncol=6))
colnames(hydro.df.rep) = c(colnames(hydro.df),"streamorde")
for (i in 1:10) {
  hydro.df.add = hydro.df
  hydro.df.add$streamorde = i
  hydro.df.rep = rbind(hydro.df.rep, hydro.df.add)
}

# plot in supplementary information
fl.ord.sum$group = "Cumulative length based\non hydrologic classification"
p1 = ggplot() + 
     geom_line(data=fl.ord.sum, 
               aes(x=factor(streamorde), 
                   y=cumulative.length,
                   group=group),
               lty="solid",
               color="black",
               size=0.9) + 
     labs(x="Stream Order", y= "Cumulative Flowline Length (km)") +
     scale_y_continuous(labels=scales::comma,limits=c(0,200000)) +
     geom_line(data=hydro.df.rep,
               aes(x=factor(streamorde),
                   y=cum.length, 
                   color=factor(type.detail, levels=rev(detail.labs)),
                   lty=factor(type.detail, levels=rev(detail.labs)),
                   group=factor(type.detail, levels=rev(detail.labs))),
               size=0.9) +
     guides(color=guide_legend(title="Cumulative length up to\nhydrologic classification"),
            lty=guide_legend(title="Cumulative length up to\nhydrologic classification")) +
     theme(text = element_text(size=14),
           legend.key.size = unit(0.7,'cm')) +
     scale_linetype_manual(values=c("dotted","dashed","dotdash","longdash"))
p2 = ggplot() + 
     geom_line(data=fl.ord.sum, 
               aes(x=factor(streamorde), 
                   y=cumulative.length,
                   group=group,
                   lty="Cumulative length\nbased on stream order"),
               color="black",
               size=0.9) + 
     labs(x="Stream Order", y= "Cumulative Flowline Length (km)") +
     scale_y_continuous(labels=scales::comma,limits=c(0,200000)) +
     guides(lty=guide_legend(title="")) +
     theme(text = element_text(size=14),
           legend.key.size = unit(0.7,'cm'))  
p1
p2
setwd(path_to_gitrepo)
ggsave("Flowline_Analyses/FigureD1_CumulativeLengthRaw.jpeg", 
       plot = p1, width = 20, height = 12, units="cm", dpi=600)
ggsave("Flowline_Analyses/FigureD1_SolidCumulativeLengthForLegend.jpeg", 
       plot = p2, width = 20, height = 12, units="cm", dpi=600)

# total length in each hydrologic class by stream order
fl.cls.ord.sum = fl.df %>%
                 group_by(streamorde, Final_Hydro_Class) %>%
                 summarize(total.length = sum(lengthkm))
fl.cls.ord.sum.br = fl.df.br %>%
                  group_by(streamorde, Brinkerhoff_Hydro_Class) %>%
                  summarize(total.length = sum(lengthkm))
fl.cls.ord.sum.br$Brinkerhoff_Hydro_Class[which(is.na(fl.cls.ord.sum.br$Brinkerhoff_Hydro_Class))]=4
colnames(fl.cls.ord.sum.br)[2] = "Final_Hydro_Class"

# combine datasets
datasets = c("Original NHDPlus","Brinkerhoff et al. (2024)")
fl.cls.ord.sum$dataset = "Original NHDPlus"
fl.cls.ord.sum.br$dataset = "Brinkerhoff et al. (2024)"
fl.cls.ord.sum2 = rbind(fl.cls.ord.sum, fl.cls.ord.sum.br)
fl.cls.ord.sum2$type.detail = rep(0, nrow(fl.cls.ord.sum2))
for (i in 1:4) { fl.cls.ord.sum2$type.detail[which(fl.cls.ord.sum2$Final_Hydro_Class == i)] = detail.labs[i] }
fl.cls.ord.sum2$cumulative.length = rep(0, nrow(fl.cls.ord.sum2))
fl.cls.ord.cumsum2 = data.frame(matrix(nrow=0,ncol=6))
for (i in 1:2) {
  i.df = fl.cls.ord.sum2[which(fl.cls.ord.sum2$dataset == datasets[i]),]
  for (j in 1:4) {
    ij.df = i.df[which(i.df$Final_Hydro_Class == j),]
    ij.df$cumulative.length = cumsum(ij.df$total.length)
    fl.cls.ord.cumsum2 = rbind(fl.cls.ord.cumsum2, ij.df)
  }
}
ggplot(fl.cls.ord.sum2, aes(y=total.length,
                           x=factor(streamorde),
                           group=interaction(type.detail, dataset),
                           color=type.detail,
                           linetype=dataset)) + 
             geom_line(linewidth=0.8) + 
             labs(x="Stream Order", y="Length (km)",
                  linetype="Dataset",color="Hydrologic Classification") +
             scale_y_continuous(labels=scales::comma)
ggplot(fl.cls.ord.cumsum2, aes(y=cumulative.length,
                            x=factor(streamorde),
                            group=interaction(type.detail, dataset),
                            color=type.detail,
                            linetype=dataset)) + 
      geom_line(linewidth=0.8) + 
      labs(x="Stream Order", y="Cumulative Length (km)",
           linetype="Dataset",color="Hydrologic Classification") +
      scale_y_continuous(labels=scales::comma,
                         limits=c(0,150000))
