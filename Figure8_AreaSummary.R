setwd("C:/Users/Chels/OneDrive - University of Illinois - Urbana/Illinois Wetlands Risk Assessment/Results")

library(ggplot2)
library(patchwork)
library(readxl)

# read in excel file
sum.area.df = read_excel("AreaBreakdown.xlsx")

# remove extra rows
sum.area.df = data.frame(sum.area.df[1:3,])
colnames(sum.area.df) = rep(c("label","area"),6)

# make sep dataframes
hist.df = sum.area.df[1:3,3:4]; hist.df$group = "Historical loss"
cwa.df = sum.area.df[1:3,5:6]; cwa.df$group = "CWA coverage"
pro.df = sum.area.df[1:2,7:8]; pro.df$group = "Protection status"
veg.df = sum.area.df[1:3,9:10]; veg.df$group = "Unprotected vegetation types"
type.df = sum.area.df[1:3,11:12]; type.df$group = "Protection level"

# stack dataframe
df.stack = rbind(hist.df, cwa.df, pro.df, veg.df, type.df)

# for each group, calculate the percentage of the total
groups = c("Historical loss","CWA coverage","Protection status",
           "Unprotected vegetation types","Protection level")
n.g = length(groups)
df.stack$percent = rep(0, nrow(df.stack))
df.stack$area.pos = rep(0, nrow(df.stack))
for (i in 1:n.g) {
  grp.ind = which(df.stack$group == groups[i])
  grp.df = df.stack[grp.ind,]
  df.stack[grp.ind,"percent"] = grp.df$area/sum(grp.df$area)*100 
  n.a = nrow(grp.df)
  rev.area = cumsum(rev(grp.df$area))
  df.stack[grp.ind,"area.pos"] = rev(c(0,rev.area[1:(n.a-1)]) + 0.5*(rev.area-c(0,rev.area[1:(n.a-1)])))
}

# format percent
df.stack$percent.format = rep(0, nrow(df.stack))
for (i in 1:nrow(df.stack)) {
  df.stack$percent.format[i] = paste(sprintf("%.1f", df.stack$percent[i]),"%",sep="")
}
df.stack$x = rep("center", nrow(df.stack))

# historical loss
p.hist = ggplot(df.stack[which(df.stack$group == "Historical loss"),],
                 aes(x=x, y=area, fill=label)) +
                 geom_bar(position="stack",stat="identity") +
                 geom_text(aes(x=x, y=area.pos, label=percent.format), 
                           color="white", size=8, fontface="bold") +
                 labs(x="",y="Area ") + 
                 geom_hline(yintercept=0,color="black") +
                 scale_y_continuous(name="Wetland Area (ha)",
                                    labels=scales::comma,
                                    breaks=c(0,397186,497764,3323284),
                                    limits = c(0,3323284)) +
                 guides(fill=guide_legend(title="Time Period")) + 
                 theme(axis.ticks.x=element_blank(),
                       axis.title.x=element_blank(), 
                       text = element_text(size=26),
                       panel.grid = element_blank(),
                       panel.background = element_rect(fill = "white")) +
                 scale_fill_manual(values=c("coral1","coral3","coral4"))
p.hist
setwd("C:/Users/Chels/OneDrive - University of Illinois - Urbana/Illinois Wetlands Risk Assessment/Documents/Figures")
ggsave("Summary/HistoricalLoss.png", plot = p.hist, 
       width=21.5, height=36, units="cm") 

cwa.labels = df.stack[which(df.stack$group == "CWA coverage"),"label"]
df.cwa = df.stack[which(df.stack$group == "CWA coverage"),]
df.cwa$area.pos[1] = df.cwa$area.pos[1]-3900
p.cwa=ggplot(df.cwa,
        aes(x=x, y=area, 
            fill=factor(label, level=cwa.labels))) +
        geom_bar(position="stack",stat="identity") +
        geom_text(aes(x=x, y=area.pos, label=percent.format), 
                  color="white", size=8, fontface="bold") +
        geom_hline(yintercept=0,color="black") +
        scale_y_continuous(name="Wetland Area (ha)",
                           labels=scales::comma,
                           breaks=c(0,286342,397186),
                           limits = c(0,398000)) +
        guides(fill=guide_legend(title="Jurisdictional Status at\nSeasonally Flooded Cutoff")) + 
        theme(axis.ticks.x=element_blank(),
              axis.title.x=element_blank(), 
              text = element_text(size=26),
              panel.grid = element_blank(),
              panel.background = element_rect(fill = "white")) +
        scale_fill_manual(values=c("skyblue1","dodgerblue3","blue4"))
p.cwa
ggsave("Summary/CWACoverage.png", plot = p.cwa, 
       width=22.75, height=36, units="cm") 

pro.labels = df.stack[which(df.stack$group == "Protection status"),"label"]
p.pro = ggplot(df.stack[which(df.stack$group == "Protection status"),],
              aes(x=x, y=area, 
                  fill=factor(label, level=pro.labels))) +
              geom_bar(position="stack",stat="identity") +
              geom_text(aes(x=x, y=area.pos, label=percent.format), 
                        color="white", size=8, fontface="bold") +
              geom_hline(yintercept=0,color="black") +
              scale_y_continuous(name="Wetland Area (ha)",
                                 labels=scales::comma,
                                 breaks=c(0,227642,286342),
                                 limits = c(0,286342)) +
              guides(fill=guide_legend(title="Protection Status")) + 
              theme(axis.ticks.x=element_blank(),
                    axis.title.x=element_blank(), 
                    text = element_text(size=26),
                    panel.grid = element_blank(),
                    panel.background = element_rect(fill = "white")) +
              scale_fill_manual(values=c("goldenrod2","darkgoldenrod"))
p.pro
ggsave("Summary/ProtectionStatus.png", plot = p.pro, 
       width = 19.25, height = 36, units="cm") 

veg.labels = df.stack[which(df.stack$group == "Unprotected vegetation types"),"label"]
p.veg = ggplot(df.stack[which(df.stack$group == "Unprotected vegetation types"),],
               aes(x=x, y=area, 
                   fill=factor(label, level=veg.labels))) +
                geom_bar(position="stack",stat="identity") +
                geom_text(aes(x=x, y=area.pos, label=percent.format), 
                          color="white", size=7, fontface="bold") +
                geom_hline(yintercept=0,color="black") +
                scale_y_continuous(name="Wetland Area (ha)",
                                   labels=scales::comma,
                                   breaks=c(0,203124,220348,227642),
                                   limits = c(0,227642)) +
                guides(fill=guide_legend(title="Wetland Type")) + 
                theme(axis.ticks.x=element_blank(),
                      axis.title.x=element_blank(), 
                      text = element_text(size=24),
                      panel.grid = element_blank(),
                      panel.background = element_rect(fill = "white")) +
                scale_fill_manual(values=c("olivedrab3","olivedrab","darkgreen"))
ggsave("Summary/VegetationTypes.png", plot = p.veg, 
       width = 20, height = 30, units="cm")

lev.labels = df.stack[which(df.stack$group == "Protection level"),"label"]
p.lev = ggplot(df.stack[which(df.stack$group == "Protection level"),],
               aes(x=x, y=area, 
                   fill=factor(label, level=lev.labels))) +
                geom_bar(position="stack",stat="identity") +
                geom_text(aes(x=x, y=area.pos, label=percent.format), 
                          color="white", size=4, fontface="bold") +
                geom_hline(yintercept=0,color="black") +
                scale_y_continuous(name="Wetland Area (ha)",
                                   labels=scales::comma,
                                   breaks=c(0,38698,52318,58701),
                                   limits = c(0,58701)) +
                guides(fill=guide_legend(title="Protection Level")) + 
                theme(axis.ticks.x=element_blank(),
                      axis.title.x=element_blank(), 
                      text = element_text(size=16),
                      panel.grid = element_blank(),
                      panel.b.ackground = element_rect(fill = "white")) +
                scale_fill_manual(values=c("plum3","mediumpurple2","darkorchid4"))
ggsave("Summary/ProtectionLevels.png", plot = p.lev, 
       width = 14, height = 8, units="cm")









