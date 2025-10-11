setwd("C:/Users/Chels/OneDrive - University of Illinois - Urbana/Illinois Wetlands Risk Assessment/JEMA-Paper-Repo")

library(ggplot2)
library(dplyr)
library(tidyr)

# read in csv 
il.df = read.csv("JD_Analysis/Illinois_JDs_202505141659.csv")

# make JD yes/no column
il.df$JD.TF = 1*(il.df$`Water.of.the.U.S.` == "Yes")
total.JD = nrow(il.df)
total.JD.true = sum(il.df$JD.TF)

# look at Cowardin descriptions
#sort(unique(il.df$Waters.Names))
#sort(unique(il.df$Resource.Types))
sort(unique(il.df$Cowardin.Code))
#sort(unique(il.df$Cowardin.Category))
#sort(unique(il.df$Cowardin.Description))
#sort(unique(il.df$HGM))
#sort(unique(il.pd.df$JD.Basis))
#il.df[il.df$Cowardin.Code == "PRB",c("Cowardin.Description")]

# combine all PAB, PEM, and PFO labels labels
pab.codes = c("PAB","PAB3","PAB4","PAB6")
pem.codes = c("PEM","PEM1","PEM2")
pfo.codes = c("PFO","PFO1")
il.df$Code.New = il.df$Cowardin.Code
il.df$Code.New[which(il.df$Code.New %in% pab.codes)] = "PAB"
il.df$Code.New[which(il.df$Code.New %in% pem.codes)] = "PEM"
il.df$Code.New[which(il.df$Code.New %in% pfo.codes)] = "PFO"
sort(unique(il.df$Code.New))

# update JD labels 
sort(unique(il.df$JD.Basis))
jd.labels = c("Navigable Waters\nProtection Rule",
              "Clean\nWater Rule",
              "Amended\n2023 Rule",
              "2023 Rule",
              "1986/88 Rule",
              "PRE2015_POSTSACKETT")
jd.order = c("NWPR",
             "CWR",
             "AMENDED_2023RULE",
             "2023RULE",
             "1986/88",
             "Pre-2015 Post-Sackett")
il.df$JD.Label = rep(0, nrow(il.df))
for (i in 1:6) { il.df$JD.Label[which(il.df$JD.Basis == jd.order[i])] = jd.labels[i] }

# remove non-wetland codes
wetland.codes = c("P","PAB","PEM","PFO","PML","POW","PRB","PSS","PUB")
il.df.wtld = subset(il.df, il.df$Code.New %in% wetland.codes)

# make column of ones
il.df.wtld$ones = rep(1, nrow(il.df.wtld))

# calculate jd proportion by cnty
il.df.cnty = il.df.wtld %>% 
             group_by(County, JD.Label) %>%
             summarize(JD.Percent = sum(JD.TF)/sum(ones))

# make wide dataframe
il.df.cnty.wide = spread(il.df.cnty, key = JD.Label, value = JD.Percent)
il.df.cnty.wide = il.df.cnty.wide[,c(1,seq(3,7))]

# export subset of columns for mapping
#write.csv(il.df.wtld[,c("Finalized.Date","District.or.Region","Longitude","Latitude","Code.New","County"])

################################################################################
# pond plot

# remove non ponds
pond.codes = c("POW","PUB","PRB","PAB")
il.df.wtld.pd = subset(il.df.wtld, il.df.wtld$Cowardin.Code %in% pond.codes)
total.pd.JD = nrow(il.df.wtld.pd)

# update Code and JD Labels
pond.labels = c("Palustrine Aquatic Bed","Palustrine Open Water",
                "Palustrine Rock Bottom","Palustrine Unconsolidated Bottom")
pond.code.order = c("PAB","POW","PRB","PUB")
il.df.wtld.pd$Code.Label = rep(0, nrow(il.df.wtld.pd))
for (i in 1:4) { il.df.wtld.pd$Code.Label[which(il.df.wtld.pd$Code.New == pond.code.order[i])] = pond.labels[i] }

# sum JD.TF by pond type and rule basis
il.pd.sum = il.df.wtld.pd %>%
            group_by(Code.Label, JD.Label) %>%
            summarize(JD.True = sum(JD.TF))

# calculate percentage for each category
il.pd.sum$JD.Percent = rep(0, nrow(il.pd.sum))
il.pd.sum$JD.Total = rep(0, nrow(il.pd.sum))
for (i in 1:4) {
  for (j in 1:6) {
    ij.sum = sum(1*((il.pd.df$Code.Label == pond.labels[i]) & (il.pd.df$JD.Label == jd.labels[j])))
    ij.id = which((il.pd.sum$Code.Label == pond.labels[i]) & (il.pd.sum$JD.Label == jd.labels[j]))
    il.pd.sum[ij.id,"JD.Total"] = ij.sum
    il.pd.sum[ij.id,"JD.Percent"] = il.pd.sum[ij.id,"JD.True"]/ij.sum*100
  }
}
il.pd.sum$Text = paste(il.pd.sum$JD.True, il.pd.sum$JD.Total, sep="/")

# plot number 
colnames(il.pd.sum)[3:4] = c("Jurisdictional Count","Jurisdictional Percent")
p1 = ggplot(il.pd.sum, aes(JD.Label, Code.Label, 
                      fill=`Jurisdictional Percent`)) + 
       geom_tile() + 
       geom_text(aes(label=Text), color="white") +
       labs(y="Cowardin Code", x="Agency Rule") +
       theme(text = element_text(size=12))
p1
ggsave("JD_Analysis/FigureB1_Pond_JD.jpg", 
       plot = p1, width = 24, height = 12, units="cm", dpi=600)

# plot results by date
il.df.wtld.pd = il.df.wtld.pd %>% separate(Finalized.Date, into=c("month","day","year"))
il.pd.year = il.df.wtld.pd %>%
             group_by(year, Code.Label) %>%
             summarize(JD.Total = sum(JD.TF))
colnames(il.pd.year)[3] = "Number of\njurisdictional JDs"
p2 = ggplot(il.pd.year, aes(x=year, y=`Number of\njurisdictional JDs`, 
                       group=Code.Label, color=Code.Label)) + 
       geom_point() + geom_line() +
       labs(x="Year") + guides(color=guide_legend(title="Cowardin Code"))
p2

