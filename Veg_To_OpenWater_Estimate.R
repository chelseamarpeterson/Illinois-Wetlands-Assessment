setwd("C:/Users/Chels/OneDrive - University of Illinois - Urbana/Illinois Wetlands Risk Assessment/Results")

library(dplyr)

# read in file
int.df = read.csv("IL_WS_Step17_AreaThres_OpenWaterInt.csv")

# multiply area by percent converted
int.df$Area_Open = int.df$Shape_Area*int.df$PercentOpenWater/100

# sum area converted by cowardin type
sum.open = int.df %>%
           group_by(WETLAND_TYPE) %>%
           summarize(tot.open = sum(Area_Open)/10^4)

# sum area converted by cowardin type and water regim
sum.open = int.df %>%
            group_by(WETLAND_TYPE, WATER_REGI) %>%
            summarize(tot.open = sum(Area_Open)/10^4)
