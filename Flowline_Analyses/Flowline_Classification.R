setwd('C:/Users/Chels/OneDrive - University of Illinois - Urbana/Illinois Wetlands Risk Assessment/Wetland Mapping/Flowline Classification')

# read in flowline table
flowline.df = read.csv("NHDFlowline_IL_Step4_HydroClass.csv")

# count number of zero flowlines
sum(flowline.df$Hydro_Class == 0)

# change zeros to NAs
flowline.df$Hydro_Class[which(flowline.df$Hydro_Class == 0)] = NA
sum(is.na(flowline.df$Hydro_Class))

# assign downstream hydrologic class
flowline.df$Downstream_Hydro_Class = rep(NA, nrow(flowline.df))
for (i in 1:nrow(flowline.df)) {
  dn.major.seq.i = flowline.df[i,"dnhydroseq"]
  dn.minor.seq.i = flowline.df[i,"dnminorhyd"]
  
  dn.major.class.i = flowline.df[which(flowline.df$hydroseq == dn.major.seq.i),"Hydro_Class"]
  dn.minor.class.i = flowline.df[which(flowline.df$hydroseq == dn.minor.seq.i),"Hydro_Class"]

  if (dn.major.seq.i != 0) {
    if (length(dn.major.class.i) > 0) {
      flowline.df[i,"Downstream_Hydro_Class"] = dn.major.class.i
    }
  } else if (dn.minor.seq.i != 0) {
    if (length(dn.minor.class.i) > 0) {
      flowline.df[i,"Downstream_Hydro_Class"] = dn.minor.class.i
    }
  }
}
sum(flowline.df$Downstream_Hydro_Class[-which(is.na(flowline.df$Downstream_Hydro_Class))] == 0)

# assign upstream hydrologic class
flowline.df$Upstream_Hydro_Class = rep(NA, nrow(flowline.df))
for (i in 1:nrow(flowline.df)) {
  up.seq.i = flowline.df[i,"uphydroseq"]
  up.class.i = flowline.df[which(flowline.df$hydroseq == up.seq.i),"Hydro_Class"]
  if (up.seq.i != 0) {
    if (length(up.class.i) > 0) {
      flowline.df[i,"Upstream_Hydro_Class"] = up.class.i
    }
  }
}
sum(flowline.df$Upstream_Hydro_Class[-which(is.na(flowline.df$Upstream_Hydro_Class))] == 0)

# assign final hydrologic class
flowline.df$Final_Hydro_Class = rep(NA, nrow(flowline.df))
for (i in 1:nrow(flowline.df)) {
  hydro.class.i = flowline.df[i,"Hydro_Class"]
  up.class.i = flowline.df[i,"Upstream_Hydro_Class"]
  down.class.i = flowline.df[i,"Downstream_Hydro_Class"]
  if (!is.na(hydro.class.i)) {
    flowline.df[i,"Final_Hydro_Class"] = hydro.class.i
  } else if ((!is.na(down.class.i)) & (!is.na(up.class.i))) {
    if (down.class.i == up.class.i) {
      flowline.df[i,"Final_Hydro_Class"] = down.class.i
    } else if (down.class.i < up.class.i) {
      flowline.df[i,"Final_Hydro_Class"] = up.class.i
    } else if (down.class.i > up.class.i) {
      flowline.df[i,"Final_Hydro_Class"] = down.class.i
    }
  } else if (!is.na(down.class.i)) {
    if (down.class.i < 3) {
      flowline.df[i,"Final_Hydro_Class"] = down.class.i + 1
    } else if (down.class.i == 3) {
      flowline.df[i,"Final_Hydro_Class"] = down.class.i
    }
  } else if (!is.na(up.class.i)) {
    flowline.df[i,"Final_Hydro_Class"] = up.class.i
  }
}

# check if hydro class and final hydro class is equal for non na entries
sum(!is.na(flowline.df$Hydro_Class))
sum(flowline.df$Hydro_Class[-which(is.na(flowline.df$Hydro_Class))] == flowline.df$Final_Hydro_Class[-which(is.na(flowline.df$Hydro_Class))])

# put code into loop
hydro.class.na = c(sum(is.na(flowline.df$Hydro_Class)), sum(is.na(flowline.df$Final_Hydro_Class)))
flowline.df$Downstream_Hydro_Class2 = flowline.df$Downstream_Hydro_Class
flowline.df$Upstream_Hydro_Class2 = flowline.df$Upstream_Hydro_Class
flowline.df$Final_Hydro_Class2 = flowline.df$Final_Hydro_Class
for (k in 2:25) {
  # assign downstream hydrologic class
  downstream.na.rows = which(is.na(flowline.df$Downstream_Hydro_Class2))
  for (i in downstream.na.rows) {
    dn.major.seq.i = flowline.df[i,"dnhydroseq"]
    dn.minor.seq.i = flowline.df[i,"dnminorhyd"]
    dn.major.class.i = flowline.df[which(flowline.df$hydroseq == dn.major.seq.i),"Final_Hydro_Class2"]
    dn.minor.class.i = flowline.df[which(flowline.df$hydroseq == dn.minor.seq.i),"Final_Hydro_Class2"]
    if (dn.major.seq.i != 0) {
      if (length(dn.major.class.i) > 0) {
        flowline.df[i,"Downstream_Hydro_Class2"] = dn.major.class.i
      }
    } else if (dn.minor.seq.i != 0) {
      if (length(dn.minor.class.i) > 0) {
        flowline.df[i,"Downstream_Hydro_Class2"] = dn.minor.class.i
      }
    }
  }
  
  # assign upstream hydrologic class
  upstream.na.rows = which(is.na(flowline.df$Upstream_Hydro_Class2))
  for (i in upstream.na.rows) {
    up.seq.i = flowline.df[i,"uphydroseq"]
    up.class.i = flowline.df[which(flowline.df$hydroseq == up.seq.i),"Final_Hydro_Class2"]
    if (up.seq.i != 0) {
      if (length(up.class.i) > 0) {
        flowline.df[i,"Upstream_Hydro_Class2"] = up.class.i
      }
    }
  }
  
  # assign final hydrologic class
  final.na.rows = which(is.na(flowline.df$Final_Hydro_Class2))
  for (i in final.na.rows) {
    hydro.class.i = flowline.df[i,"Final_Hydro_Class2"]
    up.class.i = flowline.df[i,"Upstream_Hydro_Class2"]
    down.class.i = flowline.df[i,"Downstream_Hydro_Class2"]
    if (!is.na(hydro.class.i)) {
      flowline.df[i,"Final_Hydro_Class2"] = hydro.class.i
    } else if ((!is.na(down.class.i)) & (!is.na(up.class.i))) {
      if (down.class.i == up.class.i) {
        flowline.df[i,"Final_Hydro_Class2"] = down.class.i
      } else if (down.class.i < up.class.i) {
        flowline.df[i,"Final_Hydro_Class2"] = up.class.i
      } else if (down.class.i > up.class.i) {
        flowline.df[i,"Final_Hydro_Class2"] = down.class.i
      }
    } else if (!is.na(down.class.i)) {
      if (down.class.i < 3) {
        flowline.df[i,"Final_Hydro_Class2"] = down.class.i + 1
      } else if (down.class.i == 3) {
        flowline.df[i,"Final_Hydro_Class2"] = down.class.i
      }
    } else if (!is.na(up.class.i)) {
      flowline.df[i,"Final_Hydro_Class2"] = up.class.i
    }
  }
  hydro.class.na = c(hydro.class.na, sum(is.na(flowline.df$Final_Hydro_Class2)))
}
which(hydro.class.na == min(hydro.class.na))

# count = 25, took 20 iterations to converge
flowline.df$OBJECTID = seq(1, nrow(flowline.df))
out.df = flowline.df[,c("OBJECTID","permanent_identifier","nhdplusid","Hydro_Class","Downstream_Hydro_Class2","Upstream_Hydro_Class2","Final_Hydro_Class2")]
colnames(out.df) = c("OBJECTID","permanent_identifier","nhdplusid","Hydro_Class","Downstream_Hydro_Class","Upstream_Hydro_Class","Final_Hydro_Class")
write.csv(out.df, "NHDFlowline_IL_Step5_HydroClass.csv",row.names = F, na="")

# number of isolated flowlines that couldn't be classified
na.df = out.df[is.na(out.df$Hydro_Class),]

# perennial
sum(out.df[-which(is.na(out.df$Final_Hydro_Class)),"Final_Hydro_Class"] == 1)
sum(na.df[-which(is.na(na.df$Final_Hydro_Class)),"Final_Hydro_Class"] == 1)

# intermittent
sum(out.df[-which(is.na(out.df$Final_Hydro_Class)),"Final_Hydro_Class"] == 2)
sum(na.df[-which(is.na(na.df$Final_Hydro_Class)),"Final_Hydro_Class"] == 2)

# ephemeral
sum(out.df[-which(is.na(out.df$Final_Hydro_Class)),"Final_Hydro_Class"] == 3)
sum(na.df[-which(is.na(na.df$Final_Hydro_Class)),"Final_Hydro_Class"] == 3)

# totals
sum(out.df[-which(is.na(out.df$Final_Hydro_Class)),"Final_Hydro_Class"] == 1) + sum(out.df[-which(is.na(out.df$Final_Hydro_Class)),"Final_Hydro_Class"] == 2) + sum(out.df[-which(is.na(out.df$Final_Hydro_Class)),"Final_Hydro_Class"] == 3)
nrow(out.df)
nrow(out.df[-which(is.na(out.df$Final_Hydro_Class)),])
