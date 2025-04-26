# Illinois-Wetlands-Assessment
Public repository for data files and R code used to quantify area of unprotected non-WOTUS Illinois wetlands and evaluate potential impacts on vulnerable communities. The repository is organized by folder, with each representing a separate pre-processing or analysis step

## ArcPro_Models
Folder that contains ArcToolbox file (.atbx) with all model builder files to:
(1) process NHD data to create WOTUS stream water networks with varying buffer distances and flow permanence levels
(2) process NWI data to subset of potentially jurisdictional wetlands
(3) intersect wetland layer with leveed areas and WOTUS networks to identify non-WOTUS wetlands
(4) intersect wetland layer with county boundaries and USGS PAD-US database to identify unprotected non-WOTUS wetlands
(5) intersect wetland layer with CEJST data to evaluate intersection of unprotected wetlands with vulnerable communities.

## Area_Estimates
Folder with script to estimate non-WOTUS wetland area as a function of the wetland flood frequency. It also generates plots for the non-WOTUS wetland area by reason for lack of federal jurisdiction, protection level, and Cowardin wetland type. Another folder contains non-WOTUS and protected wetland area estimates from previous studies, which are incorporated into the figures.

[1] Levin, G.A., Suloway, L., Plocher, A.E., Hutto, F.R., Miner, J.J., Phillips, C.A., Agarwal, J., Yiching, L., 2002. Status and Functions of Isolated Wetlands in Illinois, INHS Special Publications. Illinois Natural History Survey.

[2] Gold, A.C., 2024. How wet must a wetland be to have federal protections in post-Sackett US? Science 385, 1450–1453. https://doi.org/10.1126/science.adp3222

[3] Simmons, B.A., Beck, M.W., Flaherty-Walia, K., Lewis, J., Sherwood, E.T., 2024. A murky ruling threatens the fate of millions of US wetlands. Wetlands 44, 47. https://doi.org/10.1007/s13157-024-01801-y


