# Illinois-Wetlands-Assessment
Public repository for ArcPro models and R code used to quantify area of unprotected non-WOTUS Illinois wetlands and evaluate potential impacts on vulnerable communities. The repository is organized by folder, with each representing a separate pre-processing or analysis step

## ArcPro_Models
Folder that contains ArcToolbox (.atbx) with all model builder files to:

(1) process NHD data to create WOTUS stream water networks with varying buffer distances and flow permanence levels

(2) process NWI data to subset of potentially jurisdictional wetlands

(3) intersect wetland layer with leveed areas and WOTUS networks to identify non-WOTUS wetlands

(4) intersect wetland layer with county boundaries and USGS PAD-US database to identify unprotected non-WOTUS wetlands

(5) intersect wetland layer with CEJST data to evaluate intersection of unprotected wetlands with vulnerable communities

Required input files for Illinois: 

(1) National Wetlands Inventory (NWI) shapefile: https://www.fws.gov/program/national-wetlands-inventory/download-state-wetlands-data

(2) National Hydrography Dataset (NHD) Plus High Resolution: https://hydro.nationalmap.gov/arcgis/rest/services/NHDPlus_HR/MapServer

(3) Leveed Areas shapefile from the National Levee Database (NLD): https://geospatial.sec.usace.army.mil/server/rest/services/NLD2_PUBLIC/MapServer

(4) USGS Protected Areas Database for the US (PAD-US): https://www.usgs.gov/programs/gap-analysis-project/science/pad-us-data-download

(5) 2023 National Land Cover Dataset: https://www.mrlc.gov/data?f%5B0%5D=project_tax_term_term_parents_tax_term_name%3AAnnual%20NLCD

(6) TIGER/Line shapefiles for Illinois counties and state boundary: https://www.census.gov/cgi-bin/geo/shapefiles/index.php

Optional input files:

(1) NWI Difference Product vector and raster layers: https://www.fws.gov/project/national-wetlands-inventory-difference-product-line

## Area_Estimates
Folder with script to estimate non-WOTUS wetland area as a function of the wetland flood frequency. It also generates plots for the non-WOTUS wetland area by reason for lack of federal jurisdiction, protection level, and Cowardin wetland type. Another folder contains non-WOTUS and protected wetland area estimates from previous studies, which are incorporated into the figures.

[1] Levin, G.A., Suloway, L., Plocher, A.E., Hutto, F.R., Miner, J.J., Phillips, C.A., Agarwal, J., Yiching, L., 2002. Status and Functions of Isolated Wetlands in Illinois, INHS Special Publications. Illinois Natural History Survey.

[2] Gold, A.C., 2024. How wet must a wetland be to have federal protections in post-Sackett US? Science 385, 1450–1453. https://doi.org/10.1126/science.adp3222

[3] Simmons, B.A., Beck, M.W., Flaherty-Walia, K., Lewis, J., Sherwood, E.T., 2024. A murky ruling threatens the fate of millions of US wetlands. Wetlands 44, 47. https://doi.org/10.1007/s13157-024-01801-y

## CEJST_Analysis
Contains R scripts for (1) summing unprotected non-WOTUS wetland area in each IL census tract and writing results to .csv files for visualization, (2) running statistical analyses evaluating the relationship between unprotected wetland area and CEJST fields, including FLD_ET, N_CLT_EOMI, and FLD_PFS.

## County_Statistics
Contains R scripts for for estimating county-level unprotected non-WOTUS wetland area and the corresponding uncertainty in aggregate and by wetland type. These results are written to .csv files which can be joined with county maps in ArcPro. 

## Flowline_Analyses
Contains R scripts for (1) classifying unknown flowlines using connectivity rules in Table C3 and (2) quantifying flowline length in each category (phemeral, intermittent, perennial), and plotting those results.

## Summary_Figures
Contains .csv file and R script for plotting the Illinois wetland area breakdown across five categories: (1) historical change, (2) jurisdictional status (WOTUS v. non-WOTUS), (3) protection status (protected v. unprotected), (4) protection level of protected areas, and (5) wetland types of unprotected areas. 


