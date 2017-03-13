#Climate drives adaptive genetic responses associated with survival in big sagebrush (Artemisia tridentata)


[Lindsay Chaney] (http://www.lindsaychaney.com), [Bryce Richardson] (http://www.fs.fed.us/rmrs-beta/people/richardson-bryce), and [Matt Germino] (https://fresc.usgs.gov/people/Profile.aspx?Emp_ID=1204)

This repository contains all the code used in the manuscript:

```
Chaney, L., Richardson, B. A., & Germino, M. J. (2016). Climate drives adaptive genetic responses associated with survival in big sagebrush (Artemisia tridentata). Evolutionary Applications. http://dx.doi.org/10.1111/eva.12440
```

**Abstract:** 	A genecological approach was used to explore genetic variation for survival in Artemisia tridentata (big sagebrush). Artemisia tridentata is a widespread and foundational shrub species in western North America. This species has become extremely fragmented, to the detriment of dependent wildlife, and efforts to restore it are now a land management priority. Common-garden experiments were established at three sites with seedlings from 55 source-populations. Populations included each of the three predominant subspecies, and cytotype variations. Survival was monitored for 5 years to assess differences in survival between gardens and populations. We found evidence of adaptive genetic variation for survival. Survival within gardens differed by source-population and a substantial proportion of this variation was explained by seed climate of origin. Plants from areas with the coldest winters had the highest levels of survival, while populations from warmer and drier sites had the lowest levels of survival. Survival was lowest, 36%, in the garden that was prone to the lowest minimum temperatures. These results suggest the importance of climatic driven genetic differences and their effect on survival. Understanding how genetic variation is arrayed across the landscape, and its association with climate can greatly enhance the success of restoration and conservation.

##Data

Data has been archived for public access at Dryad (http://datadryad.org/resource/doi:10.5061/dryad.32s2t)

To cite this data:

```
Chaney L, Richardson BA, Germino MJ (2016) Data from: Climate drives adaptive genetic responses associated with survival in big sagebrush (Artemisia tridentata). Dryad Digital Repository. http://dx.doi.org/10.5061/dryad.32s2t
```

##Running

Create a parent folder that will be your working directory to run the analysis.
Next, make sure all data files (there are 3) are downloaded and place all data 
(csv files) into a folder named Data.Place all Analysis scripts (.R files found 
in this repository) will in a folder named Analysis -- you can simply download
the zip of the entire repository. Remember, you need to set your working 
directory to the location that contains the folders "Data" and "Analysis".

From your R console, source the Do script (`Analysis/04_SM_do.R`), this will perform all analysis.

This script sources scripts to Load data (`01_SM_load.R`) to Clean data (`02_SM_clean.R`) and 
to perform analyses (`03_SM_func.R`) [Note that the Functions script has been broken up for each 
different part of the analysis, including installing and loading packages (`03_SM_func_pack.R`.]

Figures generated in R will be saved as `.png` files in a folder called Output.

###Report of R figures and results

To reproduce R figures and results from statistical analysis, a html report is created using markdown (`Sagebrush_Mort.Rmd`).

To run the report in RStudio the “Knit” command (Ctrl+Shift+K) will render the document and display a preview of it.

## Citation

To cite this code:

```
Chaney L, Richardson BA, Germino MJ (2016) Analysis for: Climate drives adaptive genetic responses associated with survival in big sagebrush (Artemisia tridentata). Git Hub. https://github.com/lchaney/Sagebrush_Mort
```
