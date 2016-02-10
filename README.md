#Climate at the local and landscape level drive adaptive genetic responses associated with survival in big sagebrush (Artemisia tridentata)


[Lindsay Chaney] (http://www.lindsaychaney.com) and [Bryce Richardson] (http://www.fs.fed.us/rmrs-beta/people/richardson-bryce)

This repository contains all the code used in the manuscript:

enter citation here

**Abstract:** 	A genecological approach was used to explore genetic variation in survival in in Artemisia tridentata (big sagebrush). Artemisia tridentata is a fundamental shrub species in western North America, large declines of this species and its affect on associated wildlife has resulted in increased conservation and restoration efforts. Common garden experiments were established at three sites with seedlings from 55 source-populations. Populations included each of the three predominant subspecies, and cytotype variation within subspecies. Survival was monitored for five-years to assess differences in survival between gardens and populations. We found evidence of adaptive genetic variation for survival in A. tridentata. Survival within gardens differed by population and a large proportion of this variation is explained by climate of the seed source. Plants from more continental climates (greater summer-winter temperature differences) had the highest levels of survival, while populations from warmer and drier sites had the lowest levels of survival. Survival differed between gardens as result of minimum temperatures. These climatic differences among gardens are likely to be associated with atmospheric decoupling and/or cold air drainage. Understanding how genetic variation is arrayed across the landscape and its association with climate can greatly enhance the success of restoration and conservation.

##Data

Data has been archived for public access at Dryad (link)

To cite this data:

```
data citation
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

For archival purposes, the code used to produce figures for publication has been lodged with figshare here (link).

To cite this code:

```
citation
```
