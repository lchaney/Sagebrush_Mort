#Adaptive genetic response to climate affects survival in Artemisia tridentata (big sagebrush)

[Lindsay Chaney] (http://www.lindsaychaney.com) and [Bryce Richardson] (http://www.fs.fed.us/rmrs-beta/people/richardson-bryce)

This repository contains all the code used in the manuscript:

enter citation here

**Abstract:** Abstract text here.

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
