#Adaptive genetic response to climate affects survival in Artemisia tridentata (big sagebrush)

[Lindsay Chaney] (http://www.lindsaychaney.com) and [Bryce Richardson] (http://www.fs.fed.us/rmrs/people/profile.php?alias=brichardson02)

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

To be able to rerun analysis, the first step is to make sure all data files (there are 3) are downloaded.
Place all data (csv files) into a folder named Data. Analysis scripts (.R files) will
be run in a folder named Analysis. Set your working directory to the folder that contains these two folders.

From your R console, source the Do script (`04_SM_do.R`) to run all analysis.
This script sources scripts to Load data (`01_SM_load.R`) to Clean data (`02_SM_clean.R`) and 
to perform analsyses (`03_SM_func.R`) [Note that the Functions script has been broken up for each 
different part of the analysis, including installing and loading packages (`03_SM_func_pack.R`.]

Currently there is not a script to save outputs.

###Report of R figures and results

To reprodcue R figures and results from statistical analysis, a markdown report has been made (`Sagebrush_Mort.Rmd`).

To run the report in RStudio the “Knit” command (Ctrl+Shift+K) will render the document and display a preview of it.

If you are not using RStudio then you simply need to call the render function for rmarkdown:

```
rmarkdown::render("Sagebrush_Mort.Rmd")
```

If this does not work, make sure the R Markdown package is installed from CRAN as follows:

```
install.packages("rmarkdown")
```

## Citation

For archival purposes, the code used to produce figures for publication has been lodged with figshare here (link).

To cite this code:

```
citation
```
