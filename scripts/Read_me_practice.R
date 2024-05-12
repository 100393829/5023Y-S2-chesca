#__read_me--- 
## Project title 
A graphical analysis of the fake covid data using different graphical techniques 


## Project description
A graphical analysis of the fake covid data to investigate different factors within the dataset. 
We started with cleaning the data by ensuring all of the column names are in snakecase, 
checking the columns for duplicates/incorrect data 
and making sure that all of the vraible names are assigned to the correct data type. 
Following this we then produced one figure each to present different variables. 

## Packages used  
| Package | Purpose|
|----|----|
|gitcreds| synchronising progress with github
|usethis| synchronising progress with github
|tidyverse| For importing, tidying, presenting and manipulating data
|GGally|to manage transformed data
|janitor| simple tools for cleaning and examining data 
|tidyr | for tidying data
|stringr|simplifies string manipulation 
|dplyr | Used for filtering, selecting columns, sorting data and adding and deleting columns 
|emmeans| for linear model fitting and analysis
|performance|for predictor evaluations
|skimr|for summary statistics 
|lmtest|for diagnostic linear model testing
|MASS|for robust linear models and data cleaning
|ggplot2| Used for making complex plots from data frames 
|car|for qq plot for model visualisation
|see|for qq plot for model visualisation
|scales|for data visualisation

|ggtext| used for improved text rendering support 
|gghighlight| highlighting specified data by creating a new data frame for each layer 


## Data

Acsv-formatted COVID-19 linelist, the data is a fake version of a covid linelist a data format for collecting case numbers and information for epidemiology. 
This data was originally provided by Batra, Neale et al. (2021), The Epidemiologist R Handbook. https://zenodo.org/badge/doi/10.5281/zenodo.4752646.svg

##Folder structure 
|Name | Usage|
|data| Stores the data csv file that is used in this analysis |
|figures| containing all figures throughout monovariate analysis and our final pngs
|scripts| Containing scripts and RMD file used for final HTML file 

### Variables
| Variable| Definition|
|----|----|
|Subject|subject ID|
|gender| Gender of person, Male or Female|
|treatment|Usage of placebo "Placebo" or Lactobacillus rhamnosus probiotic "LGG"|
|adbundance|Read count abunance of Ruminococcus gnavus |
|time|timepoint 1 = before, 2 = after|
|sample|sample number, 2 per patient|
|abundance_before|Read count abunance of Ruminococcus gnavus at timepoint 1, before|
|abundance_after|Read count abunance of Ruminococcus gnavus at timepoint 2, after|
|difference|Difference in read count abunance of Ruminococcus gnavus from timepoint 1, to timepoint 2|
