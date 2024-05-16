## Project title 
Efficacy of Probiotic Lactobacillus rhamnosus in Controlling Human Intestinal Abundance of Pathogenic Ruminococcus gnavus. 


## Project description
Read counts of Ruminococcus gnavus collected from stool samples of 21 subjects. 
This analysis aims to quantify the ability of L. rhamnosus to counteract increase in abundance of R. gnavus,
and identify if subject gender affects probiotic treatment.

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
|effectsize|includes Cohens d
|ggpubr|for enhancing plot appearance.
|ggplot2| Used for making complex plots from data frames 
|car|for qq plot for model visualisation
|see|for qq plot for model visualisation
|scales|for data visualisation
|purrr|for working with functions and vectors
|ggtext| used for improved text rendering support 
|gghighlight|highlighting specified data by creating a new data frame for each layer 


## Data
Read counts of Ruminococcus gnavus collected from stool samples of 21 subjects. 

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





