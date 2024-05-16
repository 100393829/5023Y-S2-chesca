#___packages----
library(usethis)#synchronising progress with github
library(gitcreds)#synchronising progress with github
library(tidyverse)#For importing, tidying, presenting and manipulating data
library(GGally)# to manage transformed data
library(janitor)# simple tools for cleaning and examining data 
library(tidyr)#package for data cleaning
library(stringr)#simplifies string manipulation 
library(dplyr)# Used for filtering, selecting columns, sorting data and adding and deleting columns 
library(emmeans)#for linear model fitting and analysis
library(performance)#for predictor evaluations
library(skimr)#for summary statistics 
library(lmtest)#for diagnostic linear model testing
library(MASS)#for robust linear models and data cleaning
library(effectsize)#Cohen's d
library(ggpubr)#for enhancing plot appearance.
library(ggplot2)#Used to make plots pretty
library(car)#for qq plot for model visualisation
library(see)#for qq plot for model visualisation
library(scales)#for data visualisation
library(purrr)#for working with functions and vectors
library(ggtext)#for customising gg plot text
library(gghighlight)#highlighting specified data by creating a new data frame for each layer 

#___loading_data----

probiotic <- read_csv ("/cloud/project/data/probiotic.csv")#R reads data from data folder

#___ data cleaning----
probiotic <- janitor::clean_names(probiotic)#sanke cases the name

probiotic <- rename(probiotic,
                    "abundance"="ruminococcus_gnavus_abund") #rename long variable name


#_____ changing variable classes----

probiotic$time <- as.factor(probiotic$time)#changing the class of variables: time, gender and group to factor
probiotic$gender <- as.factor(probiotic$gender)
probiotic$group <- as.factor(probiotic$group)

#____ changing typos----

probiotic <- probiotic %>%
  mutate(gender = case_when(#using mutate to select cases and renaming them to be Female.
    gender %in% c("F") ~ "Female",
    TRUE ~ gender
  )
  )#Changing the data to be more readable

probiotic <- probiotic %>%
  mutate(gender = case_when(#Using mutate to change select the Ms and rename them
    gender %in% c("M") ~ "Male",
    TRUE ~ gender
  )
  )

#___separating_abundance_to_before_and_after_and_adding_a_difference_column---- 

probiotic2<-probiotic[-41,]#removing outlier from subject 21 from the df *done after lsmodel04 creation due to leverage
probiotic3<- probiotic2[-41,]

difference <- probiotic%>%
  group_by(subject, gender, group)%>%#grouping the df by abundance before the treatment
  summarise(abundance_before= abundance[time==1],#creating aand renaming a before column by selecting timepoint 1
            abundance_after=abundance[time==2])%>%#creating an after column by selecting only timepoint 2
  mutate(difference = abundance_after - abundance_before)#adding a difference column to the dataset

difference2<-difference[-14,]

#____Figure_2---

group_gender_summary <- difference2 %>% 
  group_by(group, gender) %>% #grouping to count by group and gender later
  summarise(n=n(),#counting the number of each subject in each group and gender category
            n_distinct=n_distinct(subject)) %>% #counting by subject
  ungroup() %>% # needed to remove group calculations
  mutate(freq=n/sum(n))#finding the percentage frequency to plot

pal<- c("steelblue1","seagreen3")#creating a colour palette

proportion <- difference2%>% 
  ggplot(aes(x=group, fill= gender))+#Specifying x axis and colouring bars by gender
  geom_bar(position=position_dodge2(preserve="single"))+ #keeps bars to appropriate widths
  coord_flip()+#changing bar orientation
  labs(x="Treatment",#changing x and y axes labels
       y = "Number of Samples",
       title= "Sampling Bias in Gender and Treatment",#adding a title and subtitle
       subtitle = "Stool samples of 21 subjects",
       fill = "Gender")+ #capitalise legend title
  geom_text(data=group_gender_summary, # use the data from the summarise object
            aes(x=group,
                y= n, # offset text to be slightly to the right of bar
                group= gender, # need species group to separate text
                label=scales::percent(freq) # automatically add %
            ),
            position=position_dodge2(width=0.8))+ # set width of dodge
  scale_fill_manual(values=pal)+#change colours to specified palette
  theme_grey()+#setting the theme as minimal and setting the font
  theme(axis.text = element_text(color = "darkgrey", size = 10),# Changes the size of text on both axis 
        plot.title = element_text(lineheight = 0.8, size = 12),#sets size of title and makes it bold, sets lineheight
        plot.subtitle = element_text(size = 12),#sets subtitle size
        axis.ticks = element_line( color = "darkgrey"))#add axis ticks to the x and y axes, specify length and change to the same colour as the text

print(proportion)
