#___packages----
library(usethis)
library(gitcreds)
library(tidyverse)
library(GGally)
library(janitor)
library(ggplot2)
library(tidyr)
library(stringr)
library(dplyr)

#talking_to_git--- 
#usethis::use_git_config(user.name = "100393829", user.email = "jug22tpu@uea.ac.uk")
#gitcreds::gitcreds_set()

#___choosing data to analyse----
#cricket <- read_csv ("data/cricket_song.csv")
#butterfly<- read_csv ("data/inbreeding_butterfly.csv")
#parasite <- read_csv ("data/parasite_exp.csv")
probiotic <- read_csv ("data/probiotic.csv")

#___cricket---
#head(cricket)
#glimpse(cricket)
#summary(cricket)
#GGally::ggpairs(cricket)

#___butterfly---
#head(butterfly)
#glimpse(butterfly)
#summary(butterfly)
#GGally::ggpairs(butterfly)

#____parasite----
#head(parasite)
#glimpse(parasite)
#summary(parasite)
#GGally::ggpairs(parasite)

#____probiotic---
head(probiotic)
glimpse(probiotic)
summary(probiotic)

#___ data cleaning----
probiotic <- janitor::clean_names(probiotic)

probiotic <- rename(probiotic,
                   "abundance"="ruminococcus_gnavus_abund") #rename long variable name

#probiotic%>%
 # duplicated()%>%
  #sum() #check for duplicated rows

#_____ changing variable classes----

probiotic$time <- as.factor(probiotic$time)
probiotic$gender <- as.factor(probiotic$gender)
probiotic$group <- as.factor(probiotic$group)

#____ mean----
#probiotic %>% 
  #summarise(across(.cols = where(is.numeric), 
 #                  .fns = ~ mean(., na.rm=TRUE)))# finds mean of numeric variables

#____checking for typos ----
#probiotic %>%
 # distinct(time)#checking for mis-entered data in each column in the data set

#probiotic %>%
 # distinct(gender)

#probiotic %>%
 # distinct(group)

#unique (ubu$sex)
#____ changing typos----

probiotic <- probiotic %>%
  mutate(gender = case_when(
    gender %in% c("F") ~ "Female",
    TRUE ~ gender
  )
  )

probiotic <- probiotic %>%
  mutate(gender = case_when(
    gender %in% c("M") ~ "Male",
    TRUE ~ gender
  )
  )

#__ checking for na---
#probiotic %>% 
#  is.na() %>% 
#  sum()

#___min max---
#probiotic %>%
#  summarise(min=min(abundance),
#            max=max(abundance))

#___separating the time column--- 

pb1 <- probiotic %>%#pipe df
  select(time, subject, gender, group, abundance)%>%#remove sample column
  group_by(time = "1")%>%#grouping the df by before the treatment
  rename("abundance_before"="abundance")#creating a new variable

pb2 <- probiotic%>% #pipe df
  select (time, subject, abundance)%>%#removing repeated columns as we don't want the same data twice in the merged df
  filter(time == "2")%>% #grouping the df by after the abundance
  rename("abundance_after"= "abundance",#creating a new variable
         "after" ="time")#renaming time so the data frames can merge

df_list <- list(pb1, pb2)#create an object of new data frames

pb <- df_list %>% reduce(full_join, by='subject')%>% #merging data frames with subject as the merge point
  select(subject, gender, group, time, abundance_before, abundance_after) # ordering and selecting relevant columns, because abundance is grouped by time either 'time' or 'after' must be left in




