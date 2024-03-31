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

probiotic%>%
  duplicated()%>%
  sum() #check for duplicated rows

#_____ changing variable classes----

probiotic$time <- as.factor(probiotic$time)
probiotic$gender <- as.factor(probiotic$gender)
probiotic$group <- as.factor(probiotic$group)

#____ mean----
probiotic %>% 
  summarise(across(.cols = where(is.numeric), 
                   .fns = ~ mean(., na.rm=TRUE)))# finds mean of numeric variables

#____checking for typos ----
probiotic %>%
  distinct(time)#checking for mis-entered data in each column in the data set

probiotic %>%
  distinct(gender)

probiotic %>%
  distinct(group)

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
probiotic %>% 
  is.na() %>% 
  sum()

#___min max---
probiotic %>%
  summarise(min=min(abundance),
            max=max(abundance))

