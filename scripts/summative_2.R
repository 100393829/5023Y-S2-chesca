#___packages----
library(usethis)
library(git)
library(tidyverse)
library(GGally)

#talking_to_git--- 
usethis::use_git_config(user.name = "100393829", user.email = "jug22tpu@uea.ac.uk")
gitcreds::gitcreds_set()

#___choosing data to analyse----
cricket <- read_csv ("data/cricket_song.csv")
butterfly<- read_csv ("data/inbreeding_butterfly.csv")
parasite <- read_csv ("data/parasite_exp.csv")
probiotic <- read_csv ("data/probiotic.csv")

#___cricket---
head(cricket)
glimpse(cricket)
summary(cricket)
GGally::ggpairs(cricket)

#___butterfly---
head(butterfly)
glimpse(butterfly)
summary(butterfly)
GGally::ggpairs(butterfly)

#____parasite----
head(parasite)
glimpse(parasite)
summary(parasite)
GGally::ggpairs(parasite)

#____probiotic---
head(probiotic)
glimpse(probiotic)
summary(probiotic)

