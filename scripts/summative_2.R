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
library(emmeans)
library(performance)
library(skimr)
#___talking_to_git----
#usethis::use_git_config(user.name = "100393829", user.email = "jug22tpu@uea.ac.uk")#entering username and password
#gitcreds::gitcreds_set()

#___choosing data to analyse----
#cricket <- read_csv ("data/cricket_song.csv")
#butterfly<- read_csv ("data/inbreeding_butterfly.csv")
#parasite <- read_csv ("data/parasite_exp.csv")
probiotic <- read_csv ("data/probiotic.csv")

#___cricket----
#head(cricket)
#glimpse(cricket)
#summary(cricket)
#GGally::ggpairs(cricket)

#___butterfly----
#head(butterfly)
#glimpse(butterfly)
#summary(butterfly)
#GGally::ggpairs(butterfly)

#____parasite----
#head(parasite)
#glimpse(parasite)
#summary(parasite)
#GGally::ggpairs(parasite)

#____probiotic----
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

#__ checking for na----
#probiotic %>% 
#  is.na() %>% 
#  sum()

#___min max----
#probiotic %>%
#  summarise(min=min(abundance),
#            max=max(abundance))

#___separating the abundance column by time---- 

pb1 <- probiotic %>%#pipe df
  select(time, subject, gender, group, abundance)%>%#remove sample column
  group_by(time = "1")%>%#grouping the df by abundance before the treatment
  rename("abundance_before"="abundance")#creating a new variable

pb2 <- probiotic%>% #pipe df
  select (time, subject, abundance)%>%#removing repeated columns as we don't want the same data twice in the merged df
  filter(time == "2")%>% #grouping the df by abundance after treatment
  rename("abundance_after"= "abundance",#creating a new variable
         "after" ="time")#renaming time so the data frames can merge

df_list <- list(pb1, pb2)#create an object of new data frames

pb <- df_list %>% reduce(full_join, by='subject')%>% #merging data frames with subject as the merge point
  select(subject, gender, group, time, abundance_before, abundance_after) # ordering and selecting relevant columns, because abundance is grouped by time either 'time' or 'after' must be left in

difference <- probiotic%>%
  group_by(gender,subject,group)%>%
summarise(abundance_before= abundance[time==1],
          abundance_after=abundance[time==2])%>%
  mutate(difference = abundance_after - abundance_before)

#____homoscedascity----
difference %>%
  group_by(gender) %>%
  summarise(n = n())

difference %>%                                    # Count NA by group
  group_by(gender) %>%
  dplyr::summarize(na_sex = sum(is.na(gender)),
                   na_year = sum(is.na(year)),
                   na_forewing_length = sum(is.na(forewing_length)))

butterfly_clean %>%
  group_by(gender) %>% 
  summarize(across(everything(), ~sum(is.na(.))))#distribution of missing values 

skimr::skim(difference)
#___monovariate explorative figures----

GGally::ggpairs(difference,
                aes(colour = gender))#clean ggally

#abundance_box<- ggplot(data = probiotic, aes(x = time, y = abundance)) +
 # geom_boxplot(aes(fill = time), # note fill is "inside" colour and colour is "edges" 
  #             alpha = 0.2, # fainter boxes so the points "pop"
   #            width = 0.5, # change width of boxplot
    #           outlier.shape=NA)+
#  geom_jitter(aes(colour = time),
 #             width=0.2)

#ggsave("figures/abundance_box.jpeg", # Give R a path to save to and a file name
#       plot = abundance_box)

#treatment_box<- ggplot(data = pb, aes(x = group, y = abundance_after)) +
 # geom_boxplot(aes(fill = group), # note fill is "inside" colour and colour is "edges" 
  #             alpha = 0.2, # fainter boxes so the points "pop"
   #            width = 0.5, # change width of boxplot
    #           outlier.shape=NA)+
#  geom_jitter(aes(colour = group),
 #             width=0.2)

#ggsave("figures/treatment_box.jpeg", 
 #      plot = treatment_box)

#gender_box<- ggplot(data = pb, aes(x = gender, y = abundance_after)) +
 # geom_boxplot(aes(fill = gender), # note fill is "inside" colour and colour is "edges" 
  #             alpha = 0.2, # fainter boxes so the points "pop"
   #            width = 0.5, # change width of boxplot
    #           outlier.shape=NA)+
  #geom_jitter(aes(colour = gender),
   #           width=0.2)

#ggsave("figures/gender_box.jpeg", 
 #      plot = gender_box)

#bar <- pb %>%     
 # group_by(gender,group) %>% 
  #summarise(n=n()) %>% 
  #ggplot(aes(x=group, y=n)) + 
 # geom_col(aes(fill=gender), 
       #    width=0.8,
       #    position=position_dodge(width=0.9), 
       #    alpha=0.6)+
 # scale_fill_manual(values=c("darkorange1", "azure4"))+
 # theme_classic()

#ggsave("figures/abundance_bar.jpeg", 
  # plot = bar)

#histogram <- probiotic %>% 
 # ggplot(aes(x= abundance))+
 # geom_histogram(bins=20, 
              #   aes(y=..density..,
               #      fill=group), 
                # position = "identity",
                # colour="black")

#ggsave("figures/abundance_histogram.jpeg", 
       #plot = histogram)

#histogram_2 <- difference %>% 
 #ggplot(aes(x= abundance_before))+
 #geom_histogram(bins=20, 
#  aes(y=..density..,
 #     fill=group), 
 #position = "identity",
 #colour="black")


#ggsave("figures/abundance_before_histogram.jpeg", 
  #plot = histogram_2)

#histogram_3 <- difference %>% 
 # ggplot(aes(x= abundance_after))+
 # geom_histogram(bins=20, 
#                 aes(y=..density..,
#                     fill=group), 
#                 position = "identity",
#                 colour="black")

#ggsave("figures/abundance_after_histogram.jpeg", 
#       plot = histogram_3)

h4<-hist(difference$abundance_before)
h5<-hist(difference$abundance_after)

#___trial_linear_models----

lsmodel0 <- lm(formula = difference ~ group, data = difference)
summary(lsmodel0)
anova(lsmodel0)

pf(0.7411, 1, 20, lower.tail=FALSE)

#look at darwin pairs grouping 

lsmodel1 <- lm(abundance ~ group, data = probiotic)
summary(lsmodel1)
anova(lsmodel1)

pf(0.543, 1, 42, lower.tail=FALSE)

model2 <- lm(forewing_length ~ jun_mean + sex + rain_jun + gender:group,
            data = difference)#change here
summary(model)


