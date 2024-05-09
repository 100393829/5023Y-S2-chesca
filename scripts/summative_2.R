#___packages----
library(usethis)#used to talk to git
library(gitcreds)#used to talk to git
library(tidyverse)#load pacakge for data cleaning, manipulating and presenting
library(GGally)
library(janitor)#load a package for data cleaning
library(ggplot2)#Used to make plots pretty
library(tidyr)#package for data cleaning
library(stringr)#simplifies string manipulation 
library(dplyr)#load mutate filter and other dplyr verbs, manipulation
library(emmeans)
library(performance)
library(skimr)
library(lmtest)
library(car)#for qq plot for model visualisation
library(see)#for qq plot for model visualisation
library(scales)

#___talking_to_git----
#usethis::use_git_config(user.name = "100393829", user.email = "jug22tpu@uea.ac.uk")#entering username and password
#gitcreds::gitcreds_set()

#___choosing data to analyse----
#cricket <- read_csv ("data/cricket_song.csv")
#butterfly<- read_csv ("data/inbreeding_butterfly.csv")
#parasite <- read_csv ("data/parasite_exp.csv")
probiotic <- read_csv ("data/probiotic.csv")#R reads data from data folder

#____probiotic----
head(probiotic)#View the top of the data set
colnames(probiotic)#view all of the column names
glimpse(probiotic)#view some of the data set
summary(probiotic)#calculates mean of numerical data and gives the class and sample number of other data

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

#___ data cleaning----
probiotic <- janitor::clean_names(probiotic)#sanke cases the name

probiotic <- rename(probiotic,
                   "abundance"="ruminococcus_gnavus_abund") #rename long variable name

#probiotic%>%
 # duplicated()%>%
  #sum() #check for duplicated rows

#_____ changing variable classes----

probiotic$time <- as.factor(probiotic$time)#changing the class of variables: time, gender and group to factor
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
  )#Changing the data to be more readable

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
#            max=max(abundance))# checking the minimum and maximum abundance numbers are concievable

#___separating_abundance_to_before_and_after_and_adding_a_difference_column---- 

difference <- probiotic%>%
  group_by(subject, gender, group)%>%#grouping the df by abundance before the treatment
summarise(abundance_before= abundance[time==1],
          abundance_after=abundance[time==2])%>%
  mutate(difference = abundance_after - abundance_before)#adding a difference column to the dataset

#_____homoscedascity----
#difference %>%
#  group_by(gender) %>%
#  summarise(n = n())#checking for sampling error, Female bias

#difference %>%
#  group_by(group) %>%
#  summarise(n = n())#Placebo group bias

#difference %>%
#  group_by(gender, group) %>%
#  summarise(n = n())%>%
#  mutate(prob_obs = n/sum(n))

#___plotting the sampling error----

group_gender_summary <- difference %>% 
  group_by(group, gender) %>% 
  summarise(n=n(),
            n_distinct=n_distinct(subject)) %>% 
  ungroup() %>% # needed to remove group calculations
  mutate(freq=n/sum(n))

proportion <- difference%>% 
  ggplot(aes(x=group, fill=gender))+
  geom_bar(position=position_dodge2(preserve="single"))+ 
  #keeps bars to appropriate widths
  coord_flip()+
  #keeps bars to appropriate widths
  labs(x="Treatment",
       y = "Number of observations")+
  geom_text(data=group_gender_summary, # use the data from the summarise object
            aes(x=group,
                y= n, # offset text to be slightly to the right of bar
                group= gender, # need species group to separate text
                label=scales::percent(freq) # automatically add %
            ),
            position=position_dodge2(width=0.8))+ # set width of dodge
  scale_fill_manual(values=c("cyan",
                             "darkorange",
                             "purple"
  ))+
  coord_flip()+
  theme_minimal()+
  theme(legend.position="bottom") # put legend at the bottom of the graph

ggsave("figures/bias_frequency_plot.jpeg", 
             plot = proportion)
       
#___monovariate explorative figures----

#abundance_box<- ggplot(data = probiotic, aes(x = time, y = abundance)) +#pipes df and sets x and y column
 # geom_boxplot(aes(fill = time), # chooses inside colours by before and after categories 
  #             alpha = 0.2, # fainter boxes so the points "pop"
   #            width = 0.5,)+ # change width of boxplot
#  geom_jitter(aes(colour = time), # adding the points overtop
 #             width=0.2) #setting the width of the points

#ggsave("figures/abundance_box.jpeg", # Give R a path to save to and a file name
#       plot = abundance_box)

#treatment_box<- ggplot(data = difference, aes(x = group, y = abundance_after)) +#pipes df and sets x and y column
 # geom_boxplot(aes(fill = group), #  # chooses inside colours by placebo and LGG categories 
  #             alpha = 0.2, # fainter boxes so the points "pop"
   #            width = 0.5)+ # change width of boxplot
#  geom_jitter(aes(colour = group),
 #             width=0.2)

#ggsave("figures/treatment_box.jpeg", 
 #      plot = treatment_box)

#gender_box<- ggplot(data = difference, aes(x = gender, y = abundance_after)) + #pipes df and sets x and y column
 # geom_boxplot(aes(fill = gender), #  # chooses inside colours by male and female categories 
  #             alpha = 0.2, # fainter boxes so the points "pop"
   #            width = 0.5)+ # change width of boxplot
  #geom_jitter(aes(colour = gender),
   #           width=0.2)

#ggsave("figures/gender_box.jpeg", 
 #      plot = gender_box)

#bar <- difference %>%     
 # group_by(gender,group) %>% 
  #summarise(n=n()) %>% 
  #ggplot(aes(x=group, y=n)) + #pipes df, establishes count and the plot, and sets x and y column
 # geom_col(aes(fill=gender),  # chooses inside colours by male and female categories 
       #    width=0.8, # change width of bar
       #    position=position_dodge(width=0.9), 
       #    alpha=0.6)+ # sets transparency
 # scale_fill_manual(values=c("darkorange1", "azure4"))+
 # theme_classic()

#ggsave("figures/abundance_bar.jpeg", 
  # plot = bar)

#histogram <- probiotic %>% 
 # ggplot(aes(x= abundance))+
 # geom_histogram(bins=20, 
              #   aes(y=..density..,
               #      fill=group),  # chooses inside colours by LGG and placebo categories 
                # position = "identity",
                # colour="black")

#ggsave("figures/abundance_histogram.jpeg", 
       #plot = histogram)

#histogram_2 <- difference %>% 
 #ggplot(aes(x= abundance_before))+
 #geom_histogram(bins=20, 
#  aes(y=..density..,
 #     fill=group),  # chooses inside colours by LGG and placebo categories 
 #position = "identity",
 #colour="black")


#ggsave("figures/abundance_before_histogram.jpeg", 
  #plot = histogram_2)

#histogram_3 <- difference %>% 
 # ggplot(aes(x= abundance_after))+
 # geom_histogram(bins=20, 
#                 aes(y=..density..,
#                     fill=group),  # chooses inside colours by LGG and placebo categories 
#                 position = "identity",
#                 colour="black")

#ggsave("figures/abundance_after_histogram.jpeg", 
#       plot = histogram_3)

#___abundance_group----

lsmodel1 <- lm(abundance_after ~ group, data = difference)
summary(lsmodel1)
anova(lsmodel1)
plot(lsmodel1)

performance::check_model(lsmodel1, detrend = F)

pf(0.543, 1, 42, lower.tail=FALSE)

group_means <- difference %>% 
  ggplot(aes(x=group, 
             y=abundance_after,
             colour=group))+
  geom_jitter(alpha=0.5,
              width=0.1)+
  stat_summary(fun=mean,
               size=1.2)+
  theme_bw()

#____difference_group---
lsmodel0 <- lm(formula = difference ~ group, data = difference)
summary(lsmodel0)

lsmodel01 <- lm(formula = difference ~ gender, data = difference)
summary(lsmodel01)

pb3 <- probiotic %>%#pipe df
  select(subject, abundance, group)

lsmodel02 <- lm( abundance ~ group + factor(subject), data = pb3) %>% 
  broom::tidy(., conf.int=T) %>% 
  slice(1:2)

summary(lsmodel02)

lsmodel03 <- lm(formula = difference ~ group + gender, data = difference)
summary(lsmodel03)
performance::check_model(lsmodel03, detrend = F)

lsmodel04 <- lm(formula = difference ~ group + gender, data = difference[-14,])
summary(lsmodel04)
performance::check_model(lsmodel04, detrend = F)

#Breusch Pagan test for normality
lmtest::bptest(lsmodel03)
lmtest::bptest(lsmodel04)
# qqplot with confidence intervals
car::qqPlot(lsmodel03)
car::qqPlot(lsmodel04) # adds a confidence interval check
# shapiro wilk test for homoscedasticity
shapiro.test(residuals(lsmodel03))
shapiro.test(residuals(lsmodel04))
#___abundance_before_and_after-----

model2 <- lm(abundance_after ~ abundance_before + gender + group + gender:group,
            data = difference)#change here
summary(model2)

par(mfrow = c(2, 2))#check how well the model fits
plot(model2)#observe the plots

#difference2 <- difference %>% 
 # mutate(ab_before_center = abundance_before - mean(abundance_before, na.rm = T),
     #    ab_after_center = abundance_after - mean(abundance_after, na.rm = T))#check the raw data

no_20<-difference2[-20,]

model3 <- lm(abundance_after ~ abundance_before + gender + group + gender:group,
             data = no_20)#change here
summary(model3)
plot(model3)

#Breusch Pagan test for normality
lmtest::bptest(model2)
lmtest::bptest(model3)
# qqplot with confidence intervals
car::qqPlot(model2)
car::qqPlot(model3) # adds a confidence interval check
# shapiro wilk test for homoscedasticity
shapiro.test(residuals(model2))
shapiro.test(residuals(model3))


model4 <- lm(abundance_after ~ abundance_before + gender + group,
             data = no_20[-14,])#change here

plot(model4)

lmtest::bptest(model4)
# qqplot with confidence intervals
car::qqPlot(model4)
# shapiro wilk test for homoscedasticity
shapiro.test(residuals(model4))

model5 <- lm(abundance_after ~ abundance_before + gender + group + gender:group,
             data = no_20[-5,])

lmtest::bptest(model5)
plot(model5)

ymodel6 <- lm(abundance_after ~ abundance_before + gender + group + gender:group,
             data = no_20[-13,])
lmtest::bptest(model6)
plot(model6)

performance::check_model(model3, detrend = F)

model7 <- lm(abundance_after ~ abundance_before + group,
             data = no_20)#change here

performance::check_model(model4, detrend = F)
