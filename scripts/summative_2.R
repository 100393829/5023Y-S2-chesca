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
library(effectsize)
library(boot)
library(ggplot2)#Used to make plots pretty
library(car)#for qq plot for model visualisation
library(see)#for qq plot for model visualisation
library(scales)#for data visualisation
#___talking_to_git----
#usethis::use_git_config(user.name = "100393829", user.email = "jug22tpu@uea.ac.uk")#entering username and password
#gitcreds::gitcreds_set()

#___loading_data----

probiotic <- read_csv ("data/probiotic.csv")#R reads data from data folder

head(probiotic)#View the top of the data set
colnames(probiotic)#view all of the column names
glimpse(probiotic)#view some of the data set
summary(probiotic)#calculates mean of numerical data and gives the class and sample number of other data

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

lgg <- probiotic%>%
  group_by(subject, gender, group)%>%
  filter(group=="LGG")%>%#grouping the df by abundance before the treatment
  summarise(abundance_before= abundance[time==1],
            abundance_after=abundance[time==2])%>%
  mutate(difference = abundance_after - abundance_before)

placebo <- probiotic%>%
  group_by(subject, gender, group)%>%#grouping the df by abundance before the treatment
  filter(group =="Placebo")%>%
  summarise(abundance_before= abundance[time==1],
            abundance_after=abundance[time==2])%>%
  mutate(difference = abundance_after - abundance_before)

abundacne_after <- probiotic%>%
  group_by(subject, gender, group)%>%#grouping the df by abundance before the treatment
  filter(time =="2")%>%
  summarise(abundance_after=abundance[time==2])


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

#lsmodel1 <- lm(abundance_after ~ group, data = difference)
#summary(lsmodel1)
#anova(lsmodel1)
#plot(lsmodel1)

#performance::check_model(lsmodel1, detrend = F)

#pf(0.543, 1, 42, lower.tail=FALSE)

#group_means <- difference %>% 
#  ggplot(aes(x=group, 
 #            y=abundance_after,
  #           colour=group))+
  #geom_jitter(alpha=0.5,
   #           width=0.1)+
  #stat_summary(fun=mean,
   #            size=1.2)+
  #theme_bw()

#____difference_group---
#lsmodel0 <- lm(formula = difference ~ group, data = difference)
#summary(lsmodel0)

#lsmodel01 <- lm(formula = difference ~ gender, data = difference)
#summary(lsmodel01)


#lsmodel03 <- lm(formula = difference ~ group + 
#                  gender,
#                data = difference)
#summary(lsmodel03)
#performance::check_model(lsmodel03, detrend = F)

lsmodel04 <- lm(formula = difference ~ group + 
                  gender, 
                data = difference[-14,])#removing 14 outlier
summary(lsmodel04)
anova(lsmodel04)
plot(lsmodel04)
performance::check_model(lsmodel04, detrend = F)
confint(lsmodel04)
broom::tidy(lsmodel04, conf.int=T, conf.level=0.95)
#GGally::ggcoef_model(lsmodel04,
#                     show_p_values=FALSE, 
#                     conf.level=0.95)

lsmodel05 <- lm(formula = difference ~ group + 
                  gender + 
                  group:gender , 
                data = difference[-14,])#adding interaction

summary(lsmodel05)
broom::tidy(lsmodel05, conf.int=T, conf.level=0.95)
drop1(lsmodel05, test = "F")
anova(lsmodel05, lsmodel04)#therefore drop the interaction term

#lsmodel06 <- lm(formula = sqrt(difference) ~  group + 
#                  gender +
#                  group:gender,
#                data=difference[-14,])#checking transformations
#
#summary(lsmodel06)
#performance::check_model(lsmodel06, detrend = F)
#MASS::boxcox(lsmodel06)
#drop1(lsmodel06, test = "F")
#anova(lsmodel06, lsmodel04)

#___testing my best model----

drop1(lsmodel04, test = "F")

dropped_group<- lm(formula = difference ~ gender, 
                    data = difference[-14,])
summary(dropped_group)
performance::check_model(dropped_group, detrend = F)

#Breusch Pagan test for normality
lmtest::bptest(lsmodel04)
# qqplot with confidence intervals
car::qqPlot(lsmodel04) # adds a confidence interval check
# shapiro wilk test for homoscedasticity
shapiro.test(residuals(lsmodel04))

#_____paired_t_test----

lsmodel_t_test <- lm(abundance~ group + factor(subject), data = probiotic)

summary(lsmodel_t_test)

broom::tidy(lsmodel_t_test, conf.int=T, conf.level=0.95)# just show first two rows

GGally::ggcoef_model(lsmodel_t_test,
                     show_p_values=FALSE, 
                     conf.level=0.95)

#lgg.t.test<-t.test(lgg$abundance_after, lgg$abundance_before)


#placebo.t.test<-t.test(placebo$abundance_after, placebo$abundance_before)

#____model_summary----
difference2<-difference[-14,]

difference2 %>%
    summarise(min=min(difference),
              max=max(difference))
  
model_sum <- emmeans::emmeans(lsmodel04, specs = ~group + gender,
                              at =list(difference2 = c(-141: 194)))%>%
  as_tibble()

difference2%>%
  ggplot(aes(x = abundance_before,
             y = abundance_after,
             fill = group))+
   geom_ribbon(data = model_sum,
              aes(x = abundance_before,
                  y = abundance_after,
                 ymin = lower.CL,
                   ymax = upper.CL,
                  fill = group),
              alpha = .2)+
  geom_line(data = model_sum,
           aes(x = jun_mean,
               y = emmean,
               colour = sex),
            show.legend = FALSE)+
  geom_point(shape = 21,
           colour = "black",
             show.legend = FALSE)+
  scale_colour_manual(values = c("darkorange", "purple"))+
  scale_fill_manual(values = c("darkorange", "purple"))+
  labs(x = "Mean June Temperature (°C)",
       y = "Forewing length (mm)",
       fill = "Sex")+
  theme_classic()+
  theme(legend.position = "top")

