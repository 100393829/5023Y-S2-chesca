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

#___talking_to_git----
#usethis::use_git_config(user.name = "100393829", user.email = "jug22tpu@uea.ac.uk")#entering username and password
#gitcreds::gitcreds_set()

#___loading_data----

probiotic <- read_csv ("/cloud/project/data/probiotic.csv")#R reads data from data folder

#head(probiotic)#View the top of the data set
#colnames(probiotic)#view all of the column names
#glimpse(probiotic)#view some of the data set
#summary(probiotic)#calculates mean of numerical data and gives the class and sample number of other data

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

#__ checking for na----
#probiotic %>% 
#  is.na() %>% 
#  sum()

#___min max----
#probiotic %>%
#  summarise(min=min(abundance),
#            max=max(abundance))# checking the minimum and maximum abundance numbers are concievable

#___separating_abundance_to_before_and_after_and_adding_a_difference_column---- 

probiotic2<-probiotic[-41,]#removing outlier from subject 21 from the df *done after lsmodel04 creation due to leverage
probiotic3<- probiotic2[-41,]

difference <- probiotic%>%
  group_by(subject, gender, group)%>%#grouping the df by abundance before the treatment
summarise(abundance_before= abundance[time==1],#creating aand renaming a before column by selecting timepoint 1
          abundance_after=abundance[time==2])%>%#creating an after column by selecting only timepoint 2
  mutate(difference = abundance_after - abundance_before)#adding a difference column to the dataset

difference2<-difference[-14,]#removing outlier from subject 21 *done after model creation
#print(max(difference2$difference))#checking the outlier has been removed
#print(min(difference2$difference))

lgg <- probiotic3%>%
  group_by(subject, gender, group)%>%
  filter(group=="LGG")# filtering the df to only include LGG from the group varible
         
lgg_diff <- probiotic3%>%
  group_by(subject, gender, group)%>%
  filter(group=="LGG")%>%
  summarise(abundance_before= abundance[time==1],
            abundance_after=abundance[time==2])%>%
  mutate(difference = abundance_after - abundance_before)# using above code together to create a df with difference of only LGG samples

placebo_diff <- probiotic3%>%
  group_by(subject, gender, group)%>%
  filter(group =="Placebo")%>%
  summarise(abundance_before= abundance[time==1],
            abundance_after=abundance[time==2])%>%
  mutate(difference = abundance_after - abundance_before)# using above code together to create a df with difference of only placebo samples

#_____homoscedascity----
#difference %>%
#  group_by(gender) %>%#grouping by the variable to be counted
#  summarise(n = n())#Summarising by n which is number of samples checking for sampling error, Female bias

#difference %>%
#  group_by(group) %>%
#  summarise(n = n())#Placebo group bias

#difference %>%
#  group_by(gender, group) %>%
#  summarise(n = n())%>%
#  mutate(prob_obs = n/sum(n))

#___plotting the sampling error----

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
       fill = "Gender")+
  geom_text(data=group_gender_summary, # use the data from the summarise object
            aes(x=group,
                y= n, # offset text to be slightly to the right of bar
                group= gender, # need species group to separate text
                label=scales::percent(freq) # automatically add %
            ),
            position=position_dodge2(width=0.8))+ # set width of dodge
  scale_fill_manual(values=pal)+#change colours to specified palette
  theme_grey(base_family = "Arial")+#setting the theme as minimal and setting the font
  theme(axis.text = element_text(color = "darkgrey", size = 10),# Changes the size of text on both axis 
        plot.title = element_text(lineheight = 0.8, size = 12),#sets size of title and makes it bold, sets lineheight
        plot.subtitle = element_text(size = 12),#sets subtitle size
        axis.ticks = element_line( color = "darkgrey"))#add axis ticks to the x and y axes, specify length and change to the same colour as the text

print(proportion)

ggsave("/cloud/project/figures/bias_frequency_plot.jpeg", 
             plot = proportion,
       width = 17, # Set .pdf width
       height = 9, # Set .pdf height
       units = "cm") # Specify units for .pdf width and height# saving the plot under a different name to the figures folder
       
#___monovariate_explorative)figures----

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
#  geom_jitter(aes(colour = group),#changing circle colour to be by group
 #             width=0.2)

#ggsave("figures/treatment_box.jpeg", 
 #      plot = treatment_box)

#gender_box<- ggplot(data = difference, aes(x = gender, y = abundance_after)) + #pipes df and sets x and y column
 # geom_boxplot(aes(fill = gender), #  # chooses inside colours by male and female categories 
  #             alpha = 0.2, # fainter boxes so the points "pop"
   #            width = 0.5)+ # change width of boxplot
  #geom_jitter(aes(colour = gender),#changing circle colour to be by gender
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
 # scale_fill_manual(values=c("darkorange1", "azure4"))+#change bar colours
 # theme_classic()

#ggsave("figures/abundance_bar.jpeg", 
  # plot = bar)

#histogram <- probiotic %>% 
 # ggplot(aes(x= abundance))+
 # geom_histogram(bins=20, #set number of bars
              #   aes(y=..density..,
               #      fill=group),  # chooses inside colours by LGG and placebo categories 
                # position = "identity",
                # colour="black")#outline colour

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

#____difference_group---
#lsmodel0 <- lm(formula = difference ~ group, data = difference)#comparing just treatment
#summary(lsmodel0) 
#plot(lsmodel0)#checking how well model fits after observing R^2

#lsmodel01 <- lm(formula = difference ~ gender, data = difference)#comparing just gender
#summary(lsmodel01)
#plot(lsmodel01)

#lsmodel03 <- lm(formula = difference ~ group + 
#                  gender,
#                data = difference)
#summary(lsmodel03)#comparing both group and gender
#performance::check_model(lsmodel03, detrend = F)#finding row 14 exerets leverage on the model

lsmodel04 <- lm(formula = difference ~ group + 
                  gender, 
                data = difference[-14,])#removing 14 outlier
summary(lsmodel04)
#anova(lsmodel04)
#plot(lsmodel04)
#performance::check_model(lsmodel04, detrend = F)#checking the model fit
#broom::tidy(lsmodel04, conf.int=T, conf.level=0.95)# finding CIs
#GGally::ggcoef_model(lsmodel04,
#                     show_p_values=FALSE, 
#                     conf.level=0.95)#plotting the significances

lsmodel05 <- lm(formula = difference ~ group + 
                 gender + 
                 group:gender , 
                data = difference[-14,])#adding interaction

summary(lsmodel05)#observing model result
#broom::tidy(lsmodel05, conf.int=T, conf.level=0.95)#Finding confidence intervals
drop1(lsmodel05, test = "F")#dropping interaction
anova(lsmodel05, lsmodel04)#therefore drop the interaction term

#lsmodel06 <- lm(formula = sqrt(difference) ~  group + 
#                  gender +
#                  group:gender,
#                data=difference[-14,])#checking transformations
#
#summary(lsmodel06)
#performance::check_model(lsmodel06, detrend = F)#checking the fit 
#MASS::boxcox(lsmodel06)
#drop1(lsmodel06, test = "F")
#anova(lsmodel06, lsmodel04)#Best model has no more explain variation

#___testing my best model----

#drop1(lsmodel04, test = "F")#dropping variables and checking they all explain the variance

#dropped_group<- lm(formula = difference ~ gender, 
#                    data = difference[-14,])
#summary(dropped_group)
#performance::check_model(dropped_group, detrend = F)#seeing if dropping group explains more

#Breusch Pagan test for normality
#lmtest::bptest(lsmodel04)
# qqplot with confidence intervals
#car::qqPlot(lsmodel04) # adds a confidence interval check
# shapiro wilk test for homoscedasticity
#shapiro.test(residuals(lsmodel04))

#_____t_testing----

lsmodel_t_test <- lm(abundance~ group + factor(subject), data = probiotic3)#Paired T for abundance difference by treatment
summary(lsmodel_t_test)#Observing results
#broom::tidy(lsmodel_t_test, conf.int=T, conf.level=0.95)# Finding confidence intervals

#GGally::ggcoef_model(lsmodel_t_test,
#                     show_p_values=FALSE, 
#                     conf.level=0.95)

lsmodel_t_test_lgg <- lm(abundance_after ~ abundance_before, data = lgg_diff)#Students t change in abundance in lgg only
summary(lsmodel_t_test_lgg)
#broom::tidy(lsmodel_t_test_lgg, conf.int=T, conf.level=0.95)

lsmodel_t_test_placebo <- lm(abundance_after ~ abundance_before, data = placebo_diff)#Student's t change in abundance in placebo only
summary(lsmodel_t_test_placebo)
#broom::tidy(lsmodel_t_test_placebo, conf.int=T, conf.level=0.95)

lsmodel_t_test_l_gender <- lm(abundance~ gender + factor(subject), data = lgg)#Paired t change in abundance by gender
summary(lsmodel_t_test_l_gender)
#broom::tidy(lsmodel_t_test_l_gender, conf.int=T, conf.level=0.95)

#____model_summary----
#sum_pair_t <- emmeans::emmeans(lsmodel_t_test, specs = ~ group,#listing the specificications of the model
#                              at =list(difference2 = c(-141: 194)))%>%#list highest and lowest values
#  as_tibble()#creating model summaries

#sum_04 <- emmeans::emmeans(lsmodel04, specs = ~group + gender,
#                              at =list(difference2 = c(-141: 194)))%>%
#  as_tibble()

#___data_visualisation----
element_textbox_highlight <- function(..., hi.labels = NULL, hi.fill = NULL,
                                      hi.col = NULL, hi.box.col = NULL, hi.family = NULL) {#a tibble of nul values to be used in conjuction with %||% later
  structure(
    c(element_textbox(...),
      list(hi.labels = hi.labels, hi.fill = hi.fill, hi.col = hi.col, hi.box.col = hi.box.col, hi.family = hi.family)
    ),#enables markdown text in a box
    class = c("element_textbox_highlight", "element_textbox", "element_text", "element")
  )#naming the values 
}#creating an extension of the element_textbox function (ggtext), creating variables that differentiate between the highlighting (hi) box and the normal box, with a tibble

element_grob.element_textbox_highlight <- function(element, label = "", ...) {#creating a grob - grid, graphical object
  if (label %in% element$hi.labels) {
    element$fill <- element$hi.fill %||% element$fill#creating objects where if the right is null the left value is assigned but they're separate so can diffferentiate between highlighted and not higlighted text. 
    element$colour <- element$hi.col %||% element$colour #this is done for box fill above, font colour
    element$box.colour <- element$hi.box.col %||% element$box.colour#box outline
    element$family <- element$hi.family %||% element$family#and family of syle of the box
  }
  NextMethod()#calls correct element based on the class in this case the parent classes
}

levels(probiotic3$time) <- c('Before','After')#changing the time column to make sub axis labels pretty

my_y_title <- expression(paste(italic("R. ganvus"), " Abundance"))#creating objects where bacteria name is italicised

t_test_box_title <- expression(paste("Difference in ", italic("Ruminococcus ganvus"), " Abundance After Probiotic Treatment"))
t_test_box_subtitle <- expression(paste("Read count of ", italic("R. ganvus"), " from Stool Samples of 21 Subjects"))

t_test_box <- ggplot(data = probiotic3, aes(x = time, y = abundance)) +#setting the x and y variables
  geom_boxplot(aes(fill = time),#changing the fill of the box plots
             alpha = 0.7, #changing the transparency
             width = 0.5, # change width of boxplot
             show.legend = FALSE)+#removing figure legend
  geom_jitter(aes(colour = time), #setting the scatter colour by time
              width=0.1)+# changing the spread of the points
  facet_wrap(~ group)+#creating the pannels
  labs(y=my_y_title,#labelling x y axes and titling using the italicised R.gnavus function
       x = "Sampling Time",
       title= t_test_box_title,# using italicised objects for the title and subtitle
       subtitle = t_test_box_subtitle)+
  scale_fill_manual(values = pal) +#instructing r what colours to used
  theme_grey(base_family = "Arial")+#setting the theme as minimal and setting the font
  theme(axis.text = element_text(color = "darkgrey", size = 10),# Changes the size of text on both axis 
        plot.title = element_text(lineheight = 0.8, size = 12),#sets size of title and makes it bold, sets lineheight
        plot.subtitle = element_text(size = 12),#sets subtitle size
        axis.ticks = element_line( color = "darkgrey"))+#add axis ticks to the x and y axes, specify length and change to the same colour as the text
  scale_color_manual(values = c("steelblue", "seagreen"), guide = "none") +# setting the jitter to darker colours
  theme(strip.text = element_textbox_highlight( color = "gray27",face = "bold", size = 12))#changing the appearance of the facet titles

print(t_test_box)
#ggsave("figures/Group_only_box.jpeg", 
#       plot = t_test_box)#saving as a.jpeg

#___gender_and_group_plot----

box_gender_title <- expression(paste("Difference in ", italic("Ruminococcus ganvus"), " Abundance After Probiotic Treatment Separated by Gender"))
box_gender_subtitle <- expression(paste("Read count of ", italic("R. ganvus"), " from Stool Samples of 21 Subjects"))

 box_gender <- ggplot(data = probiotic3, aes(x = time, y = abundance)) +#setting the x and y variables
   scale_color_manual(values = c("steelblue", "seagreen"), guide = "none") +# setting the jitter to darker colours
    geom_boxplot(aes(fill = time),#changing the fill of the box plots
                 alpha = 0.7, #changing the transparencey
                 width = 0.5, # change width of boxplot
                 show.legend = FALSE)+#removing figure legend
   labs(y = my_y_title,#labelling x y axes and titling with italicised text
        x = "Sampling Time",
        title= box_gender_title, 
        subtitle = box_gender_subtitle)+
   scale_fill_manual( values = pal) +#instructing r what colours to used
   theme_grey(base_family = "Arial")+#setting the theme as minimal and setting the font
   theme(axis.text = element_text(color = "darkgrey", size = 10),# Changes the size of text on both axis 
         plot.title = element_text(lineheight = 0.8, size = 12),#sets size of title and makes it bold, sets lineheight
         plot.subtitle = element_text(size = 12),#sets subtitle size
         axis.ticks = element_line( color = "darkgrey"))+#add axis ticks to the x and y axes, specify length and change to the same colour as the text
   geom_jitter(aes(colour = time),#colouring the points by time
                width=0.1)+#setting the span
    facet_grid(gender~ group, scales = "free_x")+#creating the grid with no x axis
         theme(strip.text = element_textbox_highlight( color = "gray27",face = "bold", size = 12))#changing the appearance of the facet titles

 print(box_gender)
 #ggsave("figures/box_grid.jpeg", 
#        plot = box_gender)#saving to figure file
 