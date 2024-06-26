#____figure_3----
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

#____visualisation----
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

pal<- c("steelblue1","seagreen3")#creating a colour palette

my_y_title <- expression(paste(italic("R. ganvus"), " Abundance"))#creating objects where bacteria name is italicised

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
  theme_grey()+#setting the theme as minimal and setting the font
  theme(axis.text = element_text(color = "darkgrey", size = 10),# Changes the size of text on both axis 
        plot.title = element_text(lineheight = 0.8, size = 12),#sets size of title and makes it bold, sets lineheight
        plot.subtitle = element_text(size = 12),#sets subtitle size
        axis.ticks = element_line( color = "darkgrey"))+#add axis ticks to the x and y axes, specify length and change to the same colour as the text
  geom_jitter(aes(colour = time),#colouring the points by time
              width=0.1)+#setting the span
  facet_grid(gender~ group, scales = "free_x")+#creating the grid with no x axis
  theme(strip.text = element_textbox_highlight( color = "gray27",face = "bold", size = 12))#changing the appearance of the facet titles

print(box_gender)
