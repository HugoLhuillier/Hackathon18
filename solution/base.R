# load the required packages
library(tidyverse)
library(gridExtra)
library(Hmisc)
library(reldist)
# you can ignore the next two lines, just to have a fancier theme in ggplot2
library(ggthemr)
ggthemr('fresh')

# set the working directory to where the data data is 
# YOU NEED TO CHANGE THAT ON YOUR COMPUTER
setwd("/Users/hugolhuillier/Desktop/hackaton/")

# load the data (taking care of giving the correct data types for wgt, hhid, new_hrs and new_wage). also add the true year variable
sample_A <- read_csv("data/sample_A.csv", 
                     col_types = cols(
                       wgt = col_double(),
                       hhid = col_double(),
                       new_hrs = col_double(),
                       new_wage = col_double() )) %>%
            mutate(true_year = year - 1)

sample_B <- read_csv("data/sample_B.csv", 
                     col_types = cols(
                       wgt = col_double(),
                       new_hrs = col_double(),
                       new_wage = col_double() ))  %>%
            mutate(true_year = year - 1)

sample_C <- read_csv("data/sample_C.csv", 
                     col_types = cols(
                       hh_wgt = col_double(),
                       new_hrs = col_double(),
                       new_wage = col_double() ))  %>%
            mutate(true_year = year - 1)
