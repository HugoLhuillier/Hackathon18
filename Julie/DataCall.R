library(tidyverse)
library(gridExtra)
library(Hmisc)
library(reldist)

# nipa_income

nipa <- read_csv("data-hackaton/nipa_income.csv", col_names = TRUE, )


# Sample A

sampA <- read_csv("data-hackaton/sample_A.csv", 
                  col_names = TRUE, 
                  col_types = cols(
                    new_hrs = col_double(),
                    new_wage = col_double()
                  ))

# Sample B 

sampB <- read_csv("data-hackaton/sample_B.csv", 
                  col_names = TRUE, 
                  col_types = cols(
                    new_hrs = col_double(),
                    new_wage = col_double()
                  ))

# Sample C

sampC <- read_csv("data-hackaton/sample_C.csv", 
                  col_names = TRUE, 
                  col_types = cols(
                    new_hrs = col_double(),
                    new_wage = col_double()
                  ))