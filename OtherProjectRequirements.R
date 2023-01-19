# Other Requirements
# Author: Chad Bell
# Date: 1/19/23

# Purpose: The purpose of this file is to complete the other project requirements that were not satisfied through my HELOC calculator. 

# DATA TYPES
# I'm going to use 5 data types here just in case I missed one in the HELOC file. 
my_string <- "Hello World"
print(class(my_string))
my_int <- 1L
print(class(my_int))
my_float <- 1.0
print(class(my_float))
my_complex <- 1i + 1
print(class(my_complex))
my_logic <- TRUE
print(class(my_logic))



library(dplyr)
mor <- as.numeric(readline(prompt = "Enter Your Mortgage Amount: "))
print(case_when(mor <= 0 ~ "So you don't have a mortgage?",
          mor < 1000000 ~ "What a nice home",
          TRUE ~ "That's a big house!!!"))



