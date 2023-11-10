# ProjectThree
This repo holds the data, code, and results of Project #3 in the Data Science for Statisticians course at North Carolina State University.

The R Packages used are as follows:  
- readr
- tidyverse
- rmarkdown
- caret
- rsample

The code used to render five individual reports based on parameters is as follows:
# Setting Education Levels vector
education_levels <- (c("No School", # dataset value = 1
                      "Elementary", # dataset value = 2
                      "Some High School", # dataset value = 3
                      "High School Graduate", # dataset value = 4
                      "Some College", # dataset value = 5
                      "College Graduate")) # dataset value = 6

# adding the .html to each
output_file <- paste0(education_levels, ".md")

params = lapply(education_levels, FUN = function(x){list(eduLevel = x)})

# finalizing and combining it in a tibble
reports <- tibble(output_file, params)

# now we can apply! 
apply(reports, MARGIN = 1,
      FUN = function(x){
        render(input = "Project3_Main_File.rmd",
               output_file = x[[1]],
               output_format = "github_document",
               params = x[[2]])
      })

The analysis for the following files are...
