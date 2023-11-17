# ST558 Project 3
This repo holds the data, code, and results of Project #3 in the Data Science for Statisticians course at North Carolina State University. This project specifically explores multiple predictive modeling techniques, ranging from regression to random forests. The main variable of interest is a binary diabetes variable. 

The R Packages used are as follows:  
- readr
- tidyverse
- rmarkdown
- caret
- rsample 
- naivebayes
- MASS
- ggcorrplot 
- Metrics

### Generated Analyses
The analysis for the following files are:
1. [Minimal Schooling](https://roccomatarazzo.github.io/ProjectThree/Minimal%20Schooling.html)
2. [Some High School](https://roccomatarazzo.github.io/ProjectThree/Some%20High%20School.html)
3. [High School Graduate](https://roccomatarazzo.github.io/ProjectThree/High%20School%20Graduate.html)
4. [Some College](https://roccomatarazzo.github.io/ProjectThree/Some%20College.html)
5. [College Graduate](https://roccomatarazzo.github.io/ProjectThree/College%20Graduate.html)

### Automated Report Code
The code used to render five individual reports based on parameters is as follows:

education_levels <- (c("Minimal Schooling", # dataset value = 1 / 2
                      "Some High School", # dataset value = 3
                      "High School Graduate", # dataset value = 4
                      "Some College", # dataset value = 5
                      "College Graduate")) # dataset value = 6

output_file <- paste0(education_levels, ".md")

params = lapply(education_levels, FUN = function(x){list(eduLevel = x)})

reports <- tibble(output_file, params)

apply(reports, MARGIN = 1,
      FUN = function(x){
        render(input = "Project3_Main_File.rmd",
               output_file = x[[1]],
               output_format = "github_document",
               params = x[[2]])
      })

