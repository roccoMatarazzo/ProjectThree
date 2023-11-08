Diabetes Data Analysis
================
Rocco Matarazzo & Karthik Edupuganti
2023-11-08

``` r
diabetes_full <- 
  diabetes %>% 
   mutate(EduLevel_Char = case_when(
    Education == 1 ~ "No School",
    Education == 2 ~ "Elementary",
    Education == 3 ~ "Some High School",
    Education == 4 ~ "High School Graduate",
    Education == 5 ~ "Some College",
    Education == 6 ~ "College Graduate"
  )
)

markdown_data <- diabetes_full %>% filter(EduLevel_Char == params$eduLevel)

head(markdown_data$EduLevel_Char)
```

    ## [1] "High School Graduate" "High School Graduate" "High School Graduate" "High School Graduate" "High School Graduate" "High School Graduate"

``` r
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
```
