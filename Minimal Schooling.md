Diabetes Data Analysis
================
Rocco Matarazzo & Karthik Edupuganti
2023-11-11

# Introduction

This report serves as an analysis on 2015 dataset with over 250,000
survey responses pertaining to diabetes. The main variable of interest
is a binary indicator for diabetes where 1 represents
diabetes/prediabetes and 0 represents no diabetes.

##### TO DO FOR THIS SECTION:

Briefly discuss more about variables/dataset. Other variables that
supplement our analysis are…*add description for our variables here*

*describes the purpose of your EDA and modeling, along with the end
result you will be creating.*

If you are interested in more information about the dataset, please
visit the following
[link.](https://www.kaggle.com/datasets/alexteboul/diabetes-health-indicators-dataset/).

# Data

Here we are reading in our data. First, we uploaded the data to our
repo, set our repo as our working directory, then read the file using
the read_csv() function. Also, to have a clearer understanding of the
data we’re working with, we used the dplyr package and mutate() function
to recreate several of the categorical variables with string/character
level names.

We combined the first group (No school/kindergarten) and the second
group (Elementary level) into one group called “Minimal Schooling.” This
way we have 5 distinct groups.

``` r
# set Repo as your working directory first
diabetes <- as_tibble(read_csv("diabetes_binary_health_indicators_BRFSS2015 (1).csv"))

# Adding meaningful names to levels of variables
diabetes_full <- 
  diabetes %>% 
   mutate(
  # Education Level   
  EduLevel_Char = case_when(
    Education == 1 ~ "Minimal Schooling",
    Education == 2 ~ "Minimal Schooling",
    Education == 3 ~ "Some High School",
    Education == 4 ~ "High School Graduate",
    Education == 5 ~ "Some College",
    Education == 6 ~ "College Graduate"
  ),
  # Income
  Income_Char = case_when(
    Income == 1 ~"< $10,000",
    Income == 2 ~"$10,000 <= Income < $15,000",
    Income == 3 ~"$15,000 <= Income < $20,000",
    Income == 4 ~"$20,000 <= Income < $25,000",
    Income == 5 ~"$25,000 <= Income < $35,000",
    Income == 6 ~"$35,000 <= Income < $50,000",
    Income == 7 ~"$50,000 <= Income < $75,000",
    Income == 8 ~"> $75,000"
),
    # Age
  Age_Char = case_when(
    Age == 1  ~ "Age 18 to 24",    
    Age == 2  ~ "Age 25 to 29",
    Age == 3  ~ "Age 30 to 34",    
    Age == 4  ~ "Age 35 to 39",
    Age == 5  ~ "Age 40 to 44",    
    Age == 6  ~ "Age 45 to 49",
    Age == 7  ~ "Age 50 to 54",    
    Age == 8  ~ "Age 55 to 59",
    Age == 9  ~ "Age 60 to 64",    
    Age == 10 ~ "Age 65 to 69",
    Age == 11 ~ "Age 70 to 74",    
    Age == 12 ~ "Age 75 to 79",
    Age == 13 ~ "Age 80 or older"
),


)
# Subsetting the data down on param. of interest for particular analysis
markdown_data <- diabetes_full %>% filter(EduLevel_Char == params$eduLevel)

# Adding labels to diabetes binary variable and then factoring it.
markdown_data$Diabetes_binary <- ifelse(markdown_data$Diabetes_binary == 0, "No", "Yes")
markdown_data$Diabetes_binary <- factor(markdown_data$Diabetes_binary)
```

# Summarizations

This section is where we will begin Exploratory Data Analysis (EDA).
There are several techniques when conducting an EDA, including finding
summary statistics and plotting various items from the dataset to
potentially identify patterns or relationships within the data.

``` r
markdown_data
```

The variables we will select are HighBP, HighChol, BMI, PhysActivity,
Fruits, Veggies, Sex, Age, Smoker and Stroke

# Modeling

In this section we will fit several models on our dataset with the
predictor being the aforementioned binary diabetes variable. First,
we’ll split our data into a training and testing set.

``` r
# # set seed for reproducible purposes
# set.seed(1234)
# 
# # actually splitting the data
# # the 0.7 at the end implies 70% of data goes into the training set
# train <- sample(1:nrow(markdown_data), size = nrow(markdown_data)*0.70)
# # the test set will be the rows from markdown_data not in the training set
# test <- dplyr::setdiff(1:nrow(markdown_data), train)

# Set seed for reproducible purposes
set.seed(1234)


# p = 0.70 at the end implies 70% of data goes into the training set
trainingIndex <- createDataPartition(markdown_data$Diabetes_binary, p=0.70, list=FALSE)

# Actually splitting into testing and training
train <- markdown_data[trainingIndex,]
test <- markdown_data[-trainingIndex,]
```

### Log Loss

*there is stuff on the discussion board about this*

### Logistic Regression

``` r
ctrl <- trainControl(method = "cv", classProbs = TRUE, number = 5, summaryFunction = mnLogLoss)

logistic_model <- train(Diabetes_binary ~ HighBP + HighChol + BMI + PhysActivity + Fruits + Veggies + Sex + Age + Smoker + Stroke, data = train, method = "glm", family = "binomial", trControl = ctrl, metric = "logLoss")
```

### LASSO Regression

``` r
lasso_model <- train(Diabetes_binary ~ HighBP + HighChol + BMI + PhysActivity + Fruits + Veggies + Sex + Age + Smoker + Stroke, data = train, method = "glmnet", tuneGrid = expand.grid(alpha = 1, lambda = seq(0, 1, by = 0.1)), trControl = ctrl, metric = "logLoss")
```

### Classification Tree

``` r
classification_tree_model <- train(Diabetes_binary ~ HighBP + HighChol + BMI + PhysActivity + Fruits + Veggies + Sex + Age + Smoker + Stroke, data = train, method = "rpart", tuneGrid = expand.grid(cp = seq(0,1, by = 0.001)), trControl = ctrl, metric = "logLoss")
```

### Random Forest

``` r
random_forest_model <- train(Diabetes_binary ~ HighBP + HighChol + BMI + PhysActivity + Fruits + Veggies + Sex + Age + Smoker + Stroke, data = train, method = "rf", tuneGrid = expand.grid(mtry = 1:10), trControl = ctrl, metric = "logLoss")
```

### Non-Class Model \#1

``` r
naive_bayes_model <- train(Diabetes_binary ~ HighBP + HighChol + BMI + PhysActivity + Fruits + Veggies + Sex + Age + Smoker + Stroke, data = train, method = "naive_bayes", trControl = ctrl, metric = "logLoss")
```

### Non-Class Model \#2

``` r
lda_model <- train(Diabetes_binary ~ HighBP + HighChol + BMI + PhysActivity + Fruits + Veggies + Sex + Age + Smoker + Stroke, data = train, method = "lda", trControl = ctrl, metric = "logLoss")
```

``` r
qda_model <- train(Diabetes_binary ~ HighBP + HighChol + BMI + PhysActivity + Fruits + Veggies + Sex + Age + Smoker + Stroke, data = train, method = "qda", trControl = ctrl, metric = "logLoss")
```

# Final Model Selection

# Automation (Ignore for now)

This is for rendering all five docs (ignore for now)

``` r
# Setting Education Levels vector
education_levels <- (c("Minimal Schooling", # dataset value = 1/2
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
