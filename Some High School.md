Diabetes Data Analysis
================
Rocco Matarazzo & Karthik Edupuganti
2023-11-17

# Introduction

This report serves as an analysis on 2015 dataset with over 250,000
survey responses pertaining to diabetes. The main variable of interest
is a binary indicator for diabetes where 1 represents
diabetes/prediabetes and 0 represents no diabetes.

Other variables that supplement our analysis include HighBP, HighChol,
BMI, PhysActivity, Fruits, Veggies, Sex, Age, Smoker and Stroke. Each of
these are binary variables, except for BMI and Age. BMI represents the
individual’s Body Mass Index, which can help identify potential health
problems using the person’s height and weight. HighBP, HighChol, Smoker,
and Stroke are each coded as 1 equals yes, and 0 equals no. For example,
an individual that has a value of 1 in each of these categories has high
blood pressure, high cholesterol, classifies as a smoker, and has had a
stroke in their life. To classify as a smoker in this dataset, an
individual must have smoked at least 100 cigarettes within their
lifetime at the time of the survey.

Just as the variables listed above, PhysActivity, Fruits, and Veggies
are another set of yes (1) and no (0) variables. PhysActivity represents
whether or not the individual exercised within the last month at the
time of survey, other than their regular job. Fruits and veggies
determine whether the individual has at least one fruit or vegetable per
day. A value of 1 in each of these categories would imply a rather
healthy lifestyle. Finally, Sex represents the gender of the individual
– 1 for male and 0 for female – and Age has several splits, but
ultimately ranges from 18 years old to 80 or older. If you are
interested in more information about the dataset, please visit the
following
[link.](https://www.kaggle.com/datasets/alexteboul/diabetes-health-indicators-dataset/)

Our Exploratory Data Analysis (EDA) attempts to discover patterns and
relationships within our data among the variables of interest. It was
difficult to create several distinct plots given the heavy number of
categorical variables. Contingency tables and bar plots served crucial
in this section. Each of these really further understanding of the
dataset and even display relationships between variables. The modeling
section explores several different model options before settling on a
final model Each model is reasonable for a binary target variable, like
diabetes, but each have their own distinct method that make them unique.
The purpose of modeling is to find a model that is good at predicting
the target variable of interest. The final model is best in terms of Log
Loss, which is discussed in the Modeling section.

The final output for this project is five individual analyses on each
level of education, which are discussed further in the data section.

# Data

Here we are reading in our data. First, we uploaded the data to our
repo, set our repo as our working directory, then read the file using
the read_csv() function. Also, to have a clearer understanding of the
data we’re working with, we used the dplyr package and mutate() function
to recreate several of the categorical variables with string/character
level names.

We combined the first group (No school/kindergarten) and the second
group (Elementary level) into one group called “Minimal Schooling.” This
way we have 5 distinct groups as follows:  
- Minimal Schooling  
- Some High School  
- High School Graduate  
- Some College  
- College Graduate

``` r
# set Repo as your working directory first before running this code
diabetes <- as_tibble(read_csv("diabetes_binary_health_indicators_BRFSS2015 (1).csv"))

# Adding meaningful names to levels of variables
diabetes_full <- 
  diabetes %>% 
   mutate(
  # Education Level   
  Education = case_when(
    Education == 1 ~ "Minimal Schooling",
    Education == 2 ~ "Minimal Schooling",
    Education == 3 ~ "Some High School",
    Education == 4 ~ "High School Graduate",
    Education == 5 ~ "Some College",
    Education == 6 ~ "College Graduate"
  ),
  # Income
  Income = case_when(
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
  Age = case_when(
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
  # High Blood Pressure
  HighBP = case_when(
    HighBP == 1 ~ "Yes",
    HighBP == 0 ~ "No"
  ), 
  # High Cholesterol
  HighChol = case_when(
    HighChol == 1 ~ "Yes",
    HighChol == 0 ~ "No"
  ), 
  # Physical Activity
  PhysActivity = case_when(
    PhysActivity == 1 ~ "Yes",
    PhysActivity == 0 ~ "No"
  ), 
  # Fruits
  Fruits = case_when(
    Fruits == 1 ~ "At least one piece of fruit per day",
    Fruits == 0 ~ "Diet contains no fruits"
  ), 
  # Vegetables
  Veggies = case_when(
    Veggies == 1 ~ "At least one vegetable per day",
    Veggies == 0 ~ "Diet contains no vegetables"
  ),
  # Sex
  Sex = case_when(
    Sex == 1 ~ "Male",
    Sex == 0 ~ "Female"
  ), 
  # Smoker
  Smoker = case_when(
    Smoker == 1 ~ "Yes",
    Smoker == 0 ~ "No"
  ),
  # Stroke
  Stroke = case_when(
    Stroke == 1 ~ "Yes",
    Stroke == 0 ~ "No"
  )
  ,
  Diabetes_binary = case_when(
    Diabetes_binary == 1 ~ "Yes",
    Diabetes_binary == 0 ~ "No"
  )

)
# Subsetting the data down on param. of interest for particular analysis
markdown_data <- diabetes_full %>% filter(Education == params$eduLevel)

# Converting all variables to factors
markdown_data$Income <- factor(markdown_data$Income)
markdown_data$Age <- factor(markdown_data$Age)
markdown_data$HighBP <- factor(markdown_data$HighBP)
markdown_data$HighChol <- factor(markdown_data$HighChol)
markdown_data$PhysActivity <- factor(markdown_data$PhysActivity)
markdown_data$Fruits <- factor(markdown_data$Fruits)
markdown_data$Veggies <- factor(markdown_data$Veggies)
markdown_data$Sex <- factor(markdown_data$Sex)
markdown_data$Smoker <- factor(markdown_data$Smoker)
markdown_data$Stroke <- factor(markdown_data$Stroke)
markdown_data$Diabetes_binary <- factor(markdown_data$Diabetes_binary)
```

# Summarizations

This section is where we will begin Exploratory Data Analysis (EDA).
There are several techniques when conducting an EDA, including finding
summary statistics and plotting various items from the dataset to
potentially identify patterns or relationships within the data.

### Summary Statistics

First, we will view summary statistics based on our dataset. The only
numerical summary we can acquire from the dataset is for BMI. Every
other variable is of the categorical nature. The IQR and Standard
Deviation are also calculated for BMI in addition to the general summary
statistics.

``` r
# Five Number Summary
summary(markdown_data$BMI)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   13.00   25.00   28.00   29.64   33.00   95.00

``` r
# IQR (Q3-Q1)
paste("The Interquartile Range (IQR) for BMI is:", round(IQR(markdown_data$BMI),3))
```

    ## [1] "The Interquartile Range (IQR) for BMI is: 8"

``` r
# Std. Deviation
paste("The Standard Deviation for BMI is:", round(sd(summary(markdown_data$BMI)),3))
```

    ## [1] "The Standard Deviation for BMI is: 29.102"

### Contingency Tables

This opening bit will focus on HighBP, HighChol, Smoker, and Stroke. All
four of these variables can imply poor health in comparison to not being
associated with these variables. For the tables below, we are looking at
three-way contingency tables. The first contingency table looks at
frequency of having HighBP and HighChol for people with and without
diabetes. The second contingency looks at frequency of Stroke and being
a Smoker for people with and without diabetes.

``` r
table(markdown_data$HighChol, markdown_data$HighBP, markdown_data$Diabetes_binary,  
      dnn = c("HighChol", "HighBP", "Diabetes"))
```

    ## , , Diabetes = No
    ## 
    ##         HighBP
    ## HighChol   No  Yes
    ##      No  2493 1534
    ##      Yes 1035 2120
    ## 
    ## , , Diabetes = Yes
    ## 
    ##         HighBP
    ## HighChol   No  Yes
    ##      No   213  501
    ##      Yes  234 1348

``` r
table(markdown_data$Stroke, markdown_data$Smoker, markdown_data$Diabetes_binary,  
      dnn = c("Stroke", "Smoker", "Diabetes"))
```

    ## , , Diabetes = No
    ## 
    ##       Smoker
    ## Stroke   No  Yes
    ##    No  2566 4088
    ##    Yes  158  370
    ## 
    ## , , Diabetes = Yes
    ## 
    ##       Smoker
    ## Stroke   No  Yes
    ##    No   774 1220
    ##    Yes   84  218

Next, we can explore the age of the individual. The first table is a
proportion table which shows the percentage of people who have or do not
have diabetes for each age group. The second table is a three-way
contingency table which looks at if a person has done physical activity
and if they have diabetes or no diabetes for each age group.

``` r
prop.table(table(markdown_data$Age, markdown_data$Diabetes_binary,  
      dnn = c("Age", "Diabetes")))
```

    ##                  Diabetes
    ## Age                        No         Yes
    ##   Age 18 to 24    0.019940916 0.000422030
    ##   Age 25 to 29    0.019202363 0.001055075
    ##   Age 30 to 34    0.036927622 0.002426672
    ##   Age 35 to 39    0.040092847 0.004009285
    ##   Age 40 to 44    0.046950834 0.007491032
    ##   Age 45 to 49    0.053175775 0.012133361
    ##   Age 50 to 54    0.080818738 0.022051066
    ##   Age 55 to 59    0.083878455 0.030808187
    ##   Age 60 to 64    0.075226841 0.033867905
    ##   Age 65 to 69    0.074066259 0.033762397
    ##   Age 70 to 74    0.078075543 0.037349652
    ##   Age 75 to 79    0.067419287 0.028803545
    ##   Age 80 or older 0.081979321 0.028064993

``` r
table(markdown_data$Age, markdown_data$Diabetes_binary, 
                 markdown_data$PhysActivity, 
      dnn = c("Age", "Diabetes", "Physical Activity"))
```

    ## , , Physical Activity = No
    ## 
    ##                  Diabetes
    ## Age                No Yes
    ##   Age 18 to 24     38   1
    ##   Age 25 to 29     49   4
    ##   Age 30 to 34    107   7
    ##   Age 35 to 39    120  20
    ##   Age 40 to 44    195  37
    ##   Age 45 to 49    194  50
    ##   Age 50 to 54    342 110
    ##   Age 55 to 59    358 146
    ##   Age 60 to 64    320 157
    ##   Age 65 to 69    298 142
    ##   Age 70 to 74    308 176
    ##   Age 75 to 79    319 136
    ##   Age 80 or older 333 152
    ## 
    ## , , Physical Activity = Yes
    ## 
    ##                  Diabetes
    ## Age                No Yes
    ##   Age 18 to 24    151   3
    ##   Age 25 to 29    133   6
    ##   Age 30 to 34    243  16
    ##   Age 35 to 39    260  18
    ##   Age 40 to 44    250  34
    ##   Age 45 to 49    310  65
    ##   Age 50 to 54    424  99
    ##   Age 55 to 59    437 146
    ##   Age 60 to 64    393 164
    ##   Age 65 to 69    404 178
    ##   Age 70 to 74    432 178
    ##   Age 75 to 79    320 137
    ##   Age 80 or older 444 114

### Plots

Next, let’s investigate BMI, whether or not a person has diabetes, and
Age on a plot. A higher BMI can indicate high body fat, which can lead
to health problems such as diabetes. The plot just shows Age and BMI in
relation with each other and color codes observations by whether or not
they have diabetes.

``` r
ggplot(markdown_data, aes(x=BMI, y=Age, col=Diabetes_binary)) + 
  geom_point() +
  xlab("Body Mass Index (BMI)") +
  ylab("Age") +
  scale_color_brewer(palette = "Dark2") +
  labs(color="Diabetes") + 
  ggtitle("Age and BMI In Relation to Diabetes Condition") +
  theme_bw() 
```

![](SOMEHI~1/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

The next plot will display an individual’s physical activity and dietary
habits in relation to their BMI and whether or not they have diabetes.
The first plot is for Physical Activity, and the next two plots are
regarding their intake of Fruits and Vegetables respectively in relation
to their BMI and whether they have diabetes. The plot color codes
observations based on if individual has diabetes or not for each plot
below.

``` r
ggplot(markdown_data, aes(x=BMI, y=PhysActivity, col=Diabetes_binary)) + 
  geom_point() +
  xlab("Body Mass Index (BMI)") +
  ylab("Participates In Physical Activity") +
  scale_color_brewer(palette = "Dark2") +
  labs(color="Diabetes") + 
  ggtitle("Physical Activity and BMI In Relation to Diabetes Condition") +
  theme_bw() 
```

![](SOMEHI~1/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
ggplot(markdown_data, aes(x=BMI, y=Fruits, col=Diabetes_binary)) + 
  geom_point() +
  xlab("Body Mass Index (BMI)") +
  ylab("Fruit Intake") +
  scale_color_brewer(palette = "Dark2") +
  labs(color="Diabetes") + 
  ggtitle("Fruit Intake and BMI In Relation to Diabetes Condition") +
  theme_bw() 
```

![](SOMEHI~1/figure-gfm/unnamed-chunk-6-2.png)<!-- -->

``` r
ggplot(markdown_data, aes(x=BMI, y=Veggies, col=Diabetes_binary)) + 
  geom_point() +
  xlab("Body Mass Index (BMI)") +
  ylab("Vegetable Intake") +
  scale_color_brewer(palette = "Dark2") +
  labs(color="Diabetes") + 
  ggtitle("Vegetable Intake and BMI In Relation to Diabetes Condition") +
  theme_bw()
```

![](SOMEHI~1/figure-gfm/unnamed-chunk-6-3.png)<!-- -->

Finally, we can view a few count plots that explore Sex and Age across
the dataset. The first plot here shows the frequency of Males and
Females for each age group. The second plot shows the frequency of each
gender and if their condition regarding if they have diabetes or not.

``` r
ggplot(markdown_data, aes(x = Sex, col = Age)) +        
  geom_bar(position = "dodge", fill = "lightgray") +
  xlab("Gender") +
  ylab("Count") +
  ggtitle("Gender and Age of Individuals") +
  theme_bw()
```

![](SOMEHI~1/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
ggplot(markdown_data, aes(x = Sex)) +        
  geom_bar(fill = "lightgray", col = "black") +
  facet_grid(~ Diabetes_binary) +
  xlab("Gender") +
  ylab("Count") +
  theme_bw() + theme(
   strip.background = element_rect(
     color="black", fill="lightblue", size=1, linetype="solid"
     ) 
   ) +
  ggtitle("Frequency of Gender in Relation to Diabetes Condition") +
  geom_text(stat='count', aes(label= after_stat(count)), vjust=1)
```

![](SOMEHI~1/figure-gfm/unnamed-chunk-7-2.png)<!-- -->

# Modeling

In this section we will fit several models on our dataset with the
predictor being the aforementioned binary diabetes variable. First,
we’ll split our data into a training and testing set.

``` r
# Set seed for reproducible purposes
set.seed(1234)


# p = 0.70 at the end implies 70% of data goes into the training set
trainingIndex <- createDataPartition(markdown_data$Diabetes_binary, p=0.70, list=FALSE)

# Actually splitting into testing and training
train <- markdown_data[trainingIndex,]
test <- markdown_data[-trainingIndex,]
```

### Log Loss

Log Loss is one of many metrics available to evaluate classification
models. The lower the Log Loss, the better predictability a model has. A
classification model will compute the probability that given a set of
variables, how likely is it that this set belongs to the diabetes or no
diabetes class. Therefore, the farther our predicted probability is from
the observed class (0 or 1), the higher the Log Loss.

We may prefer Log Loss to accuracy given it’s more complex relation with
the entire model. Accuracy simply determines how well our model
classified data correctly and only cares about the observed class.
Accuracy does not take into account the probability/uncertainty
associated with the prediction like Log Loss does.

### Logistic Regression

Logistic Regression is primarily used in binary classification tasks
where we are trying to classify a response variable with two outcomes.
Logistic Regression has no linearity assumptions, which can be a benefit
when we’re dealing with a binary target variable like diabetes. We are
able to model the average number of success for a given x, or rather the
average number of “yes” answers for the given slate of variables in this
instance. This model links the linear form of the model to the mean (the
previously mentioned average number of success for a given x).

We fit three models here. The first includes all the variables of
interest. The second model removes diet items such as the Fruits and
Veggies variables, along with the Stroke variable. The third model
includes interaction terms between Age and Sex, Smoker and Stroke, and
BMI and PhysActivity.

``` r
# 5-fold cross-validation for each method using LogLoss as summary function
ctrl <- trainControl(method = "cv", classProbs = TRUE ,number = 5, 
                     summaryFunction = mnLogLoss)

# Model 1, all variables
logistic_model_1 <- train(Diabetes_binary ~ HighBP + HighChol + BMI + PhysActivity + 
                            Fruits + Veggies + Sex + Age + Smoker + Stroke, 
                          data = train, method = "glm", family = "binomial", 
                          trControl = ctrl, metric = "logLoss")

# Model 2, removing Fruits, Veggies, and Stroke
logistic_model_2 <- train(Diabetes_binary ~ HighBP + HighChol + BMI + PhysActivity + 
                            Sex + Age + Smoker, data = train, method = "glm",
                          family = "binomial", trControl = ctrl, 
                          metric = "logLoss")

# Model 3, adding interactions
logistic_model_3 <- train(Diabetes_binary ~ HighBP + HighChol + BMI*PhysActivity + 
                            Age*Sex + Smoker*Stroke, data = train, 
                          method = "glm", family = "binomial", trControl = ctrl, 
                          metric = "logLoss")

paste("The lowest log loss among these models is:",
      round(min(
logistic_model_1$results$logLoss,
logistic_model_2$results$logLoss,
logistic_model_3$results$logLoss), 4)
      )
```

    ## [1] "The lowest log loss among these models is: 0.4858"

``` r
if (min(
logistic_model_1$results$logLoss,
logistic_model_2$results$logLoss,
logistic_model_3$results$logLoss)
  ==
logistic_model_1$results$logLoss
) {
  
  logistic_best_model <- logistic_model_1
  print("The best model here is model 1.")
  
} else if(min(
logistic_model_1$results$logLoss,
logistic_model_2$results$logLoss,
logistic_model_3$results$logLoss)
  ==
logistic_model_2$results$logLoss
) {
  
  logistic_best_model <- logistic_model_2
  print("The best model here is model 2.")
  
} else {
  
  logistic_best_model <- logistic_model_3
  print("The best model here is model 3.")
  
}
```

    ## [1] "The best model here is model 3."

### LASSO Regression

LASSO regression stands for Least Absolute Shrinkage and Selection
Operator. This method uses shrinkage methods in an attempt to improve
predictions. Shrinkage is when values of data are shrunk towards some
mean. The LASSO method estimates the regression coefficients by
minimizing them for a lambda \> 0. This is particularly used over
regular logistic so that we can select models with fewer parameters as a
simpler model with important variables is preferred over a complex model
with large amount of variables.

``` r
# Training Lasso Model
lasso_model <- train(Diabetes_binary ~ HighBP + HighChol + BMI + PhysActivity + Fruits + Veggies + Sex + Age + Smoker + Stroke, data = train, method = "glmnet", tuneGrid = expand.grid(alpha = 1, lambda = seq(0, 1, by = 0.1)), trControl = ctrl, metric = "logLoss")
```

### Classification Tree

A classification tree is used when our goal is to classify group
membership, in this case, classify whether or not an individual has
diabetes. Like any tree based method, a classification tree will split
the predictor space into many regions and have different predictions for
each region. The tree is split by minimizing the Gini Coefficient, a
number that represents how well each node classifies.

``` r
# Training Classification Tree Model
classification_tree_model <- train(Diabetes_binary ~ HighBP + HighChol + BMI + PhysActivity + Fruits + Veggies + Sex + Age + Smoker + Stroke, data = train, method = "rpart", tuneGrid = expand.grid(cp = seq(0,1, by = 0.01)), trControl = ctrl, metric = "logLoss")
```

### Random Forest

A Random Forest creates multiple bagged trees then averages the results.
For each tree a random subset of predictors is chosen to fit the tree.
This way, a single strong predictor won’t dominate the tree fits as it
will be left out of some of the other trees by random chance. We might
choose the Random Forest over the Classification Tree because the Random
Forest is typically stronger in prediction. Due to averaging over a
large number of tree fits the variance decreases and prediction
improves.

``` r
# Training Random Forest Model, used number of trees 
random_forest_model <- train(Diabetes_binary ~ HighBP + HighChol + BMI + PhysActivity + Fruits + Veggies + Sex + Age + Smoker + Stroke, data = train, method = "rf", tuneGrid = expand.grid(mtry = 2:4), trControl = ctrl, metric = "logLoss", ntree = 100)
```

*Number of trees limited to 100 (rather than default of 500) to save
computing time*

### Non-Class Model \#1 - Naive Bayes Classifer

The Naive Bayes Classifier is a method based on Bayes theorem which
assumes that predictors selected for a model are independent of each
other. It is used in classification tasks which is why it is a suitable
method here to predict if a person has diabetes or not. In the context
of our problem it essentially predicts the probability of an observation
having diabetes or not diabetes given specific predictor variables.

``` r
# Training Naive Bayes Model
naive_bayes_model <- train(Diabetes_binary ~ HighBP + HighChol + BMI + PhysActivity + Fruits + Veggies + Sex + Age + Smoker + Stroke, data = train, method = "naive_bayes", trControl = ctrl, metric = "logLoss")
```

### Non-Class Model \#2 - Linear Discriminant Analysis

Linear Discriminant Analysis essentially works by finding the best
linear combination of features that separates the response variable’s
classes. We assume that the predictor variables have multivariate normal
density with different mean vectors but same covariance matrix for each
class in response variable. It can also be used as dimensionality
reduction technique.

``` r
# Training LDA model
lda_model <- train(Diabetes_binary ~ HighBP + HighChol + BMI + PhysActivity + Fruits + Veggies + Sex + Age + Smoker + Stroke, data = train, method = "lda", trControl = ctrl, metric = "logLoss")
```

# Final Model Selection

Here we will choose the best model based on logLoss. The model with the
lowest logLoss is chosen as the best model.

``` r
# Make our predictions with test set
LogisticPredictsProb     <- predict(logistic_best_model, newdata = test, type = "prob")
LassoPredictsProb        <- predict(lasso_model, newdata = test, type = "prob")
ClassTreePredictsProb    <- predict(classification_tree_model, newdata = test, type = "prob")
RandomForestPredictsProb <- predict(random_forest_model, newdata = test, type = "prob")
BayesPredictsProb        <- predict(naive_bayes_model, newdata = test, type = "prob")
LDAPredictsProb         <- predict(lda_model, newdata = test, type = "prob")


# Function to account for cases in probability where its either 0 or 1.

Edit_Predictions <- function(predicted_probabilities) {
  predicted_probabilities$No <- ifelse(predicted_probabilities$No == 0, 0.001, ifelse(predicted_probabilities$No == 1, 0.999, predicted_probabilities$No))
  predicted_probabilities$Yes <- ifelse(predicted_probabilities$Yes == 0, 0.001, ifelse(predicted_probabilities$Yes == 1, 0.999, predicted_probabilities$Yes))
  return(predicted_probabilities)
}

# Calling function
LogisticPredictsProb <- Edit_Predictions(LogisticPredictsProb)
LassoPredictsProb <- Edit_Predictions(LassoPredictsProb)
ClassTreePredictsProb <- Edit_Predictions(ClassTreePredictsProb)
RandomForestPredictsProb <- Edit_Predictions(RandomForestPredictsProb)
BayesPredictsProb <- Edit_Predictions(BayesPredictsProb)
LDAPredictsProb <-Edit_Predictions(LDAPredictsProb)

# Calculating log loss values
logLoss_Logistic <- logLoss(as.numeric(test$Diabetes_binary == "Yes"), LogisticPredictsProb$Yes)
logLoss_Lasso <- logLoss(as.numeric(test$Diabetes_binary == "Yes"), LassoPredictsProb$Yes)
logLoss_ClassTree <- logLoss(as.numeric(test$Diabetes_binary == "Yes"), ClassTreePredictsProb$Yes)
logLoss_RandomForest <- logLoss(as.numeric(test$Diabetes_binary == "Yes"), RandomForestPredictsProb$Yes)
logLoss_NaiveBayes <- logLoss(as.numeric(test$Diabetes_binary == "Yes"), BayesPredictsProb$Yes)
logLoss_LDA <- logLoss(as.numeric(test$Diabetes_binary == "Yes"), LDAPredictsProb$Yes)

# Storing Log Loss Values into dataframe
LogLoss_Values <- data.frame(Model = c("Logistic", "Lasso", "Classification Tree", "Random Forest", "Naive Bayes", "LDA"), LogLossValue = c(logLoss_Logistic, logLoss_Lasso, logLoss_ClassTree, logLoss_RandomForest, logLoss_NaiveBayes, logLoss_LDA))
LogLoss_Values

# Getting index of most minimum log loss value
index_best_model <- which.min(LogLoss_Values$LogLossValue)

# Printing best model along with value
cat("The best model is", LogLoss_Values$Model[index_best_model], "with value of", LogLoss_Values$LogLossValue[index_best_model])
```

    ## The best model is Logistic with value of 0.478742
