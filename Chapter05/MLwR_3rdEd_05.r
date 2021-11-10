##### Chapter 5: Classification using Decision Trees and Rules -------------------
library(skimr)
import::from("sjmisc", "frq")

#### Part 1: Decision Trees -------------------

## Understanding Decision Trees ----
# calculate entropy of a two-class segment
-0.60 * log2(0.60) - 0.40 * log2(0.40)

curve(-x * log2(x) - (1 - x) * log2(1 - x),
      col = "red", xlab = "x", ylab = "Entropy", lwd = 4)

## Example: Identifying Risky Bank Loans ----
## Step 2: Exploring and preparing the data ----
credit <- read.csv("Chapter05/credit.csv", stringsAsFactors = TRUE)

skim(credit)

# look at two characteristics of the applicant
frq(credit$checking_balance)
frq(credit$savings_balance)

# look at two characteristics of the loan
summary(credit$months_loan_duration)
summary(credit$amount)

# look at the class variable
frq(credit$default)

# create a random sample for training and test data
RNGversion("3.5.2") # use an older random number generator to match the book
set.seed(123) # use set.seed to use the same random number sequence as the tutorial
train_sample <- sample(1000, 900)

str(train_sample)

# split the data frames
credit_train <- credit[train_sample, ]
credit_test  <- credit[-train_sample, ]

# check the proportion of class variable
frq(credit_train$default)
frq(credit_test$default)

## Step 3: Training a model on the data ----
# build the simplest decision tree
library(C50)
credit_model <- C5.0(credit_train[-17], credit_train$default)

# credit_model <- C5.0(credit_train[-17], credit_train$default,
#                      rules = TRUE)

# display simple facts about the tree
credit_model

# display detailed information about the tree
summary(credit_model)
plot(credit_model)
C5imp(credit_model)

## Step 4: Evaluating model performance ----
# create a factor vector of predictions on test data
credit_pred <- predict(credit_model, credit_test)

credit_pred
summary(credit_pred)

# cross tabulation of predicted versus actual classes
library(gmodels)
CrossTable(credit_test$default, credit_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))

## Step 5: Improving model performance ----

## Boosting the accuracy of decision trees
# boosted decision tree with 10 trials
credit_boost10 <- C5.0(credit_train[-17], credit_train$default,
                       trials = 10)
# credit_boost10 <- C5.0(credit_train[-17], credit_train$default,
#                        trials = 10,
#                        rules = TRUE)

credit_boost10
summary(credit_boost10)
plot(credit_boost10)
C5imp(credit_boost10)

credit_boost_pred10 <- predict(credit_boost10, credit_test)
CrossTable(credit_test$default, credit_boost_pred10,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))

## Making some mistakes more costly than others

# create dimensions for a cost matrix
matrix_dimensions <- list(c("no", "yes"), c("no", "yes"))
names(matrix_dimensions) <- c("predicted", "actual")
matrix_dimensions

# build the matrix
error_cost <- matrix(c(0, 1, 4, 0), nrow = 2, 
                     dimnames = matrix_dimensions)
error_cost

# apply the cost matrix to the tree
credit_cost <- C5.0(credit_train[-17], credit_train$default,
                    costs = error_cost)
# credit_cost <- C5.0(credit_train[-17], credit_train$default,
#                     costs = error_cost,
#                     rules = TRUE)

plot(credit_cost)
C5imp(credit_cost)

credit_cost_pred <- predict(credit_cost, credit_test)

CrossTable(credit_test$default, credit_cost_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))

#### Part 2: Rule Learners -------------------

## Example: Identifying Poisonous Mushrooms ----
## Step 2: Exploring and preparing the data ---- 
mushrooms <- read.csv("Chapter05/mushrooms.csv", stringsAsFactors = TRUE)

# examine the structure of the data frame
skim(mushrooms)

# drop the veil_type feature
mushrooms$veil_type <- NULL

# examine the class distribution
frq(mushrooms$type)

## Step 3: Training a model on the data ----
library(OneR)

# train OneR() on the data
mushroom_1R <- OneR(type ~ ., data = mushrooms)

## Step 4: Evaluating model performance ----
mushroom_1R

mushroom_1R_pred <- predict(mushroom_1R, mushrooms)
table(actual = mushrooms$type, predicted = mushroom_1R_pred)

## Step 5: Improving model performance ----
library(RWeka)
mushroom_JRip <- JRip(type ~ ., data = mushrooms)
mushroom_JRip
summary(mushroom_JRip)

# Rule Learner Using C5.0 Decision Trees (not in text)
library(C50)
mushroom_c5rules <- C5.0(type ~ odor + gill_size, data = mushrooms, rules = TRUE)
summary(mushroom_c5rules)
