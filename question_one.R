# Question One
# install and load the necessary packages
install.packages("readr")
library(readr)
# Install the caret package
install.packages("caret")

# load the dataset
used_cars <- read_csv("used_cars.csv")

# Remove rows with missing values
used_cars <- na.omit(used_cars)
################################################################################################################################################################
#(a) Train the model using 80% of this dataset and suggest an appropriate GLM to model output to input variables.
################################################################################################################################################################
#Next, I will split the dataset into training and testing sets using the createDataPartition() function from the caret package:
set.seed(123)
# Create data partition
# Load the caret package
library(caret)
train_index <- createDataPartition(used_cars$price, p = 0.8, list = FALSE)
train_data <- used_cars[train_index, ]
test_data <- used_cars[-train_index, ]

# Now, I can build a GLM model using the glm() function. Since I want to predict the price of the car, I will set price as the output variable and use the remaining variables as input variables.
glm_model <- glm(price ~ ., data = train_data, family = "gaussian")
# I set family = "gaussian" because I am predicting a continuous variable (price).
# Or can be like: model <- glm(price ~ acquisition_date + badge + body_type + category + colour + cylinders + economy + fuel, data = train_data, family = gaussian(link = "identity"))

#To check the summary of the model, using the summary() function:
summary(glm_model)

# This will show the coefficients, standard errors, t-values, and p-values for each variable in the model.

# Based on the summary, I can see that the variables acquisition_date, badge, body_type, category, colour, cylinders, economy, and fuel all have p-values less than 0.05 and are therefore statistically significant.
# Therefore, an appropriate GLM to model output to input variables could be:
# price = β0 + β1 * acquisition_date + β2 * badge + β3 * body_type + β4 * category + β5 * colour + β6 * cylinders + β7 * economy + β8 * fuel
# where β0, β1, β2, β3, β4, β5, β6, β7, and β8 are the coefficients for the intercept, acquisition date, badge, body type, category, colour, cylinders, economy, and fuel, respectively.

################################################################################################################################################################
# (b)	Specify the significant variables on the output variable at the level of 𝛼=0.05 and explore the related hypotheses test. Estimate the parameters of your model. 
################################################################################################################################################################
# To specify the significant variables on the output variable at the level of 𝛼=0.05, I can perform a hypothesis test on each of the variables in our model.

# I will use the summary() function to obtain the coefficient estimates, standard errors, t-values, and p-values for each predictor variable.

# I will use a significance level of α=0.05. For each predictor variable, I will test the null hypothesis that its coefficient is equal to zero (i.e., it has no effect on the outcome variable) versus the alternative hypothesis that its coefficient is not equal to zero (i.e., it has a significant effect on the outcome variable).
 
# I can then identify the significant variables based on their p-values. If the p-value is less than α, we reject the null hypothesis and conclude that the variable is significant.
 
# Here is the R code to estimate the parameters of our GLM model and test for significant variables:

# Fit GLM model
train <- as.data.frame(train_data)
glm_model <- glm(price ~ acquisition_date + badge + body_type + category + colour + cylinders + economy + fuel, data = train, family = "gaussian")

# Summarize model results
summary(glm_model)

# Test for significant variables
alpha <- 0.05
p_values <- summary(glm_model)$coefficients[, 4]
significant_vars <- names(p_values[p_values < alpha])
significant_vars
# This will output the summary of the GLM model and the names of the significant predictor variables. The significant variables are those with a p-value less than α=0.05.


################################################################################################################################################################
# (c)	Predict the output of the test dataset using the trained model. Provide the functional form of the optimal predictive model. 
################################################################################################################################################################
# To predict the output of the test dataset using the trained model, I can use the predict() function in R. 
# Here's the code:

test_data$badge <- factor(test_data$badge, levels = levels(train_data$badge))
# Predict the output of the test dataset
test_pred <- predict(glm_model, newdata = test_data)

# Print the predicted values
test_pred
# The predict() function takes two arguments - the glm model object and the new data for which I want to make predictions. In this case, I want to predict the price of the used cars in the test # dataset using the glm_model object I trained earlier.

# To provide the functional form of the optimal predictive model, we can use the formula of the glm() function we used earlier:
# price = β0 + β1 * acquisition_date + β2 * badge + β3 * body_type + β4 * category + β5 * colour + β6 * cylinders + β7 * economy + β8 * fuel
# where β0, β1, β2, ..., β8 are the estimated coefficients of the model. These coefficients can be obtained using the summary() function in R, which provides a summary of the glm model object including the estimated coefficients and their standard errors:
# Get the summary of the GLM model
summary(glm_model)
#This will output a summary of the model including the estimated coefficients, their standard errors, t-values, and p-values. I can use the estimated coefficients to write the functional form of the optimal predictive model.

################################################################################################################################################################
# (d)	Provide the confusion matrix and obtain the probability of correctness of predictions. 
################################################################################################################################################################
# Some data preparation
# Combine the training and test datasets
combined_data <- rbind(train_data, test_data)

# Convert the badge variable to a factor
combined_data$badge <- as.factor(combined_data$badge)

# Split the combined dataset back into training and test datasets
train_data <- combined_data[1:nrow(train_data), ]
test_data <- combined_data[(nrow(train_data) + 1):nrow(combined_data), ]

unique(train_data$badge)
unique(test_data$badge)

# To obtain the confusion matrix and probability of correctness of predictions, I need to compare the predicted values of the test set with the actual values and calculate the accuracy.
# Here's the code to obtain the confusion matrix and accuracy:

# Convert predicted values to binary classification
test_pred_binary <- ifelse(test_pred > median(train$price), "high", "low")
test_actual_binary <- ifelse(test_data$price > median(train$price), "high", "low")

# Create confusion matrix
conf_matrix <- table(Predicted = test_pred_binary, Actual = test_actual_binary)
conf_matrix

# Calculate accuracy
accuracy <- sum(diag(conf_matrix))/sum(conf_matrix)
accuracy
#This code will give the confusion matrix and the accuracy of the model. The confusion matrix shows the number of correct and incorrect predictions for each class. The accuracy gives the proportion of correct predictions overall.