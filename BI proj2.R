if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv", dependencies = TRUE)
}
library(renv)

if (!requireNamespace("languageserver", quietly = TRUE)) {
  install.packages("languageserver", dependencies = TRUE)
}
library(languageserver)

if (!requireNamespace("readr", quietly = TRUE)) {
  install.packages("readr", dependencies = TRUE)
}
library(readr)

if (!requireNamespace("Amelia", quietly = TRUE)) {
  install.packages("Amelia", dependencies = TRUE)
}
library(Amelia)

if (!requireNamespace("ggcorrplot", quietly = TRUE)) {
  install.packages("ggcorrplot", dependencies = TRUE)
}
library(ggcorrplot)
if (!requireNamespace("e1071", quietly = TRUE)) {
  install.packages("e1071", dependencies = TRUE)
}
library(e1071)
if (!requireNamespace("caret", quietly = TRUE)) {
  install.packages("caret")
}
library(caret)
if (!requireNamespace("corrplot", quietly = TRUE)) {
  install.packages("corrplot")
}
library(corrplot)
# Install and load the 'ipred' package
if (!requireNamespace("ipred", quietly = TRUE)) {
  install.packages("ipred", dependencies = TRUE)
}
library(ipred)#
# Install and load the 'earth' package
if (!requireNamespace("earth", quietly = TRUE)) {
  install.packages("earth", dependencies = TRUE)
}
library(earth)
# Install and load the 'randomForest' package
if (!requireNamespace("randomForest", quietly = TRUE)) {
  install.packages("randomForest", dependencies = TRUE)
}
library(randomForest)
install.packages("SuperLearner")
library(SuperLearner)
install.packages("caretEnsemble")
library(caretEnsemble)
library(boot)

library(readr)
Supply_Demand_Oil <- read_csv("data/Supply_Demand_Oil.csv")

# View the data
View(Supply_Demand_Oil)

# Check dimensions and data types
dim(Supply_Demand_Oil)
sapply(Supply_Demand_Oil, class)

# Replace missing values with column means
Supply_Demand_Oil <- as.data.frame(lapply(Supply_Demand_Oil, function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x)))

# Frequency and percentage for selected columns
columns_to_analyze <- c(2, 4, 7)
for (col in columns_to_analyze) {
  Supply_Demand_Oil_freq <- Supply_Demand_Oil[[col]]
  freq_table <- table(Supply_Demand_Oil_freq)
  result_table <- cbind(frequency = freq_table, percentage = prop.table(freq_table) * 100)
  print(result_table)
}

# Mode calculation for selected columns
calculate_mode <- function(column) {
  mode_names <- names(table(column))[which(table(column) == max(table(column)))]
  return(mode_names)
}

Supply_Demand_Oil_ProductPrice_mode <- calculate_mode(Supply_Demand_Oil$Product.Price)
print(Supply_Demand_Oil_ProductPrice_mode)

Supply_Demand_Oil_Category <- calculate_mode(Supply_Demand_Oil$Category)
print(Supply_Demand_Oil_Category)

Supply_Demand_Oil_ProductType_mode <- calculate_mode(Supply_Demand_Oil$Product.Type)
print(Supply_Demand_Oil_ProductType_mode)

# Summary statistics
summary(Supply_Demand_Oil)
any(is.na(Supply_Demand_Oil))
Supply_Demand_Oil <- as.data.frame(lapply(Supply_Demand_Oil, function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x)))

# Descriptive statistics for "Product Price"
sapply(Supply_Demand_Oil[c(7)], sd)
sapply(Supply_Demand_Oil[c(7)], var)
sapply(Supply_Demand_Oil[c(7)], kurtosis, type = 2)
sapply(Supply_Demand_Oil[c(7)], skewness, type = 2)

# Covariance and correlation matrices
Supply_Demand_Oil_cov <- cov(Supply_Demand_Oil[c(7)])
View(Supply_Demand_Oil_cov)

Supply_Demand_Oil_cor <- cor(Supply_Demand_Oil[c(7)])
View(Supply_Demand_Oil_cor)

# One-way ANOVA
Supply_Demand_Oil_one_way_anova <- aov(Product.Price ~ Product.Type, data = Supply_Demand_Oil)
summary(Supply_Demand_Oil_one_way_anova)

# Bar plots for selected columns
par(mfrow = c(1, length(7)))
for (col in 7) {
  barplot(table(Supply_Demand_Oil[[col]]), main = col)
}

# Missing values visualization
missmap(Supply_Demand_Oil, col = c("red", "grey"), legend = TRUE)

# Plot correlation matrix
corrplot(cor(Supply_Demand_Oil[c(7)]), method = "circle")

# Confirmation of Missing Values
missing_values <- colSums(is.na(Supply_Demand_Oil))
print(missing_values)

# Data Imputation
imputed_values <- as.data.frame(lapply(Supply_Demand_Oil[, numeric_columns], function(x) {
  ifelse(is.na(x), { 
    imputed_value <- mean(x, na.rm = TRUE)
    print(paste("Imputed value", names(x), ":", imputed_value))
    imputed_value
  }, x)
}))

# Data Transformation (Log Transformation for Product Price)
Supply_Demand_Oil$Product.Price <- log(Supply_Demand_Oil$Product.Price + 1)

# Data Transformation (Z-Score Normalization for Numeric Columns)
numeric_columns <- sapply(Supply_Demand_Oil, is.numeric)
Supply_Demand_Oil[, numeric_columns] <- scale(Supply_Demand_Oil[, numeric_columns])

# Data Transformation (Box-Cox Transformation for Numeric Columns)
library(MASS)  # Ensure the MASS package is installed
numeric_columns <- sapply(Supply_Demand_Oil, is.numeric)
Supply_Demand_Oil[, numeric_columns] <- lapply(Supply_Demand_Oil[, numeric_columns], function(x) {
  if (all(x > 0)) {
    boxcox(x)$x
  } else {
    x
  }
})

# Data Transformation (One-Hot Encoding for Categorical Columns)
categorical_columns <- sapply(Supply_Demand_Oil, function(x) is.factor(x) | is.character(x))
Supply_Demand_Oil <- model.matrix(~., Supply_Demand_Oil[, c(7, categorical_columns)], drop = TRUE)


# Data Transformation (One-Hot Encoding for Categorical Columns)
categorical_columns <- sapply(Supply_Demand_Oil, function(x) is.factor(x) | is.character(x))
Supply_Demand_Oil <- model.matrix(~., Supply_Demand_Oil[, c(7, categorical_columns)], drop = TRUE)

# Load required libraries
library(readr)
library(caret)
library(rpart)
library(kknn)
library(ROCR)
library(pROC)
library(MASS)
library(readr)
Supply_Demand_Oil <- read_csv("data/Supply_Demand_Oil.csv")

# Replace missing values with column means
Supply_Demand_Oil <- as.data.frame(lapply(Supply_Demand_Oil, function(x) {
  if (is.numeric(x) || is.logical(x)) {
    ifelse(is.na(x), mean(x, na.rm = TRUE), x)
  } else {
    x
  }
}))

# Check for missing values
if (any(is.na(Supply_Demand_Oil))) {
  # Handle missing values (e.g., imputation)
  Supply_Demand_Oil <- na.omit(Supply_Demand_Oil)
}

# 1. Data Splitting ----
set.seed(123)  # Set a seed for reproducibility
train_index <- createDataPartition(Supply_Demand_Oil$Product.Price, p = 0.75, list = FALSE)
Supply_Demand_Oil_train <- Supply_Demand_Oil[train_index, ]
Supply_Demand_Oil_test <- Supply_Demand_Oil[-train_index, ]

your_statistic_function <- function(data, indices) {
  sample_data <- data[indices]
  return(mean(sample_data))
}

# 2. Bootstrapping ----
bootstrap_results <- boot(data = Supply_Demand_Oil$Product.Price, statistic = your_statistic_function, R = 1000)

# 3. Cross Validation ----
train_control <- trainControl(method = "cv", number = 5)
set.seed(456)  # Set a seed for reproducibility
ProductPrice_model <- train(Product.Price ~ ., data = Supply_Demand_Oil_train, method = "rf", trControl = train_control)

# 4. Model Training ----
set.seed(789)  # Set a seed for reproducibility
ProductPrice2_model <- train(Product.Price ~ ., data = Supply_Demand_Oil_train, method = "rf")

# 5. Model Performance Comparison ----
set.seed(101)  # Set a seed for reproducibility
ProductPrice_model <- train(Product.Price ~ ., data = Supply_Demand_Oil_train, method = "rf", trControl = train_control)
ProductPrice2_model <- train(Product.Price ~ ., data = Supply_Demand_Oil_train, method = "rf", trControl = train_control)

# Compare models
compare_models <- resamples(list(Model1 = ProductPrice_model, Model2 = ProductPrice2_model))
summary(compare_models)

# Manual Search
train_control <- trainControl(method = "repeatedcv", number = 10, repeats = 3, search = "random")

tunegrid <- expand.grid(.mtry = c(1:5))

modellist <- list()
for (ntree in c(500, 800, 1000)) {
  set.seed(123)  # Set a seed for reproducibility
  rf_model <- train(Product.Price ~ ., data = Supply_Demand_Oil_train, method = "rf", metric = "RMSE",
                    tuneGrid = tunegrid,
                    trControl = train_control,
                    ntree = ntree)
  key <- toString(ntree)
  modellist[[key]] <- rf_model
}

# Lastly, we compare results to find which parameters gave the lowest RMSE
print(modellist)

results <- resamples(modellist)
summary(results)
dotplot(results)

# Bagging
# Train the bagged model using randomForest
set.seed(123)
bag_model <- randomForest(Product.Price ~ ., data = Supply_Demand_Oil_train, ntree = 500, importance = TRUE)
# Make sure 'Product.Price' is a numeric column in Supply_Demand_Oil_test
Supply_Demand_Oil_test$Product.Price <- as.numeric(Supply_Demand_Oil_test$Product.Price)

# Make predictions on the test set
bag_predictions <- predict(bag_model, newdata = Supply_Demand_Oil_test)

# Evaluate performance
bag_rmse <- sqrt(mean((bag_predictions - Supply_Demand_Oil_test$Product.Price)^2))
cat("Bagging RMSE:", bag_rmse, "\n")

# Create a caret model from the bagged model
bag_caret_model <- list(model = bag_model, method = "rf", trControl = train_control)

# Boosting
set.seed(456)
boost_model <- train(Product.Price ~ ., data = Supply_Demand_Oil_train, method = "gbm", trControl = train_control)

# Create a caret model from the boosting model
boost_caret_model <- list(model = boost_model, method = "gbm", trControl = train_control)

# Stacking
# Create a list of caret models for stacking
models_for_stacking <- list(bag = bag_caret_model, boost = boost_caret_model)

# Use caretStack to stack the models
stack_model <- caretStack(models_for_stacking)

# Make predictions on the test set using the stacked model
stack_predictions <- predict(stack_model, newdata = Supply_Demand_Oil_test)

# Evaluate performance of the stacked model
stack_rmse <- sqrt(mean((stack_predictions - Supply_Demand_Oil_test$Product.Price)^2))
cat("Stacked Model RMSE:", stack_rmse, "\n")

