# Import necessary libraries
library(readr)
library(caret) # For train/test split
library(Metrics) # For MSE calculation

# Load the dataset
file_path <- '/content/SeoulBikeData.csv'
data <- read_csv("C:/Users/ASUS/Downloads/SeoulBikeData.csv")

# Select relevant features and the target variable
X <- data[c('Temperature(?)', 'Humidity(%)', 'Wind speed (m/s)')]
y <- data$`Rented Bike Count`

# Split the data into training and testing sets (80% training, 20% testing)
set.seed(42)
train_index <- createDataPartition(y, p=0.8, list=FALSE)
X_train <- X[train_index, ]
X_test <- X[-train_index, ]
y_train <- y[train_index]
y_test <- y[-train_index]

# Initialize the linear regression model
model <- lm(y_train ~ ., data=X_train)

# Make predictions on the test set
y_pred <- predict(model, X_test)

# Evaluate the model
mse <- mse(y_test, y_pred)
r2 <- summary(model)$r.squared

# Display results
cat("Mean Squared Error (MSE):", mse, "\n")
cat("R-squared (R2):", r2, "\n")
cat("Model Coefficients:", coef(model), "\n")
cat("Model Intercept:", coef(model)[1], "\n")
# Import necessary libraries for visualization
library(ggplot2)

# Create a data frame with actual and predicted values
results <- data.frame(Actual = y_test, Predicted = y_pred)

# Scatter plot of actual vs. predicted values
ggplot(results, aes(x = Actual, y = Predicted)) +
  geom_point(color = 'blue', alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, color = 'red', linetype = 'dashed', size = 1) +
  ggtitle('Actual vs Predicted Rented Bike Count') +
  xlab('Actual Rented Bike Count') +
  ylab('Predicted Rented Bike Count') +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_smooth(method = 'lm', se = FALSE, color = 'black')
