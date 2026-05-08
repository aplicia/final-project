dat <- read.csv('booking.csv')
# Remove booking ID and date of reservation
hotel_data <- subset(dat, select = -c(Booking_ID, date.of.reservation))

# Change characters into factors
char_indices <- which(sapply(hotel_data, is.character))
hotel_data[, char_indices] <- lapply(hotel_data[, char_indices], as.factor)
str(hotel_data)

# Reorder factors so that not cancelled is baseline.
hotel_data$booking.status <- relevel(hotel_data$booking.status, ref = "Not_Canceled")
levels(hotel_data$booking.status)

# Split data into test and training sets
set.seed(123)
n <- nrow(hotel_data)
train_indices <- sample(1:n, size = round(0.8 * n))

train_data <- hotel_data[train_indices, ]
test_data  <- hotel_data[-train_indices, ]

# Logistic Regression

logit_model <- glm(booking.status ~ ., data = train_data, family = "binomial")
summary(logit_model)

val_probs <- predict(logit_model, newdata = test_data, type = "response")

# Convert to 1 (Canceled) or 0 (Not Canceled)
val_preds <- ifelse(val_probs > 0.5, 1, 0)
actual_values <- ifelse(test_data$booking.status == "Canceled", 1, 0)
# Test Error Rate
test_error_rate <- mean(val_preds != actual_values)
print(test_error_rate)

# Extract summary coefficients

model_summary <- as.data.frame(summary(logit_model)$coefficients)
colnames(model_summary) <- c("Estimate", "Std_Error", "z_value", "p_value")

# Filter for significant variables
significant_vars <- model_summary[model_summary$p_value < 0.05, ]
print(significant_vars)


# Random Forest
# Used ranger instead of randomForest for faster iterations
library(ranger)

rf_mod <- ranger(booking.status ~ ., 
                 data = train_data, 
                 num.trees = 500,
                 importance = "impurity",
                 probability = T)

print(rf_mod)
# Gini Index
importance_scores <- sort(rf_mod$variable.importance, decreasing = TRUE)
print(importance_scores)
barplot(importance_scores, las = 2, main = "Variable Importance (RF)")

rf_preds <- predict(rf_mod, data = test_data)$predictions
rf_probs <- rf_preds[, "Canceled"]

# Convert to binary predictions
rf_preds <- ifelse(rf_probs > 0.5, "Canceled", "Not_Canceled")

# Test Error Rate
test_error_rf <- mean(rf_preds != test_data$booking.status)
print(test_error_rf)

# Elastic Net
X <- model.matrix(booking.status ~ ., data = hotel_data)[, -1]
y <- hotel_data$booking.status

library(glmnet)
fit_stable <- cv.glmnet(X, y, family = "binomial", alpha = 0.5)
library(mgcv)
X_test <- model.matrix(booking.status ~ ., data = test_data)[, -1]
y_test <- test_data$booking.status
en_preds <- predict(fit_stable, newx = X_test, s = "lambda.min", type = "class")
en_preds <- factor(en_preds, levels = levels(y_test))
mean(en_preds != y_test)