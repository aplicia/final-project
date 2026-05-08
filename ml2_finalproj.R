# TODO
# Investigate possible transformations
# You must evaluate the predictive performance of the competing methods using the held-
# out test set. Use appropriate regression criteria such as test MSE, RMSE, MAE, or other justified measures.

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
test_error_rate <- mean(val_preds != actual_values)
print(test_error_rate)

rmse_val <- sqrt(mean((val_probs - val_preds)^2))
print(rmse_val)
# table(Predicted = val_preds, Actual = actual_values)

# Extract summary coefficients

model_summary <- as.data.frame(summary(logit_model)$coefficients)
colnames(model_summary) <- c("Estimate", "Std_Error", "z_value", "p_value")

# Filter for significant variables
significant_drivers <- model_summary[model_summary$p_value < 0.05, ]
print(significant_drivers)


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

# mean(rf_preds != test_data$booking.status)

# Define the range of mtry to test from 1 to 10
# mtry_values <- seq(1, 10, by = 1)
# oob_errors <- numeric(length(mtry_values))
# 
# for (i in seq_along(mtry_values)) {
#   temp_mod <- ranger(booking.status ~ ., 
#                      data = train_data, 
#                      mtry = mtry_values[i],
#                      num.trees = 500)
#   
#   # Record the OOB prediction error
#   oob_errors[i] <- temp_mod$prediction.error
# }
# 
# # Find the best mtry
# best_mtry <- mtry_values[which.min(oob_errors)]
# plot(mtry_values, oob_errors, type = "b", pch = 19, col = "blue",
#      main = "Tuning mtry via OOB Error")


# Elastic Net

X <- model.matrix(booking.status ~ ., data = hotel_data)[, -1]
y <- hotel_data$booking.status

library(glmnet)
fit_stable <- cv.glmnet(X, y, family = "binomial", alpha = 0.5)


library(mgcv)

X_test <- model.matrix(booking.status ~ ., data = test_data)[, -1]
y_test <- test_data$booking.status
en_preds <- predict(fit_stable, newx = X_test, s = "lambda.min", type = "class")

# Convert to factor
en_preds <- factor(en_preds, levels = levels(y_test))
mean(en_preds != y_test)
# Plotting the coefficients against the L1 penalty
# Extract coefficients at lambda.min
final_coefs <- coef(fit_stable, s = "lambda.min")
print(final_coefs)

# GAM UNUSED
# gam_mod <- gam(booking.status ~ s(lead.time) + s(average.price) +
#                  type.of.meal + market.segment.type + special.requests,
#                data = train_data,
#                family = "binomial")
# 
# 
# 
# summary(gam_mod)
# 
# plot(gam_mod, pages = 1)
# 
# 
# gam_probs <- predict(gam_mod, newdata = test_data, type = "response")
# 
# gam_preds <- ifelse(gam_probs > 0.5, "Canceled", "Not_Canceled")
# gam_preds <- factor(gam_preds, levels = levels(test_data$booking.status))
# 
# mean(gam_preds != test_data$booking.status)

# library(ggplot2)
# # Boxplot for Lead Time vs Booking Status
# # Plot histograms for your main numeric variables
# par(mfrow=c(2,2)) # View 4 plots at once
# hist(train_data$lead.time, main="Lead Time")
# hist(train_data$average.price, main="Avg Price")
# hist(train_data$number.of.week.nights, main="Week Nights")
# hist(train_data$special.requests, main="Special Requests")


