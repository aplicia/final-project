# TODO
# Tuning procedures
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


# Random Forest
# Used ranger instead of randomForest for faster iterations
library(ranger)

rf_mod <- ranger(booking.status ~ ., 
                 data = train_data, 
                 num.trees = 500,
                 importance = "impurity",
                 classification = TRUE)

print(rf_mod)

importance_scores <- sort(rf_mod$variable.importance, decreasing = TRUE)
print(importance_scores)
barplot(importance_scores, las = 2, main = "Variable Importance (RF)")

rf_preds <- predict(rf_mod, data = test_data)$predictions
mean(rf_preds != test_data$booking.status)

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

# GAM
gam_mod <- gam(booking.status ~ s(lead.time) + s(average.price) + 
                 type.of.meal + market.segment.type + special.requests, 
               data = train_data, 
               family = "binomial")



summary(gam_mod)

plot(gam_mod, pages = 1)


gam_probs <- predict(gam_mod, newdata = test_data, type = "response")

gam_preds <- ifelse(gam_probs > 0.5, "Canceled", "Not_Canceled")
gam_preds <- factor(gam_preds, levels = levels(test_data$booking.status))

mean(gam_preds != test_data$booking.status)






