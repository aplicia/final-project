dat <- read.csv('booking.csv')
# Remove booking ID and date of reservation
hotel_data <- subset(dat, select = -c(Booking_ID, date.of.reservation))


# Change characters into factors
char_indices <- which(sapply(hotel_data, is.character))
hotel_data[, char_indices] <- lapply(hotel_data[, char_indices], as.factor)
str(hotel_data)

# Reorder factors so that not cancelled is basline.
hotel_data$booking.status <- relevel(hotel_data$booking.status, ref = "Not_Canceled")
levels(hotel_data$booking.status)

# Split data into test and training sets
set.seed(123)
n <- nrow(hotel_data)
train_indices <- sample(1:n, size = round(0.8 * n))

train_data <- hotel_data[train_indices, ]
test_data  <- hotel_data[-train_indices, ]


# Elastic Net

X <- model.matrix(booking.status ~ ., data = hotel_data)[, -1]
y <- hotel_data$booking.status

library(glmnet)
fit_stable <- cv.glmnet(X, y, family = "binomial", alpha = 0.5)