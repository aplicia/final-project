dat <- read.csv('booking.csv')
#Remove booking ID and date of reservation
hotel_data <- subset(dat, select = -c(Booking_ID, date.of.reservation))


# Change characters into factors
char_indices <- which(sapply(hotel_data, is.character))
hotel_data[, char_indices] <- lapply(hotel_data[, char_indices], as.factor)
str(hotel_data)


hotel_data$booking.status <- relevel(hotel_data$booking.status, ref = "Not_Canceled")
levels(hotel_data$booking.status)
