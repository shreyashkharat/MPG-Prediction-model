# Required packages : readxl, ggplot2
require("readxl")
car_data <- read_excel("/home/shreyashkharat/Datasets/Cardata.xlsx", sheet = "Sheet1")
pairs(~mpg + cyl + disp + HP + wt + accel, data = car_data)
car_data_fixed <- read_excel("/home/shreyashkharat/Datasets/Cardata.xlsx", sheet = "Sheet1")

#Variable transformation of disp variable
car_data$disp_trans <- log(1 + car_data_fixed$disp)
pairs(~mpg + disp_trans, data = car_data)
car_data$disp_tran <- exp(car_data_fixed$disp/100)
pairs(~mpg + disp_tran, data = car_data)
car_data <- car_data[, -2]
# Clearly disp_trans can fit a better linear model
car_data <- car_data[, -7]
pairs(~mpg + cyl + disp_trans + HP + wt + accel, data = car_data)

# Variable transformation of HP
car_data$HP <- log(1 + car_data_fixed$HP)
pairs(~mpg + HP, data = car_data)
pairs(~mpg + cyl + disp_trans + HP + wt + accel, data = car_data)

# Variable transformation for wt
car_data$wt <- (log(1 + car_data_fixed$wt))/100
pairs(~mpg + cyl + disp_trans + HP + wt + accel, data = car_data)

# Variable transformation for accel
car_data$accel_trans <- log(car_data_fixed$accel)
pairs(~accel_trans + mpg, data = car_data)
pairs(~mpg + accel +accel_trans, data = car_data)
car_data <- car_data[, -4]

cor(car_data)
# As correlation in mpg and accel trans is 0.43, we remove it.
car_data <- car_data[, -6]

#Forming linear model
Model1 <- lm(mpg~., data = car_data)
summary(Model1)

#Test train split
dt = sort(sample(nrow(car_data), nrow(car_data)*.8))
train_set <- car_data[dt,]
test_set <- car_data[-dt,]
testing_model <- lm(mpg~., data = train_set)
#Test_MSE
train = predict(testing_model, train_set)
test = predict(testing_model, test_set)
mean((train_set$mpg - train)^2)
mean((test_set$mpg - test)^2)
pairs(~mpg + cyl + wt + HP + disp_trans, data = car_data)
summary(testing_model)

# New model
# As disp and cyl are highly cor, hence we remove cyl.
# Also disp and wt are highly cor, hence we remove disp.
# The removed variables above less mod(cor) with mpg than the other.
car_data2 <- car_data
car_data2 <- car_data2[, -1]
car_data2 <- car_data2[, -4]
car_data2$HP2 <- car_data2$HP^2
car_data2$wt2 <- car_data2$wt^2
model_updated <- lm(mpg~., data = car_data2)
summary(model_updated)

# Polynomial Regression model
model_poly <- lm(mpg ~ poly(wt, 11) + poly(HP, 12), data = car_data2)
summary(model_poly)
# Testing model on Train test split
model_poly_train <- lm(mpg ~ poly(wt, 11) + poly(HP, 12), data = train_set)
train_poly = predict(model_poly_train, train_set)
test_poly = predict(model_poly_train, test_set)
mean((train_set$mpg - train_poly)^2)
mean((test_set$mpg - test_poly)^2)
summary(model_poly_train)
# The Polynomial Regression Model is better than the Linear Regression Model.
