library('ggplot2')
data <- c(2.1, 2.5, 1.9, 1.5, 2)
predictions <- c(2, 2, 2, 2, 2)

sum((data - predictions)^2)

mean_data = mean(data)
mean_predictions = mean(predictions)

TSS <- sum((data - mean_data)^2)
ESS <- sum((predictions - mean_data)^2)
RSS <- TSS - ESS

R2 <- ESS/TSS

(predictions - mean_data)
predictions
mean_data

data(sleep)
sleep

sleep[6, 1]
mean(sleep$extra)^3
max(sleep$extra) + min(sleep$extra)

round(var(sleep[1:10, 'extra']), 2)

data(mtcars)
mtcars

model <- lm(data=mtcars, mpg~disp+hp+wt+am)
round(summary(model)$r.squared, 2)

coef(model)

model1 <- lm(data=mtcars, mpg~disp+hp+wt+am)
round(summary(model1)$r.squared, 2)

model2 <- lm(data=mtcars, mpg~cyl+hp+wt+am)
round(summary(model2)$r.squared, 2)

model3 <- lm(data=mtcars, mpg~disp+cyl+wt+am)
round(summary(model3)$r.squared, 2)

model4 <- lm(data=mtcars, mpg~disp+hp+cyl+am)
round(summary(model4)$r.squared, 2)
