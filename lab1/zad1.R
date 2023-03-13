# TRAINING

x = seq(0, 5, 0.01)
y = x ^ 7 - 4 * x ^ 6 - 10 * x ^ 4 - 100 * x - 0.1

set.seed(9) 
noise_training = rnorm(n = length(y), mean = 0, sd = 9)
y_training = y + noise_training
plot(x, y, xlab = "x", ylab = "y", main = "Dane mocno zaszumione")

model1 = lm(formula = y ~ I(x^3) + I(x^2) + x)
points(
  x = x,
  y = predict(model1, data.frame(x)),
  type = "l",
  col = "green"
)
model2 = lm(formula = y ~ I(x^5) + I(x^4) + I(x^3) + I(x^2) + x)
points(
  x = x,
  y = predict(model2, data.frame(x)),
  type = "l",
  col = "red"
)
model3 = lm(formula = y ~ I(x^7) + I(x^6) +I(x^5) + I(x^4) + I(x^3) + I(x^2) + x)
points(
  x = x,
  y = predict(model3, data.frame(x)),
  type = "l",
  col = "blue"
)

error1 = y_training - predict(model1, data.frame(x)) 
modelError1 = mean(error1 ^ 2)

error2 = y_training - predict(model2, data.frame(x)) 
modelError2 = mean(error2 ^ 2) 

error3 = y_training - predict(model3, data.frame(x))
modelError3 = mean(error3 ^ 2) 

sprintf("Błędy treningowe kolejnych modeli: %.2f, %.2f, %.2f", modelError1, modelError2, modelError3)

# TEST

x = seq(0, 5, 0.01)
y = x ^ 7 - 4 * x ^ 6 - 10 * x ^ 4 - 100 * x - 0.1

set.seed(99) 
noise_test = rnorm(n = length(y), mean = 0, sd = 2)
y_test = y + noise_test
plot(x, y, xlab = "x", ylab = "y", main = "Dane lekko zaszumione")

points(
  x = x,
  y = predict(model1, data.frame(x)),
  type = "l",
  col = "green"
)
points(
  x = x,
  y = predict(model2, data.frame(x)),
  type = "l",
  col = "red"
)
points(
  x = x,
  y = predict(model3, data.frame(x)),
  type = "l",
  col = "blue"
)

error1 = y_test - predict(model1, data.frame(x)) 
modelError1 = mean(error1 ^ 2)

error2 = y_test - predict(model2, data.frame(x)) 
modelError2 = mean(error2 ^ 2) 

error3 = y_test - predict(model3, data.frame(x))
modelError3 = mean(error3 ^ 2) 

sprintf("Błędy testowe kolejnych modeli: %.2f, %.2f, %.2f", modelError1, modelError2, modelError3)

