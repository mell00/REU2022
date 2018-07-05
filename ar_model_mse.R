model = ar(data)
predic_values = model$resid[-1]
value = predict(model, 1)
one = data[1] - value$pred[1]
predic_values = c(predic_values, one)
mse = mean(predic_values^2)
