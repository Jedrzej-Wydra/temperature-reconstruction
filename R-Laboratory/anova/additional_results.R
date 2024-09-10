source("custom_coeficients_functions.R")

temptime <- 1:23
temprange <- c(1, 23)
results <- data.frame()
bk <- 3
distribution <- data.frame()
distribution2 <- data.frame()


tempbasis <- create.fourier.basis(temprange, nbasis=bk)
weatherbasis <- create.fourier.basis(temprange, nbasis=bk)
  
full_data_temperature_Y <- cbind(test_sets[['field']]$Y, train_sets[['field']]$Y)
full_data_temperature_X <- cbind(test_sets[['field']]$X, train_sets[['field']]$X)
  
tempfd <- smooth.basis(temptime, full_data_temperature_Y, tempbasis, method = 'qr')$fd
weatherfd <- smooth.basis(temptime, full_data_temperature_X, weatherbasis, method = 'qr')$fd
  
smoothed_full_data_temperature_Y <- eval.fd(1:23,tempfd)
smoothed_full_data_temperature_X <- eval.fd(1:23,weatherfd)

data_control <- data.frame(
  date = seq(from = dataset_august_pila$date[1], len = 345, by = 'hour'),
  weather = as.numeric(smoothed_full_data_temperature_X),
  measurements = as.numeric(smoothed_full_data_temperature_Y)
)

data_control %>%
  ggplot(aes(date, measurements)) +
  geom_line(color = '#F8766D') +
  geom_line(aes(y = weather), color = '#619CFF') +
  theme_bw() +
  ggtitle('Comparison of data from weather station and control measurements')

shapiro.test(data_control$weather)
shapiro.test(data_control$measurements)

mean(abs(data_control$weather - data_control$measurements))

custom_coefs(smoothed_full_data_temperature_Y, tempfd)
custom_coefs(smoothed_full_data_temperature_X, weatherfd)

const_measurements <- custom_coefs(smoothed_full_data_temperature_Y, tempfd)[1,]
const_weather <- custom_coefs(smoothed_full_data_temperature_X, tempfd)[1,]

shapiro.test(const_weather)
shapiro.test(const_measurements)

t.test(const_measurements, const_weather, paired = TRUE)

hist(const_measurements - const_weather)

summary(lm(const_measurements ~ const_weather))

plot(const_weather, const_measurements)
abline(a=2.03774, b=0.86577)

sin_measurements <- custom_coefs(smoothed_full_data_temperature_Y, tempfd)[2,]
sin_weather <- custom_coefs(smoothed_full_data_temperature_X, tempfd)[2,]

shapiro.test(sin_measurements)
shapiro.test(sin_weather)

t.test(sin_measurements, sin_weather, paired = TRUE)

hist(sin_measurements - sin_weather)

plot(sin_measurements, sin_weather)
abline(a=0, b=1)

cos_measurements <- custom_coefs(smoothed_full_data_temperature_Y, tempfd)[3,]
cos_weather <- custom_coefs(smoothed_full_data_temperature_X, tempfd)[3,]

shapiro.test(cos_measurements)
shapiro.test(cos_weather)

t.test(cos_measurements, cos_weather, paired = TRUE)

hist(cos_measurements - cos_weather)

plot(cos_measurements, cos_weather)
abline(a=0, b=1)
